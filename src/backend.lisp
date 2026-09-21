(in-package #:agent-runtime-backend-podman)

;;; Long-lived Podman tasks. Suspend = persist host workspace + journal,
;;; then stop/rm the container. Resume = new container on the same mounts.
;;; No CRIU. Tests inject INVOKE-FN — podman does not need to be installed.

(defclass podman-runtime-backend (rt:runtime-backend)
  ((root :initarg :root :accessor podman-runtime-root)
   (default-image :initarg :default-image :accessor podman-runtime-default-image
                  :initform "sbcl")
   (invoke-fn :initarg :invoke-fn :accessor podman-runtime-invoke-fn
              :initform nil)
   (secret-store :initarg :secret-store :accessor podman-runtime-secret-store
                 :initform nil)
   (journal :initarg :journal :accessor podman-runtime-journal :initform nil)
   (tasks :initform (make-hash-table :test 'equal)
          :reader podman-runtime-tasks)))

(defclass podman-runtime-task ()
  ((id :initarg :id :accessor podman-runtime-task-id)
   (spec :initarg :spec :accessor podman-runtime-task-spec)
   (status :initarg :status :accessor podman-runtime-task-status)
   (root :initarg :root :accessor podman-runtime-task-root)
   (container :initarg :container :accessor podman-runtime-task-container
              :initform nil)))

(defun podman-runtime-backend-p (object)
  (typep object 'podman-runtime-backend))

(defun %unique-temp-root ()
  (loop
    (let ((path (merge-pathnames
                 (format nil "podman-runtime-~a/" (random (expt 36 8)))
                 (uiop:ensure-directory-pathname (uiop:temporary-directory)))))
      (unless (probe-file path)
        (return (uiop:ensure-directory-pathname path))))))

(defun make-podman-runtime-backend (&key root default-image invoke-fn
                                      secret-store journal)
  (let ((root (uiop:ensure-directory-pathname
               (or root (%unique-temp-root)))))
    (ensure-directories-exist root)
    (make-instance 'podman-runtime-backend
                   :root root
                   :default-image (or default-image "sbcl")
                   :invoke-fn invoke-fn
                   :secret-store secret-store
                   :journal journal)))

(defun use-podman-runtime-backend (&rest args &key &allow-other-keys)
  (setf rt:*runtime-backend* (apply #'make-podman-runtime-backend args)))

(defun %safe-name (name)
  (substitute #\- #\/ (string name)))

(defun %container-name (id)
  (format nil "runtime-~a" (%safe-name id)))

(defun %require-name (spec)
  (let ((name (rt:runtime-task-spec-name spec)))
    (unless (and name (plusp (length name)))
      (error 'rt:runtime-error :message "runtime-task-spec needs :name"))
    name))

(defun %get-task (backend id)
  (or (gethash id (podman-runtime-tasks backend))
      (error 'rt:runtime-not-found
             :id id
             :message (format nil "unknown task ~s" id))))

(defun %phase (task)
  (rt:runtime-status-phase (podman-runtime-task-status task)))

(defun %ensure-phase (task expected)
  (let* ((phase (%phase task))
         (ok (if (listp expected)
                 (member phase expected)
                 (eq phase expected))))
    (unless ok
      (error 'rt:runtime-invalid-phase
             :id (podman-runtime-task-id task)
             :phase phase
             :expected expected
             :message (format nil "task ~s is ~s, expected ~s"
                              (podman-runtime-task-id task) phase expected)))
    task))

(defun %ready-status (&key (phase :running) (ready t) reason snapshot-ref)
  (rt:make-runtime-status
   :phase phase
   :conditions (list (rt:make-runtime-condition :type :workspace-ready :status t)
                     (rt:make-runtime-condition :type :gateway-ready :status t)
                     (rt:make-runtime-condition :type :ready :status ready
                                                :reason reason))
   :snapshot-ref snapshot-ref
   :worker-id "podman"))

(defun %workspace-rel (ws)
  (%safe-name (or (rt:runtime-workspace-spec-path ws)
                  (rt:runtime-workspace-spec-name ws)
                  "workspace")))

(defun podman-runtime-workspace-path (backend id &optional workspace-name)
  (let* ((task (%get-task backend id))
         (root (uiop:ensure-directory-pathname (podman-runtime-task-root task))))
    (if workspace-name
        (uiop:ensure-directory-pathname
         (merge-pathnames (uiop:parse-unix-namestring
                           (format nil "~a/" (%safe-name workspace-name)))
                          root))
        root)))

(defun %materialize-workspaces (task-root spec)
  (ensure-directories-exist task-root)
  (dolist (ws (rt:runtime-task-spec-workspaces spec))
    (let ((ws-dir (uiop:ensure-directory-pathname
                   (merge-pathnames (uiop:parse-unix-namestring
                                     (format nil "~a/" (%workspace-rel ws)))
                                    task-root))))
      (ensure-directories-exist ws-dir)))
  task-root)

(defun secret-ref-plist (ref)
  "Journalable handle. Never includes material."
  (list :name (rt:secret-ref-name ref)
        :key (rt:secret-ref-key ref)
        :inject (rt:secret-ref-inject ref)))

(defun %resolve-secret (backend ref)
  (let ((store (podman-runtime-secret-store backend)))
    (unless store
      (error 'rt:runtime-denied
             :policy :secret
             :message "secret-ref present but no secret-store"))
    (sec:resolve-secret
     store
     (sec:make-secret-ref :name (rt:secret-ref-name ref)
                          :key (rt:secret-ref-key ref)
                          :inject (rt:secret-ref-inject ref)))))

(defun %secret-env-name (ref)
  (substitute #\_ #\- (string-upcase (rt:secret-ref-key ref))))

(defun %secret-file (task-root ref)
  (merge-pathnames
   (uiop:parse-unix-namestring
    (format nil ".secrets/~a-~a" (rt:secret-ref-name ref) (rt:secret-ref-key ref)))
   (uiop:ensure-directory-pathname task-root)))

(defun %inject-secrets (backend task-root spec)
  "Return (values env-pairs extra-mounts journal-refs). Material is transient."
  (let ((env nil)
        (mounts nil)
        (refs nil))
    (dolist (ref (rt:runtime-task-spec-secrets spec))
      (push (secret-ref-plist ref) refs)
      (let ((material (%resolve-secret backend ref)))
        (ecase (rt:secret-ref-inject ref)
          (:env (push (cons (%secret-env-name ref) material) env))
          (:file
           (let ((path (%secret-file task-root ref)))
             (ensure-directories-exist path)
             (with-open-file (out path :direction :output :if-exists :supersede
                                  :if-does-not-exist :create)
               (write-string material out))
             (push (list path (format nil "/run/secrets/~a"
                                      (rt:secret-ref-key ref)))
                   mounts))))))
    (values (nreverse env) (nreverse mounts) (nreverse refs))))

(defun %network-flag (network)
  (cond
    ((or (null network) (eq network :none) (rt:runtime-network-policy-p network))
     '("--network" "none"))
    ((eq network :allow) nil)
    (t '("--network" "none"))))

(defun %egress-annotation (network)
  (when (rt:runtime-network-policy-p network)
    '("--annotation" "compute-protocol.egress-proxy=1")))

(defun %mount-arg (host dest &optional (mode "rw"))
  (format nil "~a:~a:~a"
          (etypecase host
            (pathname (uiop:native-namestring host))
            (string host))
          dest
          mode))

(defun %workspace-mounts (task-root spec)
  (let ((root (uiop:ensure-directory-pathname task-root)))
    (or (mapcar (lambda (ws)
                  (let* ((rel (%workspace-rel ws))
                         (host (uiop:ensure-directory-pathname
                                (merge-pathnames
                                 (uiop:parse-unix-namestring (format nil "~a/" rel))
                                 root)))
                         (dest (format nil "/workspace/~a"
                                       (rt:runtime-workspace-spec-name ws))))
                    (%mount-arg host dest "rw")))
                (rt:runtime-task-spec-workspaces spec))
        (list (%mount-arg root "/workspace" "rw")))))

(defun %image (backend spec)
  (or (rt:runtime-task-spec-image spec)
      (podman-runtime-default-image backend)
      "sbcl"))

(defun %command (spec)
  (or (rt:runtime-task-spec-command spec)
      '("sleep" "infinity")))

(defun podman-runtime-argv (backend spec &key action id env extra-mounts)
  "Build argv. ACTION is :run :stop :rm :exec. Does not invoke podman."
  (let* ((spec (rt:coerce-runtime-task-spec spec))
         (id (or id (%require-name spec)))
         (name (%container-name id))
         (network (rt:runtime-task-spec-network spec)))
    (ecase action
      (:run
       (let ((acc (list "podman" "run" "-d" "--name" name)))
         (setf acc (append acc (%network-flag network) (%egress-annotation network)))
         (dolist (m (%workspace-mounts
                     (merge-pathnames (uiop:parse-unix-namestring
                                       (format nil "~a/" (%safe-name id)))
                                      (podman-runtime-root backend))
                     spec))
           (setf acc (append acc (list "-v" m))))
         (dolist (m extra-mounts)
           (setf acc (append acc (list "-v" (%mount-arg (first m) (second m) "ro")))))
         (dolist (pair env)
           (setf acc (append acc (list "-e" (format nil "~a=~a" (car pair) (cdr pair))))))
         (let ((res (rt:runtime-task-spec-resources spec)))
           (when (getf res :memory)
             (setf acc (append acc (list "--memory" (princ-to-string (getf res :memory))))))
           (when (getf res :cpu)
             (setf acc (append acc (list "--cpus" (princ-to-string (getf res :cpu)))))))
         (append acc (list (%image backend spec)) (%command spec))))
      (:stop (list "podman" "stop" name))
      (:rm (list "podman" "rm" "-f" name))
      (:exec (error 'rt:runtime-error :message "exec argv needs ARGV — use exec-in-task")))))

(defun %compute-spec (backend spec)
  "Projection for assert-egress-allowed."
  (let ((network (rt:runtime-task-spec-network spec)))
    (compute:make-sandbox-spec
     :command (%command spec)
     :runtime (%image backend spec)
     :network (cond
                ((eq network :allow) :allow)
                ((eq network :none) :none)
                ((rt:runtime-network-policy-p network)
                 (compute:make-sandbox-network-policy
                  :egress (mapcar (lambda (r)
                                    (compute:make-egress-rule
                                     :host (rt:runtime-egress-rule-host r)
                                     :port (rt:runtime-egress-rule-port r)))
                                  (rt:runtime-network-policy-egress network))))
                (t :none)))))

(defun podman-runtime-assert-egress (backend spec host port)
  (podman:assert-egress-allowed (%compute-spec backend spec) host port))

(defun %invoke (backend argv)
  (let ((fn (podman-runtime-invoke-fn backend)))
    (if fn
        (funcall fn argv)
        (multiple-value-bind (out err code)
            (uiop:run-program argv
                              :ignore-error-status t
                              :output '(:string :stripped nil)
                              :error-output '(:string :stripped nil))
          (values (or code 0) (or out "") (or err ""))))))

(defun %journal (backend task from to &key secret-refs)
  (let ((journal (podman-runtime-journal backend)))
    (when journal
      (task:journal-runtime-transition
       journal
       :task-id (podman-runtime-task-id task)
       :runtime-id (podman-runtime-task-id task)
       :from-phase from
       :to-phase to
       :snapshot-ref (uiop:native-namestring (podman-runtime-task-root task))
       :worker-id "podman"
       :secret-refs secret-refs))))

(defun %start-container (backend task &key (from :pending))
  (let* ((spec (podman-runtime-task-spec task))
         (root (podman-runtime-task-root task)))
    (%materialize-workspaces root spec)
    (multiple-value-bind (env mounts refs)
        (%inject-secrets backend root spec)
      (let ((argv (podman-runtime-argv backend spec
                                       :action :run
                                       :id (podman-runtime-task-id task)
                                       :env env
                                       :extra-mounts mounts)))
        (multiple-value-bind (code out err)
            (%invoke backend argv)
          (unless (zerop code)
            (error 'rt:runtime-error
                   :id (podman-runtime-task-id task)
                   :message (format nil "podman run failed (~a): ~a ~a"
                                    code out err)))
          (setf (podman-runtime-task-container task)
                (string-trim '(#\Space #\Newline #\Return #\Tab) out))
          (let ((status (%ready-status
                         :phase :running :ready t
                         :snapshot-ref (uiop:native-namestring root))))
            (setf (podman-runtime-task-status task) status)
            (%journal backend task from :running :secret-refs refs)
            status))))))

(defmethod rt:apply-task ((backend podman-runtime-backend) spec)
  (let* ((spec (rt:coerce-runtime-task-spec spec))
         (id (%require-name spec)))
    (when (gethash id (podman-runtime-tasks backend))
      (error 'rt:runtime-invalid-phase
             :id id
             :phase (%phase (gethash id (podman-runtime-tasks backend)))
             :expected :pending
             :message (format nil "task ~s already exists" id)))
    (let* ((root (uiop:ensure-directory-pathname
                  (merge-pathnames (uiop:parse-unix-namestring
                                    (format nil "~a/" (%safe-name id)))
                                   (podman-runtime-root backend))))
           (task (make-instance 'podman-runtime-task
                                :id id :spec spec
                                :status (%ready-status :phase :pending :ready nil)
                                :root root)))
      (setf (gethash id (podman-runtime-tasks backend)) task)
      (%start-container backend task :from :pending))))

(defmethod rt:describe-task ((backend podman-runtime-backend) id)
  (podman-runtime-task-status (%get-task backend id)))

(defmethod rt:suspend-task ((backend podman-runtime-backend) id)
  (let ((task (%get-task backend id)))
    (%ensure-phase task :running)
    (%invoke backend (podman-runtime-argv backend (podman-runtime-task-spec task)
                                          :action :stop :id id))
    (%invoke backend (podman-runtime-argv backend (podman-runtime-task-spec task)
                                          :action :rm :id id))
    (setf (podman-runtime-task-container task) nil)
    (let ((status (%ready-status
                   :phase :suspended :ready nil :reason :task-suspended
                   :snapshot-ref (uiop:native-namestring
                                  (podman-runtime-task-root task)))))
      (setf (podman-runtime-task-status task) status)
      (%journal backend task :running :suspended
                :secret-refs (mapcar #'secret-ref-plist
                                     (rt:runtime-task-spec-secrets
                                      (podman-runtime-task-spec task))))
      status)))

(defmethod rt:resume-task ((backend podman-runtime-backend) id)
  (let ((task (%get-task backend id)))
    (%ensure-phase task :suspended)
    (%start-container backend task :from :suspended)))

(defmethod rt:delete-task ((backend podman-runtime-backend) id)
  (let ((task (%get-task backend id)))
    (ignore-errors
      (%invoke backend (podman-runtime-argv backend (podman-runtime-task-spec task)
                                            :action :stop :id id)))
    (ignore-errors
      (%invoke backend (podman-runtime-argv backend (podman-runtime-task-spec task)
                                            :action :rm :id id)))
    (let ((status (%ready-status
                   :phase :terminating
                   :ready nil
                   :snapshot-ref (uiop:native-namestring
                                  (podman-runtime-task-root task)))))
      (%journal backend task (%phase task) :terminating)
      (remhash id (podman-runtime-tasks backend))
      status)))

(defmethod rt:exec-in-task ((backend podman-runtime-backend) id argv)
  (check-type argv list)
  (let* ((task (%get-task backend id))
         (spec (podman-runtime-task-spec task)))
    (unless (rt:runtime-task-spec-debug spec)
      (error 'rt:runtime-denied
             :id id :policy :debug
             :message "exec-in-task requires :debug t"))
    (%ensure-phase task :running)
    (let ((cmd (append (list "podman" "exec" (%container-name id))
                       (mapcar (lambda (x)
                                 (if (stringp x) x (princ-to-string x)))
                               argv))))
      (multiple-value-bind (code out err)
          (%invoke backend cmd)
        (list :exit-code code :stdout out :stderr err :argv cmd)))))
