;;;; Live Podman runtime demo. Uses the podman on PATH. No invoke-fn.
;;;; Workspace root is under $HOME: a podman machine does not mount /tmp.
;;;;   sbcl --load examples/lifecycle.lisp

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :agent-runtime-backend-podman)
    (require :asdf)
    (asdf:load-system "agent-runtime-backend-podman")))

(defpackage #:agent-runtime-backend-podman/demo
  (:use #:cl)
  (:local-nicknames (#:rt #:agent-runtime-protocol)
                    (#:podman.rt #:agent-runtime-backend-podman))
  (:export #:run #:podman-present-p))

(in-package #:agent-runtime-backend-podman/demo)

(defvar *image* "docker.io/library/ubuntu:24.04")

(defun podman-present-p ()
  (let ((path (uiop:getenv "PATH")))
    (when path
      (some (lambda (dir)
              (let ((bin (merge-pathnames
                          "podman"
                          (uiop:ensure-directory-pathname dir))))
                (and (probe-file bin)
                     (not (uiop:directory-pathname-p bin)))))
            (uiop:split-string path :separator ":")))))

(defun %demo-root ()
  (uiop:ensure-directory-pathname
   (merge-pathnames
    (format nil ".cache/agent-runtime-podman-demo/~36r-~36r/"
            (get-universal-time)
            (random (expt 36 6)))
    (user-homedir-pathname))))

(defun %task-id ()
  (format nil "d~36r" (random (expt 36 8))))

(defun %setenv (name value)
  (setf (uiop:getenv name) value))

(defun %unsetenv (name)
  #+sbcl
  (progn
    (require :sb-posix)
    (sb-posix:unsetenv name))
  #+ccl
  (ccl:unsetenv name)
  #-(or sbcl ccl)
  (declare (ignore name)))

(defun %write-json (path)
  (ensure-directories-exist path)
  (with-open-file (out path :direction :output :if-exists :supersede
                       :if-does-not-exist :create)
    (write-string "{}" out))
  (uiop:native-namestring path))

(defun %call-with-local-auth (thunk)
  "Empty registry auth when the caller did not set one.
Podman still consults ~/.docker credHelpers for a local image."
  (let ((prev-reg (uiop:getenv "REGISTRY_AUTH_FILE"))
        (prev-docker (uiop:getenv "DOCKER_CONFIG"))
        (auth-root (merge-pathnames ".cache/agent-runtime-podman-auth/"
                                    (user-homedir-pathname)))
        (set-reg nil)
        (set-docker nil))
    (unless prev-reg
      (%setenv "REGISTRY_AUTH_FILE"
               (%write-json (merge-pathnames "auth.json" auth-root)))
      (setf set-reg t))
    (unless prev-docker
      (let ((dir (uiop:ensure-directory-pathname
                  (merge-pathnames "docker/" auth-root))))
        (%write-json (merge-pathnames "config.json" dir))
        (%setenv "DOCKER_CONFIG" (uiop:native-namestring dir))
        (setf set-docker t)))
    (unwind-protect
         (funcall thunk)
      (when set-reg (%unsetenv "REGISTRY_AUTH_FILE"))
      (when set-docker (%unsetenv "DOCKER_CONFIG")))))

(defun %delete-root (root)
  (let ((marker ".cache/agent-runtime-podman-demo/"))
    (when (search marker (namestring root))
      (uiop:delete-directory-tree
       root
       :validate (lambda (path)
                   (search marker (namestring path)))
       :if-does-not-exist :ignore))))

(defun %exec (backend id argv)
  (loop for attempt from 1 to 20
        for result = (rt:exec-in-task backend id argv)
        when (zerop (getf result :exit-code))
          return result
        when (= attempt 20)
          do (error "exec ~s failed: ~s" argv result)
        do (sleep 0.25)))

(defun %lifecycle (stream backend id)
  (let* ((spec (rt:make-runtime-task-spec
                :name id
                :image *image*
                :command '("sleep" "infinity")
                :debug t
                :workspaces (list (rt:make-runtime-workspace-spec :name "ws"))
                :network (rt:make-runtime-network-policy
                          :egress '(("example.com" 443)))))
         (ready (rt:apply-task backend spec))
         (ws (podman.rt:podman-runtime-workspace-path backend id "ws"))
         (notes (merge-pathnames "notes.txt" ws)))
    (assert (eq :running (rt:runtime-status-phase ready)))
    (with-open-file (out notes :direction :output :if-exists :supersede
                         :if-does-not-exist :create)
      (write-string "keep-me" out))
    (let ((seen (%exec backend id '("cat" "/workspace/ws/notes.txt"))))
      (assert (search "keep-me" (getf seen :stdout))))
    (let ((wrote (%exec backend id
                        '("sh" "-c" "printf ' from-ctr' >> /workspace/ws/notes.txt"))))
      (assert (zerop (getf wrote :exit-code))))
    (assert (equal "keep-me from-ctr" (uiop:read-file-string notes)))
    (let ((suspended (rt:suspend-task backend id)))
      (assert (eq :suspended (rt:runtime-status-phase suspended)))
      (assert (equal "keep-me from-ctr" (uiop:read-file-string notes))))
    (let ((resumed (rt:resume-task backend id)))
      (assert (eq :running (rt:runtime-status-phase resumed)))
      (let ((seen (%exec backend id '("cat" "/workspace/ws/notes.txt"))))
        (assert (search "keep-me from-ctr" (getf seen :stdout))))
      (format stream "~&; apply=:running suspend=:suspended resume=~s notes=~s~%"
              (rt:runtime-status-phase resumed)
              (string-trim '(#\Newline) (uiop:read-file-string notes)))
      resumed)))

(defun run (&optional (stream *standard-output*))
  (unless (podman-present-p)
    (error "podman is not on PATH"))
  (let ((root (%demo-root))
        (id (%task-id))
        (backend nil))
    (%call-with-local-auth
     (lambda ()
       (setf backend (podman.rt:make-podman-runtime-backend :root root))
       (unwind-protect
            (%lifecycle stream backend id)
         (when backend
           (ignore-errors (rt:delete-task backend id)))
         (%delete-root root))))))

;;; SBCL 2.6 drops --load from *posix-argv*, so the script name is not there.
;;; Tests bind CL-USER::*AGENT-RUNTIME-PODMAN-DEMO-NO-AUTORUN* before loading.
#+sbcl
(when (and *load-truename*
           (not (boundp 'cl-user::*agent-runtime-podman-demo-no-autorun*))
           (equal (pathname-name *load-truename*) "lifecycle"))
  (run)
  (uiop:quit 0))
