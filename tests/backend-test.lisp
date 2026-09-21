(in-package #:agent-runtime-backend-podman/tests)

(defun %log-fn (box)
  (lambda (argv)
    (push (copy-list argv) (car box))
    (values 0 "cid-1" "")))

(defun %spec (&key (name "demo") debug secrets network)
  (agent-runtime-protocol:make-runtime-task-spec
   :name name
   :image "alpine:latest"
   :command '("sleep" "infinity")
   :debug debug
   :network (or network
                (agent-runtime-protocol:make-runtime-network-policy
                 :egress '(("example.com" 443))))
   :workspaces (list (agent-runtime-protocol:make-runtime-workspace-spec
                      :name "ws"))
   :secrets secrets))

(deftest apply-run-argv
  (let* ((box (list nil))
         (backend (agent-runtime-backend-podman:make-podman-runtime-backend
                   :invoke-fn (%log-fn box)))
         (status (agent-runtime-protocol:apply-task backend (%spec))))
    (ok (eq :running (agent-runtime-protocol:runtime-status-phase status)))
    (ok (agent-runtime-protocol:runtime-condition-status
         (agent-runtime-protocol:find-runtime-condition status :ready)))
    (let ((argv (first (car box))))
      (ok (equal "podman" (first argv)))
      (ok (equal "run" (second argv)))
      (ok (equal "-d" (third argv)))
      (ok (find "runtime-demo" argv :test #'equal))
      (ok (search '("--network" "none") argv :test #'equal))
      (ok (find "alpine:latest" argv :test #'equal)))))

(deftest suspend-keeps-workspace-resume-reruns
  (let* ((box (list nil))
         (backend (agent-runtime-backend-podman:make-podman-runtime-backend
                   :invoke-fn (%log-fn box)))
         (name "keep"))
    (agent-runtime-protocol:apply-task backend (%spec :name name))
    (let* ((ws (agent-runtime-backend-podman:podman-runtime-workspace-path
                backend name "ws"))
           (notes (merge-pathnames "notes.txt" ws)))
      (with-open-file (out notes :direction :output :if-exists :supersede)
        (write-string "keep-me" out))
      (let ((suspended (agent-runtime-protocol:suspend-task backend name)))
        (ok (eq :suspended (agent-runtime-protocol:runtime-status-phase suspended)))
        (ok (eq :task-suspended
                (agent-runtime-protocol:runtime-condition-reason
                 (agent-runtime-protocol:find-runtime-condition suspended :ready))))
        (ok (equal "keep-me" (uiop:read-file-string notes))))
      (let ((resumed (agent-runtime-protocol:resume-task backend name)))
        (ok (eq :running (agent-runtime-protocol:runtime-status-phase resumed)))
        (ok (equal "keep-me" (uiop:read-file-string notes))))
      (let ((argv (reverse (car box))))
        (ok (equal "stop" (second (second argv))))
        (ok (equal "rm" (second (third argv))))
        (ok (equal "run" (second (fourth argv))))))))

(deftest secrets-journal-refs-not-material
  (let* ((box (list nil))
         (journal (task-protocol:make-in-memory-journal))
         (store (secrets-protocol:make-in-memory-secret-store
                 :secrets '(("vault" "token" "s3cret"))))
         (ref (agent-runtime-protocol:make-secret-ref
               :name "vault" :key "token" :inject :env))
         (backend (agent-runtime-backend-podman:make-podman-runtime-backend
                   :invoke-fn (%log-fn box)
                   :secret-store store
                   :journal journal)))
    (agent-runtime-protocol:apply-task
     backend (%spec :name "sec" :secrets (list ref)))
    (let* ((argv (first (car box)))
           (events (task-protocol:journal-events journal "sec"))
           (ev (find-if #'task-protocol:runtime-transition-p events)))
      (ok (find "TOKEN=s3cret" argv :test #'equal))
      (ok (task-protocol:runtime-transition-p ev))
      (ok (equal "vault"
                 (getf (first (task-protocol:runtime-transition-secret-refs ev))
                       :name)))
      (ng (search "s3cret"
                  (prin1-to-string (task-protocol:event-plist ev))
                  :test #'char-equal)))))

(deftest egress-deny-other-port
  (let ((backend (agent-runtime-backend-podman:make-podman-runtime-backend
                  :invoke-fn (%log-fn (list nil))))
        (spec (%spec)))
    (ok (agent-runtime-backend-podman:podman-runtime-assert-egress
         backend spec "example.com" 443))
    (ok (signals (agent-runtime-backend-podman:podman-runtime-assert-egress
                  backend spec "example.com" 22)
                 'compute-protocol:sandbox-denied))))

(deftest exec-denied-without-debug
  (let ((backend (agent-runtime-backend-podman:make-podman-runtime-backend
                  :invoke-fn (%log-fn (list nil)))))
    (agent-runtime-protocol:apply-task backend (%spec :name "nodebug"))
    (ok (signals (agent-runtime-protocol:exec-in-task backend "nodebug" '("ls"))
                 'agent-runtime-protocol:runtime-denied))))
