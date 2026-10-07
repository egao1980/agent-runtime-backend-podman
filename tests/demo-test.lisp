(in-package #:agent-runtime-backend-podman/tests)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar cl-user::*agent-runtime-podman-demo-no-autorun* t)
  (load (asdf:system-relative-pathname "agent-runtime-backend-podman"
                                       "examples/lifecycle.lisp")))

(deftest lifecycle-demo-runs
  (if (not (agent-runtime-backend-podman/demo:podman-present-p))
      (skip "podman")
      (let ((status (agent-runtime-backend-podman/demo:run (make-broadcast-stream))))
        (ok (agent-runtime-protocol:runtime-status-p status))
        (ok (eq :running (agent-runtime-protocol:runtime-status-phase status))))))
