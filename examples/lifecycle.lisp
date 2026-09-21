;;;; Offline Podman runtime demo — inject-fn, no podman binary.
;;;;   sbcl --load examples/lifecycle.lisp

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :agent-runtime-backend-podman)
    (require :asdf)
    (asdf:load-system "agent-runtime-backend-podman")))

(defpackage #:agent-runtime-backend-podman/demo
  (:use #:cl)
  (:local-nicknames (#:rt #:agent-runtime-protocol)
                    (#:podman.rt #:agent-runtime-backend-podman))
  (:export #:run))

(in-package #:agent-runtime-backend-podman/demo)

(defun run (&optional (stream *standard-output*))
  (let* ((log nil)
         (backend (podman.rt:make-podman-runtime-backend
                   :invoke-fn (lambda (argv)
                                (push (copy-list argv) log)
                                (values 0 "cid" ""))))
         (spec (rt:make-runtime-task-spec
                :name "demo"
                :image "alpine:latest"
                :workspaces (list (rt:make-runtime-workspace-spec :name "ws"))
                :network (rt:make-runtime-network-policy
                          :egress '(("example.com" 443)))))
         (ready (rt:apply-task backend spec))
         (ws (podman.rt:podman-runtime-workspace-path backend "demo" "ws"))
         (notes (merge-pathnames "notes.txt" ws)))
    (assert (eq :running (rt:runtime-status-phase ready)))
    (with-open-file (out notes :direction :output :if-exists :supersede)
      (write-string "keep-me" out))
    (rt:suspend-task backend "demo")
    (assert (equal "keep-me" (uiop:read-file-string notes)))
    (let ((resumed (rt:resume-task backend "demo")))
      (format stream "~&; apply=:running suspend=:suspended resume=~s notes=~s~%"
              (rt:runtime-status-phase resumed)
              (uiop:read-file-string notes))
      (assert (eq :running (rt:runtime-status-phase resumed)))
      (rt:delete-task backend "demo")
      resumed)))

#+sbcl
(when (and *load-truename*
           (equal (pathname-name *load-truename*) "lifecycle")
           (find "examples/lifecycle.lisp" sb-ext:*posix-argv* :test #'search))
  (run)
  (uiop:quit 0))
