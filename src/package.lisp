(defpackage #:agent-runtime-backend-podman
  (:use #:cl)
  (:local-nicknames (#:rt #:agent-runtime-protocol)
                    (#:compute #:compute-protocol)
                    (#:podman #:compute-backend-podman)
                    (#:sec #:secrets-protocol)
                    (#:task #:task-protocol))
  (:export #:podman-runtime-backend
           #:podman-runtime-backend-p
           #:make-podman-runtime-backend
           #:use-podman-runtime-backend
           #:podman-runtime-root
           #:podman-runtime-default-image
           #:podman-runtime-workspace-path
           #:podman-runtime-argv
           #:podman-runtime-assert-egress
           #:secret-ref-plist))

(in-package #:agent-runtime-backend-podman)
