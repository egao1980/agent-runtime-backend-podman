(defsystem "agent-runtime-backend-podman"
  :version "0.1.0"
  :description "Podman backend for agent-runtime-protocol (long-lived fenced tasks, no CRIU)"
  :author "egao1980"
  :license "MIT"
  :depends-on ("agent-runtime-protocol" "compute-protocol" "compute-backend-podman"
               "secrets-protocol/store" "task-protocol")
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "backend"))
  :in-order-to ((test-op (test-op "agent-runtime-backend-podman/tests"))))

(defsystem "agent-runtime-backend-podman/tests"
  :depends-on ("agent-runtime-backend-podman" "rove")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "backend-test")
               (:file "demo-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
