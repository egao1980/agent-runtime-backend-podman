# agent-runtime-backend-podman

Local [agent-runtime-protocol](https://github.com/egao1980/agent-runtime-protocol) backend. Compose [`compute-protocol`](https://github.com/egao1980/compute-protocol) + [`compute-backend-podman`](https://github.com/egao1980/compute-backend-podman).

Suspend persists the host workspace and journals a `runtime-transition`. Resume starts a new container on the same mounts. **No CRIU.** CNI does not enforce host/port — call `podman-runtime-assert-egress`.

Tests inject `invoke-fn`. Podman does not need to be installed.

```lisp
(asdf:load-system "agent-runtime-backend-podman")

(let ((backend (agent-runtime-backend-podman:make-podman-runtime-backend
                :invoke-fn (lambda (argv)
                             (declare (ignore argv))
                             (values 0 "cid" "")))))
  (agent-runtime-protocol:apply-task
   backend
   (agent-runtime-protocol:make-runtime-task-spec
    :name "demo"
    :workspaces (list (agent-runtime-protocol:make-runtime-workspace-spec
                       :name "ws")))))
```

```bash
sbcl --load examples/lifecycle.lisp
```

## License

MIT
