(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (message "Initializing exec-path-from-shell")
    (when (string= system-type "darwin")
      (setq exec-path-from-shell-shell-name "/Users/ngoc/.nix-profile/bin/bash"))
    (exec-path-from-shell-initialize)
    (exec-path-from-shell-copy-env "SSH_AGENT_PID")
    (exec-path-from-shell-copy-env "SSH_AUTH_SOCK")
    ))

(provide 'init-exec-path)
