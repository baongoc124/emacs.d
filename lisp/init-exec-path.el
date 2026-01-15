(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (message "Initializing exec-path-from-shell")
    (when (string= system-type "darwin")
      (setq exec-path-from-shell-shell-name "/Users/ngoc/.nix-profile/bin/bash")
      (setq exec-path-from-shell-arguments '("-i"))
      )

    (setq exec-path-from-shell-variables
          '(
            ;; default values
            "PATH"
            "MANPATH"

            ;; my setup
            "DOCKER_CLI_HINTS"
            "LANG"
            "LC_CTYPE"
            "NIX_PROFILES"
            "NIX_SSL_CERT_FILE"
            "SSH_AGENT_PID"
            "SSH_AUTH_SOCK"
            "XDG_DATA_DIRS"

            ;; for KTZN
            "GOPRIVATE"
            "BITBUCKET_USERNAME"
            "BITBUCKET_PASSWORD"
            "BITBUCKET_API_TOKEN"
            ))

    (exec-path-from-shell-initialize)
    ))

(provide 'init-exec-path)
