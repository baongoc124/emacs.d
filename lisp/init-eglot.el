(use-package cape)

(use-package eglot
  :after (transient warnings cape)
  :init
  ;; do not display this warning from
  ;; jsonrpc ‘:events-buffer-scrollback-size’ deprecated. Use ‘events-buffer-config’.
  ;; FIXME so basically hide all warnings from emacs by default...
  (add-to-list 'warning-suppress-types '(emacs))
  :hook
  (prog-mode . eglot-ensure)
  :config
  (setq eglot-extend-to-xref t)
  (setq eglot-autoshutdown t)
  (setq eglot-report-progress t)

  (defun my/my-eglot-setup ()
    (when (eglot-managed-p)
      (setq company-backends (list
                              '(company-capf company-dabbrev-code company-files :separate)))
      )


    ;; https://github.com/company-mode/company-mode/discussions/1442
    (defun my/codeium-completion-at-point ()
      (when (and (or (null (thing-at-point 'symbol))
                     (< (length (thing-at-point 'symbol)) company-minimum-prefix-length))
                 (not (company-explicit-action-p)))
        (codeium-completion-at-point)))

    ;; (add-hook 'completion-at-point-functions #'my/codeium-completion-at-point -10 t)
    (add-hook 'completion-at-point-functions (cape-capf-nonexclusive 'eglot-completion-at-point) 0 t)
    (add-hook 'completion-at-point-functions #'codeium-completion-at-point 10 t)

    (when (eq major-mode 'terraform-mode)
      (setq completion-at-point-functions '(codeium-completion-at-point eglot-completion-at-point t))
      )
    )
  (add-hook 'eglot-managed-mode-hook #'my/my-eglot-setup)

  ;; python
  (add-to-list 'eglot-server-programs
               '(python-mode . ("basedpyright-langserver" "--stdio")))
  (add-to-list 'eglot-server-programs
               '(python-ts-mode . ("basedpyright-langserver" "--stdio")))
  (add-to-list 'eglot-server-programs
               '((dockerfile-mode dockerfile-ts-mode) . ("docker-language-server" "start" "--stdio")))
  (add-to-list 'eglot-server-programs
               '(my-docker-compose-mode . ("docker-compose-langserver" "--stdio")))
  (add-to-list 'eglot-server-programs
               '(nix-ts-mode . ("nil")))

  (transient-define-prefix ngoc/eglot-transient ()
    [:class transient-row "Eglot Menu"]

    [["Server"
      ("s" "Start server" eglot)
      ("R" "Reconnect" eglot-reconnect)
      ("S" "Shutdown" eglot-shutdown)]

     ["Refactor"
      ("r" "Rename" eglot-rename)
      ("o" "Organize imports" eglot-code-action-organize-imports)
      ("f" "Quick fix" eglot-code-action-quickfix)
      ("e" "Extract" eglot-code-action-extract)
      ("i" "Inline" eglot-code-action-inline)
      ("w" "Rewrite" eglot-code-action-rewrite)]

     ["Flymake"
      ("b" "Buffer diagnostics" flymake-show-buffer-diagnostics)
      ("p" "Project diagnostics" flymake-show-project-diagnostics)]

     ["Other"
      ("a" "Show actions" eglot-code-actions)
      ("F" "Format" eglot-format)
      ("d" "ElDoc" eldoc)
      ("h" "Toggle Inlay hints" eglot-inlay-hints-mode)]]))


(provide 'init-eglot)
