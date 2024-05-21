(use-package eglot
  :after (transient warnings)
  :init
  ;; do not display this warning from
  ;; jsonrpc ‘:events-buffer-scrollback-size’ deprecated. Use ‘events-buffer-config’.
  ;; FIXME so basically hide all warnings from emacs by default...
  (add-to-list 'warning-suppress-types '(emacs))
  :hook
  (prog-mode . eglot-ensure)
  :config
  (setq eglot-extend-to-xref t)
  ;; python
  (add-to-list 'eglot-server-programs
               '(python-mode . ("pyright-langserver" "--stdio")))

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
