(use-package devdocs
  :config
  (defvar my/devdocs-mode-defaults
    '((python-mode . ("python~3.13" "pandas~2" "requests"))
      (go-mode . ("go"))
      (js-mode . ("javascript"))
      (typescript-mode . ("typescript")))
    "Default devdocs for major modes.")

  (defun my/devdocs-set-current-docs ()
    "Set devdocs-current-docs based on major mode."
    (when-let ((default-doc (cdr (assoc major-mode my/devdocs-mode-defaults))))
      (setq-local devdocs-current-docs default-doc)))

  (add-hook 'prog-mode-hook #'my/devdocs-set-current-docs)

  (defun embark-devdocs-lookup (symbol)
    "Quick devdocs lookup for SYMBOL using embark."
    (interactive "sSymbol: ")
    (devdocs-lookup nil symbol))

  ;; Register the embark action
  (with-eval-after-load 'embark
    (define-key embark-identifier-map (kbd "M-d") #'embark-devdocs-lookup))

)

(provide 'init-programming-docs)
