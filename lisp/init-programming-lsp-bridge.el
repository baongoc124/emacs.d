(add-to-list 'load-path (expand-file-name "~/builds/lsp-bridge/"))

(require 'lsp-bridge)
(global-lsp-bridge-mode)

(setq acm-enable-lsp-workspace-symbol t)

(setq acm-backend-copilot-launch-mode 'binary)
(setq acm-enable-copilot t)

;; bind some code navigation shortcuts
(with-eval-after-load 'evil
  (defun my/evil-lsp-bridge-find-def (_string _position)
    "Use lsp-bridge to find definition when lsp-bridge-mode is active."
    (when (bound-and-true-p lsp-bridge-mode)
      (lsp-bridge-find-def)
      t))

  (add-to-list 'evil-goto-definition-functions
               #'my/evil-lsp-bridge-find-def))


(define-key lsp-bridge-mode-map
              [remap xref-find-references]
              #'lsp-bridge-find-references)

;; disable Enter key on completion popup
(keymap-set acm-mode-map "RET" nil)

(provide 'init-programming-lsp-bridge)
