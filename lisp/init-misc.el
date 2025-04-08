(use-package yaml-mode
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

  (add-hook 'yaml-ts-mode-hook
            (lambda ()
              (flymake-mode 1)
              (setq show-trailing-whitespace t)))
  )

(use-package flymake-yamllint
  :after yaml-mode
  :config
  (add-hook 'yaml-ts-mode-hook 'flymake-yamllint-setup)
  )

(use-package rust-mode
  :init
  (setq rust-mode-treesitter-derive t))


(use-package vimrc-mode
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.vim\\(rc\\)?\\'" . vimrc-mode)))


(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))


;; temporary workaround
(add-to-list 'auto-mode-alist '("\\.json5\\'" . js-json-mode))
;; (add-to-list 'auto-mode-alist '("\\.json5\\'" . js-ts-mode))
(use-package flymake-eslint
  :vc (:fetcher github :repo "orzechowskid/flymake-eslint")
  :config

  (defun my/flymake-eslint-setup ()
    (when (string= (file-name-extension buffer-file-name) "json5")
      (flymake-eslint-enable)
      )
  )

  (add-hook 'json-mode-hook 'my/flymake-eslint-setup)
  )

(use-package flycheck
  :config
  (setq flycheck-mode-line-prefix "F"))

(use-package nix-mode
  :mode "\\.nix\\'")

;; (use-package highlight-indentation
;;   :diminish highlight-indentation-mode
;;   :hook (prog-mode . highlight-indentation-mode))

(use-package ggtags
  :config
  (setq ggtags-global-abbreviate-filename nil))


(use-package ag)


(use-package dockerfile-mode)


(use-package figlet
  :config
  (setq figlet-default-font "standard"))


;; (use-package aggressive-indent)
;; (use-package tex-buf)
;; (use-package tex :ensure auctex)
;; (use-package auctex-latexmk)
(use-package better-shell)
(use-package flx)
(use-package impatient-mode)
(use-package isend-mode)
(use-package key-chord)
(use-package latex-preview-pane)
(use-package less-css-mode)
(use-package shell-switcher)
(use-package xcscope)

(use-package ned-mode
  :vc (:fetcher github :repo "dalwadani/ned-mode"))

(use-package envrc
  :hook (after-init . envrc-global-mode)
  :config
  (setcar envrc-none-lighter " env[")
  (setcar envrc-on-lighter " env[")
  (setcar envrc-error-lighter " env[")
  )


(use-package jq-mode)

;; https://news.ycombinator.com/item?id=22131815
(defun ngoc/arrayify (start end quote)
  "Turn strings on newlines into a QUOTEd, comma-separated one-liner."
  (interactive "r\nMQuote: ")
  (let ((insertion
         (mapconcat
          (lambda (x) (format "%s%s%s" quote x quote))
          (split-string (buffer-substring start end)) ", ")))
    (delete-region start end)
    (insert insertion)))

(use-package rainbow-mode)

(provide 'init-misc)
