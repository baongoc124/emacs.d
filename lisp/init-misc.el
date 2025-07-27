(use-package yaml-mode
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

  (defun my/yaml-mode-setup ()
    (flymake-mode 1)
    (setq show-trailing-whitespace t)
    )

  (add-hook 'yaml-mode-hook #'my/yaml-mode-setup)

  ;; derive a mode for docker compose from yaml-ts-mode because it handles
  ;; syntax much better than docker-compose-mode
  ;; probably no longer needed because using external tree-sitter -> i can use yaml-mode
  ;; (require 'yaml-ts-mode) ; Ensure yaml-ts-mode is available
    "My major mode for editing Docker Compose files."
    (setq-local tab-width 2))

  (add-hook 'my-docker-compose-mode-hook #'eglot-ensure)

  (add-to-list 'auto-mode-alist '("\\(docker-\\|\\)compose\\.ya?ml" . my-docker-compose-mode))
  )

(use-package flymake-yamllint
  :after yaml-mode
  :config
  (add-hook 'yaml-mode-hook 'flymake-yamllint-setup)
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


(use-package nix-ts-mode
  :mode "\\.nix\\'")

;; (use-package highlight-indentation
;;   :diminish highlight-indentation-mode
;;   :hook (prog-mode . highlight-indentation-mode))

(use-package ggtags
  :config
  (setq ggtags-global-abbreviate-filename nil))


(use-package ag)




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

(use-package inheritenv)


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

(use-package gnuplot)


;; hl-line overrides face of match -> don't use it globally
(use-package hl-line
  :hook
  (prog-mode . hl-line-mode)
  (dired-mode . hl-line-mode)
  ;; (text-mode . hl-line-mode)
  (conf-mode . hl-line-mode)
  (org-agenda-mode . hl-line-mode)
  (package-menu-mode . hl-line-mode)
  (ibuffer-mode . hl-line-mode)
  )



(provide 'init-misc)
