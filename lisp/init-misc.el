(use-package yaml-mode
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))


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


(use-package flycheck
  :config
  (setq flycheck-mode-line-prefix "F"))


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
;; (load-library "ned-mode")
(use-package shell-switcher)
(use-package xcscope)


(provide 'init-misc)
