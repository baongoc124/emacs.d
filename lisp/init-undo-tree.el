(use-package undo-tree
  :demand t
  :diminish undo-tree-mode
  :config
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-enable-undo-in-region nil) ;; disabled because it's buggy
  (setq undo-tree-history-directory-alist
        `(("." . ,(file-name-as-directory (file-name-concat user-emacs-directory
                                                           "cache"
                                                           "undo-history")))))
  (global-undo-tree-mode))

(provide 'init-undo-tree)
