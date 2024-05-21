(use-package which-key
  :after god-mode
  :diminish which-key-mode
  :config
  (setq which-key-use-C-h-commands nil) ;; use ? to show paging help
  (setq which-key-show-transient-maps t)
  (which-key-mode 1)
  (which-key-enable-god-mode-support))


(use-package free-keys)


;; easy exit from transient menu using ESC
(use-package transient
  :config
  (define-key transient-base-map (kbd "<escape>") #'transient-quit-one))


(provide 'init-shortcuts)
