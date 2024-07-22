(use-package which-key
  :diminish which-key-mode
  :config
  (setq which-key-show-transient-maps t)
  (which-key-mode 1))

(use-package free-keys)

;; easy exit from transient menu using ESC
(use-package transient
  :config
  (define-key transient-base-map (kbd "<escape>") #'transient-quit-one))


(provide 'init-shortcuts)
