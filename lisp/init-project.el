;; (use-package projectile
;;   :hook
;;   (prog-mode . projectile-mode)
;;   (text-mode . projectile-mode)
;;   :config
;;   (setq projectile-enable-caching t)
;;   (setq projectile-mode-line-function #'(lambda ()
;;                                           (if (file-remote-p default-directory)
;;                                               " Proj"
;;                                             (format " P[%s]" (projectile-project-name))))))


;; (use-package counsel-projectile
;;   :after (counsel projectile)
;;   :config
;;   (counsel-projectile-mode))

(provide 'init-project)
