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

(use-package project
  :config

  (defvar ngoc/project-marker ".ngoc-project"
    "Project marker file name.")

  (defun ngoc/project-find-root (dir)
    "Find the root directory of the current project."
    (let ((root (locate-dominating-file dir ngoc/project-marker)))
      (if root
          (list 'vc nil root)
        nil)))

  (add-hook 'project-find-functions #'ngoc/project-find-root))


(provide 'init-project)
