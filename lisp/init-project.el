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

(use-package compile
  :config
  (setq compilation-scroll-output t)
  (setq compilation-always-kill t)
  (setq compilation-ask-about-save t)

  ;; color for compilation buffer
  (add-hook 'compilation-filter-hook #'ansi-color-compilation-filter)

  (defun ngoc/auto-save-recompile ()
    "Recompile no question asked."
    (interactive)
    (let ((compilation-ask-about-save nil))
      (recompile)))

  (global-set-key (kbd "<f5>") #'ngoc/auto-save-recompile))

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
