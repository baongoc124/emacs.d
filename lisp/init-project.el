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

  ;; from 29.1
  (add-to-list 'project-vc-extra-root-markers ".ngoc-project")


  ;; Disable project-try-vc for Docker TRAMP buffers because it makes Emacs hang
  (defun my/disable-project-vc-for-docker-tramp (orig-fun dir)
    "Disable project-try-vc for Docker TRAMP buffers."
    (if (and (file-remote-p dir)
             (string-match-p "^/docker:" dir))
        (progn
          (message dir)
          nil)
      (funcall orig-fun dir)))

  (advice-add 'project-try-vc :around #'my/disable-project-vc-for-docker-tramp)
  )


;; one tab per project mode
(use-package otpp
  :after project
  :init
  ;; Enable `otpp-mode` globally
  (otpp-mode 1)
  ;; If you want to advice the commands in `otpp-override-commands`
  ;; to be run in the current's tab (so, current project's) root directory
  (otpp-override-mode 1)

  (add-to-list 'otpp-override-commands #'my/vterm-dispatch)
  )

(provide 'init-project)
