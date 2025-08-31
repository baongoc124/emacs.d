(use-package neotree
  :bind
  (
   ;; ("C-c C-p" . neotree-find-project-root)  ; Open neotree at project root
   ;; ("C-c p" . neotree-projectile-action)    ; Projectile integration
   ;; ("C-c C-d" . neotree-find)               ; Open neotree at current file
   ;; ("C-c d" . neotree-dir)                  ; Open neotree at directory
   ("C-8" . my/neotree-smart-toggle))         ; Smart toggle neotree
  :config
  (setq neo-window-fixed-size nil)
  (setq neo-window-width 36)
  (setq neo-theme 'nerd-icons)
  (setq neo-auto-indent-point t)
  (setq neo-show-hidden-files t)
  (setq neo-show-slash-for-folder nil)

  (defun my/neotree-smart-toggle ()
    "Smart neotree toggle with three-state behavior:
    - If not visible: show neotree at project root (or current dir)
    - If visible but not focused: focus neotree
    - If visible and focused: hide neotree"
    (interactive)
    (cond
     ;; Case 1: Neotree not visible - show it
     ((not (neo-global--window-exists-p))
      (let ((project-root (or (when (fboundp 'project-root)
                                (when-let ((project (project-current)))
                                  (project-root project)))
                              default-directory)))
        (neotree-dir project-root)))

     ;; Case 2: Neotree visible and focused - hide it
     ((eq (selected-window) (neo-global--get-window))
      (neotree-hide))

     ;; Case 3: Neotree visible but not focused - focus it
     (t
      (select-window (neo-global--get-window)))))
  )

(provide 'init-neotree)
