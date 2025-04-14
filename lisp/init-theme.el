;; https://emacsredux.com/blog/2025/02/03/clean-unloading-of-emacs-themes/
(defun er-disable-all-active-themes ()
  "Disable all currently active themes."
  (interactive)
  (dolist (theme custom-enabled-themes)
    (disable-theme theme)))

(defun er-load-theme (theme)
  (er-disable-all-active-themes)
  (load-theme theme t))

(defun my-load-doom-zenburn ()
  (interactive)

  (er-disable-all-active-themes)
  (load-theme 'doom-zenburn t)
  (load-theme 'my-doom-zenburn t)

  (modify-all-frames-parameters '((internal-border-width . 0)))
  )


(use-package doom-themes
  :config
;;   ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
;;   ;; Enable custom neotree theme (all-the-icons must be installed!)
;;   (doom-themes-neotree-config)
;;   ;; or for treemacs users
;;   (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
;;   (doom-themes-treemacs-config)
;;   ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)

  (my-load-doom-zenburn)
  )

(defun ngoc/load-nord-lightt ()
  (interactive)
  (save-buffer)
  (load-theme 'doom-zenburn t))

;; (load-theme 'nord-lightt t)
;; ;; (load-theme 'modus-operandi-tinted t)

(provide 'init-theme)
