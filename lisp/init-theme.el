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

  (load-theme 'doom-zenburn t)

  ;; custom settings for doom-zenburn theme
  (set-face-attribute 'shadow nil :foreground "grey70")
  (set-face-attribute 'trailing-whitespace nil :background "red")
  (eval-after-load 'ivy
    '(progn
       (set-face-attribute 'ivy-minibuffer-match-face-1 nil :foreground "#CC9393")
       (set-face-attribute 'ivy-current-match nil :background "#2b2b2b")
       ))

  ;; scale taken from emacs-zenburn theme
  (eval-after-load 'org
    '(progn
       (set-face-attribute 'org-document-title nil :height 1.3)
       (set-face-attribute 'org-level-1 nil :height 1.3)
       (set-face-attribute 'org-level-2 nil :height 1.2)
       (set-face-attribute 'org-level-3 nil :height 1.15)
       (set-face-attribute 'org-level-4 nil :height 1.1)
       (set-face-attribute 'org-todo nil :foreground "#CC9393")
       (set-face-attribute 'org-headline-done nil :foreground "#7F9F7F")
       )
    )

  (eval-after-load 'ace-window
    '(progn
       (set-face-attribute 'aw-leading-char-face nil :weight 'bold :height 1.2)
       )
    )


  (eval-after-load 'magit
    '(progn
       (set-face-attribute 'magit-blame-heading nil :background "#2b2b2b2")
       ))

  (set-face-attribute 'mode-line nil :height 1.1 :box '(:line-width (1 . 4) :color nil :style flat-button))
  (set-face-attribute 'region nil :extend t :background "#8c5353")
  )



;; (use-package zenburn-theme
;;   :config
;;   (load-theme 'zenburn t))

(defun ngoc/load-nord-lightt ()
  (interactive)
  (save-buffer)
  (load-theme 'doom-zenburn t))

;; (load-theme 'nord-lightt t)
;; ;; (load-theme 'modus-operandi-tinted t)

(provide 'init-theme)
