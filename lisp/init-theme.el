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
  (er-load-theme 'doom-zenburn)

  (set-face-attribute 'shadow nil :foreground "grey70")
  (set-face-attribute 'trailing-whitespace nil :background "red")
  (with-eval-after-load 'ivy
    (set-face-attribute 'ivy-minibuffer-match-face-1 nil :foreground "#CC9393" :weight 'unspecified)
    (set-face-attribute 'ivy-current-match nil :background "#2b2b2b")
    )

  ;; scale taken from emacs-zenburn theme
  (with-eval-after-load 'org
    (set-face-attribute 'org-document-title nil :height 1.3)
    (set-face-attribute 'org-level-1 nil :height 1.3)
    (set-face-attribute 'org-level-2 nil :height 1.2)
    (set-face-attribute 'org-level-3 nil :height 1.15)
    (set-face-attribute 'org-level-4 nil :height 1.1)
    (set-face-attribute 'org-todo nil :foreground "#CC9393")
    (set-face-attribute 'org-headline-done nil :foreground "#7F9F7F")
    )

  (with-eval-after-load 'ace-window
    (set-face-attribute 'aw-leading-char-face nil :weight 'bold :height 1.2)
    )


  (with-eval-after-load 'magit
    (set-face-attribute 'magit-blame-heading nil :background "#2b2b2b")
    )

  (with-eval-after-load 'tab-bar
    (set-face-attribute 'tab-bar nil
                        :background "#2b2b2b"
                        :foreground "#DCDCCC"
                        :box nil
                        :height 1.05)
    (set-face-attribute 'tab-bar-tab nil
                        :background "#5F5F5F"
                        :foreground "#dcdcdc"
                        :box '(:line-width (8 . 6) :color "#5F5F5F")
                        :weight 'bold)
    (set-face-attribute 'tab-bar-tab-inactive nil
                        :background "#2b2b2b"
                        :foreground "#989890"
                        :box nil
                        :height 1.05)
    )

  (set-face-attribute 'header-line nil :background "#303030" :box '(:line-width (8 . 4) :color nil :style flat-button))
  (set-face-attribute 'mode-line nil :height 1.1 :box '(:line-width (1 . 4) :color nil :style flat-button))
  (set-face-attribute 'region nil :extend t :background "#7c4343")
  ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Frame-Layout.html
  (set-face-attribute 'internal-border nil :background "red")
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
  ;; (load-theme 'doom-zenburn t)
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
