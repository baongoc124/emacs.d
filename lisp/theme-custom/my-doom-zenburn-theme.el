(deftheme my-doom-zenburn
  "Created 2025-04-15."
  :background-mode 'dark
  :kind 'color-scheme)

  (custom-theme-set-faces 'my-doom-zenburn
   '(magit-blame-heading           ((t (:background "#2b2b2b"))))
   '(shadow                        ((t (:foreground "grey70"))))
   '(trailing-whitespace           ((t (:background "red"))))
   '(ivy-minibuffer-match-face-1   ((t (:foreground "#CC9393" :weight unspecified))))
   '(ivy-current-match             ((t (:background "#2b2b2b"))))
   ;; scale taken from emacs-zenburn theme
   '(org-document-title            ((t (:height 1.3))))
   '(org-level-1                   ((t (:height 1.3))))
   '(org-level-2                   ((t (:height 1.2))))
   '(org-level-3                   ((t (:height 1.15))))
   '(org-level-4                   ((t (:height 1.1))))
   '(org-todo                      ((t (:foreground "#CC9393"))))
   '(org-headline-done             ((t (:foreground "#7F9F7F"))))
   '(aw-leading-char-face          ((t (:weight bold :height 1.2))))
   '(tab-bar                       ((t (:background "#2b2b2b" :foreground "#DCDCCC" :box nil :height 1.05))))
   '(tab-bar-tab                   ((t (:background "#5F5F5F" :foreground "#dcdcdc" :box (:line-width (8 . 6) :color "#5F5F5F") :weight bold))))
   '(tab-bar-tab-inactive          ((t (:background "#2b2b2b" :foreground "#989890" :box nil :height 1.05))))
   '(header-line                   ((t (:background "#303030" :box (:line-width (8 . 4) :color nil :style flat-button)))))
   '(mode-line                     ((t (:height 1.1 :box (:line-width (1 . 4) :color nil :style flat-button)))))
   '(region                        ((t (:extend t :background "#7c4343"))))
   ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Frame-Layout.html
   '(internal-border               ((t (:background "3F3F3F")))) ; "red"
   )

(provide-theme 'my-doom-zenburn)
