;; https://github.com/doomemacs/doomemacs/blob/master/modules/editor/evil/packages.el
;; old evil config
;; https://github.com/baongoc124/emacs.d/blob/1b9f05cb86f8c609b271dd0364bfb005b6aba36a/init.el
(use-package evil
  :config
  (evil-mode 1)
  (setq evil-want-fine-undo t)
  (setq evil-undo-system 'undo-tree)
  (setq evil-emacs-state-cursor '("pink" box))
  (setq evil-normal-state-cursor '("#00B400" box))
  (setq evil-visual-state-cursor '("orange" box))
  (setq evil-insert-state-cursor '((bar . 3) "red"))
  (setq evil-replace-state-cursor '((bar . 3) "pink"))
  (setq evil-operator-state-cursor '("red" hollow))

  (define-key evil-emacs-state-map [escape] nil)
  (define-key evil-visual-state-map (kbd "v") 'er/expand-region)

  ;; Disable evil on some modes
  (evil-set-initial-state 'term-mode 'emacs)
  (evil-set-initial-state 'help-mode 'emacs)
  (evil-set-initial-state 'info-mode 'emacs)
  (evil-set-initial-state 'eshell-mode 'emacs)
  (evil-set-initial-state 'shell-mode 'emacs)
  (evil-set-initial-state 'dired-mode 'emacs)
  (evil-set-initial-state 'win:switch-menu-mode 'emacs)
  (evil-set-initial-state 'compilation-mode 'emacs)
  (evil-set-initial-state 'elfeed-show-mode 'emacs)
  (evil-set-initial-state 'elfeed-search-mode 'emacs)
  (evil-set-initial-state 'exwm-mode 'emacs)
  (evil-set-initial-state 'xref--xref-buffer-mode 'emacs)
  (evil-set-initial-state 'inferior-python-mode 'emacs)
  (evil-set-initial-state 'special-mode 'emacs)
  (evil-set-initial-state 'treemacs-mode 'emacs)

  (evil-set-leader nil (kbd "M-m"))
  (evil-set-leader 'normal (kbd "<SPC>"))
  (evil-set-leader 'visual (kbd "<SPC>"))
  (evil-set-leader 'operator (kbd "<SPC>"))
  (evil-set-leader 'motion (kbd "<SPC>"))

  (setq ngoc/prefix-keymap-alist
        '(("`" . evil-goto-mark)
          ("=" . ngoc/align-dwim)
          ("3" . ngoc/cheatsheet)
          ("M-b" . ibuffer)
          ("b" . counsel-switch-buffer)
          ("c" . evilnc-comment-operator)
          ("d" . duplicate-dwim)
          ("e" . ngoc/eglot-transient)
          ("f" . project-find-file)
          ("g" . ngoc/history-prefix)
          ("h" . mark-paragraph)
          ("m" . iedit-mode)
          ("M-m" . yas-expand)
          ("n" . ngoc/avy-back-to-future)
          ("o" . symbol-overlay-transient)
          ("q" . ngoc/fill-function-arguments-dwim)
          ("r" . ngoc/org-roam-prefix)
          ("s" . imenu-list-smart-toggle)
          ("t" . avy-goto-char-timer)
          ("u" . undo-tree-visualize)
          ("v" . ngoc/git-transient)
          ("w" . ngoc/window-prefix)))

  (defun ngoc/bind-leader-map (key-function-pair-alist)
    "Convenient function to bind my evil leader keymap."
    (dolist (pair ngoc/prefix-keymap-alist)
      (let ((key (car pair))
            (function (cdr pair)))
        (evil-define-key nil 'global (kbd (concat "<leader>" key)) function))))

  (ngoc/bind-leader-map ngoc/prefix-keymap-alist))

(use-package evil-nerd-commenter)

(use-package evil-visualstar
  :config
  (global-evil-visualstar-mode 1))

(use-package evil-goggles
  :ensure t
  :diminish evil-goggles-mode
  :config
  (evil-goggles-mode)
  (setq evil-goggles-duration 0.3))

  ;; optionally use diff-mode's faces; as a result, deleted text
  ;; will be highlighed with `diff-removed` face which is typically
  ;; some red color (as defined by the color theme)
  ;; other faces such as `diff-added` will be used for other actions
  ;; (evil-goggles-use-diff-faces))

(provide 'init-evil)
