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


  ;; switch to evil-search because i want to use gn, gN
  (evil-select-search-module 'evil-search-module 'evil-search)

  (define-key evil-emacs-state-map [escape] nil)
  (define-key evil-visual-state-map (kbd "v") 'er/expand-region)

  ;; define as motion keys allow evil-jumps to work & other action with motions!
  (define-key evil-motion-state-map (kbd "s") 'avy-goto-char-timer)
  (define-key evil-motion-state-map (kbd "S") 'avy-goto-line)

  (define-key evil-normal-state-map (kbd "s") 'avy-goto-char-timer)
  (define-key evil-normal-state-map (kbd "S") 'avy-goto-line)

  ;; Disable evil on some modes
  (evil-set-initial-state 'compilation-mode 'emacs)
  (evil-set-initial-state 'difftastic-mode 'emacs)
  (evil-set-initial-state 'dired-mode 'emacs)
  (evil-set-initial-state 'eat-mode 'emacs)
  (evil-set-initial-state 'elfeed-search-mode 'emacs)
  (evil-set-initial-state 'elfeed-show-mode 'emacs)
  (evil-set-initial-state 'eshell-mode 'emacs)
  (evil-set-initial-state 'exwm-mode 'emacs)
  (evil-set-initial-state 'help-mode 'emacs)
  (evil-set-initial-state 'inferior-python-mode 'emacs)
  (evil-set-initial-state 'info-mode 'emacs)
  (evil-set-initial-state 'minibuffer-mode 'emacs)
  (evil-set-initial-state 'shell-mode 'emacs)
  (evil-set-initial-state 'special-mode 'emacs)
  (evil-set-initial-state 'term-mode 'emacs)
  (evil-set-initial-state 'treemacs-mode 'emacs)
  (evil-set-initial-state 'vterm-mode 'emacs)
  (evil-set-initial-state 'win:switch-menu-mode 'emacs)
  (evil-set-initial-state 'xref--xref-buffer-mode 'emacs)

  (evil-set-leader nil (kbd "M-m"))
  (evil-set-leader 'normal (kbd "<SPC>"))
  (evil-set-leader 'visual (kbd "<SPC>"))
  (evil-set-leader 'operator (kbd "<SPC>"))
  (evil-set-leader 'motion (kbd "<SPC>"))

  (defvar-local my/evil-search-timer nil
    "Timer to clear Evil search highlight after search inactivity.")

  (defun my/evil-clear-highlight-timer (buf)
    "Run `evil-ex-nohighlight` in BUF."
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (evil-ex-nohighlight))
      ;; Clear timer var in that buffer
      (with-current-buffer buf
        (setq my/evil-search-timer nil))))

  (defun my/evil-search-activity (&rest _)
    "Restart 10-second idle timer to clear highlights in current buffer."
    (when (and (local-variable-p 'my/evil-search-timer)
               my/evil-search-timer)
      (cancel-timer my/evil-search-timer))
    (setq my/evil-search-timer
          (run-with-timer
           10 nil #'my/evil-clear-highlight-timer (current-buffer))))

  (dolist (fn '(evil-next-match
                evil-previous-match
                evil-ex-search-next
                evil-ex-search-previous
                evil-ex-search-word-forward
                evil-ex-search-word-backward
                evil-ex-search-forward
                evil-ex-search-backward))
    (advice-add fn :after #'my/evil-search-activity))

  ;; (dolist (fn '(evil-ex-search-next
  ;;               evil-ex-search-previous
  ;;               evil-ex-search-word-forward
  ;;               evil-ex-search-word-backward
  ;;               evil-ex-search-forward
  ;;               evil-ex-search-backward))
  ;;   (advice-remove fn #'my/evil-search-activity))

  )

(use-package evil-nerd-commenter)

(use-package evil-visualstar
  :after evil
  :config
  (setq evil-symbol-word-search t)
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
