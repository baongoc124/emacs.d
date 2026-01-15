;; https://github.com/doomemacs/doomemacs/blob/master/modules/editor/evil/packages.el
;; old evil config
;; https://github.com/baongoc124/emacs.d/blob/1b9f05cb86f8c609b271dd0364bfb005b6aba36a/init.el
(use-package evil
  :init
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1)
  (setq evil-want-fine-undo t)
  (setq evil-undo-system 'undo-tree)

  (defun my/set-evil-cursor ()
    (interactive)
    (if (eq (frame-parameter nil 'background-mode) 'dark)
        (progn
          (setq evil-emacs-state-cursor  '("#ff3399" box))
          (setq evil-normal-state-cursor '("green" box))
          (setq evil-visual-state-cursor '("orange" box))
          (setq evil-insert-state-cursor '((bar . 3) "red"))
          (setq evil-replace-state-cursor '((bar . 3) "pink"))
          (setq evil-operator-state-cursor '("red" hollow)))
      (setq evil-emacs-state-cursor '("#cc0077" box))
      (setq evil-normal-state-cursor '("#00B400" box))
      (setq evil-visual-state-cursor '("orange" box))
      (setq evil-insert-state-cursor '((bar . 3) "red"))
      (setq evil-replace-state-cursor '((bar . 3) "magenta"))
      (setq evil-operator-state-cursor '("red" hollow))))

  (my/set-evil-cursor)

  ;; switch to evil-search because i want to use gn, gN
  (evil-select-search-module 'evil-search-module 'evil-search)

  (define-key evil-emacs-state-map [escape] nil)
  ;; (define-key evil-visual-state-map (kbd "v") 'er/expand-region)

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
  (evil-set-initial-state 'ediff-mode 'emacs)
  (evil-set-initial-state 'eshell-mode 'emacs)
  (evil-set-initial-state 'exwm-mode 'emacs)
  (evil-set-initial-state 'help-mode 'emacs)
  (evil-set-initial-state 'inferior-python-mode 'emacs)
  (evil-set-initial-state 'info-mode 'emacs)
  (evil-set-initial-state 'minibuffer-mode 'emacs)
  (evil-set-initial-state 'shell-mode 'emacs)
  (evil-set-initial-state 'shell-command-mode 'emacs)
  (evil-set-initial-state 'special-mode 'emacs)
  (evil-set-initial-state 'term-mode 'emacs)
  (evil-set-initial-state 'treemacs-mode 'emacs)
  (evil-set-initial-state 'vterm-mode 'emacs)
  (evil-set-initial-state 'win:switch-menu-mode 'emacs)
  (evil-set-initial-state 'xref--xref-buffer-mode 'emacs)
  (evil-set-initial-state 'undo-tree-visualizer-mode 'emacs)
  (evil-set-initial-state 'lsp-bridge-ref-mode 'emacs)

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


(use-package evil-mc
  :demand t
  :config
  (global-evil-mc-mode t)
  (setq evil-mc-mode-line-prefix "mc")
  (setq-default evil-mc-one-cursor-show-mode-line-text nil)
  (add-to-list 'evil-mc-incompatible-minor-modes 'minuet-auto-suggestion-mode)


  (require 'transient)

  (transient-define-prefix evil-mc-transient ()
    "Evil multiple cursors menu"
    :transient-suffix 'transient--do-stay
    :transient-non-suffix 'transient--do-warn

    [["Match"
      ("n" "Make & goto next match" evil-mc-make-and-goto-next-match)
      ("p" "Make & goto prev match" evil-mc-make-and-goto-prev-match)
      ("N" "Skip to next match" evil-mc-skip-and-goto-next-match)
      ("P" "Skip to prev match" evil-mc-skip-and-goto-prev-match)
      ""
      ("j" "Make & goto next line" evil-mc-make-cursor-move-next-line)
      ("k" "Make & goto previous line" evil-mc-make-cursor-move-prev-line)
      ""
      ("a" "Make all cursors" evil-mc-make-all-cursors)
      ]

     ["Navigation"
      ("M-n" "Make & goto next cursor" evil-mc-make-and-goto-next-cursor)
      ("M-p" "Make & goto prev cursor" evil-mc-make-and-goto-prev-cursor)
      ("M-N" "Skip to next cursor" evil-mc-skip-and-goto-next-cursor)
      ("M-P" "Skip to prev cursor" evil-mc-skip-and-goto-prev-cursor)
      ""
      ("f" "Make & goto first cursor" evil-mc-make-and-goto-first-cursor)
      ("l" "Make & goto last cursor" evil-mc-make-and-goto-last-cursor)
      ""
      ("h" "Make cursor here" evil-mc-make-cursor-here)
      ]

     ["Visual Selection"
      ("b" "Cursors at line beginnings" evil-mc-make-cursor-in-visual-selection-beg)
      ("e" "Cursors at line ends" evil-mc-make-cursor-in-visual-selection-end)
      ]
     ]

    [["Control"
      ("SPC" my/evil-mc-toggle-pause-cursors :description (lambda () (if evil-mc-frozen "Resume cursors" "Pause cursors")) :transient transient--do-exit)

      ("u" "Undo last cursor" evil-mc-undo-last-added-cursor)
      ("U" "Undo all cursors" evil-mc-undo-all-cursors)]

     ["Exit"
      ("c" "Clear mc and quit" evil-mc-undo-all-cursors :transient nil)
      ("q" "Quit" transient-quit-one)]]

    (interactive)
    (when evil-mc-frozen (evil-mc-resume-cursors))
    (transient-setup #'evil-mc-transient)
    )

  ;; Helper function to toggle pause/resume
  (defun my/evil-mc-toggle-pause-cursors ()
    "Toggle between pausing and resuming cursors based on current state."
    (interactive)
    (if evil-mc-frozen
        (evil-mc-resume-cursors)
      (evil-mc-pause-cursors)))

  (global-set-key (kbd "<leader> m") 'evil-mc-transient)

  ;; unbind default keybinds because we already have transient menu
  (evil-define-key* '(normal visual) evil-mc-key-map
                      (kbd "M-n") nil
                      (kbd "M-p") nil
                      (kbd "C-n") nil
                      (kbd "C-t") nil
                      (kbd "C-p") nil)
  )


(when (not (display-graphic-p))
  (with-eval-after-load 'evil

    (defun kitty-set-cursor-color (color)
      "Set Kitty terminal cursor color via escape sequence."
      (send-string-to-terminal (format "\e]12;%s\007" color)))

    ;; Function to update cursor based on evil state
    (defun update-kitty-cursor-for-evil-state ()
      "Update Kitty cursor color based on current evil state."
      (when (getenv "KITTY_WINDOW_ID") ; Only run if in Kitty
        (cond
         ((evil-normal-state-p)  (kitty-set-cursor-color "#4CAF50"))  ; Green
         ((evil-insert-state-p)  (kitty-set-cursor-color "#F44336"))  ; Red
         ((evil-visual-state-p)  (kitty-set-cursor-color "#FF9800"))  ; Orange
         ;; ((evil-replace-state-p) (kitty-set-cursor-color "#2196F3"))  ; Blue
         (t (kitty-set-cursor-color "#9A7DFF")))))                    ; Default

    (add-hook 'post-command-hook 'update-kitty-cursor-for-evil-state)
    )
  )


;; display a popup to show register contents automatically when it's needed
(use-package evil-owl
  :diminish evil-owl-mode
  :config
  (setq evil-owl-max-string-length 128)
  (evil-owl-mode 1))


(use-package evil-matchit
  :config
  (global-evil-matchit-mode 1))


(provide 'init-evil)
