;; prevent top bottom split
(setq split-height-threshold nil)

;; big enough so even with golden ratio mode, it won't trigger
;; FIXME: should fix split-window-sensibly
(setq split-width-threshold 180)

(define-prefix-command 'ngoc/window-prefix)
(define-key ngoc/window-prefix "c" #'my-ace-copy-window)
(define-key ngoc/window-prefix "d" #'ace-delete-window)
(define-key ngoc/window-prefix "e" #'balance-windows)
(define-key ngoc/window-prefix "h" #'hsplit-last-buffer)
(define-key ngoc/window-prefix "m" #'my-ace-move-window)
(define-key ngoc/window-prefix "t" #'transpose-frame)
(define-key ngoc/window-prefix "v" #'vsplit-last-buffer)
(define-key ngoc/window-prefix "x" #'ace-swap-window)
(define-key ngoc/window-prefix "s" #'my-save-window-config)
(define-key ngoc/window-prefix "w" #'my-restore-window-config)

;; use M-arrow keys for moving to windows
;; (global-set-key (kbd "M-<left>") 'windmove-left)
;; (global-set-key (kbd "M-<right>") 'windmove-right)
;; (global-set-key (kbd "M-<up>") 'windmove-up)
;; (global-set-key (kbd "M-<down>") 'windmove-down)
(global-set-key (kbd "C-S-h") 'windmove-left)
(global-set-key (kbd "C-S-n") 'windmove-right)
(global-set-key (kbd "C-S-c") 'windmove-up)
(global-set-key (kbd "C-S-t") 'windmove-down)

(global-set-key (kbd "M-t") '(lambda ()
                               (interactive)
                               (select-window (get-mru-window nil nil t))))


(use-package golden-ratio
  :diminish golden-ratio-mode
  :config
  (add-to-list 'golden-ratio-extra-commands #'ace-window)

  (setq golden-ratio-auto-scale nil)
  (setq golden-ratio-exclude-buffer-names '(" *undo-tree*" ;; notice the space before the actual name
                                            "*Ediff Control Panel*"
                                            ;; "COMMIT_EDITMSG" ;; magit
                                            ))
  (setq golden-ratio-exclude-modes nil)
  ;; (setq golden-ratio-exclude-modes '(
  ;;                                    magit-log-mode
  ;;                                    ))
  ;; (golden-ratio-mode 1)
  )

;; use s-M-arrow keys for moving windows into direction of the arrow
(defmacro define-buffer-move (name direction)
  `(defun ,name ()
     ,(format "Move buffer to the %s window." direction)
     (interactive)
     (let ((other-win (windmove-find-other-window ',direction)))
       (when other-win
         (let ((this-buf (current-buffer))
               (other-buf (window-buffer other-win)))
           (set-window-buffer (selected-window) other-buf)
           (set-window-buffer other-win this-buf)
           (select-window other-win))))))

(define-buffer-move my/move-buffer-left  left)
(define-buffer-move my/move-buffer-right right)
(define-buffer-move my/move-buffer-up    up)
(define-buffer-move my/move-buffer-down  down)

(global-set-key (kbd "M-s-<left>")  'my/move-buffer-left)
(global-set-key (kbd "M-s-<right>") 'my/move-buffer-right)
(global-set-key (kbd "M-s-<up>")    'my/move-buffer-up)
(global-set-key (kbd "M-s-<down>")  'my/move-buffer-down)

;; alternative
(global-set-key (kbd "C-M-S-h") 'my/move-buffer-left)
(global-set-key (kbd "C-M-S-n") 'my/move-buffer-right)
(global-set-key (kbd "C-M-S-c") 'my/move-buffer-up)
(global-set-key (kbd "C-M-S-t") 'my/move-buffer-down)


(use-package ace-window
  ;; :bind ("M-t" . ace-window)
  :demand t
  :config
  (setq aw-keys '(?h ?c ?t ?m ?w ?n ?e ?u ?j ?q)
        aw-scope 'frame)

  (global-set-key (kbd "M-T") #'(lambda () (interactive)
                                  (let ((aw-dispatch-always t))
                                    (call-interactively #'ace-window))))
  (if (display-graphic-p)
      (progn
        (ace-window-posframe-mode 1)
        (set-face-attribute 'aw-leading-char-face nil :height 3.0)
        )

    (set-face-attribute 'aw-leading-char-face nil :weight 'bold :inverse-video t)
    )

  (defun my-ace-move-window ()
    (interactive)
    (aw-select " Ace - Move Window"
               #'aw-move-window))

  (defun my-ace-copy-window ()
    (interactive)
    (aw-select " Ace - Copy Window"
               #'aw-copy-window))
  )


(use-package winner
  :demand t ;; load immediately to save window configuration
  :bind (("M-F" . winner-redo)
         ("M-B" . winner-undo))
  :config
  (winner-mode 1))


(use-package tab-bar
  :custom
  (tab-bar-mode t)
  (tab-bar-show 1)
  (tab-bar-close-button-show t)
  (tab-bar-new-button-show nil)
  (tab-bar-tab-hints t) ;; show tab number
  (tab-bar-format '(tab-bar-format-tabs tab-bar-separator))

  :config
  )


(defun n/tab-switch (num)
  "Switch to tab based on number input."
  (interactive "c")
  (when (and (>= num ?0) (<= num ?9))
    (tab-bar-select-tab (- num ?0))))

(dotimes (i 10)
  (global-set-key (kbd (format "<f8>%d" i))
                  `(lambda () (interactive) (n/tab-switch ,(string-to-char (number-to-string i))))))

;; duplicate tab and set name
(defun n/tab-duplicate-and-rename ()
  "Duplicate current tab and set name."
  (interactive)
  (tab-duplicate)
  (call-interactively 'tab-rename))

(global-set-key (kbd "<f8><f8>") 'tab-bar-switch-to-recent-tab)
(global-set-key (kbd "<f8>c") #'n/tab-duplicate-and-rename)
(global-set-key (kbd "<f8>r") 'tab-rename)
(global-set-key (kbd "<f8>k") 'tab-bar-close-tab)
(global-set-key (kbd "<f8>u") 'tab-undo)


(use-package transpose-frame)


(defun m/buffer-to-side-window ()
  "Place the current buffer in the side window at the bottom."
  (interactive)
  (let ((buf (current-buffer)))
    (display-buffer-in-side-window
     buf '((window-width . 0.25)
           (side . left)
           (slot . 1)
           (window-parameters . ((no-delete-other-windows . t)))))
    (delete-window)))

(window-parameters (get-buffer-window (current-buffer)))


(defvar my-saved-window-state nil)

(defun my-save-window-config ()
  "Save current window configuration."
  (interactive)
  ;; (setq my-saved-window-state (current-window-configuration))
  (setq my-saved-window-state (winner-conf))
  (message "Saved window config"))

(defun my-restore-window-config ()
  "Restore saved window configuration."
  (interactive)
  (when my-saved-window-state
    ;; (set-window-configuration my-saved-window-state)
    (winner-set my-saved-window-state)
    ))

;;================================= window rules ================================
(defun my/fit-window-to-buffer-horizontally (window)
  (interactive)
  (let ((fit-window-to-buffer-horizontally t))
    (fit-window-to-buffer window)))

(defun my/find-large-window-to-display (buffer alist)
  "Return the largest window for displaying BUFFER.
The window must not be a side window and not be dedicated.
Among windows of similar size, prefer least recently used (LRU).

To be used with display-buffer-use-some-window's some-window parameter.
"
  (let ((candidates '()))
    ;; Collect all suitable windows
    (dolist (window (window-list (selected-frame) 'nominibuf))
      (when (and (not (window-dedicated-p window))
                 (not (window-parameter window 'window-side)))
        (push window candidates)))

    ;; Return the largest window by area, with LRU as tiebreaker
    (when candidates
      (car (sort candidates
            (lambda (w1 w2)
              (let ((area1 (* (window-height w1) (window-width w1)))
                    (area2 (* (window-height w2) (window-width w2))))
                (if (<= (/ (abs (- area1 area2))
                           (float (max area1 area2)))
                        0.1)
                    ;; Same size - prefer LRU (smaller window use time)
                      (< (window-use-time w1)
                         (window-use-time w2))
                  ;; Different sizes - prefer larger area
                  (> area1 area2)))))))))

(setq display-buffer-alist
      '(("\\*Help\\*"
         (display-buffer-in-side-window)
         (side . right)
         (post-command-select-window . t)
         (window-width . my/fit-window-to-buffer-horizontally))

        ("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
         nil
         (window-parameters (mode-line-format . none)))

        ("\\*Async Shell Command\\*.*"
         display-buffer-no-window)

        ("\\*rg\\*"
         (display-buffer-full-frame)
         (post-command-select-window . t))

        ("^\\*Ilist\\*$"
         imenu-list-display-buffer)

        ("*evil-owl*"
         (display-buffer-in-side-window)
         (side . bottom)
         (window-height . 0.33))

        ("*undo-tree*"
         (display-buffer-in-direction)
         (direction . left)
         (window-width . my/fit-window-to-buffer-horizontally))

        ((major-mode . vterm-mode)
         (display-buffer-reuse-mode-window display-buffer-at-bottom)
         (window-height . 0.25))))

(setq display-buffer-base-action
      '((display-buffer-reuse-window
         display-buffer-in-previous-window
         display-buffer-same-window
         display-buffer-use-some-window) ;; if can't use same window because dedicated
        (some-window . my/find-large-window-to-display)
        ))

(with-eval-after-load 'magit-mode
  (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  )

(provide 'init-window-management)
