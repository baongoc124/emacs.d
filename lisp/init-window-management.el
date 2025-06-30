;; prevent top bottom split
(setq split-height-threshold nil)


(define-prefix-command 'ngoc/window-prefix)
(define-key ngoc/window-prefix "c" #'my-ace-copy-window)
(define-key ngoc/window-prefix "d" #'delete-window)
(define-key ngoc/window-prefix "e" #'balance-windows)
(define-key ngoc/window-prefix "h" #'hsplit-last-buffer)
(define-key ngoc/window-prefix "m" #'my-ace-move-window)
(define-key ngoc/window-prefix "t" #'transpose-frame)
(define-key ngoc/window-prefix "v" #'vsplit-last-buffer)
;; (define-key ngoc/window-prefix "w" #'delete-other-windows)
(define-key ngoc/window-prefix "x" #'ace-swap-window)
(define-key ngoc/window-prefix "s" #'my-save-window-config)
(define-key ngoc/window-prefix "w" #'my-restore-window-config)

(use-package ace-window
  :bind ("M-t" . ace-window)
  :config
  (setq aw-keys '(?h ?c ?t ?m ?w ?n ?e ?u ?j ?q)
        aw-scope 'frame)

  (global-set-key (kbd "M-T") #'(lambda () (interactive)
                                  (let ((aw-dispatch-always t))
                                    (call-interactively #'ace-window))))
  (ace-window-posframe-mode 1)
  (set-face-attribute 'aw-leading-char-face nil :height 3.0)

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

; duplicate tab and set name
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
  (setq my-saved-window-state (window-state-get (frame-root-window) t))
  (message "Saved window config"))

(defun my-restore-window-config ()
  "Restore saved window configuration."
  (interactive)
  (when my-saved-window-state
    (window-state-put my-saved-window-state (frame-root-window))))

;; (global-set-key (kbd "M-m w s") #'my-save-window-config)
;; (global-set-key (kbd "M-m w w") #'my-restore-window-config)



(provide 'init-window-management)
