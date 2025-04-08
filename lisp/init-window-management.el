;; prevent top bottom split
(setq split-height-threshold nil)


(define-prefix-command 'ngoc/window-prefix)
(define-key ngoc/window-prefix "d" #'delete-window)
(define-key ngoc/window-prefix "e" #'balance-windows)
(define-key ngoc/window-prefix "w" #'delete-other-windows)
(define-key ngoc/window-prefix "h" #'hsplit-last-buffer)
(define-key ngoc/window-prefix "v" #'vsplit-last-buffer)
(define-key ngoc/window-prefix "t" #'transpose-frame)
(define-key ngoc/window-prefix "x" #'ace-swap-window)

(use-package ace-window
  :bind ("M-o" . ace-window)
  :config
  (setq aw-keys '(?h ?t ?n ?s ?a ?o ?e ?u)
        aw-scope 'frame)

  (global-set-key (kbd "M-O") #'(lambda () (interactive)
                                  (let ((aw-dispatch-always t))
                                    (call-interactively #'ace-window))))
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




(provide 'init-window-management)
