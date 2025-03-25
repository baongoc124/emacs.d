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
        aw-scope 'frame))


(use-package winner
  :demand t ;; load immediately to save window configuration
  :bind (("M-F" . winner-redo)
         ("M-B" . winner-undo))
  :config
  (winner-mode 1))

;;(setq win:base-key ?`)		;; '`' is before 'a'
;;(setq win:max-configs 27)	;; '`' to 'z' are 27 chars.
;;(setq win:quick-selection nil)	;; Not assign `C-c LETTER'
;;(setq win:switch-prefix "\C-q")
;;(setq win:use-frame nil)
;;(define-key global-map win:switch-prefix nil)
;;(require 'windows)
;;(win:startup-with-window)
;;(define-key ctl-x-map "C" 'see-you-again)
;;(define-key win:switch-map "\C-q" 'win-toggle-window)
;;(define-key win:switch-map "n" 'win-switch-to-window)
;;(define-key win:switch-map "p" 'win-switch-to-window)

;; (use-package eyebrowse
;;   :init
;;   (setq eyebrowse-keymap-prefix (kbd "<f8>"))
;;   :config
;;   (eyebrowse-mode t)
;;   (define-key eyebrowse-mode-map (kbd "<f8> <f8>") 'eyebrowse-last-window-config)
;;   (setq eyebrowse-mode-line-style 'always)
;;   (setq eyebrowse-mode-line-left-delimiter "《 ")
;;   (setq eyebrowse-mode-line-right-delimiter " 》")
;;   (setq eyebrowse-mode-line-separator "  ")
;;   )


(use-package modern-tab-bar
  :vc (:fetcher github :repo aaronjensen/emacs-modern-tab-bar)
  :init
  (setq tab-bar-show t
        tab-bar-close-button-show t
        tab-bar-tab-hints t)

  (modern-tab-bar-mode))


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

(provide 'init-window-management)
