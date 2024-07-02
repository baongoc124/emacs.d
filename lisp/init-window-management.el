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
;;   (setq eyebrowse-keymap-prefix (kbd "C-'"))
;;   :config
;;   (eyebrowse-mode t))

(use-package transpose-frame)

(provide 'init-window-management)
