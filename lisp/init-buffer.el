;; buffer naming
(setq uniquify-buffer-name-style 'post-forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)    ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers
(require 'uniquify)

(use-package ibuffer)

;; (use-package ibuffer-projectile
;;   :after (ibuffer)

;;   :hook
;;   (ibuffer . ngoc/ibuffer-setup)

;;   :config
;;   (defun ngoc/ibuffer-setup ()
;;     (ibuffer-projectile-set-filter-groups)
;;     (unless (eq ibuffer-sorting-mode 'filename/process)
;;       (ibuffer-do-sort-by-filename/process))))

(use-package nerd-icons-ibuffer
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IBUFFER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (use-package ibuffer-tramp
;;   :config
;;   (add-hook 'ibuffer-hook
;;             (lambda ()
;;               (ibuffer-tramp-set-filter-groups-by-tramp-connection)
;;               (ibuffer-do-sort-by-alphabetic))))


(provide 'init-buffer)
