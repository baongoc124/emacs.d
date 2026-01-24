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

(defun my/kill-deleted-buffers ()
  "Kill buffers for deleted files and directories."
  (interactive)
  (let ((killed-count 0))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (or (and (buffer-file-name)
                       (not (file-exists-p (buffer-file-name))))
                  (and (eq major-mode 'dired-mode)
                       (not (file-exists-p dired-directory))))
          (kill-buffer buf)
          (setq killed-count (1+ killed-count)))))
    (message "Killed %d buffer(s) for deleted files/directories" killed-count)))

(provide 'init-buffer)
