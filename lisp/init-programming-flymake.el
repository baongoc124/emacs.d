(use-package flymake)

;; (use-package flymake-diagnostic-at-point
;;   :after flymake
;;   :config
;;   (add-hook 'flymake-mode-hook #'flymake-diagnostic-at-point-mode)
;;   (setq flymake-no-changes-timeout 1.1))

(use-package flymake-posframe
  :load-path (expand-file-name "lisp/flymake-posframe" user-emacs-directory)
  :hook (flymake-mode . flymake-posframe-mode))


(provide 'init-programming-flymake)
