;; (defvar python-prettify-symbols-alist
;;   '(("self"   . ?自)
;;     ("or"     . ?⋁)
;;     ("and"    . ?⋀)
;;     ("not"    . #x2757)
;;     ;; ("not in" . ?∉)
;;     ;; ("in"     . ?∈)
;;     ;; ("is not" . ?≢)
;;     ;; ("is"     . ?≡)
;;     ))

(use-package python
  :config
  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args "-i --simple-prompt")

  ;; set tab-width to python-indent-offset because minuet uses tab-width for
  ;; indentation of completion
  (add-hook 'python-mode-hook
            (lambda ()
              (setq-local tab-width python-indent-offset)
              (setq-local evil-shift-width python-indent-offset)))

  (add-hook 'python-ts-mode-hook
            (lambda ()
              (setq-local tab-width python-indent-offset)
              (setq-local evil-shift-width python-indent-offset)))
  )


(provide 'init-python)
