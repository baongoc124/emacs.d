;; need a auto completion engine as a fallback for when eglot chokes (not in a project or buffer not visiting a file)
(use-package anaconda-mode
  :hook
  (python-mode . anaconda-mode)
  (python-mode . anaconda-eldoc-mode)
  (python-ts-mode . anaconda-mode)
  (python-ts-mode . anaconda-eldoc-mode))

(use-package company-anaconda
  :after (eglot company)
  :config
  (add-to-list 'company-backends '(company-anaconda :with company-capf))
  (add-hook 'eglot-managed-mode-hook
            #'(lambda ()
                (when (and (member major-mode '(python-mode python-ts-mode))
                           (eglot-managed-p))
                  (anaconda-mode -1)
                  (anaconda-eldoc-mode -1)))))


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
