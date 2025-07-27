(use-package company
  :diminish company-mode
  :hook
  (prog-mode . company-mode)
  (text-mode . company-mode)

  :bind (("M-<space>" . company-complete)
         :map company-active-map
         ("M-8"       . yas-expand)
         ("TAB"       . company-complete-selection)
         ("<tab>"     . company-complete-selection)
         ("<escape>"  . company-abort)
         ("M-n"       . company-select-next-or-abort)
         ("M-p"       . company-select-previous-or-abort)
         :map company-search-map
         ("<escape>"  . company-abort)
         ("M-n"       . company-select-next-or-abort)
         ("M-p"       . company-select-previous-or-abort))
  :config
  (keymap-unset company-active-map "RET" t)
  (keymap-unset company-active-map "<return>" t)

  (global-set-key (kbd "M-c") #'(lambda ()
                                  (interactive)
                                  (company-cancel)
                                  (company-begin-backend 'company-dabbrev-code)
                                  )

                  )

  (setq company-dabbrev-downcase nil)
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay 0.05)
  (setq company-format-margin-function 'company-text-icons-margin)
  )

(use-package company-posframe
  :diminish company-posframe-mode
  :config
  (company-posframe-mode 1)
  )

(use-package company-quickhelp
  :after company
  :config
  (company-quickhelp-mode 1)
  (setq company-quickhelp-delay 0.35))


(use-package company-prescient
  :after company
  :config
  (company-prescient-mode 1))


(provide 'init-auto-completion)
