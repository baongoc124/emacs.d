(use-package company
  :diminish company-mode
  :hook
  (prog-mode . company-mode)
  (text-mode . company-mode)

  :bind (("M-<space>" . company-complete)
         ("M-8"       . company-yasnippet)
         :map company-active-map
         ("<escape>"  . company-abort)
         ("M-n"       . company-select-next-or-abort)
         ("M-p"       . company-select-previous-or-abort)
         :map company-search-map
         ("<escape>"  . company-abort)
         ("M-n"       . company-select-next-or-abort)
         ("M-p"       . company-select-previous-or-abort))
  :config
  (setq company-dabbrev-downcase nil)
  (setq company-minimum-prefix-length 2)
  (setq company-idle-delay 0.05)
  (setq company-format-margin-function 'company-text-icons-margin))

(use-package company-quickhelp
  :after company
  :config
  (company-quickhelp-mode 1)
  (setq company-quickhelp-delay 0.35))


(provide 'init-auto-completion)
