(use-package company
  :diminish company-mode
  :hook
  (prog-mode . company-mode)
  (text-mode . company-mode)

  :bind (("M-<space>" . company-complete)
         :map company-active-map
         ("M-8"       . yas-expand)
         ("TAB"       . ngoc/company-complete-common-or-selection)
         ("<tab>"     . ngoc/company-complete-common-or-selection)
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
  (setq company-dabbrev-downcase nil)
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay 0.05)
  (setq company-format-margin-function 'company-text-icons-margin)

  (defun ngoc/company-complete-common-or-selection ()
    "Complete common if has one, else complete current selection."
    (interactive)
    (if (and (length> company-common 0)
             (not (equal company-common company-prefix)))
        (company-complete-common)
      (company-complete-selection))))

(use-package company-quickhelp
  :after company
  :config
  (company-quickhelp-mode 1)
  (setq company-quickhelp-delay 0.35))


(provide 'init-auto-completion)
