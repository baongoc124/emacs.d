(use-package plantuml-mode
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))
  :config
  (setq plantuml-default-exec-mode 'executable)
  )

(provide 'init-plantuml)
