(use-package quickselect
  :ensure nil
  :demand t
  :config
  (quickselect-mode 1)
  (global-set-key (kbd "C-q") #'quickselect-mark)
  (global-set-key (kbd "C-'") #'quoted-insert))

(provide 'init-programming)
