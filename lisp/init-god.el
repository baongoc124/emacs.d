(use-package god-mode
  :bind (("<escape>" . god-local-mode) ; mimic vim to exit insert mode
         :map god-local-mode-map
         ("i"        . god-local-mode) ; mimic vim to enter insert mode
         ("."        . repeat)
         ("<escape>" . ignore)
         ("<f1>"     . help-command))

  :hook ((text-mode . god-local-mode)
         (prog-mode . god-local-mode)
         (conf-mode . god-local-mode))

  :diminish god-local-mode

  :config
  (define-key god-local-mode-map (kbd "h") ngoc-prefix-map)

  (defun my-god-mode-update-cursor-type ()
    (setq cursor-type (if god-local-mode 'box 'bar)))

  (add-hook 'post-command-hook #'my-god-mode-update-cursor-type))

(provide 'init-god)
