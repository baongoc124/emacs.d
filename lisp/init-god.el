(use-package god-mode
  :init
  (setq god-mode-enable-function-key-translation nil)
  :bind (("<escape>" . god-local-mode) ; mimic vim to exit insert mode
         :map god-local-mode-map
         ("i"        . god-local-mode) ; mimic vim to enter insert mode
         ("."        . repeat)
         ("<escape>" . ignore)
         ("S-SPC"    . ngoc/pop-mark))

  :hook ((text-mode . god-local-mode)
         (prog-mode . god-local-mode)
         (conf-mode . god-local-mode))

  :diminish god-local-mode

  :config
  (define-key god-local-mode-map (kbd "h") ngoc-prefix-map)

  (defun my-god-mode-update-cursor-type (&rest args)
    (setq cursor-type 'box)
    (set-face-attribute 'cursor nil :background (if god-local-mode "springgreen3" "coral")))

  (add-hook 'god-mode-enabled-hook #'my-god-mode-update-cursor-type)
  (add-hook 'god-mode-disabled-hook #'my-god-mode-update-cursor-type)
  (add-hook 'window-buffer-change-functions #'my-god-mode-update-cursor-type)
  (add-hook 'window-selection-change-functions #'my-god-mode-update-cursor-type))

(provide 'init-god)
