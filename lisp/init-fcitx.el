(use-package fcitx
  :after god-mode
  :if (eq system-type 'gnu/linux)
  :config
  (setq fcix-use-dbus 'fcitx5)
  (setq fcitx-remote-command "fcitx5-remote")
  (fcitx-default-setup)

  (fcitx--defun-maybe "god")

  (add-hook 'god-local-mode-hook
            #'(lambda ()
                (if god-local-mode
                    (fcitx--god-maybe-deactivate)
                  (fcitx--god-maybe-activate)))))

(provide 'init-fcitx)
