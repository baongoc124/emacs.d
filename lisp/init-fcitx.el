(use-package fcitx
  :after god-mode
  :if (eq system-type 'gnu/linux)
  :config
  (setq fcix-use-dbus 'fcitx5)
  (setq fcitx-remote-command "fcitx5-remote")
  (fcitx-default-setup))

(provide 'init-fcitx)
