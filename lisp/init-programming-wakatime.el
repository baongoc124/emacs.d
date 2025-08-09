(use-package wakatime-mode
  :demand t
  :diminish wakatime-mode
  :config
  (setq wakatime-api-key (m/get-password "wakatime.com" "apikey"))
  (global-wakatime-mode 1))
(provide 'init-programming-wakatime)
