(use-package rg
  :config
  (rg-enable-default-bindings (kbd "<leader>/"))

  ;; focus the rg buffer after starting the search
  (add-to-list
   'display-buffer-alist
   '("\\*rg\\*" . (nil . ((body-function . select-window)))))
  )

(provide 'init-search)
