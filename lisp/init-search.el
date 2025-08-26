(use-package rg
  :config
  (rg-enable-default-bindings (kbd "<leader>h"))

  ;; focus the rg buffer after starting the search
  (add-to-list
   'display-buffer-alist
   '("\\*rg\\*" . (nil . ((body-function . select-window)))))
  )


(use-package visual-regexp-steroids
  :bind (:map evil-normal-state-map
              ("gr" . vr/query-replace))
  :config
  (setq vr/match-separator-use-custom-face t)
  (set-face-attribute 'vr/match-separator-face nil :background 'unspecified :inherit 'default)

  ;; auto save current undo-tree state so that i can jump back to before I started query-replace
  ;; TODO: might be good to use for other commands as well?
  (defun my/save-current-undo-tree-state ()
    (interactive)
    (undo-tree-save-state-to-register ?@)
    )
  (add-hook 'vr/initialize-hook #'my/save-current-undo-tree-state)
  )

(provide 'init-search)
