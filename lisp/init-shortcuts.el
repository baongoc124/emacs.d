(global-set-key (kbd "C-x k")     'kill-current-buffer)
(global-set-key (kbd "C-0")       'delete-window)
(global-set-key (kbd "C-1")       'delete-other-windows)
(global-set-key (kbd "C-4")       'ctl-x-4-prefix)
(global-set-key (kbd "C-5")       'ctl-x-5-prefix)
(global-set-key (kbd "C-<left>")  'shrink-window-horizontally)
(global-set-key (kbd "C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-<down>")  'shrink-window)
(global-set-key (kbd "C-<up>")    'enlarge-window)

(use-package which-key
  :diminish which-key-mode
  :config
  (setq which-key-show-transient-maps t)
  (which-key-mode 1))

(use-package free-keys)

;; easy exit from transient menu using ESC
(use-package transient
  :config
  (define-key transient-base-map (kbd "<escape>") #'transient-quit-one))

(with-eval-after-load 'evil
  (define-prefix-command #'m/project-prefix-command 'project-prefix-map "Project")

  (setq ngoc/prefix-keymap-alist
        '(("`" . evil-goto-mark)
          ("=" . ngoc/align-dwim)
          ;; ("3" . ngoc/cheatsheet)
          ("M-b" . ibuffer)
          ("b" . counsel-switch-buffer)
          ("c" . evilnc-comment-operator)
          ("d" . detached-open-session)
          ("e" . ngoc/eglot-transient)
          ("f" . project-find-file)
          ("M-f" . format-all-buffer)
          ("g" . ngoc/history-prefix)
          ;; ("m" . iedit-mode)
          ("M-m" . yas-expand)
          ("n" . ngoc/avy-back-to-future)
          ("o" . symbol-overlay-transient)
          ("p" . m/project-prefix-command)
          ("q" . ngoc/fill-function-arguments-dwim)
          ("r" . ngoc/org-roam-prefix)
          ("s" . imenu-list-smart-toggle)
          ("SPC" . consult-project-buffer)
          ("u" . undo-tree-visualize)
          ("v" . ngoc/git-transient)
          ("w" . ngoc/window-prefix)))

  (defun ngoc/bind-leader-map (key-function-pair-alist)
    "Convenient function to bind my evil leader keymap."
    (dolist (pair ngoc/prefix-keymap-alist)
      (let ((key (car pair))
            (function (cdr pair)))
        (evil-define-key nil 'global (kbd (concat "<leader>" key)) function))))

  (ngoc/bind-leader-map ngoc/prefix-keymap-alist)
  )


(provide 'init-shortcuts)
