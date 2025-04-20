(use-package eat
  :after ivy
  :config
  (keymap-unset eat-semi-char-mode-map "M-t") ;; prevent shadowing my switch window shortcut

  (defun my-set-eat-buffer-name ()
    "Set eat-buffer-name to project name for current buffer."
    (when-let ((proj (project-current)))
      (setq-local eat-buffer-name
                  (format "*eat-%s*" (project-name proj)))))

  (add-hook 'after-change-major-mode-hook #'my-set-eat-buffer-name)

  (defun my-filter-buffers-by-prefix (buffers prefix)
    "Filter buffer list by PREFIX string. Also, remove current buffer from the list"
    (--filter (string-prefix-p prefix (buffer-name it))
              buffers))

  (defun my-counsel-switch-to-project-eat ()
    "Switch to an eat buffer of current project."
    (interactive)
    (let* ((proj (project-current))
           (eat-prefix "*eat")
           (eat-project-prefix (format "*eat-%s" (if proj
                                                     (project-name proj)
                                                   "")))
           (buffers-except-current (delete (current-buffer) (buffer-list)))
           (eat-buffers (my-filter-buffers-by-prefix buffers-except-current eat-prefix))
           (eat-project-buffers (my-filter-buffers-by-prefix eat-buffers eat-project-prefix))
           (final-buffers (or eat-project-buffers
                              eat-buffers))
           )
      (if final-buffers
          (ivy-read "Eat buffers: "
                    (mapcar #'buffer-name final-buffers)
                    ;; reference: counsel-switch-buffer
                    :update-fn #'counsel--switch-buffer-update-fn
                    :unwind #'counsel--switch-buffer-unwind
                    :action #'switch-to-buffer
                    )
        (message "No eat buffer found.")
        )
      )
    )

  (defun my-eat-shell ()
    (interactive)
      (eat "/Users/ngoc/.nix-profile/bin/bash -i" "new-session")
    )

  (global-set-key (kbd "<leader>t c") #'my-eat-shell)
  (global-set-key (kbd "<leader>t t") #'my-counsel-switch-to-project-eat)


  (defun my-eat-find-directory (directory)
    "Open DIRECTORY in EAT terminal, with completion similar to `find-file'."
    (interactive
     (list (read-directory-name "Open directory in terminal: " default-directory)))
    (let ((default-directory (expand-file-name directory)))
      (message "%s" default-directory)
      (eat-term-send-string
       eat-terminal
       (format "cd %s\n" (shell-quote-argument default-directory)))))

  (define-key eat-mode-map (kbd "C-c f") #'my-eat-find-directory)
  )

(provide 'init-terminal)
