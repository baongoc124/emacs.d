(use-package ediff
  :ensure nil
  :config
  ;; This is what you probably want if you are using a tiling window
  ;; manager under X, such as ratpoison.
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-diff-options "-w")
  (setq ediff-merge-filename-prefix ""))


(use-package magit
  :bind (("C-x g" . magit-status)
         ("C-c g" . magit-file-dispatch))
  :config
  (setq magit-log-margin-show-committer-date t) ;; to match with bitbucket display
  (setq magit-diff-refine-hunk t)
  (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)
  (add-hook 'magit-status-headers-hook #'magit-insert-repo-header)
  (add-hook 'magit-status-headers-hook #'magit-insert-user-header)

  (defun ngoc/disable-evil-on-magit-blame ()
    (if (bound-and-true-p magit-blame-mode)
        (evil-emacs-state)
      (evil-normal-state)))

  (add-hook 'magit-blame-mode-hook #'ngoc/disable-evil-on-magit-blame)

  ;; https://mbork.pl/2022-11-19_Streamlining_my_workflow_with_Magit_and_BitBucket
  (defun magit-open-pull-request-pr ()
    "Open the pull request URL if applicable."
    (interactive)
    (save-excursion
      (set-buffer (magit-process-buffer t))
      (goto-char (point-max))
      (magit-section-backward)
      (when
          (search-backward-regexp "remote: \\(To create a merge\\|Create pull\\) request" nil t)
        (forward-line 1)
        (re-search-forward "remote: +" (line-end-position) t)
        (browse-url-at-point))))

  (defun my/pr-create ()
    "Create a BitBucket PR for the current branch."
    (interactive)
    (let* ((branch-name (magit-get-current-branch))
           (repo-url (magit-get "remote" "origin" "url"))
           (repo-info (when (string-match "git@bitbucket.org:\\(.+\\)\\.git" repo-url)
                        (match-string 1 repo-url)))
           (target-branch (completing-read "Target branch: " '("master" "develop" "feature/test")))
           (pr-url (when repo-info
                     (format "https://bitbucket.org/%s/pull-requests/new?source=%s&dest=%s"
                             repo-info
                             branch-name
                             target-branch
                             )
                     )))
      (if pr-url
          (browse-url pr-url)
        (message "Could not determine BitBucket repository URL."))))
  )

(use-package forge
  :after magit
  :config
  )

(use-package magit-todos
  :after magit
  :config (magit-todos-mode 1))


(use-package diff-hl
  :config
  (global-diff-hl-mode t)
  (diff-hl-flydiff-mode t))


(use-package git-gutter
  :config
  (defun ngoc/maybe-enable-git-gutter ()
    (diff-hl-mode -1)
    (git-gutter-mode 1))

  (defun ngoc/maybe-disable-git-gutter ()
    (unless transient--prefix
      (diff-hl-mode 1)
      (diff-hl-update)
      (git-gutter-mode -1)))

  (defun ngoc/git-gutter-toggle-diff ()
    (interactive)
    (when (bound-and-true-p git-gutter-mode)
      (let* ((diff-buffer (get-buffer git-gutter:popup-buffer))
             (diff-window (git-gutter:popup-buffer-window)))
        (if (and diff-buffer diff-window)
            (delete-window diff-window)
          (git-gutter:popup-hunk)))))

  (transient-define-prefix ngoc/git-transient ()
    "Git transient"
    ["Git menu"
     [("n" "Next hunk" git-gutter:next-hunk :transient t)
      ("p" "Previous hunk" git-gutter:previous-hunk :transient t)]
     [("S" "Stage hunk" git-gutter:stage-hunk :transient t)
      ("R" "Revert hunk" git-gutter:revert-hunk :transient t)]
     [("d" "Toggle diff" ngoc/git-gutter-toggle-diff :transient t)
      ("m" "Magit file" magit-file-dispatch :transient transient--do-replace)]]

    (interactive)
    (cond
     ((buffer-modified-p)
      (message "Please save first!")
      (ding))
     (t
      (ngoc/maybe-enable-git-gutter)
      (add-hook 'transient-exit-hook #'ngoc/maybe-disable-git-gutter)
      (transient-setup 'ngoc/git-transient)))))

(provide 'init-version-control)
