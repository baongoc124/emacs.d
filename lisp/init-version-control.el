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
  :bind (("C-x g" . magit-status))
  :config
  (setq magit-diff-refine-hunk t)
  (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))


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
