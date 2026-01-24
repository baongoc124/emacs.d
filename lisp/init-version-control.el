(use-package ediff
  :ensure nil
  :config
  ;; This is what you probably want if you are using a tiling window
  ;; manager under X, such as ratpoison.
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-diff-options "-w")
  (setq ediff-merge-filename-prefix "")
  (setq ediff-keep-variants t)
  )


(use-package magit
  :bind (("C-x g" . magit-status)
         ("C-c g" . magit-file-dispatch))
  :config
  (setq magit-log-margin-show-committer-date t) ;; to match with bitbucket display
  (setq magit-diff-refine-hunk t)
  (setq magit-save-repository-buffers 'dontask)
  (setq magit-branch-read-upstream-first nil)

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

  (defun my/magit-show-deleted-files ()
    "Display commit logs for deleted files with committer details."
    (interactive)
    (let ((buffer (get-buffer-create "*Magit Deleted Files*")))
      (with-current-buffer buffer
	    (let ((inhibit-read-only t))
	      (erase-buffer)
	      (magit-mode)
	      (font-lock-mode 1)
	      (insert (propertize "COMMITS WITH DELETED FILES\n\n"
			                  'face 'magit-section-heading))
	      (call-process "git" nil buffer t "log" "--name-status"
			            "--diff-filter=D"
			            "--format=%n%n%h %cd %s%n%cn <%ce>" "--date=short")
	      (goto-char (point-min))
	      (while (re-search-forward "^\\([0-9a-f]+\\) \\(.*\\)$" nil t)
	        (put-text-property (match-beginning 1) (match-end 1)
			                   'face 'magit-log-author)
	        (put-text-property (+ (line-beginning-position) 18) (line-end-position)
			                   'face 'magit-keyword)
	        (next-line)
	        (put-text-property (line-beginning-position) (line-end-position)
			                   'face 'magit-dimmed)
	        )
	      (goto-char (point-min))
	      (while (re-search-forward "^D\t\\(.*\\)$" nil t)
	        (put-text-property (match-beginning 1) (match-end 1)
			                   'face 'magit-filename))
	      ))
      (switch-to-buffer buffer)
      (goto-char (point-min))
      (setq buffer-read-only t)))


  (transient-append-suffix 'magit-log "s"
    '("M-d" "Show deleted files" my/magit-show-deleted-files))


  ;; TODO: add this to stash popup
  (defun my/magit-stash-edit-message (stash message)
    "Change STASH's message to MESSAGE."
    (interactive
     (let* ((stash (magit-read-stash "Rename"))
            (old-msg (magit-git-string "show" "-s" "--format=%s" stash)))
       (list stash (magit-read-string "Stash message" old-msg))))
    (let ((commit (magit-rev-parse stash))
          (inhibit-magit-refresh t))
      (magit-stash-drop stash)
      (magit-stash-store message "refs/stash" commit))
    (magit-refresh))
  )

(use-package forge
  :after magit
  :config
  )

(use-package magit-todos
  :after magit
  :config (magit-todos-mode -1))


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


(use-package git-modes)


;; show git's current checked out branch or commit on header of dired buffers
(require 'dired)
(defvar-local dired-git-branch nil
  "Current git branch for dired buffer.")

(defun dired-git-branch-update ()
  "Update git branch information."
  (when default-directory
    (setq dired-git-branch (string-trim
                            (shell-command-to-string
                             "git symbolic-ref -q --short HEAD 2>/dev/null || git rev-parse --short HEAD 2>/dev/null || echo Not git"
                             )))))

(defun dired-git-refresh-project-buffers ()
  "Refresh git branch info in dired buffers of current project."
  (when-let ((project-root (magit-toplevel)))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (and (derived-mode-p 'dired-mode)
                   (string-prefix-p project-root
                                    (expand-file-name default-directory)))
          (dired-git-branch-update)
          (force-mode-line-update))))))

;; show on header version
(define-minor-mode dired-git-branch-mode
  "Minor mode to display git branch in dired header line."
  :lighter ""
  (if dired-git-branch-mode
      (progn
        (dired-git-branch-update)
        (add-hook 'dired-after-readin-hook #'dired-git-branch-update nil t)
        (setq header-line-format
              '(:eval (if dired-git-branch
                          (format " 🌱 %s" (propertize dired-git-branch
                                                       'face '(:height 1.1 :inherit font-lock-string-face)
                                                       ))
                        " Not a git repository"))))
    (setq header-line-format nil)
    (remove-hook 'dired-after-readin-hook #'dired-git-branch-update t)))

;; (define-minor-mode dired-git-branch-mode
;;   "Minor mode to display git branch in dired mode line."
;;   :lighter ""
;;   (if dired-git-branch-mode
;;       (progn
;;         (dired-git-branch-update)
;;         (add-hook 'dired-after-readin-hook #'dired-git-branch-update nil t)
;;         (setq-local dired-git-original-mode-line mode-line-format)
;;         (setq-local mode-line-format
;;                     (append mode-line-format
;;                             '((:eval (if dired-git-branch
;;                                          (format "Y:%s" dired-git-branch)
;;                                        ""))))))
;;     (when (boundp 'dired-git-original-mode-line)
;;       (setq-local mode-line-format dired-git-original-mode-line))
;;     (remove-hook 'dired-after-readin-hook #'dired-git-branch-update t)))

(add-hook 'dired-mode-hook #'dired-git-branch-mode)
(add-hook 'magit-post-refresh-hook #'dired-git-refresh-project-buffers)


(use-package difftastic-bindings
  :vc (:fetcher github :repo "pkryger/difftastic.el")
  ;; :ensure difftastic ;; or nil if you prefer manual installation
  :config (difftastic-bindings-mode))

(defun my/magit-cherry-pick-onto-branch ()
  "Cherry-pick commits from current branch onto a selected target branch.
Creates a new branch with random suffix. Uses ivy for branch selection."
  (interactive)
  (let* ((repo (magit-toplevel))
         (default-directory repo)
         ;; Fetch origin
         (_ (magit-fetch-from-upstream "origin" nil))
         ;; Get all remote branches
         (target-branches (magit-list-remote-branch-names))
         ;; Let user select target branch with ivy
         (target-branch
          (ivy-read "Select target branch: " target-branches
                    :preselect "origin/beta"
                    :require-match t
                    :caller 'my/magit-cherry-pick-onto-branch))
         ;; Get current branch
         (current-branch (magit-get-current-branch))
         ;; Create random suffix
         (random-suffix (format "%04x" (random #xffff)))
         ;; Name of new branch
         (new-branch (concat current-branch "-" random-suffix))
         (upstream-branch (magit-git-string "rev-parse" "--abbrev-ref" "--symbolic-full-name" "@{u}"))
         ;; Find merge base between upstream branch and current branch
         (merge-base (string-trim
                      (shell-command-to-string
                       (format "git merge-base %s %s" upstream-branch current-branch)))))
    (unless merge-base
      (error "Could not find merge base between %s and %s"
             target-branch current-branch))
    ;; Create new branch from target
    (magit-call-git "checkout" "-b" new-branch target-branch)
    ;; Cherry-pick from merge base to HEAD
    (let ((cherry-range (format "%s..%s" merge-base current-branch)))
      (magit-call-git "cherry-pick" cherry-range))
    (message "Created %s from %s with cherry-picked commits after %s. Remember to push after fixing any merging issues."
             new-branch target-branch merge-base)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; extract JIRA-style ticket IDs from branch name and insert it into commit
;; message automatically
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my/git-extract-ticket-name ()
  "Extract ticket ID from branch if it starts with 'feature/'."
  (when-let* ((branch (magit-get-current-branch)))
    (when (string-match "\\([[:alnum:]]+-[[:digit:]]+\\)" branch)
      (match-string 1 branch))))

(defun my/git-prepare-commit-message ()
  "Prepare commit message based on branch name if first line is empty."
  (when-let ((feature (my/git-extract-ticket-name)))
    (goto-char (point-min))
    ;; Check if first line is empty or whitespace-only
    (when (looking-at "^\\s-*$")
      (delete-region (line-beginning-position) (line-end-position))
      (insert "\n")
      (goto-char (point-min))
      (insert feature " - ")
      )))

(eval-after-load 'magit
  '(add-hook 'git-commit-setup-hook 'my/git-prepare-commit-message))


;; quickly open web link of current repo at current file, current line
(use-package browse-at-remote
  :bind ("C-c C-g" . browse-at-remote))

(provide 'init-version-control)
