(require 'dired)

;; gls is used by insert-directory-program
(when (eq system-type 'darwin)
  (let ((gls (executable-find "gls")))
    (unless gls
      (display-warning 'user-config "GNU ls (gls) not found. Dired might fail to work."))))

(setq dired-listing-switches "-lAh --group-directories-first")

(setq delete-by-moving-to-trash t)
(setq dired-dwim-target nil)

;; make this consistent with ripgrep
(keymap-set dired-mode-map (kbd "e") #'dired-toggle-read-only)

(defun ngoc/dired-open-last-downloaded ()
  "Open Download folder and select last downloaded file."
  (interactive)
  (dired "~/Downloads" "-laht")
  (beginning-of-buffer)
  (dired-goto-next-nontrivial-file))

(defun ngoc/make-line-taller ()
  (setq line-spacing 0.2))

;; increase line spacing in dired mode -> easier to read
(add-hook 'dired-mode-hook #'ngoc/make-line-taller)

;;; file opening procedures
(defun ngoc/dired-open-containing-dir ()
  "Try to run `nautilus' or `open' to open the containing folder of file under point and select it."
  (interactive)
  (let ((command
         (cond
          ((eq system-type 'gnu/linux)
           '("nautilus" . "nautilus -s "))
          ((eq system-type 'darwin)
           '("open" . "open -R "))
          )))
    (if (executable-find (car command))
        (let ((file (ignore-errors (dired-get-file-for-visit))))
          (start-process-shell-command (car command)
                                       nil
                                       (concat (cdr command)
                                               (shell-quote-argument (file-truename file)))))
      (message "%s not found" (car command))))
  )


(define-key dired-mode-map (kbd "<S-return>") #'ngoc/dired-open-containing-dir)

(use-package terminal-here
  :config
  (setq terminal-here-mac-terminal-command 'iterm2)
  (setq terminal-here-linux-terminal-command 'gnome-terminal)
  ;; (setq terminal-here-command-flag "--")
  (global-set-key (kbd "C-S-<return>") 'terminal-here-launch)
  )


(use-package nerd-icons)

(use-package nerd-icons-dired
  :after nerd-icons
  :diminish nerd-icons-dired-mode
  :hook
  (dired-mode . nerd-icons-dired-mode))

;; FIXME this is currently working GLOBALLY!
;; https://emacs.stackexchange.com/a/51614 with modification
(define-minor-mode ngoc/dired-follow-mode
  "Diplay file at point in dired after a move / delete."
  :lighter " dired-f"
  :global t
  (cond
   (ngoc/dired-follow-mode
    (advice-add 'dired-next-line :after (lambda (&rest arg) (dired-display-file)))
    (advice-add 'dired-internal-do-deletions :after (lambda (&rest arg) (dired-display-file))))
   (t
    (advice-remove 'dired-next-line (lambda (&rest arg) (dired-display-file)))
    (advice-remove 'dired-internal-do-deletions (lambda (&rest arg) (dired-display-file))))))

; alias for quick invoke
(defalias 'fnd 'find-name-dired)

(defun m/find-name-project ()
  "Find files in project by name."
  (interactive)
  (let ((default-directory (project-root (project-current))))
    (call-interactively 'find-name-dired)))

(defalias 'fnp #'m/find-name-project)

(use-package dired-filter)

(use-package dired-ranger)


(provide 'init-dired)
