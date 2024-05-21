(require 'dired)
(setq dired-listing-switches "-lah")
(setq delete-by-moving-to-trash t)
(setq dired-dwim-target nil)


(defun ngoc/dired-open-last-downloaded ()
  "Open Download folder and select last downloaded file."
  (interactive)
  (dired "~/Downloads" "-laht")
  (beginning-of-buffer)
  (dired-goto-next-nontrivial-file))

(require 'init-prefix-map)
(keymap-set ngoc-prefix-map "j" 'ngoc/dired-open-last-downloaded)

(defun ngoc/make-line-taller ()
  (setq line-spacing 0.2))

;; increase line spacing in dired mode -> easier to read
(add-hook 'dired-mode-hook #'ngoc/make-line-taller)

;;; file opening procedures
(defun ngoc/dired-open-containing-dir ()
  "Try to run `nautilus' to open the containing folder of file under point and select it."
  (interactive)
  (if (executable-find "nautilus")
      (let ((file (ignore-errors (dired-get-file-for-visit))))
        (start-process-shell-command "nautilus"
                                     nil
                                     (concat "nautilus -s "
                                             (shell-quote-argument (file-truename file)))))
    (message "xdg-open not found")))


(define-key dired-mode-map (kbd "<S-return>") #'ngoc/dired-open-containing-dir)

(use-package terminal-here
  :config
  (progn
    (setq terminal-here-linux-terminal-command 'gnome-terminal
          terminal-here-command-flag "--")
    (global-set-key (kbd "C-!") 'terminal-here-launch)))


(use-package nerd-icons)

(use-package nerd-icons-dired
  :after nerd-icons
  :hook
  (dired-mode . nerd-icons-dired-mode))

(provide 'init-dired)
