;; cheatsheet
;; allow me to define a cheatsheet file for each type of major-mode
;; and quickly open cheatsheet with a shortcut key
(defvar-local ngoc/cheatsheet-file-name nil)
(defvar ngoc/cheatsheet-dir "~/Dropbox/org/cheatsheets/")
(defvar ngoc/cheatsheet-file-alist
  '((java-ts-mode . "java.org")))

(defun ngoc/cheatsheet-setup ()
  (interactive)
  (setq ngoc/cheatsheet-file-name
        (cdr (assq major-mode ngoc/cheatsheet-file-alist))))

(add-hook 'prog-mode-hook #'ngoc/cheatsheet-setup)

(defun ngoc/cheatsheet ()
  (interactive)
  (if ngoc/cheatsheet-file-name
      (find-file (file-name-concat ngoc/cheatsheet-dir
                                   ngoc/cheatsheet-file-name))
    (message "No cheatsheet file found")))

(provide 'init-cheatsheet)
