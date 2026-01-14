(require 'eshell)

(setq eshell-prompt-function
      (lambda ()
        (concat
         (propertize "┌─[" 'face `(:foreground "dark green"))
         (propertize (format-time-string "%H:%M" (current-time)) 'face `(:foreground "dark yellow"))
         (propertize "]──[" 'face `(:foreground "dark green"))
         (propertize (user-login-name) 'face `(:foreground "dark red"))
         (propertize "@" 'face `(:foreground "dark green"))
         (propertize (system-name) 'face `(:foreground "dark blue"))
         (propertize "]──[" 'face `(:foreground "dark green"))
         (propertize (concat (eshell/pwd)) 'face `(:foreground "dark white"))
         (propertize "]\n" 'face `(:foreground "dark green"))
         (propertize "└─>" 'face `(:foreground "dark green"))
         (propertize (if (= (user-uid) 0) " # " " $ ") 'face `(:foreground "dark green"))
         )))

;; https://www.reddit.com/r/emacs/comments/1ddkmmx/comment/l872gr6/?utm_source=share&utm_medium=web3x&utm_name=web3xcss&utm_term=1&utm_content=share_button
;; Bash completion for eshell.
(use-package bash-completion
  :ensure t
  :after eshell
  :config
  (defun +eshell-bash-completion-capf-nonexclusive ()
    "Bash completion function for `completion-at-point-functions'.

Returns the same list as the one returned by
`bash-completion-dynamic-complete-nocomint' appended with
\(:exclusive no) so that other completion functions are tried
when bash-completion fails to match the text at point."
    (let* ((bol-pos (save-mark-and-excursion
                      (eshell-bol)
                      (point)))
           (compl (bash-completion-dynamic-complete-nocomint
                   bol-pos
                   (point) t)))
      (when compl
        (append compl '(:exclusive no)))))

  (defun +eshell-setup-bash-completion-h ()
    (add-hook 'completion-at-point-functions
              #'+eshell-bash-completion-capf-nonexclusive nil t))
  (add-hook 'eshell-mode-hook #'+eshell-setup-bash-completion-h))

(provide 'init-eshell)
