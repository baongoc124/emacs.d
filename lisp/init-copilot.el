;; behavior: 1. if copilot shows up, cancel company, except when company is manually started.
;;           2. don't show copilot when god-mode is activated.
;;           3. disable copilot on Leetcode
(use-package copilot
  :vc (:fetcher github :repo "copilot-emacs/copilot.el")
  :bind (:map copilot-completion-map
              ("<escape>" . ngoc/copilot-clear-no-notify)
              ("<tab>"    . copilot-accept-completion) ; don't bind "TAB" so that i can use C-i when i need to adjust indent
              ("M-t"      . copilot-accept-completion-by-word)
              ("M-T"      . copilot-accept-completion-by-line)
              ("M-n"      . copilot-next-completion)
              ("M-p"      . copilot-previous-completion)
              ("M-c"      . ngoc/abort-copilot-start-company))

  :hook
  (prog-mode . copilot-mode)

  :config
  (setq copilot-install-dir (expand-file-name
                             (locate-user-emacs-file (file-name-concat "cache"
                                                                       "copilot"))))

  (defun ngoc/copilot-clear-no-notify ()
    (interactive)
    (copilot-clear-overlay t))

  (defun ngoc/god-mode-not-enabled ()
    (not (bound-and-true-p god-local-mode)))

  (defun ngoc/not-in-leetcode ()
    (not (and (bound-and-true-p atomic-chrome-edit-mode)
              (string-match-p " - LeetCode$" (buffer-name)))))

  (defun ngoc/company-not-manually-started ()
    (if (company-explicit-action-p)
        nil
      (company-abort)
      t))

  (defun ngoc/abort-company-when-copilot-overlay-visible (manually-started)
    (when (and (copilot--overlay-visible)
               (not manually-started))
      (company-abort)))

  (defun ngoc/abort-copilot-start-company ()
    (interactive)
    (copilot-clear-overlay t)             ; pass t to clear overlay so that it doesn't notify server about rejection
    (company-manual-begin))

  (remove-hook  'copilot-enable-predicates          'evil-insert-state-p)  ;  i  don't  use  evil  mode
  (add-hook     'copilot-enable-predicates          'ngoc/god-mode-not-enabled)
  (add-hook     'copilot-enable-predicates          'ngoc/not-in-leetcode)
  (add-hook     'copilot-enable-display-predicates  'ngoc/company-not-manually-started)
  (add-hook     'company-completion-started-hook    'ngoc/abort-company-when-copilot-overlay-visible)

  ;; disable warnings
  (add-to-list 'warning-suppress-types '(copilot copilot-exceeds-max-char))
  (setq copilot-indent-offset-warning-disable t)

  ;; ;; temporary fix for tab key, sometimes overlay is visible but copilot's keymap is not active
  ;; ;; FIXME need to add support for yasnippet, company
  ;; (defun ngoc/copilot-compatible-tab (&optional arg)
  ;;   (interactive "P")
  ;;   (if (copilot--overlay-visible)
  ;;       (copilot-accept-completion)
  ;;     (indent-for-tab-command arg)))

  ;; ;; don't bind this tab outside of prog-mode
  ;; (add-hook 'prog-mode-hook
  ;;           (lambda ()
  ;;             (local-set-key (kbd "<tab>") 'ngoc/copilot-compatible-tab)))

  )

(provide 'init-copilot)
