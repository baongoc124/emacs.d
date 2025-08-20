;; custom commands to paste from register but trim whitespaces before & after content
;; TODO: implement evil-paste-pop and evil-paste-pop-next

(with-eval-after-load 'evil
  (evil-define-command evil-trim-paste-after (count &optional register yank-handler)
    "Temporarily trim register content, then call normal evil-paste."
    (interactive "*P<x>")
    (let* ((reg (or register ?\"))
           (orig (evil-get-register reg))
           (trimmed (and orig (string-trim orig))))
      (when trimmed
        ;; Temporarily set the register to trimmed content
        (evil-set-register reg (substring-no-properties trimmed)))

      ;; Call normal evil paste after cursor
      (call-interactively 'evil-paste-after)

      (when trimmed
        (evil-set-register reg orig))))


  (evil-define-command evil-trim-paste-before (count &optional register yank-handler)
    "Temporarily trim register content, then call normal evil-paste."
    (interactive "*P<x>")
    (let* ((reg (or register ?\"))
           (orig (evil-get-register reg))
           (trimmed (and orig (string-trim orig))))
      (when trimmed
        ;; Temporarily set the register to trimmed content
        (evil-set-register reg (substring-no-properties trimmed)))

      ;; Call normal evil paste before cursor
      (call-interactively 'evil-paste-before)
      ;; Restore original content
      (when trimmed
        (evil-set-register reg orig))))


  (define-key evil-normal-state-map (kbd "zp") #'evil-trim-paste-after)
  (define-key evil-normal-state-map (kbd "zP") #'evil-trim-paste-before)
  )

(provide 'init-evil-paste)
