(defun toggle-fixme-nndebug ()
  "Toggle a 'FIXME: NNDEBUG' comment above the current line.
If such a comment exists in the current paragraph (current line or above),
remove it. Otherwise, add it above the current line."
  (interactive)
  (save-excursion
    (let ((current-line (line-number-at-pos))
          (comment-prefix (or comment-start "// ")) ; Use language-specific comment or default to //
          (fixme-pattern (concat "\\s-*" (regexp-quote comment-start) "\\s-*FIXME:\\s-*NNDEBUG\\s-*$"))
          found-line)

      ;; First, look for existing FIXME: NNDEBUG in current paragraph
      ;; Search upward from current line to beginning of paragraph
      (beginning-of-line)
      (while (and (not found-line)
                  (not (bobp))
                  (not (looking-at "^\\s-*$"))) ; Stop at blank line (paragraph boundary)
        (when (looking-at fixme-pattern)
          (setq found-line (line-number-at-pos)))
        (unless found-line
          (forward-line -1)))

      ;; Also check current line
      (goto-line current-line)
      (when (looking-at fixme-pattern)
        (setq found-line current-line))

      (if found-line
          ;; Remove the existing FIXME comment
          (progn
            (goto-line found-line)
            (beginning-of-line)
            (kill-whole-line)
            (message "Removed FIXME: NNDEBUG comment"))

        ;; Add new FIXME comment above current line
        (goto-line current-line)
        (beginning-of-line)
        (let ((current-indentation (current-indentation)))
          (open-line 1)
          (insert (concat (make-string current-indentation ?\s)
                         comment-prefix "FIXME: NNDEBUG")))
        (message "Added FIXME: NNDEBUG comment")))))

(global-set-key (kbd "<leader> SPC") 'toggle-fixme-nndebug)

(provide 'init-programming-toggle-debug-comment)
