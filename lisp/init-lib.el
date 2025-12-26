(defun my/json-string-escape-region (start end)
  "Escape the region as a JSON string using jq -Rs."
  (interactive "r")
  (shell-command-on-region start end "jq -Rs" (current-buffer) t))

(defun my/json-string-unescape-region (start end)
  "Unescape the region as a JSON using jq -r."
  (interactive "r")
  (shell-command-on-region start end "jq -r" (current-buffer) t))

(defun my/json-prettify-region (start end)
  "Escape the region as a JSON string using jq -Rs."
  (interactive "r")
  (shell-command-on-region start end "jq" (current-buffer) t))

(provide 'init-lib)
