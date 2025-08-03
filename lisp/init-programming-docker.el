;===============================================================================
; path mapping for quick jumping from error logs to source files
;===============================================================================
(defun my/get-docker-mounts ()
  "Update docker path mapping from running container."
  (let* ((output (inheritenv-apply 'shell-command-to-string "docker-mounts.sh 2>/dev/null"))
         (pairs (read (concat "(" output ")"))))
    pairs))

(defun my/reset-docker-path-mapping ()
  (interactive)
  (setq compilation-transform-file-match-alist nil))

(defun my/update-docker-path-mapping ()
  "Update `compilation-transform-file-match-alist` using mounts from CONTAINER."
  (interactive)
  (let ((mounts (my/get-docker-mounts)))
    (dolist (pair mounts)
      (let ((docker-path (car pair))
            (host-path (cdr pair)))
        (message "%s:%s" docker-path host-path)
        (add-to-list
         'compilation-transform-file-match-alist
         (list (concat "^" (regexp-quote docker-path)) host-path))))
    (message "Updated compilation path transforms for current project: %s" compilation-transform-file-match-alist)))

(provide 'init-programming-docker)
