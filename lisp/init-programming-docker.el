;===============================================================================
; path mapping for quick jumping from error logs to source files
;===============================================================================
(defun my/get-docker-mounts (container)
  "Update docker path mapping from running container."
  (interactive "sDocker compose service name: ")
  (let* ((output (inheritenv-apply 'shell-command-to-string (concat "docker-mounts.sh " container)))
         (pairs (read (concat "(" output ")"))))
    pairs))

(defun my/reset-compilation-transform-file-match-alist ()
  (interactive)
  (setq compilation-transform-file-match-alist nil))

(defun my/update-compilation-path-mappings (container)
  "Update `compilation-transform-file-match-alist` using mounts from CONTAINER."
  (interactive "sDocker compose service name: ")
  (let ((mounts (my/get-docker-mounts container)))
    (dolist (pair mounts)
      (let ((docker-path (car pair))
            (host-path (cdr pair)))
        (message "%s:%s" docker-path host-path)
        (add-to-list
         'compilation-transform-file-match-alist
         (list (concat "^" (regexp-quote docker-path)) host-path))))
    (message "Updated compilation path mappings for %s" container)))


(provide 'init-programming-docker)
