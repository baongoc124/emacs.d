;; to rebind C-h and M-h to personal prefix map
(defvar ngoc-prefix-map (make-keymap "😎"))

(global-set-key (kbd "C-<f9>") ngoc-prefix-map)
(global-set-key (kbd "M-h") ngoc-prefix-map)

(setq ngoc/prefix-keymap-alist
      '(("=" . ngoc/align-dwim)
        ("M-b" . ibuffer)
        ("b" . counsel-switch-buffer)
        ("c" . avy-goto-char-timer)
        ("d" . duplicate-dwim)
        ("e" . ngoc/eglot-transient)
        ;; ("f" . counsel-projectile-find-file)
        ("g" . ngoc/history-prefix)
        ("h" . mark-paragraph)
        ("m" . iedit-mode)
        ("n" . avy-goto-line)
        ("o" . symbol-overlay-transient)
        ("q" . ngoc/fill-function-arguments-dwim)
        ("r" . ngoc/org-roam-prefix)
        ("s" . imenu-list-smart-toggle)
        ("t" . avy-goto-word-or-subword-1)
        ("u" . undo-tree-visualize)
        ("v" . ngoc/git-transient)
        ("w" . ngoc/window-prefix)))

(defun ngoc/mass-bind-keymap (keymap key-function-pair-alist)
  "Convenient function to bind a list of key-function pairs to a keymap."
  (dolist (pair ngoc/prefix-keymap-alist)
    (let ((key (car pair))
          (function (cdr pair)))
      (keymap-set keymap key function))))

(ngoc/mass-bind-keymap ngoc-prefix-map ngoc/prefix-keymap-alist)


;; FIXME this makes me cannot access other keybindngs contains C-h
;; but it's a good tradeoff
(define-key key-translation-map [?\C-h] (kbd "C-<f9>"))


(defun ngoc/ensure-prefix-binding ()
  "If a major mode binds M-h, then bind it to `ngoc-prefix-map' instead,
 and bind \"M-h h\" to the original function."
  (interactive)
  (let* ((local-map (current-local-map))
         (original-function (lookup-key local-map (kbd "M-h")))
         (child-prefix-map (make-sparse-keymap "🤠"))
         (new-god-map (make-sparse-keymap))
         (new-local-map (make-sparse-keymap)))
    (when (and local-map
               original-function)
      (set-keymap-parent child-prefix-map ngoc-prefix-map)
      (setq-local ngoc-prefix-map child-prefix-map)
      (define-key ngoc-prefix-map (kbd "h") original-function)

      (set-keymap-parent new-local-map local-map)
      (use-local-map new-local-map)
      (define-key new-local-map (kbd "M-h") ngoc-prefix-map)
      (define-key new-local-map (kbd "C-<f9>") ngoc-prefix-map)

      (set-keymap-parent new-god-map god-local-mode-map)
      (define-key new-god-map (kbd "h") ngoc-prefix-map)
      (add-to-list 'minor-mode-overriding-map-alist
                   `(god-local-mode . ,new-god-map)))))

(add-hook 'after-change-major-mode-hook 'ngoc/ensure-prefix-binding)

(provide 'init-prefix-map)
