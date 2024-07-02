(use-package quickselect
  :ensure nil
  :demand t
  :config
  (quickselect-mode 1)
  (global-set-key (kbd "C-q") #'quickselect-mark)
  (global-set-key (kbd "C-'") #'quoted-insert))

;;                                             _     _ _     _
;;   __ _ _ __ __ _ _   _ _ __ ___   ___ _ __ | |_  | (_)___| |_
;;  / _` | '__/ _` | | | | '_ ` _ \ / _ \ '_ \| __| | | / __| __|
;; | (_| | | | (_| | |_| | | | | | |  __/ | | | |_  | | \__ \ |_
;;  \__,_|_|  \__, |\__,_|_| |_| |_|\___|_| |_|\__| |_|_|___/\__|
;;            |___/
(use-package fill-function-arguments
  :config
  (setq fill-function-arguments-fall-through-to-fill-paragraph nil)
  (setq fill-function-arguments-indent-after-fill t)
  (setq fill-function-arguments-trailing-separator t)

  ;; without prefix argument, do default behavior
  ;; with prefix argument, do compact version
  ;; and DO NOTHING if not inside a parenthesis
  (defun ngoc/fill-function-arguments-dwim (&optional arg)
    (interactive "P")
    (if (<= (nth 0 (syntax-ppss)) 0)
        (message "Not inside a parenthesis")
      (if (not arg)
          (fill-function-arguments-dwim)
        (let ((fill-function-arguments-trailing-separator nil)
              (fill-function-arguments-first-argument-same-line t)
              (fill-function-arguments-last-argument-same-line t))
          (fill-function-arguments-dwim))))))


;; FIXME yasnippet doesn't work when first open a buffer
(use-package yasnippet
  :demand t
  :diminish yas-minor-mode
  :bind (("M-8" . yas-expand)
         :map yas-minor-mode-map
              ("<tab>" . nil)
              ("TAB" . nil))
  :hook
  (prog-mode . yas-minor-mode)
  (text-mode . yas-minor-mode)
  :config
  ;; https://emacs.stackexchange.com/questions/38242/problem-redoing-with-yasnippet
  (setq yas-snippet-revival nil))

(provide 'init-programming)
