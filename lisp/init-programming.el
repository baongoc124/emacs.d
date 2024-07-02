(use-package quickselect
  :ensure nil
  :demand t
  :config
  (quickselect-mode 1)
  (global-set-key (kbd "C-q") #'quickselect-mark)
  (global-set-key (kbd "C-'") #'quoted-insert))

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
