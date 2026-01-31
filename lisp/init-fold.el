(use-package ts-fold
  :vc (:url "https://github.com/emacs-tree-sitter/ts-fold")
  )

(with-eval-after-load 'evil
  ;; use zf family for ts-fold
  (define-key evil-normal-state-map (kbd "ZZ") #'ts-fold-toggle)
  ;; (define-key evil-normal-state-map (kbd "Zo") #'ts-fold-open)
  ;; (define-key evil-normal-state-map (kbd "Zc") #'ts-fold-close)
  (define-key evil-normal-state-map (kbd "ZO") #'ts-fold-open-all)
  (define-key evil-normal-state-map (kbd "ZR") #'ts-fold-open-recursively)
  (define-key evil-normal-state-map (kbd "ZC") #'ts-fold-close-all))

(provide 'init-fold)
