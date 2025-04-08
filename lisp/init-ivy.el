(use-package ivy
  :diminish ivy-mode
  :config
  (ivy-mode 1)
  (setq ivy-sort-max-size 1000000)
  )

(use-package counsel
  :bind (("C-x b" . 'counsel-switch-buffer)))

(use-package ivy-rich
  :after (ivy)
  :config
  (setq ivy-rich-parse-remote-buffer nil)
  (ivy-rich-mode 1)
  (ivy-rich-project-root-cache-mode 1)
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))

;; on emacs 25 maybe should change this to string-collate-lessp
;; (add-to-list 'ivy-sort-functions-alist '(read-file-name-internal . string-lessp))

(use-package swiper
  :config
  (global-set-key (kbd "M-S") 'swiper-from-isearch))


(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-S-Y") 'counsel-yank-pop)


(use-package ivy-prescient
  :after ivy
  :config
  (ivy-prescient-mode 1)
  )


(provide 'init-ivy)
