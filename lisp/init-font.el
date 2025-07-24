(setq my/font
      (cond
       ((eq system-type 'gnu/linux)
        '("Cascadia Code NF" . 13.0))
       ((eq system-type 'darwin)
        '("Monaco" . 14.0))
       (t
        '("Monospace" . 13.0))))

(defun ngoc/setup-font (&optional frame)
  (interactive)
  ;; set fallback font for Japanese
  (let ((jp-font "Droid Sans Fallback"))
    (set-fontset-font t 'han jp-font)
    (set-fontset-font t 'kana jp-font)
    (set-fontset-font t 'cjk-misc jp-font))

  (set-frame-font (font-spec :family (car my/font)
                             :weight 'regular
                             :size (cdr my/font))
                  t t t)

  ;; fixed issue with unreadable char in Info docs
  (set-face-attribute 'fixed-pitch-serif nil :family my/font :inherit 'default))

(ngoc/setup-font)
(add-hook 'after-make-frame-functions #'ngoc/setup-font)

(setq text-scale-mode-step 1.1)
(global-set-key (kbd "C-x C-=") #'global-text-scale-adjust)
(global-set-key (kbd "C-x C--") #'global-text-scale-adjust)
(global-set-key (kbd "C-x C-0") #'global-text-scale-adjust)

;; unbind Control - wheel up and down shortcuts
;; disable text-scale-adjust using mouse
(global-unset-key (kbd "<C-wheel-up>"))
(global-unset-key (kbd "<C-wheel-down>"))


(provide 'init-font)
