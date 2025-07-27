(setq my/font
      (cond
       ((eq system-type 'gnu/linux)
        '("Cascadia Code NF" . 13.0))
       ((eq system-type 'darwin)
        ;; '("Monaco" . 14.0))
        ;; '("SF Mono" . 14.0))
        ;; '("Spleen 6x12" . 15.0))
       ;; '("Spleen 8x16" . 14.0))
       ;; '("Menlo" . 14.0))
       '("JetBrains Mono NL" . 16.0))
       ;; '("Atlassian Mono" . 16.0))
       ;; '("Comic Code" . 15.0))
       (t
        '("Monospace" . 14.0))))


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
;; (remove-hook 'after-make-frame-functions #'ngoc/setup-font)

(setq text-scale-mode-step 1.1)
(global-set-key (kbd "C-x C-=") #'global-text-scale-adjust)
(global-set-key (kbd "C-x C--") #'global-text-scale-adjust)
(global-set-key (kbd "C-x C-0") #'global-text-scale-adjust)

;; unbind Control - wheel up and down shortcuts
;; disable text-scale-adjust using mouse
(global-unset-key (kbd "<C-wheel-up>"))
(global-unset-key (kbd "<C-wheel-down>"))


(defvar my/font-weight-cycle
  '(thin extra-light light semi-light normal medium semi-bold bold)
  "List of font weights to cycle through.")

(defun my/get-current-weight-index ()
  "Return index of current font weight in `my/font-weight-cycle`."
  (let ((current (face-attribute 'default :weight)))
    (or (cl-position current my/font-weight-cycle) 1))) ; default to 'normal

(defun my/get-current-weight-index ()
  "Return index of current font weight in `my/font-weight-cycle`."
  (let ((current (face-attribute 'default :weight)))
    (or (cl-position current my/font-weight-cycle) 1))) ; default to normal

(defun my/set-font-weight-by-index (index)
  "Set font weight to INDEX from `my/font-weight-cycle`."
  (let* ((len (length my/font-weight-cycle))
         (i (max 0 (min index (1- len))))
         (weight (nth i my/font-weight-cycle)))
    (set-face-attribute 'default nil :weight weight)
    (message "Font weight: %s" weight)
    i))

(defvar my/font-weight-index (my/get-current-weight-index))

(defun my/font-weight-increase ()
  (interactive)
  (setq my/font-weight-index (my/set-font-weight-by-index (1+ my/font-weight-index))))

(defun my/font-weight-decrease ()
  (interactive)
  (setq my/font-weight-index (my/set-font-weight-by-index (1- my/font-weight-index))))

(defun my/font-weight-reset ()
  (interactive)
  (setq my/font-weight-index (my/set-font-weight-by-index 1))) ; index of 'normal

(use-package hydra)

;; (defhydra hydra-font-weight (:hint nil)
;;   " Font Weight: _+_: heavier  _-_: lighter  _0_: reset  _q_: quit "
;;   ("+" my/font-weight-increase)
;;   ("-" my/font-weight-decrease)
;;   ("0" my/font-weight-reset)
;;   ("q" nil "quit"))

(defhydra hydra-font-weight (:hint nil)
  "
Font Weight: _=_: heavier  _-_: lighter  _0_: reset  _q_: quit
"
  ("=" my/font-weight-increase)
  ("-" my/font-weight-decrease)
  ("0" my/font-weight-reset)
  ("q" nil "quit"))



(global-set-key (kbd "C-M-0") 'hydra-font-weight/body)


(provide 'init-font)
