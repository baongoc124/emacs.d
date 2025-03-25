;; common settings for normal instance and exwm instance

(defun display-startup-echo-area-message ()
  (message "Knowledge is power!"))

;; text-mode as default mode for new buffer
(setq-default major-mode 'text-mode)

;; weird naming of horizontal & vertical split -> I swapped them
(defun hsplit-last-buffer ()
  (interactive)
  (split-window-vertically)
  (other-window 1 nil)
  (switch-to-next-buffer)
  (other-window -1 nil))

(defun vsplit-last-buffer ()
  (interactive)
  (split-window-horizontally)
  (other-window 1 nil)
  (switch-to-next-buffer)
  (other-window -1 nil))

(defun switch-to-last-buffer ()
  (interactive)
  (switch-to-buffer nil))



(global-set-key (kbd "C-/")    nil)      ; don't use and easily mispress when in god-mode
(global-set-key "\C-x\C-z" nil)          ; suspend emacs accidentally no more
(global-set-key "\C-x\C-c" nil)          ; exit emacs accidentally no more
(global-set-key (kbd "M-z") nil)

;; unset shortcut for transpose functions
;; because I never use them and sometimes press them accidentally
(global-unset-key (kbd "C-x C-t"))
(global-unset-key (kbd "M-t"))
(global-unset-key (kbd "C-M-t"))


;;   ____ _   _ ___   _                      _
;;  / ___| | | |_ _| | |___      _____  __ _| | _____
;; | |  _| | | || |  | __\ \ /\ / / _ \/ _` | |/ / __|
;; | |_| | |_| || |  | |_ \ V  V /  __/ (_| |   <\__ \
;;  \____|\___/|___|  \__| \_/\_/ \___|\__,_|_|\_\___/

(defface ngoc/invisible-face
  '((t :foreground "white" :background "white"))
  "Face for invisible text.")

(setq echo-keystrokes 0.0001)
(setq minibuffer-prompt-properties '(readonly t point-entered minibuffer-avoid-prompt face minibuffer-prompt))

(blink-cursor-mode 0)


(add-hook 'window-setup-hook
          #'(lambda ()
              (set-cursor-color "#69E300")))

(setq text-scale-mode-step 1.1)
(global-set-key (kbd "C-x C-=") #'global-text-scale-adjust)
(global-set-key (kbd "C-x C--") #'global-text-scale-adjust)
(global-set-key (kbd "C-x C-0") #'global-text-scale-adjust)

;;                      _        _ _
;;  _ __ ___   ___   __| | ___  | (_)_ __   ___
;; | '_ ` _ \ / _ \ / _` |/ _ \ | | | '_ \ / _ \
;; | | | | | | (_) | (_| |  __/ | | | | | |  __/
;; |_| |_| |_|\___/ \__,_|\___| |_|_|_| |_|\___|
(column-number-mode 1)
(setq mode-line-position-column-line-format '(" %l,%c"))

;; remove parentheses wrapping around major, minor modes
;; copied from original source code with minor edit
(let ((recursive-edit-help-echo
       "Recursive edit, type M-C-c to get out"))
  (setq mode-line-modes
        (list (propertize "%[" 'help-echo recursive-edit-help-echo)
	          `(:propertize ("" mode-name)
			                help-echo "Major mode\n\
mouse-1: Display major mode menu\n\
mouse-2: Show help for major mode\n\
mouse-3: Toggle minor modes"
			                mouse-face mode-line-highlight
			                local-map ,mode-line-major-mode-keymap)
	          '("" mode-line-process)
	          `(:propertize ("" minor-mode-alist)
			                mouse-face mode-line-highlight
			                help-echo "Minor mode\n\
mouse-1: Display minor mode menu\n\
mouse-2: Show help for minor mode\n\
mouse-3: Toggle minor modes"
			                local-map ,mode-line-minor-mode-keymap)
	          (propertize "%n" 'help-echo "mouse-2: Remove narrowing from buffer"
		                  'mouse-face 'mode-line-highlight
		                  'local-map (make-mode-line-mouse-map
				                      'mouse-2 #'mode-line-widen))
	          (propertize "%]" 'help-echo recursive-edit-help-echo)
	          " ")))


(setq ngoc/minor-mode-order-alist '(projectile-mode flymake-mode))
(defun ngoc/sort-minor-modes (&rest args)
  "Sort minor modes by order in `ngoc/minor-mode-order-alist'."
  (let* ((table-size (length ngoc/minor-mode-order-alist))
         (order-table (make-hash-table :test 'equal
                                       :size table-size)))
    (cl-loop for index from 0
             for item in ngoc/minor-mode-order-alist
             do (puthash item index order-table))
    (setq minor-mode-alist (sort minor-mode-alist
                                 (lambda (a b)
                                   (let ((a-index (gethash (car a) order-table table-size))
                                         (b-index (gethash (car b) order-table table-size)))
                                     (< a-index b-index)))))))

;; DONE order all minor modes in modeline
;; FIXME find a better way to reorder minor mode name on modeline
(add-hook 'after-load-functions 'ngoc/sort-minor-modes)


(defun rmrf/setup-frame (&optional frame)
  (tool-bar-mode -1)
  (toggle-scroll-bar -1)
  (menu-bar-mode -1)
  (modify-frame-parameters frame
                           '((vertical-scroll-bars . nil)
                             (horizontal-scroll-bars . nil)))
  ;; (set-frame-font (font-spec :family "DejaVu Sans Mono" :size 10.0) 1 frame t)
  (setq frame-resize-pixelwise t)
  (setq frame-inhibit-implied-resize t))

(rmrf/setup-frame)
(add-hook 'after-make-frame-functions 'rmrf/setup-frame)


(use-package diminish)


(provide 'init-core)
