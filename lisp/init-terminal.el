;; (use-package eat
;;   :vc (:fetcher codeberg :repo "akib/emacs-eat")
;;   :demand t
;;   :after ivy
;;   :hook ((eat-mode . compilation-shell-minor-mode)
;;          (eat-mode . goto-address-mode))
;;   :config
;;   (keymap-unset eat-semi-char-mode-map "M-t") ;; prevent shadowing my switch window shortcut

;;   (defun my-set-eat-buffer-name ()
;;     "Set eat-buffer-name to project name for current buffer."
;;     (when-let ((proj (project-current)))
;;       (setq-local eat-buffer-name
;;                   (format "*eat-%s*" (project-name proj)))))

;;   (add-hook 'after-change-major-mode-hook #'my-set-eat-buffer-name)

;;   (defun my-filter-buffers-by-prefix (buffers prefix)
;;     "Filter buffer list by PREFIX string. Also, remove current buffer from the list"
;;     (--filter (string-prefix-p prefix (buffer-name it))
;;               buffers))

;;   (defun my-counsel-switch-to-project-eat ()
;;     "Switch to an eat buffer of current project."
;;     (interactive)
;;     (let* ((proj (project-current))
;;            (eat-prefix "*eat")
;;            (eat-project-prefix (format "*eat-%s" (if proj
;;                                                      (project-name proj)
;;                                                    "")))
;;            (buffers-except-current (delete (current-buffer) (buffer-list)))
;;            (eat-buffers (my-filter-buffers-by-prefix buffers-except-current eat-prefix))
;;            (eat-project-buffers (my-filter-buffers-by-prefix eat-buffers eat-project-prefix))
;;            (final-buffers (or eat-project-buffers
;;                               eat-buffers))
;;            )
;;       (if final-buffers
;;           (ivy-read "Eat buffers: "
;;                     (mapcar #'buffer-name final-buffers)
;;                     ;; reference: counsel-switch-buffer
;;                     :update-fn #'counsel--switch-buffer-update-fn
;;                     :unwind #'counsel--switch-buffer-unwind
;;                     :action #'switch-to-buffer
;;                     )
;;         (message "No eat buffer found.")
;;         )
;;       )
;;     )

;;   (defun my-eat-shell ()
;;     (interactive)
;;       (eat "/Users/ngoc/.nix-profile/bin/bash -i" "new-session")
;;     )

;;   ;; (global-set-key (kbd "<leader>t c") #'my-eat-shell)
;;   ;; (global-set-key (kbd "<leader>t t") #'my-counsel-switch-to-project-eat)


;;   (defun my-eat-find-directory (directory)
;;     "Open DIRECTORY in EAT terminal, with completion similar to `find-file'."
;;     (interactive
;;      (list (read-directory-name "Open directory in terminal: " default-directory)))
;;     (let ((default-directory (expand-file-name directory)))
;;       (message "%s" default-directory)
;;       (eat-term-send-string
;;        eat-terminal
;;        (format "cd %s\n" (shell-quote-argument default-directory)))))

;;   (define-key eat-mode-map (kbd "C-c f") #'my-eat-find-directory)
;;   )

(use-package vterm
  :demand t
  :init
  (setq vterm-module-cmake-args "-DCMAKE_C_COMPILER=clang")
  :hook ((vterm-mode . compilation-shell-minor-mode)
         (vterm-mode . goto-address-mode))
  :config
  (setopt vterm-max-scrollback 4096)
  (keymap-unset vterm-mode-map "M-t") ;; prevent shadowing my switch window shortcut
  (keymap-unset vterm-mode-map "<f8>") ;; prevent shadowing my tab switch shortcut
  (keymap-unset vterm-mode-map "M-B") ;; prevent shadowing my winner shortcut
  (keymap-unset vterm-mode-map "M-F") ;; prevent shadowing my winner shortcut
  (keymap-unset vterm-mode-map "M-j") ;; prevent shadowing my term switch
  (keymap-unset vterm-mode-map "M-u") ;; prevent shadowing my alternate C-u

  ;; prevent shadowing my window shortcuts
  (keymap-unset vterm-mode-map "M-<left>")
  (keymap-unset vterm-mode-map "M-<right>")
  (keymap-unset vterm-mode-map "M-<up>")
  (keymap-unset vterm-mode-map "M-<down>")

  (setq vterm-shell "/Users/ngoc/.nix-profile/bin/bash")

  ;; allow killing vterm buffers without confirmation
  (defun my/disable-confirm-kill-process-in-vterm ()
            (when-let (proc (get-buffer-process (current-buffer)))
                (set-process-query-on-exit-flag proc nil)))

  (add-hook 'vterm-mode-hook #'my/disable-confirm-kill-process-in-vterm)

  (defvar my/project-vterm-buffers (make-hash-table :test #'equal)
    "Hash table mapping project root to list of vterm buffers.")

  (defun my/project-root ()
    "Get current project root using project.el."
    (when-let ((proj (project-current)))
      (expand-file-name (project-root proj))))

  (defun my/project-vterm-buffer-names (project-root)
    "Get vterm buffer names for a given PROJECT-ROOT."
    (gethash project-root my/project-vterm-buffers))

  (defun my/add-project-vterm-buffer (project-root buffer-name)
    "Add BUFFER-NAME to project PROJECT-ROOT's vterm buffer list."
    (puthash project-root
             (cons buffer-name
                   (remove buffer-name (gethash project-root my/project-vterm-buffers)))
             my/project-vterm-buffers))

  (defun my/vterm-new ()
    "Open a new vterm buffer tied to the current project."
    (interactive)
    (let* ((project-root (my/project-root))
           (buffer-name (generate-new-buffer-name
                         (format "*vterm:%s*" (file-name-nondirectory (directory-file-name project-root))))))
      (with-current-buffer (vterm buffer-name)
        (my/add-project-vterm-buffer project-root buffer-name))
      (switch-to-buffer buffer-name)))

  (defun my/vterm-switch ()
    "Switch to a vterm buffer from the current project with live preview in current window.
Buffers ordered by recency, auto-select if only one is available."
    (interactive)
    (let* ((project-root (my/project-root))
           (buffers (my/project-vterm-buffer-names project-root))
           ;; Sort buffers by recency based on current buffer-list order
           (sorted-buffers
            (seq-filter
             (lambda (b) (member b buffers))
             (mapcar #'buffer-name (buffer-list))))
           (initial-buf (cl-find-if (lambda (b) (not (equal b (buffer-name))))
                                    sorted-buffers)))
      (cond
       ((null sorted-buffers)
        (message "No vterm buffers for this project."))
       ((= (length sorted-buffers) 1)
        (switch-to-buffer (car sorted-buffers)))
       (t
        (ivy-read "Project vterm: "
                  sorted-buffers
                  :action #'switch-to-buffer
                  :preselect initial-buf
                  :update-fn (lambda ()
                               (let ((buf (ivy-state-current ivy-last)))
                                 (when (get-buffer buf)
                                   (with-ivy-window
                                     (switch-to-buffer buf)))))
                  :caller 'my/vterm-switch)))))

  ;; (global-set-key (kbd "M-j c") #'my/vterm-new)
  ;; (global-set-key (kbd "<leader>t t") #'my/vterm-switch)

  ;; lambda to launch my/vterm-switch when call without prefix C-u and my/vterm-new with C-u
  (global-set-key (kbd "M-j")
                  (lambda (orig-fun &rest args)
                    (interactive "P")
                    (if current-prefix-arg
                        (apply #'my/vterm-new args)
                      (apply #'my/vterm-switch args))))

  (defun my/vterm-copy-mode-evil-setup ()
    "Enter Evil normal state when entering `vterm-copy-mode',
and go back to emacs state when leaving."
    (if vterm-copy-mode
        (evil-normal-state)
      (evil-emacs-state)))

  (add-hook 'vterm-copy-mode-hook #'my/vterm-copy-mode-evil-setup)
  )

;; (use-package term-keys
;;   :vc (:fetcher github :repo "CyberShadow/term-keys")
;;   :config
;;   (term-keys-mode 1)

;; ;; this is to write config for kitty
;; (require 'term-keys-kitty)
;; (with-temp-buffer
;;   (insert (term-keys/kitty-conf))
;;   (write-region (point-min) (point-max) "~/kitty-for-term-keys.conf"))
;; )

;; to use system clipboard when in terminal
(use-package xclip
  :if (not (display-graphic-p))
  :config
  (xclip-mode 1))

;; kitty keyboard protocol
;; https://sw.kovidgoyal.net/kitty/keyboard-protocol/
(use-package kkp
  :if (not (display-graphic-p))
  :config
  ;; (setq kkp-alt-modifier 'alt) ;; use this if you want to map the Alt keyboard modifier to Alt in Emacs (and not to Meta)
  (global-kkp-mode +1))

(when (not (display-graphic-p))
  (xterm-mouse-mode 1)
  )

(provide 'init-terminal)
