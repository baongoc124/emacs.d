;; TODO checkout gumshoe
;; FIXME magit not always ask password using same interface
;; TODO check out real-save-mode
;; TODO check out git-timemachine
;; TODO put custom settings in a separated file
;; TODO checkout lsp-booster
;; TODO checkout jinx.el spell checker
;; FIXME faces of history mode
;; TODO make undo-tree history (and other) save to a unique directory for each machine


(setq garbage-collection-messages t)
(setq max-lisp-eval-depth 6400)

;============================== mac compatibility ==============================
(setq mac-option-modifier 'meta)      ;; Use Option as Meta
(setq mac-command-modifier 'super)    ;; Optional: use Command as Super

(global-set-key (kbd "M-u") #'universal-argument)

(add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))

(require 'init-package-management)


;=========== used to detect Dropbox conflict files when open in emacs ==========
(add-to-list 'load-path (expand-file-name "lisp/dropbox-conflicts-el/" user-emacs-directory))
(require 'dropbox-conflicts)
(dropbox-conflicts-mode t)

;===================== must be loaded, otherwise suffering =====================
(require 'init-lib)
(require 'init-exec-path)
(require 'init-evil)
(require 'init-shortcuts)
(require 'init-core)
(require 'init-auth)

(require 'init-auto-completion)
(require 'init-buffer)
(require 'init-dired)
;; (require 'init-eglot)
(require 'init-fcitx)
(require 'init-ivy)
(require 'init-misc)
(require 'init-navigation)
(require 'init-org-mode)
(require 'init-plantuml)
(require 'init-project)
(require 'init-programming)
(require 'init-programming-lsp-bridge)
(require 'init-programming-swift)
(require 'init-programming-flymake)
(require 'init-programming-vue)
(require 'init-python)
(require 'init-search)
(require 'init-ssh)
(require 'init-terminal)
(require 'init-treesit)
(require 'init-undo-tree)
(require 'init-version-control)
(require 'init-web)
(require 'init-window-management)
(require 'init-writing)

;================================= nice to have ================================
(require 'init-eshell)
(require 'init-neotree)
(require 'init-embark)
(require 'init-ai)

(require 'init-evil-paste)
(require 'init-fold)

(require 'init-programming-docker)
(require 'init-programming-docs)
(require 'init-programming-golang)
(require 'init-programming-serverless)
(require 'init-programming-toggle-debug-comment)

(require 'init-font)
(require 'init-calendar)

(require 'init-programming-wakatime)

;============================= less important stuff ============================

(require 'init-cheatsheet)
(require 'init-theme)
;; (require 'init-visual-holo-layer)
(require 'init-daemon)



;; (setq frame-title-format
;;       '("" invocation-name ": " (:eval (replace-regexp-in-string
;;                                         "^ +" "" (buffer-name)))))
(setq frame-title-format
      '(:eval (if buffer-file-name
                  (abbreviate-file-name buffer-file-name)
                "%b")))
(setq mouse-yank-at-point t)

;; set default browser based on OS
(if (string-equal system-type "darwin")
    (setq browse-url-generic-program "/usr/bin/open")
  (setq browse-url-generic-program "google-chrome-stable"))
(setq browse-url-browser-function 'browse-url-generic)

(setq truncate-lines nil)


;; (setq grep-highlight-matches 'auto-detect)



;;  _____ ____      _    __  __ ____
;; |_   _|  _ \    / \  |  \/  |  _ \
;;   | | | |_) |  / _ \ | |\/| | |_) |
;;   | | |  _ <  / ___ \| |  | |  __/
;;   |_| |_| \_\/_/   \_\_|  |_|_|

(require 'tramp)
(setq tramp-default-method "scp")
(setq tramp-connection-timeout 10)

(defun sudo-edit-current-file ()
  (interactive)
  (let ((my-file-name) ; fill this with the file to open
        (position))    ; if the file is already open save position
    (if (equal major-mode 'dired-mode) ; test if we are in dired-mode
        (progn
          (setq my-file-name (dired-get-file-for-visit))
          (find-alternate-file (prepare-tramp-sudo-string my-file-name)))
      (setq my-file-name (buffer-file-name); hopefully anything else is an already opened file
            position (point))
      (find-alternate-file (prepare-tramp-sudo-string my-file-name))
      (goto-char position))))

(defun prepare-tramp-sudo-string (tempfile)
  (if (file-remote-p tempfile)
      (let ((vec (tramp-dissect-file-name tempfile)))

        (tramp-make-tramp-file-name
         "sudo"
         (tramp-file-name-user nil)
         (tramp-file-name-host vec)
         (tramp-file-name-localname vec)
         (format "ssh:%s@%s|"
                 (tramp-file-name-user vec)
                 (tramp-file-name-host vec))))
    (concat "/sudo:root@localhost:" tempfile)))

(use-package cdlatex)


(setq TeX-PDF-mode t)
(latex-preview-pane-enable)


(use-package pdf-tools
  :config
  (pdf-tools-install)
  (setq pdf-view-midnight-colors '("black" . "wheat"))
  (defun rmrf/pdf-view-setup ()
    (pdf-view-midnight-minor-mode t))
  (add-hook 'pdf-view-mode-hook 'rmrf/pdf-view-setup))

(add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              LATEX AUCTEX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)



;; (require 'netlogo-mode)

;; ; a way to find out what packages are using cl
;; (require 'loadhist)
;; (file-dependents (feature-file 'cl))

(setq sentence-end-double-space nil) ; disable archaic double space after sentence

(require 'recentf)
(setq recentf-auto-cleanup 17) ;; disable before we start recentf!
(setq recentf-max-menu-items 20)
(setq recentf-max-saved-items 2000)
(setq recentf-save-file (concat recentf-save-file "-" (system-name)))
(recentf-mode 1)
(global-set-key "\C-x\ \C-r" #'counsel-recentf)


(use-package autorevert
  :diminish auto-revert-mode
  :config
  (global-auto-revert-mode 1))


(require 'display-line-numbers)
(setq display-line-numbers-current-absolute nil)
(setq display-line-numbers-type 'relative)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'text-mode-hook 'display-line-numbers-mode)
(add-hook 'conf-mode-hook 'display-line-numbers-mode)


(use-package hl-todo
  :hook
  (prog-mode . hl-todo-mode)
  (text-mode . hl-todo-mode)
  :config
  (add-hook 'org-mode-hook #'(lambda ()
                               (hl-todo-mode -1))))


(setq-default fill-column 80)
;; (use-package display-fill-column-indicator
;;   :hook
;;   (prog-mode . display-fill-column-indicator-mode)
;;   (text-mode . display-fill-column-indicator-mode)
;;   :config
;;   ;; (setq-default display-fill-column-indicator-character ?\u2591)
;;   (setq-default display-fill-column-indicator-character ?\u2506)
;;   (set-face-attribute 'fill-column-indicator nil :foreground "grey30")
;;   )


;; for auto detecting indentation
(use-package dtrt-indent
  :diminish dtrt-indent-mode
  :config

  (defun my/enable-dtrt-indent ()
    (unless (derived-mode-p 'python-mode)
      (dtrt-indent-mode 1))
    )

  (add-hook 'prog-mode-hook #'my/enable-dtrt-indent)


  (add-to-list 'dtrt-indent-hook-generic-mapping-list
               '(t tab-width))

  )

(setq-default indent-tabs-mode nil)
;; set default tab char's display width to 4 spaces
(setq-default tab-width 8) ; emacs 23.1, 24.2, default to 8. Org mode export depends on this.
(setq backward-delete-char-untabify-method 'hungry)
(setq-default c-basic-offset 4)
(setq-default cperl-indent-level 4)

;; make tab key call indent command or insert tab character, depending on cursor position
;; default is t and it will always indent
;; (setq-default tab-always-indent nil)


(defun ngoc/align-dwim ()
  (interactive)
  (if current-prefix-arg
      (save-mark-and-excursion
        (if (not (region-active-p))
            (mark-paragraph))
        (call-interactively 'align-regexp))
    (if (region-active-p)
        (call-interactively 'align-entire)
      (align-current))))


(require 'dabbrev)
(setq dabbrev-abbrev-char-regexp "\\sw\\|\\s_\\|\\s.")
(setq dabbrev-case-fold-search nil)

(use-package wgrep
  :config
  (setq wgrep-too-many-file-length 20)) ;; TODO how to use this

(use-package iedit
  :bind (:map iedit-mode-keymap
         ("C-c C-c" . iedit-mode)))


;; (require 'cl)
;; (require 'cl-macs)
;; (require 'dwim-compile)
;; (global-set-key [(control ?c) ?c] 'dwim-c/compile)
;; (setq compilation-finish-functions nil)

;;                                  _                      _
;;   _____  ___ __   __ _ _ __   __| |      _ __ ___  __ _(_) ___  _ __
;;  / _ \ \/ / '_ \ / _` | '_ \ / _` |_____| '__/ _ \/ _` | |/ _ \| '_ \
;; |  __/>  <| |_) | (_| | | | | (_| |_____| | |  __/ (_| | | (_) | | | |
;;  \___/_/\_\ .__/ \__,_|_| |_|\__,_|     |_|  \___|\__, |_|\___/|_| |_|
;;           |_|                                     |___/

;; TODO make inside/outside functions repeatable
(use-package expand-region
  :defer nil
  :bind ("M-3" . er/mark-symbol)

  :config
  (setq expand-region-reset-fast-key "<escape>")
  (require 'er-basic-expansions))


;;     _  _____ ___  __  __ ___ ____    ____ _   _ ____   ___  __  __ _____
;;    / \|_   _/ _ \|  \/  |_ _/ ___|  / ___| | | |  _ \ / _ \|  \/  | ____|
;;   / _ \ | || | | | |\/| || | |     | |   | |_| | |_) | | | | |\/| |  _|
;;  / ___ \| || |_| | |  | || | |___  | |___|  _  |  _ <| |_| | |  | | |___
;; /_/   \_\_| \___/|_|  |_|___\____|  \____|_| |_|_| \_\\___/|_|  |_|_____|
(use-package atomic-chrome
  :bind (:map atomic-chrome-edit-mode-map
              ("C-c C-c" . nil))
  :config
  (setq atomic-chrome-buffer-open-style 'full)
  (setq atomic-chrome-url-major-mode-alist
        '((".*\\.wix\\.com" . js-ts-mode)
          ("leetcode\\.com" . python-ts-mode)))
  (atomic-chrome-start-server))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              IMPATIENT MODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'impatient-mode)

;; ugly with fixed backend
(defun my-imp-filter (buffer)
  (let ((m (with-current-buffer buffer major-mode)))
    (case m
      (org-mode
       (insert-buffer-substring (htmlize-buffer
                                 (with-current-buffer buffer
                                   (org-export-as 'reveal)))))
      (t
       (let ((html-buffer (save-match-data (htmlize-buffer buffer))))
         (insert-buffer-substring html-buffer)
         (kill-buffer html-buffer))))))

(setq-default imp-user-filter 'my-imp-filter)

(defun chrome-reload ()
  (interactive)
  (shell-command "chromereload.py"))

(defun save-and-refresh ()
  (interactive)
  (save-buffer)
  (chrome-reload))


(setq custom-file (expand-file-name "init-custom.el" user-emacs-directory))
(load custom-file)
