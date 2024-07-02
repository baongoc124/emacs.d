;; TODO checkout org-roam
;; TODO checkout gumshoe
;; FIXME magit not always ask password using same interface
;; TODO visual-mark
;; TODO check out real-save-mode
;; TODO check out git-timemachine
;; TODO add hideshow mode
;; TODO how to use occur to edit in multiple files
;; TODO find good functions for M-n and M-p keys
;; TODO put custom settings in a separated file
;; TODO checkout project instead of projectile
;; TODO checkout lsp-booster
;; TODO learn how to use straight.el
;; TODO checkout jinx.el spell checker
;; TODO change copilot cache directory to cache instead of .cache
;; FIXME faces of history mode
;; TODO make undo-tree history (and other) save to a unique directory for each machine
;; DONE use symbol overlay
;; DONE persistent undo-tree


(setq garbage-collection-messages t)
(setq max-lisp-eval-depth 6400)

(add-to-list 'load-path "~/.emacs.d/lisp/")

(require 'init-package-management)
(require 'init-font)
(require 'init-prefix-map)
(require 'init-god)
(require 'init-core)

(require 'init-auto-completion)
(require 'init-buffer)
(require 'init-calendar)
(require 'init-cheatsheet)
(require 'init-copilot)
(require 'init-dired)
(require 'init-eglot)
(require 'init-fcitx)
(require 'init-ivy)
(require 'init-misc)
(require 'init-navigation)
(require 'init-org-mode)
(require 'init-plantuml)
(require 'init-project)
(require 'init-programming)
(require 'init-python)
(require 'init-search)
(require 'init-shortcuts)
(require 'init-ssh)
(require 'init-treesit)
(require 'init-undo-tree)
(require 'init-version-control)
(require 'init-web)
(require 'init-window-management)
(require 'init-writing)

(require 'init-daemon)
;; (require 'init-exwm)

;; (require 'ironoko)



;; FIXME sometimes it's weird
(defun open-line-keep-indent ()
  (interactive)
    (let* ((col (current-column))
           (cur-indent (current-indentation))
           (should-indent (eq col cur-indent)))
      (save-excursion
        (newline 1 t))
      (message "col: %d, cur-indent: %d" col cur-indent)
      (when should-indent
        (indent-to-column cur-indent))))

(global-set-key (kbd "C-o") #'open-line-keep-indent)

(add-hook 'before-save-hook 'delete-trailing-whitespace)


;; hl-line overrides face of match -> don't use it globally
(use-package hl-line
  :hook
  (prog-mode . hl-line-mode)
  (text-mode . hl-line-mode)
  (conf-mode . hl-line-mode))


(setq frame-title-format
      '("" invocation-name ": " (:eval (replace-regexp-in-string
                                        "^ +" "" (buffer-name)))))
(setq mouse-yank-at-point t)

(setq browse-url-generic-program "microsoft-edge-stable")
(setq browse-url-browser-function 'browse-url-generic)
;; prevent async output buffer to show up
(add-to-list 'display-buffer-alist (cons "\\*Async Shell Command\\*.*" (cons #'display-buffer-no-window nil)))


;; https://emacs.stackexchange.com/a/52576
;; modify default isearch behavior
;; 1. always exit at start of the match
;; 2. C-RET to have default exit behavior
(defvar ngoc/isearch-default-exit-behavior nil
  "If non-nil, isearch will exit with default behavior.")

(defun ngoc/default-isearch-exit ()
  (interactive)
  (let ((ngoc/isearch-default-exit-behavior t))
    (isearch-exit)))

(defun ngoc/isearch-always-exits-at-start ()
  (when (and isearch-forward
             (number-or-marker-p isearch-other-end)
             (not mark-active)
             (not isearch-mode-end-hook-quit)
             (not ngoc/isearch-default-exit-behavior))
    (goto-char isearch-other-end)))

(with-eval-after-load "isearch"
  (add-hook 'isearch-mode-end-hook #'ngoc/isearch-always-exits-at-start)
  (define-key isearch-mode-map (kbd "C-<return>") #'ngoc/default-isearch-exit))

(setq truncate-lines nil)

(global-set-key (kbd "C-x k")     'kill-this-buffer)
(global-set-key (kbd "C-1")       'delete-other-windows)
(global-set-key (kbd "C-0")       'delete-window)
(global-set-key (kbd "C-<left>")  'shrink-window-horizontally)
(global-set-key (kbd "C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-<down>")  'shrink-window)
(global-set-key (kbd "C-<up>")    'enlarge-window)


;; (setq grep-highlight-matches 'auto-detect)



;;  _____ ____      _    __  __ ____
;; |_   _|  _ \    / \  |  \/  |  _ \
;;   | | | |_) |  / _ \ | |\/| | |_) |
;;   | | |  _ <  / ___ \| |  | |  __/
;;   |_| |_| \_\/_/   \_\_|  |_|_|

(require 'tramp)
(setq tramp-default-method "scp")

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


;; used to detect Dropbox conflict files when open in emacs
;; (require 'dropbox-conflicts)
;; (dropbox-conflicts-mode t)

(setq TeX-PDF-mode t)
(latex-preview-pane-enable)


(use-package keyfreq
  :init
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))


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

;; editing
;; don't need zap-to-char because zap-up-to-char + d = zap-to-char (in god-mode)
(global-set-key (kbd "C-z")     'zap-up-to-char)
(global-set-key (kbd "M-2")     'mark-word)


(defvar xah-brackets '("“”" "()" "[]" "{}" "<>" "＜＞" "（）" "［］" "｛｝" "⦅⦆" "〚〛" "⦃⦄" "‹›" "«»" "「」" "〈〉" "《》" "【】" "〔〕" "⦗⦘" "『』" "〖〗" "〘〙" "｢｣" "⟦⟧" "⟨⟩" "⟪⟫" "⟮⟯" "⟬⟭" "⌈⌉" "⌊⌋" "⦇⦈" "⦉⦊" "❛❜" "❝❞" "❨❩" "❪❫" "❴❵" "❬❭" "❮❯" "❰❱" "❲❳" "〈〉" "⦑⦒" "⧼⧽" "﹙﹚" "﹛﹜" "﹝﹞" "⁽⁾" "₍₎" "⦋⦌" "⦍⦎" "⦏⦐" "⁅⁆" "⸢⸣" "⸤⸥" "⟅⟆" "⦓⦔" "⦕⦖" "⸦⸧" "⸨⸩" "｟｠")
 "A list of strings, each element is a string of 2 chars, the left bracket and a matching right bracket.
Used by `xah-select-text-in-quote' and others.")

(defconst xah-left-brackets
  (mapcar (lambda (x) (substring x 0 1)) xah-brackets)
  "List of left bracket chars. Each element is a string.")

(defconst xah-right-brackets
  (mapcar (lambda (x) (substring x 1 2)) xah-brackets)
  "List of right bracket chars. Each element is a string.")

(defun xah-backward-left-bracket ()
  "Move cursor to the previous occurrence of left bracket.
The list of brackets to jump to is defined by `xah-left-brackets'.

URL `http://xahlee.info/emacs/emacs/emacs_navigating_keys_for_brackets.html'
Version: 2015-10-01"
  (interactive)
  (re-search-backward (regexp-opt xah-left-brackets) nil t))

(defun xah-forward-right-bracket ()
  "Move cursor to the next occurrence of right bracket.
The list of brackets to jump to is defined by `xah-right-brackets'.

URL `http://xahlee.info/emacs/emacs/emacs_navigating_keys_for_brackets.html'
Version: 2015-10-01"
  (interactive)
  (re-search-forward (regexp-opt xah-right-brackets) nil t))

;; use middle fingers for paragraph movements
;; use ring fingers for bracket movements
;; to alternating hands like Dvorak's idea
(global-set-key (kbd "C-2") 'xah-backward-left-bracket)
(global-set-key (kbd "C-9") 'xah-forward-right-bracket)
(global-set-key (kbd "C-3") 'backward-paragraph)
(global-set-key (kbd "C-8") 'forward-paragraph)


(require 'recentf)
(setq recentf-auto-cleanup 'never) ;; disable before we start recentf!
(setq recentf-max-menu-items 200)
(setq recentf-max-saved-items 200)
(setq recentf-save-file (concat recentf-save-file "-" (system-name)))
(recentf-mode 1)
(global-set-key "\C-x\ \C-r" #'counsel-recentf)


(use-package autorevert
  :diminish auto-revert-mode)


(require 'display-line-numbers)
(setq display-line-numbers-current-absolute nil)
(setq display-line-numbers-type 'relative)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'text-mode-hook 'display-line-numbers-mode)
(add-hook 'conf-mode-hook 'display-line-numbers-mode)


(use-package elec-pair
  :config
  (electric-pair-mode 1)
  (show-paren-mode 1)
  (setq show-paren-delay 0.025))

(use-package hl-todo
  :hook
  (prog-mode . hl-todo-mode)
  (text-mode . hl-todo-mode)
  :config
  (add-hook 'org-mode-hook #'(lambda ()
                               (hl-todo-mode -1))))

(use-package display-fill-column-indicator
  :hook
  (prog-mode . display-fill-column-indicator-mode)
  (text-mode . display-fill-column-indicator-mode)
  :config
  (setq-default fill-column 80)
  (setq-default display-fill-column-indicator-character ?\u2591))

;; for auto detecting indentation
(use-package dtrt-indent
  :diminish dtrt-indent-mode
  :hook
  (prog-mode . dtrt-indent-mode))

(setq-default indent-tabs-mode nil)
;; set default tab char's display width to 4 spaces
(setq-default tab-width 4) ; emacs 23.1, 24.2, default to 8
(setq backward-delete-char-untabify-method 'hungry)
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)

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

;; COMMENT
(use-package comment-dwim-2
  :bind ("C-;" . comment-dwim-2))

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




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              PRETTIFY SYMBOL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://gist.github.com/kwf/dea7bc51101083acb95c875140e2a96d
(setq-default prettify-symbols-alist
              '(("lambda"   . ?λ)
                ("function" . ?ƒ)
                ;; ("function"    . ?𝐅)
                ;; ("return"   . ?⏎)
                ;; ("return"   . ?)
                ;; ("return"   . ?↰)
                ("return"   . ?↩)
                ;; ("class"       . ?𝐂)
                ("!="       . ?≠)
                ("<-"       . ?←)
                ("->"       . ?➜)
                ("=>"       . ?⤇)
                ("<="       . ?≤)
                (">="       . ?≥)))

;; (setq prettify-symbols-unprettify-at-point t)
;; (global-prettify-symbols-mode +1)

;; (require 'emojify)
;; (setq emojify-emoji-styles '(unicode))
;; (global-emojify-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              FLYCHECK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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


;;   ___  __  __ _   _     _____
;;  / _ \|  \/  | \ | | __|_   _|_     _
;; | | | | |\/| |  \| |/ _ \| |_| |_ _| |_
;; | |_| | |  | | |\  |  __/| |_   _|_   _|
;;  \___/|_|  |_|_| \_|\___||_| |_|   |_|
(setenv "PATH" (concat (expand-file-name "~/apps/omnetpp-5.7/bin:") (getenv "PATH")))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("5e1777af6af8bdf6376b4d7b17a91a8057063ca60f4d8ed7b547d06fd02277d1" "81179c35607016fd92ed2751068a14438ef847c4db82dabcf74b2ff94be4eabc" "be51dc2e9c578c884b442d3bf46555454e095785b8223e8530bd1e949d7ebcb7" "c3b359e781e5418343a9a0e4f9a6d8f1b57ea4be4c020fe329b60bad12c6fb6e" "6c685287c7234f04cc1beedf2920dbfff8193dec34266ac22fa55dbaa047b39e" "733a3296be05c918fb3ee4f83dd8d7725014c0dff1b22265b44391e247f7d8c7" "dda894ceb5ad3685778c84596014de3bf60bd31107f16b2a38d71e3707a4793a" "571661a9d205cb32dfed5566019ad54f5bb3415d2d88f7ea1d00c7c794e70a36" default))
 '(package-vc-selected-packages
   '((transient-showcase :url "https://github.com/positron-solutions/transient-showcase.git")))
 '(safe-local-variable-values
   '((eval setq cc-search-directories
           (list "."
                 (expand-file-name "~/builds/comm/include")
                 (expand-file-name "~/builds/comm/src")
                 (expand-file-name "~/builds/comm/src/**")
                 "/usr/include" "/usr/local/include/*"))
     (eval setq flycheck-clang-include-path
           (list
            (expand-file-name "~/apps/omnetpp-5.7/include")
            (expand-file-name "~/builds/comm/include")
            (expand-file-name "~/builds/comm/src/message"))))))

