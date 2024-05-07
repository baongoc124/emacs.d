(setq gc-cons-threshold 50000000
      garbage-collection-messages t)

(setq max-lisp-eval-depth 10000)

;; to rebind C-h
(define-prefix-command 'ngoc-prefix)
(global-set-key (kbd "C-<f9>") 'ngoc-prefix)
;; FIXME this makes me cannot access other keybindngs contains C-h
;; but it's a good tradeoff
(define-key key-translation-map [?\C-h] (kbd "C-<f9>"))

(global-set-key (kbd "C-/")    nil)      ; don't use and easily mispress when in god-mode
(global-set-key "\C-x\C-z" nil)          ; suspend emacs accidentally no more


(add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list 'load-path "~/.emacs.d/lisp/evil-plugins/")
(add-to-list 'load-path "~/.emacs.d/lisp/highlight-symbol/")
(add-to-list 'load-path "~/.emacs.d/lisp/history")
;; (add-to-list 'load-path "~/.emacs.d/lisp/ned-mode")
(add-to-list 'load-path "~/.emacs.d/lisp/netlogo-mode/emacs")
;; (add-to-list 'load-path "~/.emacs.d/lisp/dropbox-conflicts-el")
;;(add-to-list 'load-path "~/.emacs.d/lisp/elim/elisp")
(defun display-startup-echo-area-message ()
  (message "Knowledge is power!"))

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

(require 'use-package)
(setq use-package-always-ensure t)

;; Straight package manager
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; only auto update on weekends
(use-package auto-package-update
  :config
  (setq auto-package-update-interval 3
        auto-package-update-prompt-before-update t)

  (defun ngoc/attempt-auto-update-on-weekend ()
    (require 'calendar)
    (let ((current-day (calendar-day-of-week (calendar-current-date))))
      (when (or (= current-day 0)
                (= current-day 6))
        (auto-package-update-maybe))))

  ;; first attempt 1 min after startup. After that, attempt every 3 hours.
  (run-at-time "1 min" (* 3 3600) #'ngoc/attempt-auto-update-on-weekend))


;; (use-package aggressive-indent)
;; (use-package tex-buf)
;; (use-package tex :ensure auctex)
;; (use-package auctex-latexmk)
(use-package diminish)
(use-package better-shell)
(use-package flyspell-lazy)
(use-package flx)
(use-package ivy)
(use-package impatient-mode)
(use-package isend-mode)
(use-package key-chord)
(use-package langtool)
(use-package latex-preview-pane)
(use-package less-css-mode)
;; (load-library "ned-mode")
(use-package shell-switcher)
(use-package xcscope)


(use-package undo-tree
  :demand t
  :bind (:map ngoc-prefix
         ("u" . undo-tree-visualize))
  :diminish undo-tree-mode
  :config
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-enable-undo-in-region nil) ;; disabled because it's buggy
  (setq undo-tree-history-directory-alist
        `(("." . ,(file-name-as-directory (file-name-concat user-emacs-directory
                                                           "cache"
                                                           "undo-history")))))
  (global-undo-tree-mode))


(use-package ace-link
  :config
  (ace-link-setup-default))


(use-package avy
  :bind (:map ngoc-prefix
         ("t" . avy-goto-word-or-subword-1)
         ("c" . avy-goto-char-timer)
         ("n" . avy-goto-line))
  :config
  (setq avy-all-windows t)
  (setq avy-keys-alist `((avy-goto-line . ,(append (number-sequence ?a ?z) (number-sequence ?0 ?9)))))
  (setq avy-keys '(?a ?o ?e ?u ?h ?t ?n ?s ?c ?r ?j ?m)))


(use-package ace-window
  :bind ("M-o" . ace-window)
  :config
  (setq aw-keys '(?h ?t ?n ?s ?a ?o ?e ?u)
        aw-scope 'frame))


;; (require 'key-chord)
;; (key-chord-mode t)
;; (key-chord-define evil-insert-state-map "hj" 'xah-forward-right-bracket)
;; (key-chord-define evil-insert-state-map "hk" 'xah-backward-left-bracket)
;; (key-chord-define evil-insert-state-map "tj" 'yas-expand) ; tj tk are also good key-chord


;;   ____ _   _ ___   _                      _
;;  / ___| | | |_ _| | |___      _____  __ _| | _____
;; | |  _| | | || |  | __\ \ /\ / / _ \/ _` | |/ / __|
;; | |_| | |_| || |  | |_ \ V  V /  __/ (_| |   <\__ \
;;  \____|\___/|___|  \__| \_/\_/ \___|\__,_|_|\_\___/

(setq echo-keystrokes 0.0001)
(setq minibuffer-prompt-properties '(readonly t point-entered minibuffer-avoid-prompt face minibuffer-prompt))

(blink-cursor-mode 0)

;; original dracula theme's compatibility is not as good as doom-dracula theme
(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-dracula t)
  
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(setq ngoc/interface-font-name
      (cond
       ((eq system-type 'gnu/linux)
        "Hack")
       ((eq system-type 'darwin)
        "Menlo")
       (t
        "Monospace")))

(add-hook 'window-setup-hook
          #'(lambda ()
              (set-cursor-color "#69E300")
              (when (eq system-type 'gnu/linux)
                (set-frame-font (font-spec :family ngoc/interface-font-name
                                           :size 10.5)))))

;; mode line
(column-number-mode 1)
(setq mode-line-position-column-line-format '(" %l,%c"))

(use-package hl-line
  :config
  (global-hl-line-mode t))

(setq frame-title-format
      '("" invocation-name ": " (:eval (replace-regexp-in-string
                                        "^ +" "" (buffer-name)))))
(setq mouse-yank-at-point t)

(set-frame-parameter nil 'alpha-background 100)


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

(setq browse-url-generic-program "google-chrome-stable")
(setq browse-url-browser-function 'browse-url-generic)
;; prevent async output buffer to show up
(add-to-list 'display-buffer-alist (cons "\\*Async Shell Command\\*.*" (cons #'display-buffer-no-window nil)))

;; save last edit position
(use-package saveplace
  :config
  (save-place-mode t)
  (setq save-place-forget-unreadable-files nil))

;; buffer naming
(setq uniquify-buffer-name-style 'post-forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)    ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers
(require 'uniquify)


(setq scroll-margin 10000 ;; large number so it always keep 1/4 of the screen in margin
      maximum-scroll-margin 0.25
      scroll-conservatively 10
      scroll-preserve-screen-position t
      next-screen-context-lines 5
      isearch-allow-scroll t)

;; ;; improve scrolling performance
(setq redisplay-skip-fontification-on-input t)

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

(use-package winner
  :demand t ;; load immediately to save window configuration
  :bind (("M-F" . winner-redo)
         ("M-B" . winner-undo))
  :config
  (winner-mode 1))

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

(global-set-key (kbd "C-x k")     'kill-this-buffer)
(global-set-key (kbd "C-1")       'delete-other-windows)
(global-set-key (kbd "C-<left>")  'shrink-window-horizontally)
(global-set-key (kbd "C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-<down>")  'shrink-window)
(global-set-key (kbd "C-<up>")    'enlarge-window)
(global-set-key (kbd "C-c g")     'ag-regexp) ; TODO use counsel-ag

(define-prefix-command 'ngoc/window-prefix)
(define-key ngoc-prefix "w" 'ngoc/window-prefix)
(define-key ngoc/window-prefix "d" #'delete-window)
(define-key ngoc/window-prefix "e" #'balance-windows)
(define-key ngoc/window-prefix "w" #'delete-other-windows)
(define-key ngoc/window-prefix "v" #'vsplit-last-buffer)
(define-key ngoc/window-prefix "h" #'hsplit-last-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              VOLATILE HIGHLIGHTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package volatile-highlights
  :diminish volatile-highlights-mode
  :config
  (setq Vhl/highlight-zero-width-ranges t)
  (volatile-highlights-mode t))


;; (setq grep-highlight-matches 'auto-detect)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; WINDOWS CONFIG

;;(setq win:base-key ?`)		;; '`' is before 'a'
;;(setq win:max-configs 27)	;; '`' to 'z' are 27 chars.
;;(setq win:quick-selection nil)	;; Not assign `C-c LETTER'
;;(setq win:switch-prefix "\C-q")
;;(setq win:use-frame nil)
;;(define-key global-map win:switch-prefix nil)
;;(require 'windows)
;;(win:startup-with-window)
;;(define-key ctl-x-map "C" 'see-you-again)
;;(define-key win:switch-map "\C-q" 'win-toggle-window)
;;(define-key win:switch-map "n" 'win-switch-to-window)
;;(define-key win:switch-map "p" 'win-switch-to-window)

(use-package eyebrowse
  :init
  (setq eyebrowse-keymap-prefix (kbd "C-'"))
  :config
  (eyebrowse-mode t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              IVY COUNSEL SWIPER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'ivy)
(require 'counsel)
(ivy-mode t)
(diminish 'ivy-mode)

;; on emacs 25 maybe should change this to string-collate-lessp
(add-to-list 'ivy-sort-functions-alist '(read-file-name-internal . string-lessp))

(setq ivy-re-builders-alist
      '((t . ivy--regex-plus)))

(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(define-key ngoc-prefix "f" 'counsel-projectile-find-file)
(global-set-key (kbd "C-S-Y") 'counsel-yank-pop)
(global-set-key (kbd "C-x b") 'counsel-switch-buffer)


;;      _                _             _
;;  ___| |__   ___  _ __| |_ ___ _   _| |_ ___
;; / __| '_ \ / _ \| '__| __/ __| | | | __/ __|
;; \__ \ | | | (_) | |  | || (__| |_| | |_\__ \
;; |___/_| |_|\___/|_|   \__\___|\__,_|\__|___/
(use-package which-key
  :after god-mode
  :diminish which-key-mode
  :config
  (which-key-mode 1)
  (which-key-enable-god-mode-support))

(use-package free-keys)

;; easy exit from transient menu using ESC
(use-package transient
  :config
  (define-key transient-base-map (kbd "<escape>") #'transient-quit-one))


(use-package fcitx
  :if (eq system-type 'gnu/linux)
  :config
  (setq fcix-use-dbus 'fcitx5)
  (setq fcitx-remote-command "fcitx5-remote")
  (fcitx-default-setup))
;; TODO atm, fcix doesn't work

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


;;   ___                                      _
;;  / _ \ _ __ __ _       _ __ ___   ___   __| | ___
;; | | | | '__/ _` |_____| '_ ` _ \ / _ \ / _` |/ _ \
;; | |_| | | | (_| |_____| | | | | | (_) | (_| |  __/
;;  \___/|_|  \__, |     |_| |_| |_|\___/ \__,_|\___|
;;            |___/

(use-package cdlatex)
(require 'org)
(require 'org-agenda)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cr" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-startup-truncated nil)
(setq org-directory "~/Dropbox/org/gtd")
(setq org-mobile-directory "~/Dropbox/org/mobileorg")
(setq org-tag-persistent-alist '((:startgroup . nil)
                                 ("home" . ?h)
                                 ("office" . ?o)
                                 (:endgroup . nil)))
(defun org-file-path (filename)
  "Return the absolute address of an org file, given its relative name."
  (concat (file-name-as-directory org-directory) filename))

(setq org-link-abbrev-alist '(("gmail" . "https://mail.google.com/mail/u/0/#search/rfc822msgid%3A%h")))
(setq org-agenda-span 'fortnight)
(setq org-todo-keywords
      '((sequence "TODO" "WANT(t)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELED(c@)" "DEFERRED(f@)")))
;; (setq org-inbox-file (org-file-path "inbox.org"))
(setq org-archive-location
      (concat (org-file-path "archive.org") "::* From %s"))

(setq org-agenda-files (list org-directory
                             (org-file-path "projects")))
(setq org-refile-targets
      '((nil :maxlevel . 2)
        (org-agenda-files :maxlevel . 2)))

(add-to-list 'org-agenda-sorting-strategy '(agenda time-up habit-down todo-state-down timestamp-up priority-down category-keep))
(setq org-agenda-time-grid
      '((daily today require-timed remove-match)
        (800 1000 1200 1400 1600 1800 2000)
        "......"
        "----------------"))

(setq org-capture-templates
      '(("r" "Reading"
         checkitem
         (file "toread.org"))

        ("t" "Want"
         entry
         (file "inbox.org")
         "* WANT %?\n")))

;; log both reschedule and redeadline
(setq org-log-reschedule 'time
      org-log-redeadline 'time)

;; 4 level priorities using important, urgent matrix
;;     A is important, urgent
;;     B is important, not urgent
;;     C is not important, urgent
;;     D is not important, not urgent
(setq org-lowest-priority ?D)
(setq org-default-priority ?D)
(setq org-agenda-show-all-dates nil)
(require 'org-habit)
(add-to-list 'org-modules 'org-habit)
(setq org-agenda-prefer-last-repeat t) ;; mostly to display missed habits on today agenda
(setq org-habit-graph-column 60) ;; move graph to the right so it shows a little more text

(require 'org-protocol)
(add-to-list 'org-agenda-custom-commands
             (quote ("d" "Undated tasks" alltodo ""
                     ((org-agenda-todo-ignore-with-date t)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                      VIETNAMESE LUNAR CALENDAR
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq calendar-chinese-time-zone 420)
(setq calendar-chinese-celestial-stem
      ["Giáp" "Ất" "Bính" "Đinh" "Mậu" "Kỷ" "Canh" "Tân" "Nhâm" "Quý"]
      calendar-chinese-terrestrial-branch
      ["Tý" "Sửu" "Dần" "Mão" "Thìn" "Tị" "Ngọ" "Mùi" "Thân" "Dậu" "Tuất" "Hợi"]
      calendar-chinese-month-name-array
      ["Tháng 1" "Tháng 2" "Tháng 3" "Tháng 4" "Tháng 5" "Tháng 6" "Tháng 7" "Tháng 8" "Tháng 9" "Tháng 10" "Tháng 11" "Tháng 12"])
(setq calendar-chinese-year-cache
      '((2008 (12 733049) (1 733079) (2 733109) (3 733138) (4 733167) (5 733197)
              (6 733226) (7 733255) (8 733285) (9 733314) (10 733344) (11 733373))
        (2009 (12 733403) (1 733433) (2 733463) (3 733492) (4 733522) (5 733551)
              (5.5 733581) (6 733610) (7 733639) (8 733669) (9 733698) (10 733728)
              (11 733757))
        (2010 (12 733787) (1 733817) (2 733847) (3 733876) (4 733906) (5 733935)
              (6 733965) (7 733994) (8 734023) (9 734053) (10 734082) (11 734112))
        (2011 (12 734141) (1 734171) (2 734201) (3 734230) (4 734260) (5 734290)
              (6 734319) (7 734349) (8 734378) (9 734407) (10 734437) (11 734466))
        (2012 (12 734496) (1 734525) (2 734555) (3 734584) (4 734614) (4.5 734644)
              (5 734673) (6 734703) (7 734732) (8 734762) (9 734791) (10 734821)
              (11 734850))
        (2013 (12 734880) (1 734909) (2 734939) (3 734968) (4 734998) (5 735027)
              (6 735057) (7 735087) (8 735116) (9 735146) (10 735175) (11 735205))
        (2014 (12 735234) (1 735264) (2 735293) (3 735323) (4 735352) (5 735382)
              (6 735411) (7 735441) (8 735470) (9 735500) (9.5 735530) (10 735559)
              (11 735589))
        (2015 (12 735618) (1 735648) (2 735677) (3 735707) (4 735736) (5 735765)
              (6 735795) (7 735824) (8 735854) (9 735884) (10 735914) (11 735943))
        (2016 (12 735973) (1 736002) (2 736032) (3 736061) (4 736091) (5 736120)
              (6 736149) (7 736179) (8 736208) (9 736238) (10 736268) (11 736297))
        (2017 (12 736327) (1 736357) (2 736386) (3 736416) (4 736445) (5 736475)
              (6 736504) (6.5 736533) (7 736563) (8 736592) (9 736622) (10 736651)
              (11 736681))
        (2018 (12 736711) (1 736741) (2 736770) (3 736800) (4 736829) (5 736859)
              (6 736888) (7 736917) (8 736947) (9 736976) (10 737005) (11 737035))
        (2019 (12 737065) (1 737095) (2 737124) (3 737154) (4 737184) (5 737213)
              (6 737243) (7 737272) (8 737301) (9 737331) (10 737360) (11 737389))
        (2020 (12 737419) (1 737449) (2 737478) (3 737508) (4 737538) (4.5 737568)
              (5 737597) (6 737627) (7 737656) (8 737685) (9 737715) (10 737744)
              (11 737773))
        (2021 (12 737803) (1 737833) (2 737862) (3 737892) (4 737922) (5 737951)
              (6 737981) (7 738010) (8 738040) (9 738069) (10 738099) (11 738128))
        (2022 (12 738158) (1 738187) (2 738217) (3 738246) (4 738276) (5 738305)
              (6 738335) (7 738365) (8 738394) (9 738424) (10 738453) (11 738483))
        (2023 (12 738512) (1 738542) (2 738571) (2.5 738601) (3 738630) (4 738659)
              (5 738689) (6 738719) (7 738748) (8 738778) (9 738808) (10 738837)
              (11 738867))
        (2024 (12 738896) (1 738926) (2 738955) (3 738985) (4 739014) (5 739043)
              (6 739073) (7 739102) (8 739132) (9 739162) (10 739191) (11 739221))
        (2025 (12 739251) (1 739280) (2 739310) (3 739339) (4 739369) (5 739398)
              (6 739427) (6.5 739457) (7 739486) (8 739516) (9 739545) (10 739575)
              (11 739605))
        (2026 (12 739635) (1 739664) (2 739694) (3 739723) (4 739753) (5 739782)
              (6 739811) (7 739841) (8 739870) (9 739899) (10 739929) (11 739959))
        (2027 (12 739989) (1 740018) (2 740048) (3 740078) (4 740107) (5 740137)
              (6 740166) (7 740195) (8 740225) (9 740254) (10 740283) (11 740313))
        (2028 (12 740343) (1 740372) (2 740402) (3 740432) (4 740462) (5 740491)
              (5.5 740521) (6 740550) (7 740579) (8 740609) (9 740638) (10 740667)
              (11 740697))))


;; used to detect Dropbox conflict files when open in emacs
;; (require 'dropbox-conflicts)
;; (dropbox-conflicts-mode t)

(setq TeX-PDF-mode t)
(latex-preview-pane-enable)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                         SPELL AND GRAMMAR CHECKING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'flyspell-lazy)
(flyspell-lazy-mode 1)

(require 'flyspell)
(add-hook 'LaTeX-mode-hook 'turn-on-flyspell)
(add-hook 'org-mode-hook 'turn-on-flyspell)

;; (ispell-change-dictionary "american")
(setq ispell-dictionary "american")

(require 'langtool)
(setq langtool-language-tool-jar "~/builds/LanguageTool-3.4/languagetool-commandline.jar")
(defun langtool-autoshow-detail-popup (overlays)
  (when (require 'popup nil t)
    ;; Do not interrupt current popup
    (unless (or popup-instances
                ;; suppress popup after type `C-g` .
                (memq last-command '(keyboard-quit)))
      (let ((msg (langtool-details-error-message overlays)))
        (popup-tip msg)))))
(setq langtool-autoshow-message-function
      'langtool-autoshow-detail-popup)

;;(setq-default display-buffer-alist nil)
;;(add-to-list 'display-buffer-alist
;;                    `(,(rx bos "*helm" (* not-newline) "*" eos)
;;                         (display-buffer-in-side-window)
;;                         (inhibit-same-window . t)
;;                         (window-height . 0.4)))
;;(add-to-list 'display-buffer-alist
;;                    `(,(rx bos "*Completions*" eos)
;;                         (display-buffer-in-side-window)
;;                         (inhibit-same-window . t)
;;                         (window-height . 0.4)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EMACS DAEMON
;; Allow access from emacsclient
;;----------------------------------------------------------------------------
(require 'server)
(unless (server-running-p)
  (server-start))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              DIRED
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO dired dwim target function
;; TODO dired open file in GUI file manager / associated program
(require 'dired)
(setq dired-listing-switches "-lah")
(setq delete-by-moving-to-trash t)
(setq dired-dwim-target nil)


(defun ngoc/make-line-taller ()
  (setq line-spacing 0.2))

;; increase line spacing in dired mode -> easier to read
(add-hook 'dired-mode-hook #'ngoc/make-line-taller)

;; (use-package dired-efap
;;   :config
;;   (define-key dired-mode-map (kbd "r") 'dired-efap))

(use-package dired-open
  :config
  (define-key dired-mode-map (kbd "<S-return>") 'dired-open-xdg))

(use-package terminal-here
  :config
  (progn
    (setq terminal-here-terminal-command '("gnome-terminal")
          terminal-here-command-flag "--")
    (global-set-key (kbd "C-!") 'terminal-here-launch)))

;; (use-package stripe-buffer
;;   :hook
;;   (dired-mode . turn-on-stripe-buffer-mode))

(use-package all-the-icons
  :if (display-graphic-p))

(use-package all-the-icons-dired
  :diminish all-the-icons-dired-mode
  :hook
    (dired-mode . all-the-icons-dired-mode))


(use-package keyfreq
  :init
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))


(use-package dockerfile-mode)

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

(use-package god-mode
  :bind (("<escape>" . god-local-mode) ; mimic vim to exit insert mode
         :map god-local-mode-map
         ("i"        . god-local-mode) ; mimic vim to enter insert mode
         ("."        . repeat)
         ("<escape>" . ignore)
         ("h"        . ngoc-prefix)
         ("<f1>"     . help-command))
  
  :hook ((text-mode . god-local-mode)
         (prog-mode . god-local-mode)
         (conf-mode . god-local-mode))
  
  :diminish god-local-mode
  
  :config
  (defun my-god-mode-update-cursor-type ()
    (setq cursor-type (if god-local-mode 'box 'bar)))

  (add-hook 'post-command-hook #'my-god-mode-update-cursor-type))


;; editing
;; don't need zap-to-char because zap-up-to-char + d = zap-to-char (in god-mode)
(global-set-key (kbd "C-z")     'zap-up-to-char)
(global-set-key (kbd "M-2")     'mark-word)
;; TODO add ability to duplicate to upper or lower
(define-key ngoc-prefix "d" 'duplicate-dwim)


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
(setq recentf-save-file (concat recentf-save-file "-" (system-name)))
(recentf-mode 1)
(global-set-key "\C-x\ \C-r" 'counsel-recentf)


(use-package figlet
  :config
  (setq figlet-default-font "standard"))

(use-package beacon
  :diminish beacon-mode
  :config
  (setq beacon-blink-when-focused t)
  (setq beacon-color "#579E33")
  (beacon-mode 1))

(use-package autorevert
  :diminish auto-revert-mode)
;;  _            _              _ _ _   _
;; | |_ _____  _| |_    ___  __| (_) |_(_)_ __   __ _
;; | __/ _ \ \/ / __|  / _ \/ _` | | __| | '_ \ / _` |
;; | ||  __/>  <| |_  |  __/ (_| | | |_| | | | | (_| |
;;  \__\___/_/\_\\__|  \___|\__,_|_|\__|_|_| |_|\__, |
;;                                              |___/
(use-package whole-line-or-region
  :diminish whole-line-or-region-local-mode
  :config
  (whole-line-or-region-global-mode 1))
;; TODO: do similar thing to transpose-regions

;;      _ _     _                  _   _                __
;;   __| (_)___| |_ _ __ __ _  ___| |_(_) ___  _ __    / _|_ __ ___  ___
;;  / _` | / __| __| '__/ _` |/ __| __| |/ _ \| '_ \  | |_| '__/ _ \/ _ \
;; | (_| | \__ \ |_| | | (_| | (__| |_| | (_) | | | | |  _| | |  __/  __/
;;  \__,_|_|___/\__|_|  \__,_|\___|\__|_|\___/|_| |_| |_| |_|  \___|\___|

(use-package writeroom-mode
  :config
  (remove-hook 'writeroom-global-effects 'writeroom-set-fullscreen)
  (add-hook 'writeroom-global-effects 'writeroom-set-internal-border-width))


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
  (text-mode . hl-todo-mode))

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

(define-key ngoc-prefix "=" 'ngoc/align-dwim)


(require 'dabbrev)
(setq dabbrev-abbrev-char-regexp "\\sw\\|\\s_\\|\\s.")
(setq dabbrev-case-fold-search nil)

;; COMMENT
(use-package comment-dwim-2
  :bind ("C-;" . comment-dwim-2))

(use-package wgrep) ;; TODO how to use this

(use-package iedit
  :bind (:map ngoc-prefix
         ("m" . iedit-mode)
         :map iedit-mode-keymap
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
  :bind (("M-3" . er/expand-region)
         :map ngoc-prefix
         ("a" . er/mark-outside-pairs)
         ("i" . er/mark-inside-pairs)
         ("'" . er/mark-inside-quotes)
         ("\"" . er/mark-outside-quotes))
         
  :config
  (setq expand-region-reset-fast-key "<escape>")
  (require 'er-basic-expansions))


;;            _                  _
;;  ___ _ __ (_)_ __  _ __   ___| |_ ___
;; / __| '_ \| | '_ \| '_ \ / _ \ __/ __|
;; \__ \ | | | | |_) | |_) |  __/ |_\__ \
;; |___/_| |_|_| .__/| .__/ \___|\__|___/
;;             |_|   |_|
(use-package yasnippet
  :diminish yas-minor-mode
  :bind (:map yas-minor-mode-map
              ("<tab>" . nil)
              ("TAB" . nil))
  :hook
  (prog-mode . yas-minor-mode)
  (text-mode . yas-minor-mode)
  :config
  ;; https://emacs.stackexchange.com/questions/38242/problem-redoing-with-yasnippet
  (setq yas-snippet-revival nil))

(use-package php-mode)

(use-package web-mode)
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'"   . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'"   . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'"    . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'"       . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'"     . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'"  . web-mode))
(add-to-list 'auto-mode-alist '("\\.phtml\\'"     . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\'"       . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))

(add-hook 'web-mode-before-auto-complete-hooks
          '(lambda ()
             (let ((web-mode-cur-language
                    (web-mode-language-at-pos)))
               (if (string= web-mode-cur-language "php")
                   (yas-activate-extra-mode 'php-mode)
                 (yas-deactivate-extra-mode 'php-mode))
               (if (string= web-mode-cur-language "css")
                   (setq emmet-use-css-transform t)
                 (setq emmet-use-css-transform nil)))))
(setq web-mode-ac-sources-alist
      '(("php" . (ac-source-php ac-source-words-in-same-mode-buffers))
        ("css" . (ac-source-css-property))
        ("html" . (ac-source-words-in-same-mode-buffers ac-source-abbrev))))

(add-hook 'web-mode-hook
          '(lambda ()
             (emmet-mode t)
             (yas-activate-extra-mode 'html-mode)))

(setq-default web-mode-markup-indent-offset tab-width)
(setq-default web-mode-css-indent-offset tab-width)
(setq-default web-mode-code-indent-offset tab-width)
(setq-default web-mode-sql-indent-offset tab-width)
(setq web-mode-enable-control-block-indentation nil)

(use-package skewer-mode
  :hook
  (js2-mode . skewer-mode)
  (css-mode . skewer-css-mode)
  :config
  (setq httpd-port 8089))

;;  ____            _           _   _ _
;; |  _ \ _ __ ___ (_) ___  ___| |_(_) | ___
;; | |_) | '__/ _ \| |/ _ \/ __| __| | |/ _ \
;; |  __/| | | (_) | |  __/ (__| |_| | |  __/
;; |_|   |_|  \___// |\___|\___|\__|_|_|\___|
;;               |__/

(use-package ag)
(use-package projectile
  :hook
  (prog-mode . projectile-mode)
  (text-mode . projectile-mode)
  :config
  (setq projectile-enable-caching t)
  (setq projectile-mode-line-function #'(lambda ()
                                          (if
                                              (file-remote-p default-directory)
                                              " Proj"
                                            (format " P[%s]" (projectile-project-name))))))

;; TODO: reorder minor mode name on modeline, projectile first seems better

(setq projectile-mode-line
      '(:eval
        (if
            (file-remote-p default-directory)
            " Projectile"
          (format " P[%s]"
                  (projectile-project-name)))))


(use-package counsel-projectile
  :after (counsel projectile)
  :config
  (counsel-projectile-mode))


(use-package ibuffer
  :bind ("C-x C-b" . ibuffer))

(use-package ibuffer-projectile
  :after (ibuffer)

  :hook
  (ibuffer . ngoc/ibuffer-setup)
  
  :config
  (defun ngoc/ibuffer-setup ()
    (ibuffer-projectile-set-filter-groups)
    (unless (eq ibuffer-sorting-mode 'filename/process)
      (ibuffer-do-sort-by-filename/process))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IBUFFER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (use-package ibuffer-tramp
;;   :config
;;   (add-hook 'ibuffer-hook
;;             (lambda ()
;;               (ibuffer-tramp-set-filter-groups-by-tramp-connection)
;;               (ibuffer-do-sort-by-alphabetic))))


(use-package frog-jump-buffer
  :bind (:map ngoc-prefix
         ("b" . frog-jump-buffer))
  :config
  (setq frog-menu-avy-keys '(?o ?e ?u ?i ?d ?h ?t ?n ?k ?j ?b ?m ?c ?r))
  (setq frog-menu-posframe-border-width 15))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              GGTAGS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package ggtags
  :config
  (setq ggtags-global-abbreviate-filename nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              EDIFF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This is what you probably want if you are using a tiling window
;; manager under X, such as ratpoison.
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-diff-options "-w")
(setq ediff-merge-filename-prefix "")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              MAGIT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package magit
  :bind (("C-x g" . magit-status)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              DIFF HL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package diff-hl
  :config
  (global-diff-hl-mode t)
  (diff-hl-flydiff-mode t))


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

(defvar python-prettify-symbols-alist
  '(("self"   . ?自)
    ("or"     . ?⋁)
    ("and"    . ?⋀)
    ("not"    . #x2757)
    ;; ("not in" . ?∉)
    ;; ("in"     . ?∈)
    ;; ("is not" . ?≢)
    ;; ("is"     . ?≡)
    ))

(add-hook 'php-mode-hook
          (lambda ()
            (push '("true" . ?✅) prettify-symbols-alist)
            (push '("false" . ?❎) prettify-symbols-alist)
            (push '("global" . ?🌐) prettify-symbols-alist)
            (push '("$this" . ?自) prettify-symbols-alist)
            (push '("||"    . ?⋁) prettify-symbols-alist)
            (push '("&&" . ?⋀) prettify-symbols-alist)))

(add-hook 'js2-mode-hook
          (lambda ()
            (push '("true" . ?✅) prettify-symbols-alist)
            (push '("false" . ?❎) prettify-symbols-alist)
            (push '("global" . ?🌐) prettify-symbols-alist)
            (push '("this" . ?自) prettify-symbols-alist)
            (push '("||"    . ?⋁) prettify-symbols-alist)
            (push '("&&" . ?⋀) prettify-symbols-alist)))
;; (setq prettify-symbols-unprettify-at-point t)
;; (global-prettify-symbols-mode +1)

;; (require 'emojify)
;; (setq emojify-emoji-styles '(unicode))
;; (global-emojify-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              FLYCHECK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package flycheck
  :config
  (setq flycheck-mode-line-prefix "F"))


(use-package highlight-indentation
  :diminish highlight-indentation-mode
  :hook (prog-mode . highlight-indentation-mode))


;;                    _             _   _
;;  _ __   __ ___   _(_) __ _  __ _| |_(_) ___  _ __
;; | '_ \ / _` \ \ / / |/ _` |/ _` | __| |/ _ \| '_ \
;; | | | | (_| |\ V /| | (_| | (_| | |_| | (_) | | | |
;; |_| |_|\__,_| \_/ |_|\__, |\__,_|\__|_|\___/|_| |_|
;;                      |___/
;; DONE add beacon blink when traverse history
;; DONE add removal ability
;; DONE change history faces
;; TODO make history cyclic
;; TODO add messages for cases like: show history but no history, clear history
(use-package history
  :bind (:map ngoc-prefix
         ("h p" . history-goto-history) ; h p = history -> preview
         ("h a" . history-add-history)
         ("h c h" . history-kill-histories)) ; h c h = history -> clear history
  :config
  (define-key history-map (kbd "d") 'ngoc/history-remove-current)
  (define-key history-map (kbd "p") 'history-preview-prev-history)
  (define-key history-map (kbd "n") 'history-preview-next-history)
  (define-key history-map (kbd "C-g") 'history-preview-cancel-history)

  (advice-add 'history-use-current-history :after #'beacon-blink)

  (set-face-attribute 'history-current-history nil :inherit 'match :foreground nil :background (face-attribute 'default :background))
  (set-face-attribute 'history-other-history nil :inherit 'default :foreground nil :background nil :height 1.8)

  (defun ngoc/history-remove-current ()
    (interactive)
    (when history-stack
      (let* ((history (nth history-index history-stack))
             (marker (plist-get history :marker)))
        (set-marker marker nil)
        (history-remove-invalid-history)))
    (cond
     (history-stack
      (delete-minibuffer-contents)
      (insert (history-histories-string))
      (re-search-backward "\\*")
      ;; Use history.
      (with-selected-window (history-window)
        (history-use-current-history)))
     (t
      (history-preview-cancel-history)))))



(use-package dumb-jump
  :bind (("M-g o"   . dumb-jump-go-other-window)
         ("M-g j"   . dumb-jump-go)
         ("M-g M-g" . dumb-jump-go)
         ("M-g b"   . dumb-jump-back)
         ("M-g i"   . dumb-jump-go-prompt)
         ("M-g x"   . dumb-jump-go-prefer-external)
         ("M-g z"   . dumb-jump-go-prefer-external-other-window))
  :config
  (setq dumb-jump-selector 'ivy)
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package imenu-list
  :bind (:map ngoc-prefix
         ("s" . imenu-list-smart-toggle)))


(use-package symbol-overlay
  :config
  (require 'transient)
  (transient-define-prefix symbol-overlay-transient ()
    "Symbol Overlay transient"
    ["Symbol Overlay"
     ["Overlays"
      ("o" "Add/Remove at point" symbol-overlay-put)
      ("c" "Remove All" (lambda ()
                          (interactive)
                          (call-interactively 'symbol-overlay-remove-all)))]
     ["Other"
      ("m" "Highlight symbol-at-point" symbol-overlay-mode)]])

  (define-key 'ngoc-prefix (kbd "o") 'symbol-overlay-transient)

  (setcdr symbol-overlay-map nil) ;; clear overlay keymap
  (define-key symbol-overlay-map (kbd "M-n") #'symbol-overlay-jump-next)
  (define-key symbol-overlay-map (kbd "M-p") #'symbol-overlay-jump-prev)
  (global-set-key (kbd "M-N") 'symbol-overlay-switch-forward)
  (global-set-key (kbd "M-P") 'symbol-overlay-switch-backward))


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

(use-package eglot
  :after transient
  :init
  ;; do not display this warning from
  ;; jsonrpc ‘:events-buffer-scrollback-size’ deprecated. Use ‘events-buffer-config’.
  ;; FIXME so basically hide all warnings from emacs by default...
  (add-to-list 'warning-suppress-types '(emacs))
  :hook
  (prog-mode . eglot-ensure)
  :config
  (setq eglot-extend-to-xref t)
  ;; python
  (add-to-list 'eglot-server-programs
               '(python-mode . ("pyright-langserver" "--stdio")))

  (transient-define-prefix ngoc/eglot-transient ()
    [:class transient-row "Eglot Menu"]

    [["Server"
      ("s" "Start server" eglot)
      ("R" "Reconnect" eglot-reconnect)
      ("S" "Shutdown" eglot-shutdown)]

     ["Refactor"
      ("r" "Rename" eglot-rename)
      ("o" "Organize imports" eglot-code-action-organize-imports)
      ("f" "Quick fix" eglot-code-action-quickfix)
      ("e" "Extract" eglot-code-action-extract)
      ("i" "Inline" eglot-code-action-inline)
      ("w" "Rewrite" eglot-code-action-rewrite)]

     ["Flymake"
      ("b" "Buffer diagnostics" flymake-show-buffer-diagnostics)
      ("p" "Project diagnostics" flymake-show-project-diagnostics)]

     ["Other"
      ("a" "Show actions" eglot-code-actions)
      ("F" "Format" eglot-format)
      ("d" "ElDoc" eldoc)
      ("h" "Toggle Inlay hints" eglot-inlay-hints-mode)]])

  (define-key ngoc-prefix "e" #'ngoc/eglot-transient))


;;              _                                    _      _
;;   __ _ _   _| |_ ___     ___ ___  _ __ ___  _ __ | | ___| |_ ___
;;  / _` | | | | __/ _ \   / __/ _ \| '_ ` _ \| '_ \| |/ _ \ __/ _ \
;; | (_| | |_| | || (_) | | (_| (_) | | | | | | |_) | |  __/ ||  __/
;;  \__,_|\__,_|\__\___/   \___\___/|_| |_| |_| .__/|_|\___|\__\___|
;;                                            |_|
(use-package company
  :diminish company-mode
  :hook
  (prog-mode . company-mode)
  (text-mode . company-mode)
  
  :bind (("M-8"      . company-yasnippet)
         :map company-active-map
         ("<escape>" . company-abort)
         ("M-n"      . company-select-next-or-abort)
         ("M-p"      . company-select-previous-or-abort)
         :map company-search-map
         ("<escape>" . company-abort)
         ("M-n"      . company-select-next-or-abort)
         ("M-p"      . company-select-previous-or-abort))
  :config
  (setq company-dabbrev-downcase nil)
  (setq company-minimum-prefix-length 2)
  (setq company-idle-delay 0.05)
  (setq company-format-margin-function 'company-text-icons-margin))

 ;;   ____            _ _       _
;;  / ___|___  _ __ (_) | ___ | |_
;; | |   / _ \| '_ \| | |/ _ \| __|
;; | |__| (_) | |_) | | | (_) | |_
;;  \____\___/| .__/|_|_|\___/ \__|
;;            |_|
;;
;; behavior: 1. if copilot shows up, cancel company, except when company is manually started.
;;           2. don't show copilot when god-mode is activated.
;;           3. disable copilot on Leetcode
(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
  :bind (:map copilot-completion-map
              ("<escape>" . ngoc/copilot-clear-no-notify)
              ("<tab>"    . copilot-accept-completion) ; don't bind "TAB" so that i can use C-i when i need to adjust indent
              ("M-t"      . copilot-accept-completion-by-word)
              ("M-T"      . copilot-accept-completion-by-line)
              ("M-n"      . copilot-next-completion)
              ("M-p"      . copilot-previous-completion)
              ("M-c"      . ngoc/abort-copilot-start-company))

  :hook
  (prog-mode . copilot-mode)

  :config
  (defun ngoc/copilot-clear-no-notify ()
    (interactive)
    (copilot-clear-overlay t))
  
  (defun ngoc/god-mode-not-enabled ()
    (not (bound-and-true-p god-local-mode)))

  (defun ngoc/not-in-leetcode ()
    (not (and (bound-and-true-p atomic-chrome-edit-mode)
              (string-match-p " - LeetCode$" (buffer-name)))))

  (defun ngoc/company-not-manually-started ()
    (if (company-explicit-action-p)
        nil
      (company-abort)
      t))

  (defun ngoc/abort-company-when-copilot-overlay-visible (manually-started)
    (when (and (copilot--overlay-visible)
               (not manually-started))
      (company-abort)))

  (defun ngoc/abort-copilot-start-company ()
    (interactive)
    (copilot-clear-overlay t)             ; pass t to clear overlay so that it doesn't notify server about rejection
    (company-manual-begin))

  (remove-hook  'copilot-enable-predicates          'evil-insert-state-p)  ;  i  don't  use  evil  mode
  (add-hook     'copilot-enable-predicates          'ngoc/god-mode-not-enabled)
  (add-hook     'copilot-enable-predicates          'ngoc/not-in-leetcode)
  (add-hook     'copilot-enable-display-predicates  'ngoc/company-not-manually-started)
  (add-hook     'company-completion-started-hook    'ngoc/abort-company-when-copilot-overlay-visible)

  ;; disable warnings
  (add-to-list 'warning-suppress-types '(copilot copilot-exceeds-max-char))
  (setq copilot-indent-offset-warning-disable t)


  ;; ;; temporary fix for tab key, sometimes overlay is visible but copilot's keymap is not active
  ;; (defun ngoc/copilot-compatible-tab (&optional arg)
  ;;   (interactive "P")
  ;;   (if (copilot--overlay-visible)
  ;;       (copilot-accept-completion)
  ;;     (indent-for-tab-command arg)))

  ;; ;; don't bind this tab outside of prog-mode
  ;; (add-hook 'prog-mode-hook
  ;;           (lambda ()
  ;;             (local-set-key (kbd "<tab>") 'ngoc/copilot-compatible-tab)))

  )



;;                                             _     _ _     _
;;   __ _ _ __ __ _ _   _ _ __ ___   ___ _ __ | |_  | (_)___| |_
;;  / _` | '__/ _` | | | | '_ ` _ \ / _ \ '_ \| __| | | / __| __|
;; | (_| | | | (_| | |_| | | | | | |  __/ | | | |_  | | \__ \ |_
;;  \__,_|_|  \__, |\__,_|_| |_| |_|\___|_| |_|\__| |_|_|___/\__|
;;            |___/
(use-package fill-function-arguments
  :bind (:map ngoc-prefix
              ("q" . ngoc/fill-function-arguments-dwim))
  
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

(use-package zeal-at-point)
(global-set-key "\C-cd" 'zeal-at-point)
(add-hook 'projectile-rails-mode-hook
          (lambda () (setq zeal-at-point-docset "ruby,rails")))
(add-to-list 'zeal-at-point-mode-alist '(c++-mode . ""))


;; better language grammar
(use-package treesit
  :ensure nil
  :config
  (setq treesit-language-source-alist
        '((bash "https://github.com/tree-sitter/tree-sitter-bash")
          (cmake "https://github.com/uyha/tree-sitter-cmake")
          (css "https://github.com/tree-sitter/tree-sitter-css")
          (elisp "https://github.com/Wilfred/tree-sitter-elisp")
          (go "https://github.com/tree-sitter/tree-sitter-go")
          (html "https://github.com/tree-sitter/tree-sitter-html")
          (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
          (json "https://github.com/tree-sitter/tree-sitter-json")
          (make "https://github.com/alemuller/tree-sitter-make")
          (markdown "https://github.com/ikatyang/tree-sitter-markdown")
          (python "https://github.com/tree-sitter/tree-sitter-python")
          (toml "https://github.com/tree-sitter/tree-sitter-toml")
          (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
          (yaml "https://github.com/ikatyang/tree-sitter-yaml"))))


(use-package treesit-auto
  :config
  (global-treesit-auto-mode))

;;   ____
;;  / ___| _     _
;; | |   _| |_ _| |_
;; | |__|_   _|_   _|
;;  \____||_|   |_|

;;   ___  __  __ _   _     _____
;;  / _ \|  \/  | \ | | __|_   _|_     _
;; | | | | |\/| |  \| |/ _ \| |_| |_ _| |_
;; | |_| | |  | | |\  |  __/| |_   _|_   _|
;;  \___/|_|  |_|_| \_|\___||_| |_|   |_|
(setenv "PATH" (concat (expand-file-name "~/apps/omnetpp-5.7/bin:") (getenv "PATH")))
;;      _                                _       _
;;     | | __ ___   ____ _ ___  ___ _ __(_)_ __ | |_
;;  _  | |/ _` \ \ / / _` / __|/ __| '__| | '_ \| __|
;; | |_| | (_| |\ V / (_| \__ \ (__| |  | | |_) | |_
;;  \___/ \__,_| \_/ \__,_|___/\___|_|  |_| .__/ \__|
;;                                        |_|
(use-package js2-mode
  :hook
  (js-mode . js2-minor-mode))


(use-package yaml-mode
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))


(use-package plantuml-mode
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))
  :config
  (setq plantuml-default-exec-mode 'jar)
  (setq plantuml-jar-path "/usr/share/java/plantuml/plantuml.jar"))

;;  ____        _   _
;; |  _ \ _   _| |_| |__   ___  _ __
;; | |_) | | | | __| '_ \ / _ \| '_ \
;; |  __/| |_| | |_| | | | (_) | | | |
;; |_|    \__, |\__|_| |_|\___/|_| |_|
;;        |___/

;; need a auto completion engine as a fallback for when eglot chokes (not in a project or buffer not visiting a file)
(use-package anaconda-mode
  :hook
  (python-mode . anaconda-mode)
  (python-mode . anaconda-eldoc-mode)
  (python-ts-mode . anaconda-mode)
  (python-ts-mode . anaconda-eldoc-mode))

(use-package company-anaconda
  :after (eglot company)
  :config
  (add-to-list 'company-backends '(company-anaconda :with company-capf))
  (add-hook 'eglot-managed-mode-hook
            #'(lambda ()
                (when (and (member major-mode '(python-mode python-ts-mode))
                           (eglot-managed-p))
                  (anaconda-mode -1)
                  (anaconda-eldoc-mode -1)))))

(use-package vimrc-mode
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.vim\\(rc\\)?\\'" . vimrc-mode)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
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
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

