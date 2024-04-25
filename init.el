(setq gc-cons-threshold 50000000
      garbage-collection-messages t)

(add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list 'load-path "~/.emacs.d/lisp/evil-plugins/")
(add-to-list 'load-path "~/.emacs.d/lisp/highlight-symbol/")
(add-to-list 'load-path "~/.emacs.d/lisp/dpaste.el")
(add-to-list 'load-path "~/.emacs.d/lisp/ned-mode")
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


(use-package ag)
(use-package aggressive-indent)
(use-package tex :ensure auctex)
(use-package auctex-latexmk)
(use-package bfbuilder)
(use-package better-shell)
(use-package counsel-projectile)
(use-package diminish)
(use-package dtrt-indent)
(use-package evil)
(use-package expand-region)
(use-package fancy-battery)
(use-package flycheck)
(use-package flyspell-lazy)
(use-package flx)
(use-package guide-key)
(use-package ggtags)
(use-package ivy)
(use-package impatient-mode)
(use-package isend-mode)
(use-package js2-mode)
(use-package key-chord)
(use-package langtool)
(use-package latex-preview-pane)
(use-package less-css-mode)
(use-package linum-relative)
;; (use-package lua-mode)
(use-package magit)
(load-library "ned-mode")
(use-package ox-reveal)
(use-package php-mode)
(use-package powerline)
(use-package projectile)
;; (use-package quasi-monochrome-theme)
(use-package spaceline)
(use-package shell-switcher)
(use-package sublimity)
;; (use-package tao-theme)
(use-package undo-tree)
(use-package use-package)
(use-package volatile-highlights)
(use-package web-mode)
(use-package xcscope)
(use-package yaml-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))
(use-package yasnippet)


(defun replace-hook (hook fun)
  (remove-hook hook fun)
  (add-hook hook fun))

(global-undo-tree-mode)

;; (require 'framemove)
;; (setq framemove-hook-into-windmove t)
(require 'ace-link)
(ace-link-setup-default)

(require 'elec-pair)
(electric-pair-mode t)
(show-paren-mode 1)
(setq show-paren-delay 0)

;; (require 'linum-relative)
;; (global-linum-mode t)
;; (linum-relative-on)
;; (setq linum-relative-current-symbol "")

(require 'nlinum-relative)
(nlinum-relative-setup-evil)                    ;; setup for evil
;; (global-nlinum-relative-mode t)
(setq nlinum-relative-redisplay-delay 0.2)      ;; delay
(setq nlinum-relative-current-symbol "")      ;; or "" for display current line number
(setq nlinum-relative-offset 0)                 ;; 1 if you want 0, 2, 3...
(add-hook 'prog-mode-hook
          (lambda () (nlinum-mode t)
            (nlinum-relative-on)))
(add-hook 'text-mode-hook
          (lambda () (nlinum-mode t)
            (nlinum-relative-on)))

(blink-cursor-mode 0)
(require 'hl-line+)
(set-face-background 'hl-line "#FFFAE6")
(global-hl-line-mode t)


(require 'avy)
(setq avy-all-windows t)
(setq avy-keys-alist `((avy-goto-line . ,(append (number-sequence ?a ?z) (number-sequence ?0 ?9)))))
(setq avy-keys '(?a ?o ?e ?u ?h ?t ?n ?s ?c ?r ?j ?m))
  "List of left bracket chars.")
(defvar xah-right-brackets '(")" "]" "}" ">" "\"" "'")
  "list of right bracket chars.")

(defun xah-backward-left-bracket ()
  "Move cursor to the previous occurrence of left bracket.
The list of brackets to jump to is defined by `xah-left-brackets'.
URL `http://ergoemacs.org/emacs/emacs_navigating_keys_for_brackets.html'
Version 2015-10-01"
  (interactive)
  (message "%s" (regexp-opt xah-left-brackets))
  (search-backward-regexp (regexp-opt xah-left-brackets) nil t))

(defun xah-forward-right-bracket ()
  "Move cursor to the next occurrence of right bracket.
The list of brackets to jump to is defined by `xah-right-brackets'.
URL `http://ergoemacs.org/emacs/emacs_navigating_keys_for_brackets.html'
Version 2015-10-01"
  (interactive)
  (search-forward-regexp (regexp-opt xah-right-brackets) nil t))

;; (require 'key-chord)
;; (key-chord-mode t)
;; (key-chord-define evil-insert-state-map "hj" 'xah-forward-right-bracket)
;; (key-chord-define evil-insert-state-map "hk" 'xah-backward-left-bracket)
;; (key-chord-define evil-insert-state-map "tj" 'yas-expand) ; tj tk are also good key-chord


;; disable zap-to-char key
(global-set-key (kbd "M-z") nil)

(require 'god-mode)
(global-set-key (kbd "<escape>") #'god-local-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              COMPANY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package company
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (global-set-key (kbd "M-t") 'company-yasnippet)
  (setq company-dabbrev-downcase nil)
  (setq company-minimum-prefix-length 2)
  (setq company-idle-delay 0.05)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GUI tweaks
(setq echo-keystrokes 0.0001)
(setq minibuffer-prompt-properties '(readonly t point-entered minibuffer-avoid-prompt face minibuffer-prompt))

(setq frame-title-format
      '("" invocation-name ": " (:eval (replace-regexp-in-string
                                        "^ +" "" (buffer-name)))))
(setq mouse-yank-at-point t)

(defun rmrf/setup-frame (&optional frame)
  (tool-bar-mode -1)
  (toggle-scroll-bar -1)
  (menu-bar-mode -1)
  (modify-frame-parameters frame
                           '((vertical-scroll-bars . nil)
                             (horizontal-scroll-bars . nil)))
  (set-frame-font (font-spec :family "DejaVu Sans Mono" :size 10.0) 1 frame t)
  (setq frame-resize-pixelwise t)
  ;; (set-background-color "gainsboro")
  )

;; (set-frame-font (font-spec :family "DejaVu Sans Mono" :size 10.0))
(rmrf/setup-frame)
(replace-hook 'after-make-frame-functions 'rmrf/setup-frame)
(set-face-foreground 'default "#242424") ; black is too contrasty so make it a little gray
(set-background-color "#F5FAF4")

(setq browse-url-generic-program "google-chrome-stable")
(setq browse-url-browser-function 'browse-url-generic)
;; prevent async output buffer to show up
(add-to-list 'display-buffer-alist (cons "\\*Async Shell Command\\*.*" (cons #'display-buffer-no-window nil)))

;; save last edit position
(require 'saveplace)
(save-place-mode t)
(setq save-place-forget-unreadable-files nil)

(setq uniquify-buffer-name-style 'post-forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)    ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers
(require 'uniquify)

;; highlight cursor when jump
;; (beacon-mode t)
;; (setq beacon-color "#666600")

(setq scroll-margin 3
      scroll-conservatively 9999 ;; seems to be not working with sublimity
      scroll-preserve-screen-position t)

(setq truncate-lines nil)
;; don't suspend emacs accidentally
(global-set-key "\C-x\C-z" nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TAB AND INDENT SETUP
(setq-default indent-tabs-mode nil)
;; set default tab char's display width to 4 spaces
(setq-default tab-width 4) ; emacs 23.1, 24.2, default to 8
(setq backward-delete-char-untabify-method 'hungry)
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)

;; for auto detecting indentation
(require 'dtrt-indent)
(dtrt-indent-mode t)

;; make tab key call indent command or insert tab character, depending on cursor position
(setq-default tab-always-indent nil)

(when (fboundp 'winner-mode)
  (winner-mode 1))

(global-set-key (kbd "M-N") 'winner-redo)
(global-set-key (kbd "M-P") 'winner-undo)

(require 'repeat)
(defun make-repeatable-command (cmd)
  "Returns a new command that is a repeatable version of CMD.
The new command is named CMD-repeat.  CMD should be a quoted
command.
This allows you to bind the command to a compound keystroke and
repeat it with just the final key.  For example:
  (global-set-key (kbd \"C-c a\") (make-repeatable-command 'foo))
will create a new command called foo-repeat.  Typing C-c a will
just invoke foo.  Typing C-c a a a will invoke foo three times,
and so on.
See related discussion here:
http://batsov.com/articles/2012/03/08/emacs-tip-number-4-repeat-last-command/#comment-459843643
https://groups.google.com/forum/?hl=en&fromgroups=#!topic/gnu.emacs.help/RHKP2gjx7I8"
  (fset (intern (concat (symbol-name cmd) "-repeat"))
        `(lambda ,(help-function-arglist cmd) ;; arg list
           ,(format "A repeatable version of `%s'." (symbol-name cmd)) ;; doc string
           ,(interactive-form cmd) ;; interactive form
           ;; see also repeat-message-function
           (setq last-repeatable-command ',cmd)
           (repeat nil)))
  (intern (concat (symbol-name cmd) "-repeat")))

(defun vsplit-last-buffer ()
  (interactive)
  (split-window-vertically)
  (other-window 1 nil)
  (switch-to-next-buffer)
  (other-window -1 nil))

(defun hsplit-last-buffer ()
  (interactive)
  (split-window-horizontally)
  (other-window 1 nil)
  (switch-to-next-buffer)
  (other-window -1 nil))

(global-set-key (kbd "C-x o") (make-repeatable-command 'other-window))
;; (global-set-key (kbd "C-<tab>") 'other-window)
;; (global-set-key (kbd "C-'") 'other-window)
;; (global-set-key (kbd "C-q") 'quoted-insert)
(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "C-1") 'delete-other-windows)
(global-set-key (kbd "C-2") 'split-window-below)
(global-set-key (kbd "C-3") 'hsplit-last-buffer)
(global-set-key (kbd "C-0") 'delete-window)
(global-set-key (kbd "C-=") 'balance-windows)
(global-set-key (kbd "C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-<down>") 'shrink-window)
(global-set-key (kbd "C-<up>") 'enlarge-window)
(global-set-key (kbd "C-c g") 'ag-regexp)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IBUFFER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package ibuffer-tramp
  :config
  (add-hook 'ibuffer-hook
            (lambda ()
              (ibuffer-tramp-set-filter-groups-by-tramp-connection)
              (ibuffer-do-sort-by-alphabetic))))

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
  ;; (setq eyebrowse-keymap-prefix [?\C-;])
  (eyebrowse-mode t))

;; (require 'helm-config)
;; (helm-mode 1)
;; (global-set-key (kbd "M-x") 'helm-M-x)
;; (global-set-key (kbd "C-x C-f") 'helm-find-files)
;; (setq helm-M-x-fuzzy-match 't)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              IVY COUNSEL SWIPER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'ivy)
(require 'counsel)
(ivy-mode t)

;; on emacs 25 maybe should change this to string-collate-lessp
(add-to-list 'ivy-sort-functions-alist '(read-file-name-internal . string-lessp))

(setq ivy-re-builders-alist
      '((t . ivy--regex-plus)))

(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)

(use-package js2-mode
  :config
  (add-hook 'js-mode-hook 'js2-minor-mode)
)

(require 'txl-mode)
(add-to-list 'auto-mode-alist '("\\.\\([tT]xl\\|[gG]rm\\|[gG]rammar\\|[rR]ul\\(es\\)?\\|[mM]od\\(ule\\)?\\)$" . txl-mode))
(define-key txl-mode-map (kbd "<backspace>") 'backward-delete-char-untabify)
(define-key txl-mode-map (kbd "C-c r") nil)
(define-key txl-mode-map (kbd "C-c c") nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              AUTO COMPLETE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (require 'ac-dabbrev)
(require 'dabbrev)
(setq dabbrev-abbrev-char-regexp "\\sw\\|\\s_\\|\\s.")
(setq dabbrev-case-fold-search nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              GUIDE KEY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'guide-key)
(setq guide-key/guide-key-sequence t)
(setq guide-key/popup-window-position 'bottom)
(guide-key-mode 1)  ; Enable guide-key-mode

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(require 'fcitx)
(fcitx-default-setup)
(setq fcitx-use-dbus t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              PYTHON
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package anaconda-mode
  :config
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'anaconda-eldoc-mode))

(use-package company-anaconda
  :config
  (add-to-list 'company-backends 'company-anaconda))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              TRAMP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              EMMS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'emms-setup)
(emms-all)
(emms-default-players)
(setq emms-source-file-default-directory "~/Music/")

(defun my-emms-info-track-description (track)
  " http://bbs.chinaunix.net/thread-2007327-1-1.html
Return a description of the current track.  Overwrite the
official one in emms.el to return filename only (no path)
--lgfang"
  (let ((artist (emms-track-get track 'info-artist))
        (title (emms-track-get track 'info-title)))
    (if (and artist title)
        (format "%s - %s" artist title)
      (if (eq 'file (emms-track-type track))
          (file-name-nondirectory (emms-track-name track))
        (concat (symbol-name (emms-track-type track))
                ": " (emms-track-name track)))
      )))

(setq emms-track-description-function 'my-emms-info-track-description)

(require 'emms-history)
(emms-history-load)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              ORGMODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package cdlatex)
(require 'org)
(require 'org-agenda)
(require 'ox-reveal)
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


;; PDFs visited in Org-mode are opened in Okular (and not in the default choice) http://stackoverflow.com/a/8836108/789593
(defun rmrf/org-mode-hook ()
  (delete '("\\.pdf\\'" . default) org-file-apps)
  (add-to-list 'org-file-apps '("\\.pdf\\'" . "okular %s"))
  (setq word-wrap t)
  (turn-on-org-cdlatex)
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0)))

(replace-hook 'org-mode-hook 'rmrf/org-mode-hook)

(require 'ob-latex)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((org . t)
   (latex . t)))
;; (org-babel-do-load-languages
;;  'org-babel-load-languages
;;  '((emacs-lisp . t)
;;    (python . t)
;;    (ditaa . t)
;;    (dot . t)
;;    (plantuml . t)
;;    (gnuplot . t)
;;    (sh . t)
;;    (org . t)
;;    (latex . t)))
;; use imagemagick to preview latex
(setq org-latex-create-formula-image-program 'imagemagick)
;; set up tikz as one of the default packages for LaTeX
(setq org-latex-packages-alist
      (quote (("" "color" t)
              ("" "minted" t)
              ("" "parskip" t)
              ("" "tikz" t))))
(require 'ox-md)
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
;;                              ANSI-TERM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'term)
(defun expose-global-binding-in-term (binding)
  (define-key term-raw-map binding
    (lookup-key (current-global-map) binding)))

;; expose WindowsMode binding
(expose-global-binding-in-term (kbd "M-o"))

(add-hook 'term-mode-hook
          (lambda ()
            (term-pager-enable)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EMACS DAEMON
;; Allow access from emacsclient
;;----------------------------------------------------------------------------
(require 'server)
(unless (server-running-p)
  (server-start))

(require 'cl)
(require 'cl-macs)
(require 'dwim-compile)
(global-set-key [(control ?c) ?c] 'dwim-c/compile)
(setq compilation-finish-functions nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              DIRED
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'dired)
(setq dired-listing-switches "-lah")
(setq delete-by-moving-to-trash t)
(setq dired-dwim-target t)
(use-package dired-efap
  :config
  (define-key dired-mode-map (kbd "r") 'dired-efap))

(use-package dired-open
  :config
  (define-key dired-mode-map (kbd "<S-return>") 'dired-open-xdg))

(use-package terminal-here
  :config
  (progn
    (setq terminal-here-terminal-command '("gnome-terminal")
          terminal-here-command-flag "--")
    (global-set-key (kbd "C-;") 'terminal-here-launch)))

(use-package stripe-buffer
  :config
  (add-hook 'dired-mode-hook 'turn-on-stripe-buffer-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              YASNIPPET
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'yasnippet)

(yas-global-mode 1)
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
;; https://emacs.stackexchange.com/questions/38242/problem-redoing-with-yasnippet
(setq yas-snippet-revival nil)
;; (define-key yas-minor-mode-map (kbd "C-/") 'yas-expand)
;; (define-key ac-completing-map "\t" 'yas-expand)
;; (define-key ac-completing-map [tab] 'yas-expand)

;; (defun my-smart-fallback ()
;;   (interactive)
;;   (message "hohoho")
;;   (print ac-menu)
;;   (print ac-candidate-menu-min)
;;   (if t
;;       (progn (call-interactively 'ac-complete)
;;              (message "hahaha"))
;;     (progn
;;       (setq yas-fallback-behavior 'call-other-command)
;;       (yas--fallback)
;;       (setq yas-fallback-behavior '(apply my-smart-fallback)))))

;; (setq yas-fallback-behavior '(apply my-smart-fallback))

(use-package yasnippet-snippets)

(require 'php-mode)
;; (add-hook 'php-mode-hook
;;           '(lambda ()
;;              (auto-complete-mode t)
;;              (require 'ac-php)
;;              (setq ac-sources  '(ac-source-words-in-buffer ac-source-php))) )

;; (define-key php-mode-map  (kbd "C-]") 'ac-php-find-symbol-at-point)   ;goto define
;; (define-key php-mode-map  (kbd "C-t") 'ac-php-location-stack-back   ) ;go back

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

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


(require 'skewer-mode)
(add-hook 'js2-mode-hook 'skewer-mode)
(add-hook 'css-mode-hook 'skewer-css-mode)
(setq httpd-port 8089)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              FREE-KEYS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package free-keys)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              LUA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (require 'lua-mode)

;; (defvar love2d-program "love")

;; (defun love2d-launch-current ()
;;   (interactive)
;;   (let ((app-root (locate-dominating-file (buffer-file-name) "main.lua")))
;;     (if app-root
;;         (shell-command (format "%s %s &" love2d-program app-root))
;;       (error "main.lua not found"))))

;; (defun get-love2d-approot ()
;;   (locate-dominating-file (buffer-file-name) "main.lua"))

;;                                         ;(remove-hook 'lua-mode-hook (car lua-mode-hook))
;; (add-hook 'lua-mode-hook
;;           (lambda ()
;;             (let ((approot (get-love2d-approot)))
;;               (if approot
;;                   (progn
;;                     (set (make-local-variable 'compile-command) (format "%s %s" love2d-program approot))
;;                     (add-to-list 'compilation-error-regexp-alist 'love t)
;;                     (add-to-list 'compilation-error-regexp-alist-alist
;;                                  '(love "^Error: Syntax error: \\(.*?\\):\\([0-9]+\\):.*$" 1 2) t))
;;                 (set (make-local-variable 'compile-command)
;;                      (concat "lua " (file-name-nondirectory buffer-file-name)))))))

;; (setq shell-switcher-mode t)
;; (require 'shell-switcher)
;; (shell-switcher-mode t)
;; (require 'better-shell)
;; (global-set-key (kbd "C-;") 'better-shell-shell)

(require 'bfbuilder)
(add-to-list 'auto-mode-alist '("\\.bf$" . bfbuilder-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              PROJECTILE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'ag)
(require 'projectile)
(require 'counsel-projectile)
(setq projectile-mode-line
      '(:eval
        (if
            (file-remote-p default-directory)
            " Projectile"
          (format " P[%s]"
                  (projectile-project-name)))))
;; https://github.com/bbatsov/projectile/issues/835
;;(projectile-global-mode t)
(add-hook 'text-mode-hook 'projectile-mode)
(counsel-projectile-mode)
(setq projectile-enable-caching t)


(require 'recentf)
(setq recentf-auto-cleanup 'never) ;; disable before we start recentf!
(setq recentf-max-menu-items 200)
(setq recentf-save-file (concat recentf-save-file "-" (system-name)))
(recentf-mode 1)
(global-set-key "\C-x\ \C-r" 'counsel-recentf)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              GGTAGS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'ggtags)
(define-key evil-normal-state-map (kbd "C-]") 'ggtags-find-tag-dwim)
(setq ggtags-global-abbreviate-filename nil)
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
(global-set-key (kbd "C-x g") 'magit-status)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              DIFF HL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'diff-hl)
(global-diff-hl-mode t)
(diff-hl-flydiff-mode t)

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
(require 'flycheck)
;; (global-flycheck-mode t)
(setq flycheck-mode-line-prefix "F")

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              VOLATILE HIGHLIGHTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (volatile-highlights-mode t)
;; (vhl/define-extension 'evil 'evil-paste-after 'evil-paste-before 'evil-yank
;;                       'evil-paste-pop 'evil-move)
;; (vhl/install-extension 'evil)


(require 'zeal-at-point)
(global-set-key "\C-cd" 'zeal-at-point)
(add-hook 'projectile-rails-mode-hook
          (lambda () (setq zeal-at-point-docset "ruby,rails")))
(add-to-list 'zeal-at-point-mode-alist '(c++-mode . ""))

(require 'projectile-rails)
;; (projectile-rails-global-mode t)


;; ;; NOTE issue with broken char display
;; (use-package highlight-indent-guides
;;   :config
;;   (setq highlight-indent-guides-method 'character)
;;   (setq highlight-indent-guides-auto-character-face-perc 9)

;;   (defun hide-first-level-highlighter (level responsive display)
;;     (if (> 1 level) ; replace `1' with the number of guides you want to hide
;;         nil
;;         (highlight-indent-guides--highlighter-default level responsive display)))

;;   (setq highlight-indent-guides-highlighter-function 'hide-first-level-highlighter)

;;   (add-hook 'prog-mode-hook 'highlight-indent-guides-mode))

;; (use-package keyfreq
;;   :init
;;   (keyfreq-mode 1)
;;   (keyfreq-autosave-mode 1))

(use-package highlight-indentation
  :config
  (add-hook 'prog-mode-hook 'highlight-indentation-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              ROTATE TEXT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar rotate-text-rotations
  '(("true" "false")
    ("True" "False")
    ("0" "1")
    ("yes" "no")
    ("On" "Off"))
  "List of text rotation sets.")

(defun rotate-region (beg end)
  "Rotate all matches in `rotate-text-rotations' between point and mark."
  (interactive "r")
  (let ((regexp (rotate-convert-rotations-to-regexp
                 rotate-text-rotations))
        (end-mark (copy-marker end)))
    (save-excursion
      (goto-char beg)
      (while (re-search-forward regexp (marker-position end-mark) t)
        (let* ((found (match-string 0))
               (replace (rotate-next found)))
          (replace-match replace))))))

(defun rotate-string (string &optional rotations)
  "Rotate all matches in STRING using associations in ROTATIONS.
If ROTATIONS are not given it defaults to `rotate-text-rotations'."
  (let ((regexp (rotate-convert-rotations-to-regexp
                 (or rotations rotate-text-rotations)))
        (start 0))
    (while (string-match regexp string start)
      (let* ((found (match-string 0 string))
             (replace (rotate-next
                       found
                       (or rotations rotate-text-rotations))))
        (setq start (+ (match-end 0)
                       (- (length replace) (length found))))
        (setq string (replace-match replace nil t string))))
    string))

(defun rotate-next (string &optional rotations)
  "Return the next element after STRING in ROTATIONS."
  (let ((rots (rotate-get-rotations-for
               string
               (or rotations rotate-text-rotations))))
    (if (> (length rots) 1)
        (error (format "Ambiguous rotation for %s" string))
      (if (< (length rots) 1)
          ;; If we get this far, this should not occur:
          (error (format "Unknown rotation for %s" string))
        (let ((occurs-in-rots (member string (car rots))))
          (if (null occurs-in-rots)
              ;; If we get this far, this should *never* occur:
              (error (format "Unknown rotation for %s" string))
            (if (null (cdr occurs-in-rots))
                (caar rots)
              (cadr occurs-in-rots))))))))

(defun rotate-get-rotations-for (string &optional rotations)
  "Return the string rotations for STRING in ROTATIONS."
  (remq nil (mapcar (lambda (rot) (if (member string rot) rot))
                    (or rotations rotate-text-rotations))))

(defun rotate-convert-rotations-to-regexp (rotations)
  (regexp-opt (rotate-flatten-list rotations)))

(defun rotate-flatten-list (list-of-lists)
  "Flatten LIST-OF-LISTS to a single list.
Example:
  (rotate-flatten-list '((a b c) (1 ((2 3)))))
    => (a b c 1 2 3)"
  (if (null list-of-lists)
      list-of-lists
    (if (listp list-of-lists)
        (append (rotate-flatten-list (car list-of-lists))
                (rotate-flatten-list (cdr list-of-lists)))
      (list list-of-lists))))

(defun rotate-word-at-point ()
  "Rotate word at point based on sets in `rotate-text-rotations'."
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'word))
        (opoint (point)))
    (when (consp bounds)
      (let ((beg (car bounds))
            (end (copy-marker (cdr bounds))))
        (rotate-region beg end)
        (goto-char (if (> opoint end) end opoint))))))

(defun indent-or-rotate ()
  "If point is at end of a word, then else indent the line."
  (interactive)
  (if (looking-at "\\>")
      (rotate-region (save-excursion (forward-word -1) (point))
                     (point))
    (indent-for-tab-command)))

;; (local-set-key [tab] 'indent-or-rotate)

(require 'dockerfile-mode)

;; (require 'bgex)
;; ;; color
;; (when (boundp 'bgex-exist-p)
;;   (bgex-set-image-default "/home/rmrf/Pictures/tiledbackground.jpg"))

(use-package pdf-tools
  :config
  (pdf-tools-install)
  (setq pdf-view-midnight-colors '("black" . "wheat"))
  (defun rmrf/pdf-view-setup ()
    (pdf-view-midnight-minor-mode t))
  (replace-hook 'pdf-view-mode-hook 'rmrf/pdf-view-setup))

(add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)

(use-package interleave
  :config
  (setq interleave-disable-narrowing t))

(use-package ace-window
  :config
  (global-set-key (kbd "M-o") 'ace-window)
  (setq aw-keys '(?h ?t ?n ?s ?a ?o ?e ?u)
        aw-scope 'frame))


(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (c-mode . lsp)
         (c++-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              PASTE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'dpaste)
(setq dpaste-poster "Bao Ngoc")
(setq dpaste-expiry-days 1)

(defun paste ()
  "Simple function to skip typing dpaste title every time."
  (interactive)
  (dpaste-region-or-buffer "Undefined"))

(use-package dumb-jump
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g M-g" . dumb-jump-go)
         ("M-g b" . dumb-jump-back)
         ("M-g i" . dumb-jump-go-prompt)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :config (setq dumb-jump-selector 'ivy) ;; (setq dumb-jump-selector 'helm)
  :ensure)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              ATOMIC CHROME
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (use-package atomic-chrome
;;   :demand t
;;   :straight (atomic-chrome
;;              :repo "KarimAziev/atomic-chrome"
;;              :type git
;;              :host github)
;;   :commands (atomic-chrome-start-server)
;;   :config
;;   (setq-default atomic-chrome-extension-type-list '(atomic-chrome))
;;   (atomic-chrome-start-server))
(use-package atomic-chrome
  :config
  (setq atomic-chrome-buffer-open-style 'full)
  (setq atomic-chrome-url-major-mode-alist
        '((".*\\.wix\\.com" . js-mode)
          ("leetcode\\.com" . python-mode)))
  (atomic-chrome-start-server))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              LATEX AUCTEX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              POPWIN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (use-package popwin
;;   :config
;;   (popwin-mode 1)
;;   (setq display-buffer-function 'popwin:display-buffer)
;;   (push '("^CAPTURE-.+\*.org$" :regexp t) popwin:special-display-config)
;;   (push '("*Org Select*") popwin:special-display-config)

;;   ;; undo-tree
;;   (push '(" *undo-tree*" :width 0.3 :position right) popwin:special-display-config)
;;   )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              OMNET++
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setenv "PATH" (concat (expand-file-name "~/apps/omnetpp-5.7/bin:") (getenv "PATH")))


;; C++
(require 'member-functions)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              PlantUML
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package plantuml-mode
  :config
  (setq plantuml-default-exec-mode 'jar)
  (setq plantuml-jar-path "/usr/share/java/plantuml/plantuml.jar")
  (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode)))


(require 'netlogo-mode)

(require 'diminish)
(diminish 'evil-snipe-local-mode)
(diminish 'guide-key-mode)
(diminish 'ivy-mode)
(diminish 'undo-tree-mode)
(diminish 'auto-revert-mode)
(diminish 'abbrev-mode)
(diminish 'dtrt-indent-mode)
(diminish 'beacon-mode)
(diminish 'volatile-highlights-mode)
(diminish 'flycheck-mode)
(diminish 'evil-goggles-mode)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("99c86852decaeb0c6f51ce8bd46e4906a4f28ab4c5b201bdc3fdf85b24f88518" default))
 '(package-selected-packages
   '(highlight-indentation-ome company-anaconda anaconda-mode anaconda evil-args lsp-mode netlogo-mode ned-mode gradle-mode meghanada monokai-theme pdf-tools ensime terminal-here shackle dired-efap smooth-scroll cdlatex imenu-list ace-window interleave ztree zenburn-theme zeal-at-point yasnippet-snippets yard-mode yaml-mode web-mode volatile-highlights use-package tao-theme tango-plus-theme sublimity start-menu spacemacs-theme spaceline skewer-mode shell-switcher robe rainbow-mode quasi-monochrome-theme psysh projectile-rails powerline-evil plan9-theme parent-mode ox-reveal openwith nlinum-relative multi-term moe-theme minimap minimal-theme markdown-mode magit linum-relative leuven-theme less-css-mode ldap-mode latex-preview-pane langtool kite-mini key-chord json-mode jedi isend-mode impatient-mode ibuffer-tramp highlight-tail guide-key graphviz-dot-mode glsl-mode git-gutter ggtags geben fuzzy free-keys frames-only-mode framemove flyspell-lazy flymake-php flymake-lua flycheck flx flatui-theme fcitx fancy-battery eyebrowse exwm-x expand-region exec-path-from-shell evil-visualstar evil-surround evil-snipe evil-smartparens evil-numbers evil-nerd-commenter evil-multiedit evil-mc evil-matchit evil-leader evil-goggles evil-god-state evil-easymotion evil-commentary evil-avy evil-anzu enh-ruby-mode emojify emms elfeed dumb-jump dtrt-indent dockerfile-mode dmenu dired-open diminish diff-hl counsel-projectile color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized bfbuilder better-shell beacon base16-theme auto-complete-pcmp auto-complete-exuberant-ctags auto-complete-auctex auctex-lua auctex-latexmk anti-zenburn-theme ample-zen-theme ample-theme airline-themes aggressive-indent ag ace-link ace-jump-mode ac-php ac-inf-ruby ac-html-csswatcher ac-html-bootstrap ac-html ac-emmet ac-dabbrev 0blayout))
 '(safe-local-variable-values
   '((eval setenv "PATH"
           (concat ":"
                   (expand-file-name "~/apps/omnetpp-5.7/bin")
                   (getenv "PATH")))
     (eval setenv "PATH"
           (concat
            (expand-file-name ":~/apps/omnetpp-5.7/bin")
            (getenv "PATH")))
     (eval setenv "PATH"
           (concat
            (expand-file-name "~/apps/omnetpp-5.7/bin")
            (getenv "PATH")))
     (eval setq flycheck-clang-include-path
           (list
            (expand-file-name "~/apps/omnetpp-5.7/include")
            (expand-file-name "~/builds/comm/include")
            (expand-file-name "~/builds/comm/src/message")))
     (eval setq cc-search-directories
           (list "."
                 (expand-file-name "~/builds/comm/include")
                 (expand-file-name "~/builds/comm/src")
                 (expand-file-name "~/builds/comm/src/**")
                 "/usr/include" "/usr/local/include/*"))
     (eval setq flycheck-clang-include-path
           (list
            (expand-file-name "~/apps/omnetpp-5.6.2/include")
            (expand-file-name "~/builds/comm/include")
            (expand-file-name "~/builds/comm/src/message")))
     (eval setq flycheck-clang-include-path
           (list
            (expand-file-name "~/apps/omnetpp-5.6.2/include")
            (expand-file-name "~/builds/comm/include")
            "../src/message"))
     (eval setq flycheck-clang-include-path
           (list
            (expand-file-name "~/apps/omnetpp-5.6.2/include")
            "../src/message"))
     (eval setq flycheck-clang-include-path
           (list
            (expand-file-name "~/apps/omnetpp-5.6.2/include")
            "../message"))
     (eval setq flycheck-clang-include-path
           (list
            (expand-file-name "~/apps/omnetpp-5.6.2/include")))
     (cc-search-directories "." "/home/rmrf/builds/comm/include" "/home/rmrf/builds/comm/src" "/home/rmrf/builds/comm/src/**" "/usr/include" "/usr/local/include/*")
     (cc-search-directories "." "/home/rmrf/builds/comm/include" "/usr/include" "/usr/local/include/*")
     (eval setq flycheck-clang-include-path
           (list
            (expand-file-name "/home/rmrf/apps/omnetpp-5.6.2/include")))
     (eval setq flycheck-clang-include-path
           (list
            (expand-file-name "/home/rmrf/apps/omnetpp-5.6.2")))
     (eval setq flycheck-clang-include-path
           (list
            (expand-file-name "/opt/omnetpp/include/"))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'narrow-to-region 'disabled nil)
