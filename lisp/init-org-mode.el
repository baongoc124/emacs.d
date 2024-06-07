(require 'init-calendar)
(require 'org)
(require 'org-agenda)

(keymap-unset org-mode-map "C-'") ; unbind because I use this for quote
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cr" 'org-capture)
(global-set-key "\C-cb" 'org-switchb)
(setq org-startup-truncated nil)
(setq org-directory "~/Dropbox/org/gtd")
(setq org-mobile-directory "~/Dropbox/org/mobileorg")
;; (setq org-tag-persistent-alist '((:startgroup . nil)
;;                                  ("home" . ?h)
;;                                  ("office" . ?o)
;;                                  (:endgroup . nil)))
(setq org-tag-persistent-alist '(("home" . ?h)
                                 ("office" . ?o)
                                 ("VN" . ?v)))
(defun org-file-path (filename)
  "Return the absolute address of an org file, given its relative name."
  (concat (file-name-as-directory org-directory) filename))

(setq org-link-abbrev-alist '(("gmail" . "https://mail.google.com/mail/u/0/#search/rfc822msgid%3A%h")))


(setq org-agenda-span 3)
(setq org-agenda-start-on-weekday 1)

;; set agenda files to subdirectories of org-directory, recursively
(require 'grep)
(setq org-agenda-files
      (split-string (shell-command-to-string
                     (concat find-program
                             " "
                             (shell-quote-argument (expand-file-name
                                                    org-directory))
                             " -type d -print0"))
                    "\0"
                    t))


(defun ngoc/org-agenda-dim-event-lines ()
  (interactive)
  ;; Dim lines that are in `event' category in the agenda.
  (highlight-lines-matching-regexp "^[[:space:]]*event\\b" 'font-lock-comment-face)
  ;; Dim D-priority tasks
  (highlight-regexp "\\[#C\\].*$" 'font-lock-comment-face)
  ;; Dim prewarning deadlines
  (highlight-regexp "[[:space:]]+[[:digit:]]+ >>[[:space:]]+[A-Z]+ \\(.*$\\)" 'font-lock-comment-face 1)
  ;; Hide label for habits
  (highlight-regexp "[[:space:]]+habits\\b" 'ngoc/invisible-face))



(add-hook 'org-agenda-finalize-hook #'ngoc/org-agenda-dim-event-lines)

(setq org-agenda-scheduled-leaders '("      " "%2d  ! "))
(setq org-agenda-deadline-leaders '("    X " "%2d >> " "%2d  X "))

(defun ngoc/org-agenda-space ()
  "Add spaces to align agenda items without scheduled, deadline, or habits"
  ;; check if an item is a habit
  (let ((habitp (org-is-habit-p))
        (scheduled (org-get-scheduled-time (point)))
        (deadline (org-get-deadline-time (point))))
    (if (or habitp (and (not scheduled) (not deadline)))
        "       "
      "")))

(setq org-agenda-prefix-format
      '((agenda . " %i %-12c%-5t % s%(ngoc/org-agenda-space)")
        (todo . " %i %-12:c")
        (tags . " %i %-12:c")
        (search . " %i %-12:c")))

(setq org-agenda-format-date " %^a      %e %b")

(setq org-agenda-sorting-strategy
      '(time-up todo-state-down habit-down timestamp-up
                priority-down category-keep))

(setq org-agenda-time-grid
      '((daily today require-timed remove-match)
        (800 1200 1800 2200)
        ""
        "----------------"))

;; autosave
(add-hook 'auto-save-hook 'org-save-all-org-buffers)
(add-hook 'org-agenda-mode-hook #'auto-save-mode)
(setq auto-save-interval 3000)
(setq auto-save-timeout 11)


(setq org-agenda-custom-commands nil)
(setq org-agenda-custom-commands
      '(("d" . "Personal prefix")
        ("dd" "" ((agenda "" ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                             (org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled)))
                 (agenda "" ((org-agenda-overriding-header "")
                             (org-agenda-format-date "Yay!!!")
                             (org-agenda-span 'day)
                             (org-habit-show-habits nil)
                             (org-agenda-use-time-grid nil)
                             (org-agenda-show-log t)
                             (org-agenda-log-mode-items '(closed clock state))
                             (org-agenda-skip-function '(org-agenda-skip-entry-if
                                                         'notregexp "State[[:space:]]+\"DONE\""))))
                 (tags "+TODO=\"ONGO\""
                       ((org-agenda-overriding-header "On Going")))
                 (tags "+TODO=\"WAIT\""
                       ((org-agenda-overriding-header "Waiting")))
                 (tags-todo "CATEGORY=\"inbox\""
                            ((org-agenda-overriding-header "Fresh off the boat")))))
        ("du" "Undated tasks" tags-todo "-PRIORITY=\"D\""
         ((org-agenda-todo-ignore-with-date t)
          (org-agenda-tags-todo-honor-ignore-options t)))
        ("dm" "Maybe someday" tags-todo "+PRIORITY=\"D\""
         ())
        ("ds" "TLS" ((agenda "")
                     (alltodo "" ((org-agenda-todo-ignore-with-date t))))
         ((org-agenda-category-filter-preset '("+tls"))
          (org-agenda-span 7)))))


(setq org-agenda-log-mode-items '(closed clock state))

(setq org-todo-keywords
      '((sequence "TODO(t)" "WAIT(w@/!)" "ONGO(o!)" "|" "DONE(d!)" "CANCELED(c@)" "DEFERRED(f@)")))

(setq org-adapt-indentation t)

(require 'org-archive)
(setq org-archive-location "%s_archive::")

(setq org-refile-targets
      '((nil :maxlevel . 2)
        (org-agenda-files :maxlevel . 2)))

(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)

;; attempt to use org-refile instead of org-archive
(defun ngoc/get-archive-file-name (org-file)
  (let* ((relative-file (file-relative-name org-file org-directory))
         (archive-file (file-name-concat org-directory ".." "archive" relative-file)))
    archive-file))

;; ;; attempt to use org-refile instead of org-archive
(define-key org-mode-map (kbd "C-c C-x C-a") 'ngoc/archive-current)
(defun ngoc/archive-current ()
  (interactive)
  (let* ((org-refile-allow-creating-parent-nodes 'confirm)
         (org-file (org-entry-get (point) "FILE"))
         (org-outline-path-complete-in-steps nil)
         (archive-file (ngoc/get-archive-file-name org-file))
         (org-refile-targets '((archive-file :maxlevel . 3))))
    (org-refile)))


(setq org-capture-templates
      '(("r" "Reading"
         checkitem
         (file "toread.org"))

        ("t" "Todo"
         entry
         (file "inbox.org")
         "* TODO %?\n")))

;; log both reschedule and redeadline
(setq org-log-reschedule 'time
      org-log-redeadline 'time)

;; 4 level priorities using important, using MoSCoW method
;; https://en.wikipedia.org/wiki/MoSCoW_method
(setq org-lowest-priority ?D)
(setq org-default-priority ?B)
(setq org-agenda-show-all-dates t)

(require 'org-habit)
(add-to-list 'org-modules 'org-habit)
(setq org-agenda-prefer-last-repeat t) ;; mostly to display missed habits on today agenda
(setq org-habit-graph-column 63) ;; move graph to the right so it shows a little more text
(setq org-habit-today-glyph ?*)
(setq org-habit-completed-glyph ?V) ;; V for Vegeta, no, for Victory
(setq org-habit-show-all-today t)
(setq org-habit-following-days 1)

(require 'org-protocol)

(use-package org-menu
  :vc (:fetcher github :repo "sheijk/org-menu")
  :config
  (define-key org-mode-map (kbd "C-c m") 'org-menu))


;; FIXME org-roam-node-find doesn't work with ivy-occur
;; https://github.com/org-roam/org-roam/issues/1684
(use-package org-roam
  :demand t
  :custom
  (org-roam-directory (file-truename (file-name-concat org-directory "/../org-roam/")))
  :bind (:map ngoc/org-roam-prefix
         ("l" . org-roam-buffer-toggle)
         ("f" . org-roam-node-find)
         ("g" . org-roam-graph)
         ("i" . org-roam-node-insert)
         ("c" . org-roam-capture)
         ;; Dailies
         ("j" . org-roam-dailies-capture-today))
  :config
  (define-prefix-command 'ngoc/org-roam-prefix)
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol))

(use-package org-cliplink
  :bind (("<f12>" . org-cliplink)))

(provide 'init-org-mode)
