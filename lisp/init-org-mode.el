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
(setq org-tag-persistent-alist '((:startgroup .nil)
                                 ("home" . ?h)
                                 ("office" . ?o)
                                 ("VN" . ?v)
                                 (:endgroup . nil)))
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


;; sort habits by priority
;; https://emacs.stackexchange.com/questions/32430/how-to-sort-habits-by-priority-in-the-org-agenda-view
(defun hw-org-agenda-sort-habits (a b)
  "Sort habits first by user priority, then by schedule+deadline+consistency."
  (let ((ha (get-text-property 1 'org-habit-p a))
        (hb (get-text-property 1 'org-habit-p b)))
    (when (and ha hb)
      (let ((pa (org-get-priority a))
            (pb (org-get-priority b)))
        (cond ((> pa pb) +1)
              ((< pa pb) -1)
              ((= pa pb) (org-cmp-values a b 'priority)))))))

(setq org-agenda-cmp-user-defined 'hw-org-agenda-sort-habits)
(setq org-agenda-sorting-strategy
      '(time-up todo-state-down user-defined-down habit-down timestamp-up
                priority-down category-keep))

(setq org-agenda-time-grid
      '((daily today require-timed remove-match)
        (800 1200 1800 2200)
        ""
        "----------------"))

(setq org-agenda-custom-commands nil)
(setq org-agenda-custom-commands
      '(("d" . "Personal prefix")
        ("dd" "My agenda" ((agenda "" ((org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled)
                                       (org-habit-show-habits nil)
                                       (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))))
                           (agenda ""
                                   ((org-agenda-overriding-header "Habits")
                                    (org-agenda-format-date "")
                                    (org-agenda-span 'day)
                                    (org-agenda-prefix-format '((agenda . " %i %-12c")))
                                    (org-agenda-skip-function
                                     '(org-agenda-skip-entry-if 'notregexp ":STYLE:[[:space:]]*habit"))))
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

        ("m" "Movies"
         checkitem
         (file "movies.org"))

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
(setq org-habit-show-done-always-green t)

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

(use-package org-preview-html)

(require 'ox-publish)
(require 'ox-html)
(require 'hl-line)
(setq org-html-htmlize-output-type 'css)

(defun ngoc/generate-css-to-string ()
  "Generate CSS string for org export."
  (save-window-excursion
    (org-html-htmlize-generate-css)
    (with-current-buffer "*html*"
      (buffer-substring-no-properties (point-min) (point-max)))))

(setq org-publish-project-alist
      `(("org-roam"
         :base-directory "~/Dropbox/org/org-roam"
         :base-extension "org"
         :publishing-directory "~/Dropbox/org/org-roam/export/"
         :recursive t
         :publishing-function org-html-publish-to-html
         :headline-levels 4
         :section-numbers t
         :with-toc t
         :html-head ,(ngoc/generate-css-to-string)
         :html-head-extra "<link rel=\"stylesheet\" type=\"text/css\" href=\"../style.css\"/>"
         :html-head-include-default-style t
         :html-preamble t
         :html-postamble t
         :auto-sitemap t
         :sitemap-filename "index.org"
         :sitemap-title "Index"
         :sitemap-sort-files anti-chronologically
         :sitemap-file-entry-format "%d %t"
         :sitemap-date-format "%Y-%m-%d"
         ;; :sitemap-function org-publish-org-sitemap
         :html-link-home "index.html"
         :html-link-up "index.html"
         :auto-preamble t
         :auto-postamble t
         :html-head-include-scripts nil)))

;; https://org-roam.discourse.group/t/export-backlinks-on-org-export/1756/28
(defun collect-backlinks-string (backend)
  (when (org-roam-node-at-point)
    (let* ((source-node (org-roam-node-at-point))
           (source-file (org-roam-node-file source-node))
           (nodes-in-file (--filter (s-equals? (org-roam-node-file it) source-file)
                                    (org-roam-node-list)))
           (nodes-start-position (-map 'org-roam-node-point nodes-in-file))
           ;; Nodes don't store the last position, so get the next headline position
           ;; and subtract one character (or, if no next headline, get point-max)
           (nodes-end-position (-map (lambda (nodes-start-position)
                                       (goto-char nodes-start-position)
                                       (if (org-before-first-heading-p) ;; file node
                                           (point-max)
                                          ('org-forward-heading-same-level)
                                         (if (> (point) nodes-start-position)
                                             (- (point) 1) ;; successfully found next
                                           (point-max)))) ;; there was no next
                                     nodes-start-position))
           ;; sort in order of decreasing end position
           (nodes-in-file-sorted (->> (-zip nodes-in-file nodes-end-position)
                                      (--sort (> (cdr it) (cdr other))))))
      (dolist (node-and-end nodes-in-file-sorted)
        (-when-let* (((node . end-position) node-and-end)
                     (backlinks (--filter (->> (org-roam-backlink-source-node it)
                                               (org-roam-node-file)
                                               (s-contains? "private/") (not))
                                          (org-roam-backlinks-get node)))
                     (heading (format "\n\n%s Backlinks\n"
                                      (s-repeat (+ (org-roam-node-level node) 1) "*")))
                     (properties-drawer ":PROPERTIES:\n:HTML_CONTAINER_CLASS: references\n:END:\n"))
          (goto-char end-position)
          (insert heading)
          (insert properties-drawer)
          (dolist (backlink backlinks)
            (let* ((source-node (org-roam-backlink-source-node backlink))
                   (source-file (org-roam-node-file source-node))
                   (properties (org-roam-backlink-properties backlink))
                   (outline (when-let ((outline (plist-get properties :outline)))
                              (when (> (length outline) 1)
                                (mapconcat #'org-link-display-format outline " > "))))
                   (point (org-roam-backlink-point backlink))
                   (text  (org-roam-preview-get-contents
                           source-file
                           point))
                   (reference (format "\n ----- \n%s [[id:%s][%s]]\n%s\n%s\n\n"
                                      (s-repeat (+ (org-roam-node-level node) 2) "*")
                                      (org-roam-node-id source-node)
                                      (org-roam-node-title source-node)
                                      (if outline (format "%s (/%s/)"
                                                          (s-repeat (+ (org-roam-node-level node) 3) "*") outline) "")
                                      text))
                   (label-list (with-temp-buffer
                                 (insert text)
                                 (org-element-map (org-element-parse-buffer) 'footnote-reference
                                   (lambda (reference)
                                     (org-element-property :label reference)))))
                   (footnote-string-list
                    (with-temp-buffer
                      (insert-file-contents source-file)
                      (-map (lambda (label) (buffer-substring-no-properties
                                             (nth 1 (org-footnote-get-definition label))
                                             (nth 2 (org-footnote-get-definition label))))
                            label-list))))
              (-map (lambda (footnote-string) (insert footnote-string)) footnote-string-list)
              (insert reference))))))))

(add-hook 'org-export-before-processing-hook 'collect-backlinks-string)

;; highlight external http, https links
(defface org-external-link
  '((t :inherit org-link))
  "Face for external links.")

(org-link-set-parameters "http" :face 'org-external-link)
(org-link-set-parameters "https" :face 'org-external-link)

(use-package org-noter)
(use-package org-fragtog
  :hook
  (org-mode . org-fragtog-mode))

;; change scale of org-format-latex-options
(setq org-format-latex-options (plist-put org-format-latex-options :scale 2))

(provide 'init-org-mode)
