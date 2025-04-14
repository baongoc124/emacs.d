(add-to-list 'load-path "~/.emacs.d/lisp/highlight-symbol/")
(add-to-list 'load-path "~/.emacs.d/lisp/history")
(add-to-list 'load-path "~/.emacs.d/lisp/netlogo-mode/emacs")

(add-to-list 'custom-theme-load-path "~/.emacs.d/lisp/nord-lightt/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/lisp/theme-custom/")


(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/"))
(package-initialize)


(require 'use-package)
(setq use-package-always-ensure t)


(unless (package-installed-p 'vc-use-package)
  (package-vc-install "https://github.com/slotThe/vc-use-package"))
(require 'vc-use-package)

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

(use-package pacdiff)

(provide 'init-package-management)
