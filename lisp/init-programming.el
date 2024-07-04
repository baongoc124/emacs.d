(use-package quickselect
  :ensure nil
  :demand t
  :config
  (quickselect-mode 1)
  (global-set-key (kbd "C-q") #'quickselect-mark)
  (global-set-key (kbd "C-'") #'quoted-insert))


(use-package flymake)
(use-package flymake-diagnostic-at-point
  :after flymake
  :config
  (add-hook 'flymake-mode-hook #'flymake-diagnostic-at-point-mode))


;;                                             _     _ _     _
;;   __ _ _ __ __ _ _   _ _ __ ___   ___ _ __ | |_  | (_)___| |_
;;  / _` | '__/ _` | | | | '_ ` _ \ / _ \ '_ \| __| | | / __| __|
;; | (_| | | | (_| | |_| | | | | | |  __/ | | | |_  | | \__ \ |_
;;  \__,_|_|  \__, |\__,_|_| |_| |_|\___|_| |_|\__| |_|_|___/\__|
;;            |___/
(use-package fill-function-arguments
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


;; FIXME yasnippet doesn't work when first open a buffer
(use-package yasnippet
  :demand t
  :diminish yas-minor-mode
  :bind (("M-8" . yas-expand)
         :map yas-minor-mode-map
              ("<tab>" . nil)
              ("TAB" . nil))
  :hook
  (prog-mode . yas-minor-mode)
  (text-mode . yas-minor-mode)
  :config
  ;; https://emacs.stackexchange.com/questions/38242/problem-redoing-with-yasnippet
  (setq yas-snippet-revival nil))

;; don't bind any key because i use counsel-dash for searching/filtering
(use-package zeal-at-point
  :config
  (setq zeal-at-point-mode-alist
        '((actionscript-mode    . "actionscript")
          (arduino-mode         . "arduino")
          (c++-mode             . "cpp")
          (c++-ts-mode          . "cpp")
          (c-mode               . "c")
          (clojure-mode         . "clojure")
          (coffee-mode          . "coffee")
          (lisp-mode            . "lisp")
          (cperl-mode           . "perl")
          (css-mode             . "css")
          (elixir-mode          . "elixir")
          (emacs-lisp-mode      . "elisp")
          (enh-ruby-mode        . "ruby")
          (erlang-mode          . "erlang")
          (gfm-mode             . "markdown")
          (go-mode              . "go")
          (groovy-mode          . "groovy")
          (haskell-mode         . "haskell")
          (html-mode            . "html")
          (java-mode            . "java20")
          (java-ts-mode         . "java20")
          (js2-mode             . "javascript")
          (js3-mode             . "nodejs")
          (less-css-mode        . "less")
          (lua-mode             . "lua")
          (markdown-mode        . "markdown")
          (objc-mode            . "iphoneos")
          (perl-mode            . "perl")
          (php-mode             . ("php" "wordpress"))
          (processing-mode      . "processing")
          (puppet-mode          . "puppet")
          (python-mode          . "python3")
          (python-ts-mode       . "python3")
          (ruby-mode            . "ruby")
          (rust-mode            . "rust")
          (sass-mode            . "sass")
          (scala-mode           . "scala")
          (tcl-mode             . "tcl")
          (vim-mode             . "vim"))))

(use-package counsel-dash
  :after dash-docs
  :bind (:map ngoc-prefix-map
              ("," . counsel-dash)
              ("." . counsel-dash-at-point))
  :config
  (setq counsel-dash-min-length 2)
  (setq counsel-dash-enable-debugging nil)
  (setq counsel-dash-browser-func 'browse-url)

  (setq ngoc/counsel-dash-mode-alist
        '((c++-ts-mode          . ("C++"))
          (c-mode               . ("c"))
          (emacs-lisp-mode      . ("elisp"))
          (go-mode              . ("go"))
          (html-mode            . ("html"))
          (java-ts-mode         . ("Java_SE20"))
          (js2-mode             . ("javascript"))
          (js3-mode             . ("nodejs"))
          (php-mode             . ("PHP" "WordPress"))
          (python-ts-mode       . ("Python_3"))))

  (defun ngoc/counsel-dash-set-docset ()
    (interactive)
    (let ((docset (cdr (assoc major-mode ngoc/counsel-dash-mode-alist))))
      (if docset
          (setq-local counsel-dash-docsets docset)
        (message "No docset found for %s" major-mode))))

  (add-hook 'prog-mode-hook #'ngoc/counsel-dash-set-docset)

  (defun dash-docs-browse-url (search-result)
    "Hacky workaround to browse dash docs with counsel-dash but show
results in Zeal."
    (let ((docset-name (car search-result))
          (match (nth 1 (cadr search-result))))
      (zeal-at-point-run-search (zeal-at-point-maybe-add-docset match)))))

(provide 'init-programming)
