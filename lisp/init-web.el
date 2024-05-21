(use-package php-mode)

(use-package web-mode
  :config
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
            #'(lambda ()
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
            #'(lambda ()
                (emmet-mode t)
                (yas-activate-extra-mode 'html-mode)))

  (setq-default web-mode-markup-indent-offset tab-width)
  (setq-default web-mode-css-indent-offset tab-width)
  (setq-default web-mode-code-indent-offset tab-width)
  (setq-default web-mode-sql-indent-offset tab-width)
  (setq web-mode-enable-control-block-indentation nil))


(use-package skewer-mode
  :hook
  (js2-mode . skewer-mode)
  (css-mode . skewer-css-mode)
  :config
  (setq httpd-port 8089))


(use-package js2-mode
  :hook
  (js-mode . js2-minor-mode))


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


(provide 'init-web)
