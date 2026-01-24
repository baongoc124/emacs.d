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
          (java "https://github.com/tree-sitter/tree-sitter-java.git")
          (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
          (json "https://github.com/tree-sitter/tree-sitter-json")
          (make "https://github.com/alemuller/tree-sitter-make")
          (ma "https://github.com/ikatyang/tree-sitter-markdown")
          (php "https://github.com/tree-sitter/tree-sitter-php")
          (python "https://github.com/tree-sitter/tree-sitter-python")
          (rust "https://github.com/tree-sitter/tree-sitter-rust")
          (toml "https://github.com/tree-sitter/tree-sitter-toml")
          (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
          (yaml "https://github.com/ikatyang/tree-sitter-yaml"))))


;; (use-package treesit-auto
;;   :config
;;   (global-treesit-auto-mode))


;; ;; use external package instead
;; (use-package tree-sitter
;;   :diminish tree-sitter-mode
;;   :config
;;   (global-tree-sitter-mode t)
;;   (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

;;   (tree-sitter-hl-add-patterns
;;       'yaml
;;     [
;;      (block_mapping_pair \. (flow_node) @variable)
;;      ])
;;   )

;; (use-package tree-sitter-langs)

(provide 'init-treesit)
