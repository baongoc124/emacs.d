(require 'eglot)
(require 'web-mode)

(define-derived-mode vue-mode web-mode "Vue")
(add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode))

;; (add-hook 'vue-mode-hook #'eglot-ensure)
(add-to-list 'eglot-server-programs
  '(vue-mode "vue-language-server" "--stdio"))


;; https://github.com/joaotavora/eglot/discussions/1184
(defun vue-eglot-init-options ()
  "Set SDK path and default options."
  (let* ((tsdk-path (expand-file-name
                    "lib/node_modules/typescript/lib/"
                    (shell-command-to-string
                     (string-join '("nix-store  --query `which tsc`"
                                    "tr -d '\n'")
                                    " | ")))))
    `(:typescript (:tsdk ,tsdk-path
                         :languageFeatures (:completion
                                            (:defaultTagNameCase "both"
                                                                 :defaultAttrNameCase "kebabCase"
                                                                 :getDocumentNameCasesRequest nil
                                                                 :getDocumentSelectionRequest nil)
                                            :diagnostics
                                            (:getDocumentVersionRequest nil))
                         :documentFeatures (:documentFormatting
                                            (:defaultPrintWidth 100
                                                                :getDocumentPrintWidthRequest nil)
                                            :documentSymbol t
                                            :documentColor t))
       :vue (:hybridMode :json-false))))

(add-to-list 'eglot-server-programs ;; nix-env -iA nixpkgs.nodePackages.volar
             `(vue-mode . ("vue-language-server" "--stdio" :initializationOptions ,(vue-eglot-init-options))))


;; ;; remove vue-mode from assoc list eglot-server-programs
;; (setq eglot-server-programs
;;       (assq-delete-all 'vue-mode eglot-server-programs))

(provide 'init-programming-vue)
