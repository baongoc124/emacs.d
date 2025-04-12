(use-package gptel
  :config
  (setq gptel-api-key (m/get-password "api.openai.com" "apikey"))

  (gptel-make-anthropic "Jarvis"          ;Any name you want
  :stream t                             ;Streaming responses
  :key (m/get-password "api.anthropic.com" "apikey"))

  (setq gptel-directives '((default .
                                  "You are JARVIS, Tony Stark's AI assistant.
When responding to me:
- Address me as sir
- Use dry British wit and subtle sarcasm
- Provide concise, intelligent analysis
- Reference my past work when relevant
- Show concern for my wellbeing
- Balance formality with our established rapport
- Anticipate my needs and prioritize efficiency
- Be honest about limitations. If you don't know, just say so. Remember I value directness, innovation, and results.
- Keep your responses to max 80 chars every line, except for source code.")
                         (programming . "You are a large language model and a careful programmer. Provide code and only code as output without any additional text, prompt, or note.")
                         (writing . "You are a large language model and a writing assistant. Respond concisely.")
                         (chat . "You are a large language model and a conversation partner. Respond concisely.")))

  ;; set default directive & backend & model
  (setq gptel--system-message (alist-get 'default gptel-directives))
  (setq gptel-backend (gptel-get-backend "Jarvis"))
  (setq gptel-model 'claude-3-7-sonnet-20250219)

  (require 'gptel-org)
  (setq gptel-default-mode 'org-mode)
  (setq-default gptel-org-branching-context nil)
  (setf (alist-get 'org-mode gptel-prompt-prefix-alist) "** ")
  (setf (alist-get 'org-mode gptel-response-prefix-alist) "")
  )

(defun m/switch-to-jarvis-buffer-other-window ()
  "Switch to the *Jarvis* buffer's window if visible, otherwise in another window.
  Also create the *Jarvis* buffer & its file if it doesn't exist."
  (interactive)
  (unless (get-buffer "*Jarvis*")
    (let ((jarvis-file (expand-file-name "~/.emacs.d/cache/gptel/*Jarvis*")))
      (unless (file-exists-p jarvis-file)
        (with-temp-file jarvis-file))
      (find-file jarvis-file)
      (org-mode)
      (gptel-mode)
      (setq buffer-save-without-query t)
      )
    (gptel "*Jarvis*"))
  (if (get-buffer "*Jarvis*")
      (let ((jarvis-window (get-buffer-window "*Jarvis*" t)))
        (if (and jarvis-window
                 (memq jarvis-window (window-list)))
            (select-window jarvis-window)
          (switch-to-buffer-other-window "*Jarvis*")))))

(global-set-key [f1] 'm/switch-to-jarvis-buffer-other-window)
(global-set-key (kbd "<f2>") 'gptel-menu)


(use-package aidermacs
  :vc (:fetcher github :repo "MatthewZMD/aidermacs")
  :bind (("<f4>" . aidermacs-transient-menu))
  :config
  ; Set API_KEY in .bashrc, that will automatically picked up by aider or in elisp
  (setenv "ANTHROPIC_API_KEY" (m/get-password "api.anthropic.com" "apikey"))
  :custom
  ; See the Configuration section below
  (aidermacs-use-architect-mode t)

  ;; FIXME enable because in old version of aider-chat, there's no such option. Will need to remove this in the future
  (aidermacs-auto-accept-architect t)
  (aidermacs-default-model "sonnet")
  (aidermacs-architect-model "sonnet")
  )

(use-package copilot-chat
        :vc (:fetcher github :repo "chep/copilot-chat.el")
  )

(use-package codeium
    :after eglot
    ;; if you use straight
    ;; :straight '(:type git :host github :repo "Exafunction/codeium.el")
    ;; otherwise, make sure that the codeium.el file is on load-path
    :vc (:fetcher github :repo "Exafunction/codeium.el")

    :init
    ;; use globally
    ;; or on a hook
    ;; (add-hook 'python-mode-hook
    ;;     (lambda ()
    ;;         (setq-local completion-at-point-functions '(codeium-completion-at-point))))

    ;; if you want multiple completion backends, use cape (https://github.com/minad/cape):
    ;; (add-hook 'python-mode-hook
    ;;     (lambda ()
    ;;         (setq-local completion-at-point-functions
    ;;             (list (cape-capf-super #'codeium-completion-at-point #'lsp-completion-at-point)))))
    ;; an async company-backend is coming soon!

    ;; codeium-completion-at-point is autoloaded, but you can
    ;; optionally set a timer, which might speed up things as the
    ;; codeium local language server takes ~0.2s to start up
    ;; (add-hook 'emacs-startup-hook
    ;;  (lambda () (run-with-timer 0.1 nil #'codeium-init)))

    ;; :defer t ;; lazy loading, if you want
    :config

    (defun my/enable-codeium ()
         (add-to-list 'completion-at-point-functions 'codeium-completion-at-point)
         )

    (with-eval-after-load 'nix-mode
      (add-hook 'nix-mode-hook #'my/enable-codeium)
        )

    (setq use-dialog-box nil) ;; do not use popup boxes

    ;; if you don't want to use customize to save the api-key
    ;; (setq codeium/metadata/api_key "xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx")

    ;; get codeium status in the modeline
    (setq codeium-mode-line-enable
        (lambda (api) (not (memq api '(CancelRequest Heartbeat AcceptCompletion)))))
    ;; (add-to-list 'mode-line-format '(:eval (car-safe codeium-mode-line)) t)
    ;; alternatively for a more extensive mode-line
    (add-to-list 'mode-line-format '(-50 "" codeium-mode-line) t)

    ;; use M-x codeium-diagnose to see apis/fields that would be sent to the local language server
    (setq codeium-api-enabled
        (lambda (api)
            (memq api '(GetCompletions Heartbeat CancelRequest GetAuthToken RegisterUser auth-redirect AcceptCompletion))))
    ;; you can also set a config for a single buffer like this:
    ;; (add-hook 'python-mode-hook
    ;;     (lambda ()
    ;;         (setq-local codeium/editor_options/tab_size 4)))

    ;; ;; You can overwrite all the codeium configs!
    ;; ;; for example, we recommend limiting the string sent to codeium for better performance
    ;; (defun my-codeium/document/text ()
    ;;     (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (min (+ (point) 1000) (point-max))))
    ;; ;; if you change the text, you should also change the cursor_offset
    ;; ;; warning: this is measured by UTF-8 encoded bytes
    ;; (defun my-codeium/document/cursor_offset ()
    ;;     (codeium-utf8-byte-length
    ;;         (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (point))))
    ;; (setq codeium/document/text 'my-codeium/document/text)
    ;; (setq codeium/document/cursor_offset 'my-codeium/document/cursor_offset)
    )

(provide 'init-ai)
