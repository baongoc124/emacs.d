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
- Be honest about limitations. Remember I value directness, innovation, and results.
- Keep your responses to max 80 chars every line, except for source code.")
                         (programming . "You are a large language model and a careful programmer. Provide code and only code as output without any additional text, prompt, or note.")
                         (writing . "You are a large language model and a writing assistant. Respond concisely.")
                         (chat . "You are a large language model and a conversation partner. Respond concisely.")))

  ;; set default directive & backend & model
  (setq gptel--system-message (alist-get 'default gptel-directives))
  (setq gptel-backend (gptel-get-backend "Jarvis"))
  (setq gptel-model 'claude-3-7-sonnet-20250219)

  (setq gptel-model 'claude-3.7-sonnet
        gptel-backend (gptel-make-gh-copilot "Copilot"))

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
  (aidermacs-default-model "github_copilot/claude-3.7-sonnet")
  (aidermacs-architect-model "github_copilot/claude-3.7-sonnet")
  (aidermacs-default-model "github_copilot/claude-3.7-sonnet")
  (aidermacs-weak-model "github_copilot/claude-3.5-sonnet")
  )


(use-package minuet
  :vc (:fetcher github :repo "milanglacier/minuet-ai.el")
  :diminish minuet-auto-suggestion-mode
  :bind
  (
   ;; ("M-y" . #'minuet-complete-with-minibuffer) ;; use minibuffer for completion
   ("C-c SPC" . #'minuet-show-suggestion) ;; use overlay for completion
   ;; ("C-c m" . #'minuet-configure-provider)
   :map minuet-active-mode-map
   ;; These keymaps activate only when a minuet suggestion is displayed in the current buffer
   ("M-p" . #'minuet-previous-suggestion) ;; invoke completion or cycle to next completion
   ("M-n" . #'minuet-next-suggestion) ;; invoke completion or cycle to previous completion
   ("M-SPC" . #'minuet-accept-suggestion) ;; accept whole completion
   ;; Accept the first line of completion, or N lines with a numeric-prefix:
   ;; e.g. C-u 2 M-a will accepts 2 lines of completion.
   ("M-a" . #'minuet-accept-suggestion-line)
   ("M-e" . #'minuet-dismiss-suggestion))

  :init
  ;; if you want to enable auto suggestion.
  ;; Note that you can manually invoke completions without enable minuet-auto-suggestion-mode
  (add-hook 'prog-mode-hook #'minuet-auto-suggestion-mode)

  :config
  ;; You can use M-x minuet-configure-provider to interactively configure provider and model
  ;; (setq minuet-provider 'openai-fim-compatible)

  (setq minuet-request-timeout 5)
  (setq minuet-auto-suggestion-throttle-delay 1.0) ;; minimum time in secs between auto suggestions
  (setq minuet-auto-suggestion-debounce-delay 1.0) ;; delay in secs to show suggestions after stopping typing

  (defvar my-key-index 0)
  (defvar my-keys (split-string (m/get-password "gemini.google.com" "apikeys") ","))
  (setq my-key-index (random (length my-keys)))

  (defun my/rotate-keys ()
    "Rotate through keys in round-robin fashion."
    (interactive)
    ;; show a message with index of current key
    (message "Minuet: Using key %d" my-key-index)
    (let ((key (nth my-key-index my-keys)))
      (setq my-key-index (mod (1+ my-key-index) (length my-keys)))
      key))

  (defun my/get-openrouter-api-key ()
    (m/get-password "openrouter.ai" "apikey")
    )

  (setq minuet-show-error-message-on-minibuffer t)
  (setq minuet-provider 'openai-fim-compatible)
  ;; (plist-put minuet-openai-compatible-options :end-point "https://openrouter.ai/api/v1/chat/completions")
  ;; (plist-put minuet-openai-compatible-options :api-key #'my/get-openrouter-api-key)
  ;; (plist-put minuet-openai-compatible-options :model "deepseek/deepseek-chat-v3-0324:free")

  ;; Prioritize throughput for faster completion
  ;; (minuet-set-optional-options minuet-openai-compatible-options :provider '(:sort "throughput"))
  ;; (minuet-set-optional-options minuet-openai-compatible-options :max_tokens 1024)
  ;; (minuet-set-optional-options minuet-openai-compatible-options :top_p 0.9)

  ;; (minuet-set-optional-options minuet-openai-fim-compatible-options :max_tokens 64))


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Codestral
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (defun my/get-codestral-api-key ()
    (m/get-password "codestral.ai" "apikey")
    )
  (setq minuet-provider 'codestral)
  (plist-put minuet-codestral-options :api-key #'my/get-codestral-api-key)
  (minuet-set-optional-options minuet-codestral-options :max_tokens 256)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; GEMINI
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (setq minuet-provider 'gemini)
  (plist-put minuet-gemini-options :model "gemini-2.5-flash")
  (plist-put minuet-gemini-options :api-key #'my/rotate-keys)
  (minuet-set-optional-options
   minuet-gemini-options :generationConfig
   '(:maxOutputTokens 256
                      :topP 0.9
                      ;; When using `gemini-2.5-flash`, it is recommended to entirely
                      ;; disable thinking for faster completion retrieval.
                      :thinkingConfig (:thinkingBudget 0)))

  (minuet-set-optional-options
   minuet-gemini-options :safetySettings
   [(:category "HARM_CATEGORY_DANGEROUS_CONTENT"
               :threshold "BLOCK_NONE")
    (:category "HARM_CATEGORY_HATE_SPEECH"
               :threshold "BLOCK_NONE")
    (:category "HARM_CATEGORY_HARASSMENT"
               :threshold "BLOCK_NONE")
    (:category "HARM_CATEGORY_SEXUALLY_EXPLICIT"
               :threshold "BLOCK_NONE")])

  (add-hook 'minuet-auto-suggestion-block-functions #'(lambda ()
                                                        (not (evil-insert-state-p))))
  )


  ;; For Evil users: When defining `minuet-active-mode-map` in insert
  ;; or normal states, the following one-liner is required.

  ;; (add-hook 'minuet-active-mode-hook #'evil-normalize-keymaps)

  ;; This is *not* necessary when defining `minuet-active-mode-map`.

  ;; To minimize frequent overhead, it is recommended to avoid adding
  ;; `evil-normalize-keymaps` to `minuet-active-mode-hook`. Instead,
  ;; bind keybindings directly within `minuet-active-mode-map` using
  ;; standard Emacs key sequences, such as `M-xxx`. This approach should
  ;; not conflict with Evil's keybindings, as Evil primarily avoids
  ;; using `M-xxx` bindings.


(provide 'init-ai)
