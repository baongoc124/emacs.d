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
  Also create the *Jarvis* buffer if it doesn't exist."
  (interactive)
  (unless (get-buffer "*Jarvis*")
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

(provide 'init-ai)
