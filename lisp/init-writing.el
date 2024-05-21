(use-package flyspell-lazy
  :config
  (flyspell-lazy-mode 1))


;; (require 'flyspell)
;; (add-hook 'LaTeX-mode-hook 'turn-on-flyspell)
;; (add-hook 'org-mode-hook 'turn-on-flyspell)

(require 'ispell)
(setq ispell-dictionary "american")


(use-package langtool)
(setq langtool-language-tool-jar "~/builds/LanguageTool-3.4/languagetool-commandline.jar")
(defun langtool-autoshow-detail-popup (overlays)
  (when (require 'popup nil t)
    ;; Do not interrupt current popup
    (unless (or popup-instances
                ;; suppress popup after type `C-g` .
                (memq last-command '(keyboard-quit)))
      (let ((msg (langtool-details-error-message overlays)))
        (popup-tip msg)))))
(setq langtool-autoshow-message-function
      'langtool-autoshow-detail-popup)


(use-package writeroom-mode
  :config
  (remove-hook 'writeroom-global-effects 'writeroom-set-fullscreen)
  (add-hook 'writeroom-global-effects 'writeroom-set-internal-border-width))


(provide 'init-writing)
