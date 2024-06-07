(defun ngoc/pop-mark ()
  "Jump back to the  mark. If point is at the mark, pop the mark first."
  (interactive)
  (let ((current-prefix-arg 4))
    (when (eq (mark) (point))
        (pop-mark))
    (call-interactively #'set-mark-command)))

(keymap-global-set "C-S-SPC" #'ngoc/pop-mark)

(use-package ace-link
  :config
  (ace-link-setup-default "t"))

;; https://karthinks.com/software/avy-can-do-anything/
;; supplementary code https://gist.github.com/karthink/af013ffd77fe09e67360f040b57b4c7b
(use-package avy
  :config
  (setq avy-all-windows t)
  (setq avy-keys-alist `((avy-goto-line . ,(append (number-sequence ?a ?z) (number-sequence ?0 ?9)))))
  (setq avy-keys '(?a ?o ?e ?u ?h ?t ?n ?s ?c ?j ?q ?m))

  ;; Copy text
  (defun avy-action-copy-whole-line (pt)
    (save-excursion
      (goto-char pt)
      (cl-destructuring-bind (start . end)
          (bounds-of-thing-at-point 'line)
        (copy-region-as-kill start end)))
    (select-window
     (cdr
      (ring-ref avy-ring 0)))
    t)

  (setf (alist-get ?w avy-dispatch-alist) 'avy-action-copy
        (alist-get ?W avy-dispatch-alist) 'avy-action-copy-whole-line)

  ;; Yank text
  (defun avy-action-yank-whole-line (pt)
    (avy-action-copy-whole-line pt)
    (save-excursion (yank))
    t)

  (setf (alist-get ?y avy-dispatch-alist) 'avy-action-yank
        (alist-get ?Y avy-dispatch-alist) 'avy-action-yank-whole-line)


  ;; Mark text
  (defun avy-action-mark-to-char (pt)
    (activate-mark)
    (goto-char pt))

  (setf (alist-get ?  avy-dispatch-alist) 'avy-action-mark-to-char)

  (defun avy-action-copy-region (pt)
    (save-excursion
      (let* ((end (save-excursion
                    (avy-goto-char-timer)
                    (point))))
        (copy-region-as-kill pt end)
        (yank))))

  (setf (alist-get ?r  avy-dispatch-alist) 'avy-action-copy-region))


;; save last edit position
(use-package saveplace
  :config
  (save-place-mode t)
  (setq save-place-forget-unreadable-files nil))


(setq scroll-margin 10000 ;; large number so it always keep 1/4 of the screen in margin
      maximum-scroll-margin 0.25
      scroll-conservatively 10
      scroll-preserve-screen-position t
      next-screen-context-lines 5
      isearch-allow-scroll t)

;; ;; improve scrolling performance
(setq redisplay-skip-fontification-on-input t)


(use-package beacon
  :diminish beacon-mode
  :config
  (setq beacon-blink-when-focused t)
  (setq beacon-color "#579E33")
  (beacon-mode 1))


(use-package history
  :vc (:fetcher github :repo baongoc124/history)
  :init
  (define-prefix-command 'ngoc/history-prefix)
  :bind (:map ngoc/history-prefix
         ("p" . history-goto-history)
         ("a" . history-add-history)
         ("c h" . history-kill-histories))
  :config
  (advice-add 'history-use-current-history :after #'beacon-blink))


(use-package dumb-jump
  :bind (("M-g o"   . dumb-jump-go-other-window)
         ("M-g j"   . dumb-jump-go)
         ("M-g M-g" . dumb-jump-go)
         ("M-g b"   . dumb-jump-back)
         ("M-g i"   . dumb-jump-go-prompt)
         ("M-g x"   . dumb-jump-go-prefer-external)
         ("M-g z"   . dumb-jump-go-prefer-external-other-window))
  :config
  (setq dumb-jump-selector 'ivy)
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package imenu-list)


(use-package symbol-overlay
  :config
  (require 'transient)
  (transient-define-prefix symbol-overlay-transient ()
    "Symbol Overlay transient"
    ["Symbol Overlay"
     ["Overlays"
      ("o" "Add/Remove at point" symbol-overlay-put)
      ("c" "Remove All" (lambda ()
                          (interactive)
                          (call-interactively 'symbol-overlay-remove-all)))]
     ["Other"
      ("m" "Highlight symbol-at-point" symbol-overlay-mode)]])

  (setcdr symbol-overlay-map nil) ;; clear overlay keymap
  (define-key symbol-overlay-map (kbd "M-n") #'symbol-overlay-jump-next)
  (define-key symbol-overlay-map (kbd "M-p") #'symbol-overlay-jump-prev)
  (global-set-key (kbd "M-N") 'symbol-overlay-switch-forward)
  (global-set-key (kbd "M-P") 'symbol-overlay-switch-backward))


(provide 'init-navigation)
