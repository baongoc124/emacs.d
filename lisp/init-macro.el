;; simple global macro functionality
;; Two entry points:
;; - my/run-macro: for use inside Emacs (output to kill ring, no focus change)
;; - my/run-macro-from-outside: for global hotkey (output to clipboard, raise frame)

(require 'xclip)

;; ---- macro definitions ----
;; Each macro takes optional input parameter, returns text as output

(defun my/macro-current-datetime (&optional input)
  "Return current date and time as YYYYMMDD-HHMMSS.
Optional INPUT is ignored."
  (format-time-string "%Y%m%d-%H%M%S"))

;; Example with input parameter
(defun my/macro-uppercase (&optional input)
  "Return INPUT converted to uppercase.
If INPUT is nil, prompt for it."
  (let ((text (or input (read-string "Text to uppercase: "))))
    (upcase text)))

;; name -> function mapping
(defvar my/macro-alist
  '(("Copy datetime YYYYMMDD-HHMMSS" . my/macro-current-datetime)
    ("Uppercase text" . my/macro-uppercase))
  "Alist of macro names to functions.")

;; ---- output handler ----

(defun my/macro-output (text from-outside)
  "Handle macro output TEXT.
If FROM-OUTSIDE is non-nil, copy to clipboard using xclip.
Otherwise, add to kill ring."
  (if from-outside
      (progn
        (xclip-set-selection 'clipboard text)
        (message "Copied to clipboard: %s" text))
    (kill-new text)
    (message "Added to kill ring: %s" text)))

;; ---- macro execution ----

(defun my/run-macro-internal (from-outside)
  "Prompt for macro with completion and run it.
FROM-OUTSIDE determines whether to use clipboard or kill ring."
  (let* ((choice (completing-read
                  "Run macro: "
                  (mapcar #'car my/macro-alist)
                  nil t))
         (fn (cdr (assoc choice my/macro-alist))))
    (when fn
      (let* ((input (when current-prefix-arg
                      (read-string "Input: ")))
             (output (funcall fn input)))
        (when output
          (my/macro-output output from-outside))))))

;; ---- entry points ----

(defun my/run-macro ()
  "Prompt for a macro and run it, adding output to kill ring.
For use inside Emacs. Does not change focus.
With prefix arg, prompt for input parameter."
  (interactive)
  (my/run-macro-internal nil))

;; TODO: this one should also accept an arg as input.
;; That'll be passed from shell command line
(defun my/run-macro-from-outside ()
  "Raise Emacs, prompt for a macro, run it, copy to clipboard.
For use with global hotkey when in other applications.
With prefix arg, prompt for input parameter."
  (interactive)
  ;; raise Emacs frame
  (select-frame-set-input-focus (selected-frame))
  ;; run macro with clipboard output
  (my/run-macro-internal t)
  (do-applescript "tell application \"System Events\" to key code 48 using command down")
  ;; kill the frame
  (delete-frame)
  )

(provide 'init-macro)
