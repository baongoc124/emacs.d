(setq ngoc/interface-font-name
      (cond
       ((eq system-type 'gnu/linux)
        "Cascadia Code NF")
       ((eq system-type 'darwin)
        "Menlo")
       (t
        "Monospace")))

(defun ngoc/setup-font (&optional frame)
  (interactive)
  ;; set fallback font for Japanese
  (let ((jp-font "Droid Sans"))
    (set-fontset-font t 'han jp-font)
    (set-fontset-font t 'kana jp-font)
    (set-fontset-font t 'cjk-misc jp-font))

  (set-frame-font (font-spec :family ngoc/interface-font-name
                             :weight 'regular
                             :size 10.5)
                  t t t)

  ;; fixed issue with unreadable char in Info docs
  (set-face-attribute 'fixed-pitch-serif nil :family ngoc/interface-font-name :inherit 'default))

(ngoc/setup-font)
(add-hook 'after-make-frame-functions #'ngoc/setup-font)

(use-package ligature
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                       ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                       "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                       "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                       "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                       "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                       "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                       "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                       ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                       "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                       "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                       "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                       "\\\\" "://"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

(provide 'init-font)
