(require 'auth-source)
(setq auth-sources '("~/.authinfo"))

(defun m/get-password (host username)
  "Retrieve password for HOST and USERNAME from auth-source."
  (let* ((auth-info (auth-source-search :host host
                                       :user username
                                       :require '(:secret)
                                       :max 1))
         (password-getter (plist-get (car auth-info) :secret)))
    (if password-getter
        (funcall password-getter)
      (message "No credentials found for %s@%s" username host)
      nil)))

(provide 'init-auth)
