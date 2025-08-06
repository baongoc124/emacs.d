(add-to-list 'compilation-error-regexp-alist-alist
             '(my-go-stacktrace "^\\(/[^:\n]+\\):\\([0-9]+\\) ([^)\n]+)" 1 2))
(add-to-list 'compilation-error-regexp-alist 'my-go-stacktrace)

(provide 'init-programming-golang)
