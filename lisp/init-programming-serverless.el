(defun my/serverless-current-function-block-node ()
  "Return tsc node of the function block under cursor, or nil if not inside one."
  (interactive)

  (require 'tree-sitter)
  (require 'tree-sitter-langs)


  (when (not (bound-and-true-p tree-sitter-mode))
    (tree-sitter-mode))

  (let* ((root (tsc-root-node tree-sitter-tree))
         (query (tsc-make-query
                 tree-sitter-language
                 "(block_node (block_mapping (block_mapping_pair (flow_node (plain_scalar) @function_block_name)
                  (block_node (block_mapping (block_mapping_pair) @function)
                      )))
                  (#eq? @function_block_name \"functions\"))"))
         (cursor (tsc-make-query-cursor))
         (pos (point))
         (matches (tsc-query-captures query root #'tsc--buffer-substring-no-properties)))
    (let ((found nil))
      (seq-do (lambda (match)
                (let ((name (car match))
                      (node (cdr match)))
                  ;; (message "Checking node %s: %s-%s (pos=%s)"
                  ;;          name
                  ;;          (tsc-node-start-position node)
                  ;;          (tsc-node-end-position node)
                  ;;          pos)
                  (when (and (eq name 'function)
                             (>= pos (tsc-node-start-position node))
                             (<= pos (tsc-node-end-position node)))
                    (message "Match found!")
                    (setq found node))))
              matches)
      found)
    ))


(defun my/serverless-current-function-handler-value ()
  "Extract handler value from function block at point in serverless.yml using tree-sitter."
  (interactive)
  (let* ((node (my/serverless-current-function-block-node))
         (query-string "(block_node (block_mapping (block_mapping_pair
            (flow_node (plain_scalar (string_scalar) @handler_key))
            (flow_node (plain_scalar (string_scalar) @handler_value))))
            (#match? @handler_key \"handler|command\"))")
         (query (tsc-make-query tree-sitter-language query-string)))
    (let ((matches (tsc-query-captures query node #'tsc--buffer-substring-no-properties)))
      (catch 'found
        (seq-doseq (match matches)
          (let ((capture-name (car match))
                (text (tsc-node-text (cdr match))))
            (when (eq capture-name 'handler_value)
              (message "Found handler: %s" text)
              (throw 'found text))))
        nil))))


(defun my/serverless-jump-to-handler (&optional handler-value)
  "Jump to the Python function defined in serverless handler."
  (interactive)
  (message "Attempting to jump to handler...")
  (let* ((handler-path (or handler-value (my/serverless-current-function-handler-value)))
         (parts (split-string handler-path "\\."))
         (module-path (string-join (butlast parts) "."))
         (function-name (car (last parts)))
         (default-directory (project-root (project-current)))
         )

    (when handler-path
      ;; Use Python to find the actual file path
      (let ((python-cmd (format "import importlib.util; import sys;
import os;
import sys
try:
    spec = importlib.util.find_spec('%s');
    print(spec.origin if spec else '')
except:
    # print stack trace
    import traceback; traceback.print_exc(file=sys.stderr)
    print('Current working directory: ' + os.getcwd())
    print('Current sys.path: ' + str(sys.path))
    print('')" module-path)))

        (message "Looking up: %s" module-path)
        (let ((file-path (string-trim
                          (inheritenv-apply 'shell-command-to-string
                                            (format "python -c \"%s\"" python-cmd)))))
          ;; (message "%s" process-environment)
          (message "%s" file-path)
          (if (and file-path (not (string= file-path "")))
              (progn
                (find-file file-path)
                ;; Try to find the function definition
                (goto-char (point-min))
                (if (re-search-forward (format "def\\s-+%s\\s-*(" function-name) nil t)
                    (message "Found handler: %s" function-name)
                  (message "Found file but couldn't locate function %s" function-name)))
            (message "Couldn't find module: %s" module-path)))))))


(defun my-serverless-xref-backend ()
  "Backend for jumping to serverless handlers."
  (when (and buffer-file-name
             (string-match-p "serverless\\.yml\\'" buffer-file-name))
    'serverless))

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql serverless)))
  (my/serverless-current-function-handler-value))

(cl-defmethod xref-backend-definitions ((_backend (eql serverless)) identifier)
  (let ((handler-location (my-find-serverless-handler identifier)))
    (when handler-location
      (list (xref-make identifier handler-location)))))

(defun my-find-serverless-handler (identifier)
  "Create xref location from handler jumping function."
  (save-window-excursion
    (condition-case nil
        (progn
          (my/serverless-jump-to-handler identifier)
          (xref-make-file-location
           (buffer-file-name)
           (line-number-at-pos)
           (current-column)))
      (error nil))))

(add-hook 'yaml-mode-hook #'(lambda ()
                              (add-hook 'xref-backend-functions #'my-serverless-xref-backend -1)
                              ))

(provide 'init-programming-serverless)

;; (setq shell-command-switch "-c")
;; (setq shell-file-name "/Users/ngoc/.nix-profile/bin/bash")
