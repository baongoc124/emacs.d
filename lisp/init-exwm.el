
;;  _______  ____        ____  __
;; | ____\ \/ /\ \      / /  \/  |
;; |  _|  \  /  \ \ /\ / /| |\/| |
;; | |___ /  \   \ V  V / | |  | |
;; |_____/_/\_\   \_/\_/  |_|  |_|

(use-package exwm)
(require 'exwm-config)

(define-prefix-command 'ngoc/exwm-prefix)
(keymap-set ngoc/exwm-prefix "k" #'kill-this-buffer)
(keymap-set ngoc/exwm-prefix "b" #'exwm-workspace-switch-to-buffer)
(keymap-set ngoc/exwm-prefix "h" #'hsplit-last-buffer)
(keymap-set ngoc/exwm-prefix "v" #'vsplit-last-buffer)
(keymap-set ngoc/exwm-prefix "m" #'delete-other-windows)
(keymap-set ngoc/exwm-prefix "d" #'delete-window)
(keymap-set ngoc/exwm-prefix "=" #'balance-windows)
(keymap-set ngoc/exwm-prefix "x" #'ace-swap-window)
(keymap-set ngoc/exwm-prefix "n" #'exwm-floating-hide) ;; ninja -> hide


(defun ngoc/exwm-update-title ()
  "Update buffer name to \"[class name] title\"."
  (let* ((class-name (or exwm-class-name "N/A"))
         (title (or exwm-title "N/A")))
    (exwm-workspace-rename-buffer (format "[%s] %s" class-name title))))

(defun ngoc/start-appfinder ()
  (interactive)
  (call-process "xfce4-appfinder" nil 0 nil "--collapsed"))

(defun ngoc/exwm-reset-and-release-keyboard ()
  (interactive)
  (exwm-reset)
  (call-interactively #'exwm-input-release-keyboard))

;; my workspace setup is for dual monitor
;; only two workspace: 0 for main monitor, 1 for secondary monitor
;; to toggle between them, do 1 - current_workspace
(defun ngoc/exwm-move-active-window-to-other-monitor ()
  "Move active window to other monitor."
  (interactive)
  (let* ((current-workspace exwm-workspace-current-index)
         (target-workspace (- 1 current-workspace))
         (buffer (window-buffer))
         (win-id (exwm--buffer->id buffer)))
    (exwm-workspace-move-window target-workspace win-id)
    (select-frame-set-input-focus (exwm-workspace--workspace-from-frame-or-index target-workspace))))


(defun ngoc/exwm-focus-other-monitor ()
  "Focus other monitor."
  (interactive)
  (let* ((current-workspace exwm-workspace-current-index)
         (target-workspace (- 1 current-workspace)))
    (exwm-workspace-switch target-workspace)))

(defun ngoc/exwm-start (&rest arg)
  (interactive)

  ;; char-mode by default
  (setq exwm-manage-configurations '((t char-mode t)))

  (setq exwm-input-global-keys
        `((,(kbd "s-h") . ngoc/exwm-prefix)
          (,(kbd "s-SPC") . ngoc/exwm-focus-other-monitor)
          (,(kbd "C-s-SPC") . ngoc/exwm-move-active-window-to-other-monitor)
          (,(kbd "s-R") . exwm-restart)
          (,(kbd "s-,") . exwm-floating-toggle-floating)
          (,(kbd "s-b") . winner-undo)
          (,(kbd "s-f") . winner-redo)
          (,(kbd "M-<tab>") . switch-to-last-buffer)
          (,(kbd "s-<tab>") . other-window)
          (,(kbd "s-<return>") . ngoc/start-appfinder)
          (,(kbd "s-g") . keyboard-quit)
          (,(kbd "s-x") . counsel-M-x)
          (,(kbd "M-s-<left>") . shrink-window-horizontally)
          (,(kbd "M-s-<right>") . enlarge-window-horizontally)
          (,(kbd "M-s-<down>") . shrink-window)
          (,(kbd "M-s-<up>") . enlarge-window)
          ([?\s-r] . ngoc/exwm-reset-and-release-keyboard)
          ([?\s-w] . exwm-workspace-switch)
          ([?\s-&] . (lambda (command)
                       (interactive (list (read-shell-command "$ ")))
                       (start-process-shell-command command nil command)))
          ;; 's-N': Switch to certain workspace.
          ,@(mapcar (lambda (i)
                      `(,(kbd (format "s-%d" i)) .
                        (lambda ()
                          (interactive)
                          (exwm-workspace-switch-create ,i))))
                    (number-sequence 0 9))))

  (add-hook 'exwm-update-class-hook #'ngoc/exwm-update-title)
  (add-hook 'exwm-update-title-hook #'ngoc/exwm-update-title)

  (require 'exwm-randr)
  ;; one workspace for main monitor, one for secondary monitor
  ;; need keys to switch between them and move windows between them
  (setq exwm-randr-workspace-monitor-plist '(0 "DP-0" 1 "HDMI-0"))
  ;; (shell-command "xrandr --output HDMI-0 --mode 2560x1080 --pos 0x0 --rotate normal --output DP-0 --primary --mode 2560x1440 --pos 0x1080 --rotate normal &")
  (exwm-randr-enable)

  (exwm-enable)
  (exwm-workspace-switch-create 1)
  (exwm-workspace-switch-create 0))

(setq command-switch-alist
      '(("--exwm" . ngoc/exwm-start)))

(provide 'init-exwm)
