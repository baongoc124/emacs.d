(add-to-list 'load-path "/Users/ngoc/builds/holo-layer")
(require 'holo-layer)
(setq holo-layer-python-command "/Users/ngoc/builds/holo-layer/.venv/bin/python")
(setq holo-layer-enable-cursor-animation t)
(setq holo-layer-cursor-animation-type "jelly easing")
(setq holo-layer-cursor-alpha 128)
(setq holo-layer-cursor-animation-interval 30)
(setq holo-layer-cursor-block-commands '(list
                                         "watch-other-window-up"
                                         "watch-other-window-down"
                                         "self-insert-command"
                                         "vterm--self-insert"
                                         "evil-forward-char"
                                         "evil-backward-char"
                                         ))

(holo-layer-enable)

(provide 'init-visual-holo-layer)
