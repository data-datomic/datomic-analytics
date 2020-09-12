
;; (setq ns-use-srgb-colorspace t)

(setq mac-option-modifier   'meta)
(setq mac-command-modifier  'alt)
(setq mac-function-modifier 'super)

(when window-system
  (tool-bar-mode -1)
  (mouse-wheel-mode t))

(provide 'mac)
