

(add-hook 'after-init-hook 'global-company-mode)

;; (define-key company-active-map (kbd "\C-n")  'company-select-next)
;; (define-key company-active-map (kbd "\C-p")  'company-select-previous)
;; (define-key company-active-map (kbd "\C-d")  'company-show-doc-buffer)
;; (define-key company-active-map (kbd "<tab>") 'company-complete)


(setq company-idle-delay 0.2)
(setq company-lighter-base "c")
(setq company-minimum-prefix-length 3)
(setq company-selection-wrap-around t)
(setq company-show-numbers t)
(setq company-tooltip-limit 10)

(provide 'complete-company)
