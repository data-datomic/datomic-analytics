
(set-terminal-coding-system  'utf-8)
(set-keyboard-coding-system  'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system        'utf-8)

(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; (global-set-key (kbd "M-l")
;;                 (lambda () (interactive)
;;                   (insert (make-char 'greek-iso8859-7 107))))

(provide 'text)
