

(define-key emacs-lisp-mode-map [kp-add] 'eval-last-sexp)
(define-key emacs-lisp-mode-map [(control x) ?v] 'eval-buffer)
(define-key emacs-lisp-mode-map (kbd "C-\\") 'completion-at-point)
(define-key lisp-mode-shared-map (kbd "RET") 'reindent-then-newline-and-indent)
;(define-key lisp-mode-map        (kbd "TAB") 'indent-or-complete)
(define-key read-expression-map  (kbd "TAB") 'lisp-complete-symbol)

(add-hook 'emacs-lisp-mode-hook 'imenu-add-menubar-index)
(add-hook 'emacs-lisp-mode-hook 'lisp-mode-auto-fill)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)

(add-hook 'lisp-mode-hook 'imenu-add-menubar-index)
(add-hook 'lisp-mode-hook 'lisp-mode-auto-fill)
(add-hook 'lisp-mode-hook 'turn-on-eldoc-mode)

(custom-set-variables
 '(lisp-font-lock-keywords lisp-font-lock-keywords-2)
 '(blink-matching-paren t t)
;; '(show-paren-style 'expression t)
 '(show-paren-delay 0.1 t)
 '(show-paren-ring-bell-on-mismatch nil t)
 '(show-paren-highlight-openparen t t)
 '(lisp-indent-function 'common-lisp-indent-function t)
 '(lisp-indent-maximum-backtracking 6 t)
 '(eldoc-minor-mode-string " d"))

;; (custom-set-faces
;;  '(hl-line ((t (:background "gray12"))))
;;  '(highlight ((t (:background "skyblue4"))))
;;  '(show-paren-match-face ((t (:foreground "gray12"))))
;;  '(show-paren-mismatch-face ((t (;; :background "dimgray"
;;                                  :foreground "firebrick1")))))

(require 'rainbow-delimiters)
(rainbow-delimiters-mode 1)

(provide 'lisp)
