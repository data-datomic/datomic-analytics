
(require 'cl)

(defvar *emacs-load-start* (current-time))

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

(add-to-list 'load-path "/Users/dan/.emacs.d/local/")

(require 'account)
(require 'mac)
(require 'git)

(setq message-log-max t)
(column-number-mode 1)
(line-number-mode t)
(set-default 'indent-tabs-mode nil)
(set-default 'indicate-empty-lines t)
(set-default 'imenu-auto-rescan t)
(defalias 'yes-or-no-p 'y-or-n-p)

(require 'text)
(require 'theme)
(require 'keyboard)

(global-set-key [kp-0]  'undo) 
;; (global-set-key [kp-decimal]  'redo)

(define-key read-expression-map (kbd "TAB") 'lisp-complete-symbol)
(define-key lisp-mode-shared-map (kbd "C-c l") "lambda")
(define-key lisp-mode-shared-map (kbd "RET") 'reindent-then-newline-and-indent)
(define-key lisp-mode-shared-map (kbd "C-\\") 'lisp-complete-symbol)
(define-key lisp-mode-shared-map (kbd "C-x v") 'eval-buffer)

(global-set-key (kbd "C-x \\") 'align-regexp)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "\C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(global-set-key (kbd "C-x C-p") 'find-file-at-point)
(global-set-key (kbd "C-x O") (lambda ()
                                (interactive)
                                (other-window -1))) ;; back one
(global-set-key (kbd "C-x C-o") (lambda ()
                                  (interactive)
                                  (other-window 2))) ;; forward two
(global-set-key (kbd "C-h a") 'apropos)




(add-hook 'emacs-lisp-mode-hook 'imenu-add-menubar-index)
(add-hook 'emacs-lisp-mode-hook 'lisp-mode-auto-fill)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)

(add-hook 'lisp-mode-hook 'imenu-add-menubar-index)
(add-hook 'lisp-mode-hook 'lisp-mode-auto-fill)
(add-hook 'lisp-mode-hook 'turn-on-eldoc-mode)

(setq lisp-indent-function 'common-lisp-indent-function)


;; (require 'complete-company)

;; (load "~/quicklisp/slime-helper")
;; (load "~/quicklisp/clhs-use-local")

;;(require 'slime)
(load (expand-file-name "~/quicklisp/slime-helper.el"))
(load (expand-file-name "~/quicklisp/clhs-use-local.el") t)

(add-hook 'slime-mode-hook
          (lambda ()
            ;; (install-custom-slime-keymaps)
            (setq slime-truncate-lines nil)
            (setq slime-enable-evaluate-in-emacs t)
            (setq slime-complete-symbol*-fancy t)
            (unless (slime-connected-p)
              (save-excursion (slime)))))

(slime-setup '(slime-fancy
               slime-repl
               slime-asdf
               slime-banner
               slime-motd
               slime-clipboard
               slime-fancy-inspector
               slime-parse
               slime-scratch
               slime-sprof
               slime-c-p-c
               slime-tramp
               slime-editing-commands
               slime-hyperdoc
               slime-references
               slime-autodoc
               slime-fontifying-fu
               slime-snapshot
               slime-indentation
               slime-package-fu
               slime-xref-browser
               slime-sbcl-exts
               slime-compiler-notes-tree
               slime-presentations
               slime-fuzzy))

(defun slime-repl-quickload ()
  (interactive)
  (let* ((completings
          (slime-repl-shortcut-eval
           `(cl:mapcar 'ql-dist:system-file-name (ql:system-list))))
         (system (completing-read "System: " completings)))
    (slime-repl-shortcut-eval-async
     `(cl:progn
       (ql:quickload ,system)
       (cl:format t "; System ~a loaded.~%" ,system)))))

(add-hook 'slime-mode-hook '(lambda ()
                             (defslime-repl-shortcut slime-quickload ("quickload")
                              (:handler 'slime-repl-quickload)
                              (:one-liner "Quickload a Lisp system."))))


(setq inferior-lisp-program "~/bin/sbcl")

;; (load "/Volumes/u/dan/emacs23/dan/slime-quicklisp.el")
;; (load "/Volumes/u/dan/emacs23/dan/slime-reload.el")
;; (load "/Volumes/u/dan/emacs23/dan/slime-selector.el")
;; ;; (load "/Volumes/u/dan/emacs23/dan/ob-lisp.el")
;; (load "/Volumes/u/dan/emacs23/dan/lisp-beautification.el")


(require 'clojure)



(destructuring-bind (hi lo ms _) (current-time)
  (message "User Localization completed in %ds" 
           (- (+ hi lo)
              (+ (first *emacs-load-start*)
                 (second *emacs-load-start*)))))

(provide 'startup)


