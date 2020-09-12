;;; Slime / Common-Lisp

(push (expand-file-name "~/.emacs.d/lib/slime/") load-path)
(push (expand-file-name "~/.emacs.d/lib/slime/contrib/") load-path)

(eval-after-load "slime"
    '(progn
      (slime-require :swank-listener-hooks)))

(add-hook 'slime-mode-hook
            (lambda ()
              (setq slime-truncate-lines nil)
              (setq tab-always-indent 'complete)
              (setq slime-enable-evaluate-in-emacs t)
              (setq slime-complete-symbol*-fancy t)
              (unless (slime-connected-p)
                (save-excursion (slime)))))


(require 'slime)

;; (load (expand-file-name "~/quicklisp/slime-helper.el"))
;; (load (expand-file-name "~/quicklisp/clhs-use-local.el") t)

(setenv "SBCL_HOME" (expand-file-name "~/opt/sbcl/lib/sbcl"))
(setq inferior-lisp-program (expand-file-name "~/opt/sbcl/bin/sbcl"))
(setq default-enable-multibyte-characters t)
(setq slime-net-coding-system 'utf-8-unix)
(setq slime-lisp-host "localhost")
(setq slime-scratch-file "~/src/scratch.lisp")

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

(slime-setup  '(slime-fancy         slime-repl
                  slime-asdf          slime-banner
                  slime-motd          slime-clipboard
;; slime-media
                slime-fancy-inspector
                slime-fancy-trace
                slime-trace-dialog
                  slime-parse         slime-scratch
                  slime-sprof         slime-c-p-c
                  slime-tramp         slime-editing-commands
                  slime-hyperdoc      slime-references
;; slime-autodoc
;; slime-enclosing-context
;; slime-mdot-fu
                  slime-fontifying-fu
                  slime-snapshot      slime-indentation
                  slime-package-fu    slime-xref-browser
                  slime-sbcl-exts     slime-compiler-notes-tree
                  slime-presentations slime-fuzzy
                  slime-presentation-streams))

(put 'slime-indulge-pretty-colors 'define-test t)
(put 'slime-indulge-pretty-colors 'defpclass* t)
(put 'slime-indulge-pretty-colors 'defpassociation* t)
(put 'slime-indulge-pretty-colors 'defptype t)

(put 'slime-indulge-pretty-colors 'def t)

(put 'slime-indulge-pretty-colors 'defResource t)
(put 'slime-indulge-pretty-colors 'defNamespace t)
(put 'slime-indulge-pretty-colors 'defConcept t)
(put 'slime-indulge-pretty-colors 'defIndividual t)
(put 'slime-indulge-pretty-colors 'defOntology t)
(put 'slime-indulge-pretty-colors 'defservice t)
(put 'slime-indulge-pretty-colors 'an t)
(put 'slime-indulge-pretty-colors 'a t)


(provide 'slime-quicklisp)
