;;; mode: org-mode

;;;
;;; Dependencies
;;;

(require 'org-bullets)

;;;
;;; Org Hooks
;;;

(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
(add-hook 'org-mode-hook 'imenu-add-menubar-index)
(add-hook 'org-mode-hook 'yas/minor-mode-on)
(add-hook 'org-mode-hook 'turn-on-eldoc-mode)
(add-hook 'org-mode-hook 'flyspell-mode-off)
(add-hook 'org-mode-hook (lambda() (setq show-trailing-whitespace nil)))

;;;
;;; Org TODO
;;;

(setq org-todo-keywords '((sequence "TODO(t)" "STARTED(s)" "WAITING(w@/!)" "|"
                           "DONE(d!)" "CANCELLED(c!)")))

(setq org-use-fast-todo-selection t)

;;;
;;; Org Misc
;;;

(setq org-insert-mode-line-in-empty-file t)
(setq org-log-done-with-time t)
(setq org-insert-heading-respect-content t)
(setq org-M-RET-may-split-line '((default . t)))
(setq org-imenu-depth 3)

;;;
;;; Org Global Keybindings
;;;

(global-unset-key [(control o)])

(global-set-key [(control o) ?l]          'org-store-link)
(global-set-key [(control o) (control l)] 'org-store-link)

(global-set-key [(control o) ?a]          'org-agenda)
(global-set-key [(control o) (control a)] 'org-agenda)

(global-set-key [(control o) ?b]          'org-iswitchb)
(global-set-key [(control o) (control b)] 'org-iswitchb)

(global-set-key [(control o) ?c]          'org-capture)
(global-set-key [(control o) (control c)] 'org-capture)

(global-set-key [(control o) ?f]          'org-footnote-action)
(global-set-key [(control o) (control f)] 'org-footnote-action)

(global-set-key [(control o) ?p]          'org-set-property)
(global-set-key [(control o) (control p)] 'org-set-property)

(global-set-key [(control o) ?i]          'org-clock-in)
(global-set-key [(control o) (control i)] 'org-clock-in)

(global-set-key [(control o) ?o]          'org-clock-out)
(global-set-key [(control o) (control o)] 'org-clock-out)

(global-set-key [(control o) ?t]          'org-set-tags)
(global-set-key [(control o) (control t)] 'org-set-tags)

(setq org-default-notes-file (concat org-directory "/notes.org"))

(setq org-capture-templates
      '(("t" "Todo"    entry (file+headline "~/org/tasks.org" "Tasks")
         "* TODO %?\n  %i\n  %a")
        ("j" "Journal" entry (file+datetree "~/org/journal.org")
         "* %?\nEntered on %U\n  %i\n  %a")))

(setq org-agenda-files
      '("~/org/index.org"
        "~/org/tasks.org"
        "~/org/pershing.org"
        "~/org/notes.org"
        "~/org/journal.org"))

(setq org-todo-keyword-faces
      '(("TODO"     . (:foreground "#859900" :weight bold))
        ("STARTED"  . (:foreground "#fdf6e3" :weight bold))
        ("CANCELED" . (:foreground "#586e75" :slant italic))
        ("WAITING"  . (:foreground "#d33682" :weight bold))
        ("DONE" . "#2aa198")))

(provide 'org-lentz)
