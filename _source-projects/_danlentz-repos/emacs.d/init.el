(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#839496" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#002b36"))
 '(blink-matching-paren t)
 '(column-number-mode t)
 '(custom-enabled-themes (quote (sanityinc-solarized-dark)))
 '(custom-safe-themes
   (quote
    ("ce557950466bf42096853c6dac6875b9ae9c782b8665f62478980cc5e3b6028d" "705f3f6154b4e8fac069849507fd8b660ece013b64a0a31846624ca18d6cf5e1" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" default)))
 '(eldoc-minor-mode-string " d")
 '(fci-rule-color "#073642")
 '(fill-column 72)
 '(indicate-buffer-boundaries (quote left))
 '(lisp-el-font-lock-keywords lisp-font-lock-keywords-2 t)
 '(lisp-indent-function (quote common-lisp-indent-function))
 '(lisp-indent-maximum-backtracking 6)
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(org-agenda-files
   (quote
    ("~/org/journal.org" "~/org/notes.org" "~/org/tasks.org" "~/org/pershing.org")))
 '(package-selected-packages
   (quote
    (rainbow-delimiters recentf-ext redo+ paredit-menu undo-tree smartparens outorg osx-dictionary osx-clipboard osx-browse orgtbl-ascii-plot org-mac-link org-link-travis org-fstree org-elisp-help org-cliplink org-caldav org-bullets org-beautify-theme markdown-mode+ magit-gh-pulls magit-find-file java-snippets haml-mode dropdown-list docbook-snippets dired-imenu dired+ datomic-snippets company common-lisp-snippets color-theme-sanityinc-solarized clojure-snippets clojure-quick-repls clojure-mode-extra-font-locking clojure-cheatsheet clj-refactor cider-eval-sexp-fu ac-cider)))
 '(safe-local-variable-values (quote ((Syntax . ANSI-Common-Lisp))))
 '(save-place t nil (saveplace))
;; '(session-use-package t nil (session))
 '(show-paren-delay 0.1)
 '(show-paren-highlight-openparen t t)
 '(show-paren-mode t)
 '(show-paren-ring-bell-on-mismatch nil)
 '(show-paren-style (quote expression))
 '(size-indication-mode t)
 '(tool-bar-mode nil)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#dc322f")
     (40 . "#cb4b16")
     (60 . "#b58900")
     (80 . "#859900")
     (100 . "#2aa198")
     (120 . "#268bd2")
     (140 . "#d33682")
     (160 . "#6c71c4")
     (180 . "#dc322f")
     (200 . "#cb4b16")
     (220 . "#b58900")
     (240 . "#859900")
     (260 . "#2aa198")
     (280 . "#268bd2")
     (300 . "#d33682")
     (320 . "#6c71c4")
     (340 . "#dc322f")
     (360 . "#cb4b16"))))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "menlo" :height 96))))
 '(bold ((t (:family "menlo" :weight bold))))
 '(cider-repl-stdout-face ((t (:inherit italic :foreground "#6c71c4"))))
 '(clojure-interop-method-face ((t (:inherit italic :foreground "#b58900"))))
 '(clojure-keyword-face ((t (:inherit default :foreground "#585858"))))
 '(company-template-field ((t (:background "#EAE5CC" :foreground "black"))))
 '(company-tooltip ((t (:background "#0"))))
 '(font-lock-builtin-face ((t (:inherit default :foreground "LightSteelBlue3"))))
 '(font-lock-comment-face ((t (:inherit italic :foreground "azure4"))))
 '(font-lock-constant-face ((t (:inherit default :foreground "SlateBlue2"))))
 '(font-lock-doc-face ((t (:inherit font-lock-string-face :inherit italic :foreground "PaleVioletRed"))))
 '(font-lock-function-name-face ((t (:inherit default :inherit underline :foreground "DeepSkyBlue"))))
 '(font-lock-keyword-face ((t (:inherit bold :foreground "dodgerBlue"))))
 '(font-lock-preprocessor-face ((t (:inherit default :foreground "SpringGreen"))))
 '(font-lock-string-face ((t (:inherit italic :foreground "knobColor"))))
 '(font-lock-type-face ((t (:inherit default :foreground "MediumSeaGreen"))))
 '(font-lock-variable-name-face ((t (:inherit default :foreground "khaki"))))
 '(font-lock-warning-face ((t (:inherit italic :foreground "mistyrose"))))
 '(highlight ((t (:background "skyblue4"))))
 '(hl-line ((t (:background "gray10"))))
 '(italic ((t (:family "menlo" :slant italic))))
 '(mode-line ((t (:family "monaco" :height 90 :foreground "honeydew" :background "gray0"))))
 '(mode-line-inactive ((t (:inherit mode-line :foreground "gray45" :background "gray30"))))
 '(region ((t (:background "SkyBlue4"))))
 '(show-paren-match ((t (:inverse-video nil :background "#073642"))))
 '(show-paren-mismatch ((t (:background "dimgray" :foreground "firebrick1"))))
 '(slime-repl-inputed-output-face ((t (:inherit italic :foreground "knobColor"))))
 '(slime-repl-prompt-face ((t (:inherit bold :foreground "PaleVioletRed3"))))
 '(slime-repl-result-face ((t (:inherit default :foreground "selectedControlColor"))))
 '(variable-pitch ((t (:family "Tahoma")))))

(require 'cl)
(require 'package)

(setq package-archives
      '(("melpa" . "http://melpa.org/packages/")
        ("gnu"   . "http://elpa.gnu.org/packages/")))

;; '("marmalade" . "http://marmalade-repo.org/packages/"))

(package-initialize)
(add-to-list 'load-path
 (expand-file-name "~/.emacs.d/local/"))


;;; GLOBAL

(require 'session-init)
(require 'account)
(require 'mac)
(require 'git)
(require 'text)
(require 'theme)
(require 'backup)
(require 'recently)
(require 'fixme)
(require 'dired-init)
;; (require 'complete-ac)
(require 'complete-company)
; (require 'yas)
(require 'undo)
(require 'lisp)
(require 'slime-quicklisp)
(require 'clojure)
(require 'sql-lentz)
(require 'paredit-menu)
(require 'keyboard)
(require 'keypad)


(column-number-mode 1)
(line-number-mode t)
(set-default 'indent-tabs-mode nil)
(set-default 'indicate-empty-lines t)
(set-default 'imenu-auto-rescan t)
(defalias 'yes-or-no-p 'y-or-n-p)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(require 'markup)
(require 'org-lentz)
(require 'server-init)

;; (put 'downcase-region 'disabled nil)
;; (put 'upcase-region 'disabled nil)
