

;; Backup Directory

(add-to-list 'backup-directory-alist '("." . "~/tmp/"))
;; (setq version-control t)


;; Autosave

(defvar autosave-dir  (expand-file-name "~/tmp/autosave/"))
(make-directory autosave-dir t)
(setq auto-save-file-name-transforms
      `(("\\(?:[^/]*/\\)*\\(.*\\)" ,(concat autosave-dir "\\1") t)))

;; Auto Revert

(autoload 'auto-revert-mode "autorevert" nil t)
(autoload 'turn-on-auto-revert-mode "autorevert" nil nil)
(autoload 'global-auto-revert-mode "autorevert" nil t)
(global-auto-revert-mode t)

(provide 'backup)
