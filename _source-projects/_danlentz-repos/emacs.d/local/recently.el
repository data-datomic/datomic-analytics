
(require 'recentf)

(setq recentf-save-file (expand-file-name "~/tmp/recentf.el"))
(setq recentf-max-saved-items 256)
(setq recentf-max-menu-items 24)
(setq recentf-menu-open-all-flag t)
(recentf-mode t)

(provide 'recently)
