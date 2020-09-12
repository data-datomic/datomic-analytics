
(require 'session)
(add-hook 'after-init-hook 'session-initialize)

(setq session-save-file (expand-file-name "~/tmp/session.el"))

(provide 'session-init)
