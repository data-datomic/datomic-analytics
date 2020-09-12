

(require 'undo-tree)
(undo-tree-mode t)

(require 'redo+)

(global-set-key (kbd "<kp-multiply>") 'undo-tree-visualizer-mode)
(global-set-key (kbd "<kp-0>") 'undo-tree-undo)
(global-set-key (kbd "C-<kp-decimal>") 'undo-tree-redo)
(global-set-key (kbd "<kp-decimal>") 'redo)

(global-set-key (kbd "<kp-equal>") 'undo-tree-save-state-to-register)
(global-set-key (kbd "<kp-divide>") 'undo-tree-restore-state-from-register)


(provide 'undo)
