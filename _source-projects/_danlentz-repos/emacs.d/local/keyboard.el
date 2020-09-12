
(defun remap-parens ()
  "Swap the () and [] keys in emacs."
  (interactive)
  (or keyboard-translate-table
      (progn
        ;; First make a translate table that does the identity translation.
        (setq keyboard-translate-table (make-string 128 0))
        (let ((i 0))
          (while (< i 128)
            (aset keyboard-translate-table i i)
            (setq i (1+ i))))))
  (aset keyboard-translate-table ?\( ?\{)
  (aset keyboard-translate-table ?\) ?\})
  ;; (aset keyboard-translate-table ?\( ?\[)
  ;; (aset keyboard-translate-table ?\) ?\])
  (aset keyboard-translate-table ?\[ ?\()
  (aset keyboard-translate-table ?\] ?\))
  (aset keyboard-translate-table ?\{ ?\[)
  (aset keyboard-translate-table ?\} ?\]))

(defun unmap-parens ()
  "Restore the position of the () and [] keys in emacs."
  (interactive)
  (aset keyboard-translate-table ?\( ?\()
  (aset keyboard-translate-table ?\) ?\))
  (aset keyboard-translate-table ?\{ ?\{)
  (aset keyboard-translate-table ?\} ?\})
  (aset keyboard-translate-table ?\[ ?\[)
  (aset keyboard-translate-table ?\] ?\]))

(remap-parens)

(require 'which-key)

;; (which-key-setup-side-window-right)
;; (which-key-setup-side-window-bottom)

(which-key-mode)

(provide 'keyboard)
