
(defun show-fixme-lines (&optional arg)
  "Emphasise TODO comments. If ARG is positive, enable highlighting.
  If ARG is negative, disable highlighting. Otherwise, toggle highlighting."
    (interactive)
    (if (or (and (not arg) (assoc "TODO" hi-lock-interactive-patterns))
  	  (and arg (minusp arg)))
        (unhighlight-regexp "TODO:")
      (highlight-phrase "TODO:" 'fixme-face))
    (if (or (and (not arg) (assoc "FIXME:" hi-lock-interactive-patterns))
  	  (and arg (minusp arg)))
        (unhighlight-regexp "FIXME:")
      (highlight-phrase "FIXME:" 'fixme-face)))

(show-fixme-lines 1)

;; TODO: xxx
;; FIXME: xxx

(provide 'fixme)
