;;{{{ toggle char case
(defun ywb-toggle-case-char (char)
  (cond ((and (> char 64) (< char 91)) (downcase char))
        ((and (> char 96) (< char 123)) (upcase char))
        (t char)))
;;;###autoload 
(defun ywb-toggle-case-region (beg end)
  (interactive "r")
  (save-excursion
    (goto-char beg)
    (while (and (not (eobp)) (< (point) end))
      (insert-char (ywb-toggle-case-char (char-after)) 1)
      (delete-char 1))))
;;;###autoload 
(defun ywb-toggle-case-dwim (arg)
  (interactive "p")
  (let ((start (point))
        (end (+ (point) arg)))
    (if (and mark-active transient-mark-mode)
        (setq start (region-beginning)
              end (region-end)))
    (ywb-toggle-case-region start end)
    (goto-char end)))
;;}}}
