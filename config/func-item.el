;; insert-item
;;;###autoload
(defun ywb-insert-item ()
  (interactive)
  (let (curr next)
    (beginning-of-line)
    (cond ((looking-at "\\(\\s-*\\)\\([0-9]+\\)\\.\\s-*")
           (setq curr (string-to-number (buffer-substring (match-beginning 2)
                                                          (match-end 2))))
           (setq next (number-to-string (1+ curr)))
           (end-of-line)
           (insert "\n" (buffer-substring (match-beginning 1)
                                          (match-end 1))
                   next ". ")
           (ywb-sync-item))
          ((looking-at "\\s-*[-+]\\s-*")
           (progn
             (end-of-line)
             (insert "\n" (buffer-substring (match-beginning 0)
                                            (match-end 0)))))
          (t
           (progn
             (end-of-line)
             (newline-and-indent))))))
;;;###autoload
(defun ywb-sync-item ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (looking-at "\\(\\s-*\\)\\([0-9]+\\)\\.\\s-*")
        (let ((curr (string-to-number (buffer-substring (match-beginning 2)
                                                        (match-end 2))))
              (blank1 (buffer-substring (match-beginning 1)
                                        (match-end 1)))
              (blank2 (buffer-substring (match-end 2)
                                        (match-end 0))))
          (while (progn
                   (beginning-of-line 2)
                   (looking-at "\\s-*[0-9]+\\.\\s-*"))
            (setq curr (1+ curr))
            (delete-region (match-beginning 0) (match-end 0))
            (insert blank1 (number-to-string curr) blank2))))))
