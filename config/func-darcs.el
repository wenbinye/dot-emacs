;;;###autoload
(defun darcs-add-changelog ()
  "Read darcs changelog from _darcs/inventory, and add them to
ChangeLog in current directory."
  (interactive)
  (unless (file-exists-p "_darcs/inventory")
    (error "No _darcs/inventory file! Not in a darcs project?"))
  (let ((standard-output (find-file-noselect "ChangeLog"))
        (system-time-locale "POSIX")
        start-mark header time-string)
    (with-current-buffer standard-output
      (goto-char (point-min))
      (setq start-mark (point-min-marker))
      (unwind-protect
          (progn
            (if (re-search-forward
                 ;; match Thu Jul  2 22:29:30 CST 2007
                 (concat
                  "[a-z]\\{3\\} [a-z]\\{3\\} [0-9 ]\\{2\\}"
                  " [0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}"
                  " CST [0-9]\\{4\\}") nil t)
                (setq time-string
                      (format-time-string "%Y%m%d%H%M%S"
                                          (seconds-to-time
                                           (- (time-to-seconds
                                               (date-to-time
                                                (match-string 0)))
                                              79200)))))
            (goto-char (point-min))
            (with-temp-buffer
              (insert-file-contents "_darcs/inventory")
              (when time-string
                (if (re-search-forward (concat "\\*\\*" time-string)
                                       nil t)
                    (if (looking-at "]\\s-*")
                        (goto-char (match-end 0))
                      (re-search-forward "^]\\s-*"))
                  (error "No changelog timestamp %s in inventory"
                         time-string)))
              (if (eobp)
                  (message "No changes!")
                (while (not (eobp))
                  (forward-char 1)
                  (setq header
                        (buffer-substring
                         (point) (progn (forward-line 1) (1- (point)))))
                  (if (re-search-forward "\\*\\*\\(20[0-9]\\{12\\}\\)" nil t)
                      (progn
                        (setq time-string (match-string 1))
                        (princ (format-time-string
                                "%a %b %e %T CST %Y  "
                                (apply 'encode-time
                                       (nconc
                                        (nreverse
                                         (mapcar
                                          (lambda (start)
                                            (string-to-number
                                             (substring time-string start (+ start 2))))
                                          (number-sequence 4 12 2)))
                                        (list (string-to-number (substring time-string 0 4)))
                                        (current-time-zone)))))
                        (princ (buffer-substring (line-beginning-position) (match-beginning 0)))
                        (princ "\n") (princ "  * ") (princ header) (princ "\n")
                        (if (looking-at "]\\s-*\n")
                            (forward-line 1)
                          (forward-line 1)
                          (while (not (looking-at "^]\\s-*\n"))
                            (princ " ")
                            (princ (buffer-substring (point)
                                                     (progn (forward-line 1) (point)))))
                          (goto-char (match-end 0)))
                        (princ "\n")
                        (with-current-buffer standard-output
                          (goto-char (point-min))))
                    (goto-char (point-max))))
                (display-buffer standard-output))))
        (delete-region (point-min) start-mark)))))
