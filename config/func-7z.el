(defun archive-find-type ()
  (widen)
  (goto-char (point-min))
  ;; The funny [] here make it unlikely that the .elc file will be treated
  ;; as an archive by other software.
  (let (case-fold-search)
    (cond
     ((looking-at "7z") '7z)
     ((looking-at "[P]K\003\004") 'zip)
     ((looking-at "..-l[hz][0-9ds]-") 'lzh)
     ((looking-at "....................[\334]\247\304\375") 'zoo)
     ((and (looking-at "\C-z")          ; signature too simple, IMHO
           (string-match "\\.[aA][rR][cC]$"
                         (or buffer-file-name (buffer-name))))
      'arc)
     ;; This pattern modelled on the BSD/GNU+Linux `file' command.
     ;; Have seen capital "LHA's", and file has lower case "LHa's" too.
     ;; Note this regexp is also in archive-exe-p.
     ((looking-at "MZ\\(.\\|\n\\)\\{34\\}LH[aA]'s SFX ") 'lzh-exe)
     (t (error "Buffer format not recognized")))))

;; -------------------------------------------------------------------------
;; Section: 7z Archives
(defun archive-7z-summarize ()
  (let ((ofile (buffer-file-name))
        files file sum col)
    (with-temp-buffer
      (call-process "7z" nil t nil "l" ofile)
      (goto-char (point-min))
      (if (re-search-forward "^Listing archive:" nil t)
          (progn
            (forward-line 2)
            (setq sum (buffer-substring (point) (point-max)))
            (re-search-forward "Name")
            (setq col (- (current-column) 4))
            (re-search-forward "^-[-]+")
            (forward-line 1)
            (while (not (looking-at "^-[-]+"))
              (forward-char col)
              (setq file (buffer-substring (point)
                                           (progn (forward-line 1)
                                                  (1- (point)))))
              (push (vector file file nil #o644) files)))
        (error "Not a valid 7z archive file")))
    (goto-char (point-min))
    (insert sum)
    (save-excursion
      (forward-line -2)
      (setq archive-file-list-end (point-marker))
      (goto-char (point-min))
      (forward-line 2)
      (setq archive-file-list-start (point-marker)
            archive-file-name-indent 0))
    (apply 'vector (nreverse files))))

(defun archive-7z-extract (archive name)
  (call-process "7z" nil t nil "x" archive name "-so")
  (goto-char (point-min))
  (re-search-forward "^Extracting  ")
  (forward-char (length name))
  (delete-region (point-min) (point))
  (goto-char (point-max))
  (forward-line -2)
  (delete-region (point) (point-max))
  t)

