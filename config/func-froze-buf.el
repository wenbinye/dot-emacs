;;{{{ froze and thaw buffer
;;;###autoload
(defun ywb-frozen-to-file (file no-confirm)
  (interactive (list
                (read-file-name "Frozen buffer to file: "
                                nil buffer-file-name)
                current-prefix-arg))
  (let ((buf (current-buffer))
        (mode major-mode))
    (with-temp-buffer
      (setq major-mode mode)
      (insert-buffer-substring buf)
      (ywb-thaw-buffer)
      (write-file file (not no-confirm)))))

(defun ywb-thaw-buffer ()
  (setq ywb-thaw-content
        (cons (point)
              (delete-and-extract-region (point-min) (point-max))))
  (insert (format "#### ywb-frozen-buffer: %S ####\n" major-mode))
  (insert (format "%S" (cdr ywb-thaw-content))))

(defvar ywb-thaw-content nil)
;;;###autoload
(defun ywb-thaw-file-mode ()
  (let ((mode (intern-soft (match-string 1)))
        (pos (point)))
    (text-mode)
    (delete-region (point) (progn (forward-line 1) (point)))
    (insert (read (delete-and-extract-region (point-min) (point-max))))
    (and (fboundp mode) (funcall mode))
    (goto-char pos)
    (make-local-variable 'write-file-functions)
    (make-local-variable 'after-save-hook)
    (add-hook 'write-file-functions 'ywb-thaw-buffer)
    (add-hook 'after-save-hook
              (lambda ()
                (erase-buffer)
                (insert (cdr ywb-thaw-content))
                (goto-char (car ywb-thaw-content))
                (set-buffer-modified-p nil)
                (setq ywb-thaw-content nil)))
    (set-buffer-modified-p nil)))

(defun ywb-thaw-file (file)
  (interactive "fFile to open: ")
  (let ((bufs (buffer-list)))
    (with-current-buffer (find-file-noselect file)
      (unless (member (current-buffer) bufs) ; if not opened
        (save-excursion
          (goto-char (point-min))
          (if (looking-at "^#### ywb-frozen-buffer: \\(.*?-mode\\) ####$")
              (ywb-thaw-file-mode)
            (message "Not a frozen file!"))))
      (switch-to-buffer (current-buffer)))))
;;}}}
