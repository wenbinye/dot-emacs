;;{{{ sql help function: query syntax, switch buffer
;; query syntax
(require 'file-stat nil t)
(defvar sql-mysql-syntax-file "/home/ywb/.emacs.d/mysql-syntax.txt")
(defvar sql-mysql-syntax-index nil "")

(defun sql-mysql-read-syntax ()
  (interactive)
  (setq sql-mysql-syntax-index nil)
  (when (fboundp 'file-stat-mtime)
    (put 'sql-mysql-syntax-file 'mtime
         (file-stat-mtime sql-mysql-syntax-file)))
  (with-temp-buffer
    (insert-file-contents sql-mysql-syntax-file)
    (while (re-search-forward "^[*] \\(.*\\) Syntax$" nil t)
      (setq sql-mysql-syntax-index
            (cons (cons (match-string 1)
                        (progn (forward-line 2)
                               (point)))
                  sql-mysql-syntax-index))))
  (setq sql-mysql-syntax-index (nreverse sql-mysql-syntax-index)))

;;;###autoload 
(defun sql-mysql-query-syntax (word)
  "Query SQL syntax."
  (interactive (list
                (progn
                  (unless sql-mysql-syntax-index (sql-mysql-read-syntax))
                  (completing-read
                   "Query syntax of: "
                   sql-mysql-syntax-index nil t))))
  (when (and (fboundp 'file-stat-mtime)
             (time-less-p (get 'sql-mysql-syntax-file 'mtime)
                          (file-stat-mtime sql-mysql-syntax-file)))
    (message "Syntax file has change. Reread from file...")
    (sql-mysql-read-syntax))
  (with-current-buffer (get-buffer-create " *MySQL-doc*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert
       (with-temp-buffer
         (insert-file-contents sql-mysql-syntax-file)
         (goto-char (cdr (assoc word sql-mysql-syntax-index)))
         (buffer-substring (point)
                           (progn (if (re-search-forward "^[*] " nil t)
                                      (forward-char -2)
                                    (goto-char (point-max)))
                                  (point)))))
      (display-buffer (current-buffer))
      (setq buffer-read-only t))))

;;;###autoload 
(defun sql-mysql-current-syntax ()
  "Query SQL command in current line"
  (interactive)
  (unless sql-mysql-syntax-index (sql-mysql-read-syntax))
  (let (word)
    (save-excursion
      (comint-bol)
      (setq word (upcase (current-word)))
      (unless (assoc word sql-mysql-syntax-index)
        (forward-word)
        (setq word (upcase (concat word " " (current-word))))
        (unless (assoc word sql-mysql-syntax-index)
          (setq word nil)))
      (sql-mysql-query-syntax
       (completing-read
        "Query syntax of: "
        sql-mysql-syntax-index nil t word)))))

;; swith buffer
(defun sql-mysql-switch-buffer (&optional arg)
  (interactive "P")
  (if (get-buffer sql-buffer)
      (if arg
          (display-buffer sql-buffer)
        (pop-to-buffer sql-buffer))
    (error "No current process.")))
;;}}}
