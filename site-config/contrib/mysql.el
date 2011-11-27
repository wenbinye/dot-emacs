;;; mysql.el --- mysql front-end

;; Copyright 2006 Ye Wenbin
;;
;; Author: wenbinye@163.com
;; Version: $Id: mysql.el,v 1.1.1.1 2007-03-13 13:16:10 ywb Exp $
;; Keywords: 
;; X-URL: not distributed yet

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'mysql)

;;; Code:

(provide 'mysql)
(eval-when-compile
  (require 'cl))

(defvar mysql-program "mysql")
(defvar mysql-user "root")
(defvar mysql-password "")
(defvar mysql-proc-name "mysql")
(defvar mysql-intrival 0.5)
(defvar mysql-timeout 5)

(defvar mysql-options '("-s")
  "-s to output prompt")

(defvar mysql-prompt-regexp "mysql> $"
  "prompt to judge whether the query is finish")
(defvar mysql-error-regexp "^ERROR [0-9]+ (.+?): ")

(defvar mysql-timer nil)
(defvar mysql-elapse-time 0)

(defvar mysql-proc-list nil)
(defvar mysql-process nil)

(defun mysql-connect (&optional user pwd db)
  (or user (setq user mysql-user))
  (or pwd (setq pwd mysql-password))
  (let ((args (append mysql-options
                      (if (> (length user) 0) (list "-u" user))
                      (if (> (length pwd) 0) (list "-p" pwd))
                      (if db (list db))))
        proc)
    (setq proc (apply 'start-process mysql-proc-name nil mysql-program args))
    (set-process-plist proc '(output nil counter 0 finished t))
    (set-process-filter proc 'mysql-filter)
    proc))

(defalias 'mysql-disconnect 'process-send-eof)

(defun mysql-filter (proc string)
  (if (string-match mysql-prompt-regexp string)
      (progn
        (process-put proc 'output
                 (concat (mysql-output proc) (substring string 0 (match-beginning 0))))
        (process-put proc 'finished t)
        (mysql-set-counter proc (1+ (mysql-counter proc))))
    (process-put proc 'output
                 (concat (mysql-output proc) string))))

(defsubst mysql-counter (proc)
  (process-get proc 'counter))

(defsubst mysql-set-counter (proc val)
  (process-put proc 'counter val))
               
(defsubst mysql-check-process (proc)
  (and (processp proc)
       (eq (process-status proc) 'run)))

(defsubst mysql-output (proc)
  (process-get proc 'output))

(defsubst mysql-cached-sql (proc)
  (process-get proc 'cached-sql))

(defsubst mysql-output-table (output)
  (when output
    (if (string-match mysql-error-regexp output)
        (error "%s" output)
      (mapcar (lambda (str) (split-string str "\t"))
              (butlast (split-string output "\n"))))))

(defsubst mysql-finished (proc)
  (process-get proc 'finished))

(defsubst mysql-elapse-time (proc)
  (process-get proc 'elapse-time))

(defsubst mysql-wait-to-count (proc count)
  (while (< (mysql-counter proc) count)
    (sit-for 0.1)))

(defun mysql-strip-sql (sql)
  (concat
   (replace-regexp-in-string ";*\\'" ""
                             (replace-regexp-in-string
                              "\n" "" sql))
   ";\n"))

;;; query with mysql-process
(defun mysql-query-1 (sql proc)
  (unless (mysql-finished proc)
    (error "Last query is not finished!"))
  (process-put proc 'finished nil)
  (if (mysql-check-process proc)
      (process-send-string proc sql)
    (error "Process is not run")))

(defun mysql-query (sql proc)
  "Query util get result. But if the time which mysql takes to process
the SQL longer than 0.5 sec, you will get no result.
If you are sure their is output, set WAIT non-nil"
  (process-put proc 'output nil)
  (setq sql (mysql-strip-sql sql))
  (mysql-query-1 sql proc)
  (while (not (mysql-finished proc))
    (sit-for 0.1))
  (mysql-output proc))

(defsubst mysql-query-table (sql proc)
  (mysql-output-table (mysql-query sql proc)))

(defun mysql-query-background (sql proc &optional func)
  "Query in background. When finish query, the FUNC will call to
process output"
  (process-put proc 'output nil)
  (if func (process-put proc 'after-query func))
  (setq sql (mysql-strip-sql sql))
  (mysql-query-1 sql proc)
  (process-put proc 'elapse-time 0)
  (setq mysql-proc-list (append mysql-proc-list (list proc)))
  (unless mysql-timer
    (setq mysql-timer
          (run-at-time nil mysql-intrival 'mysql-query-timer))))

(defun mysql-query-timer ()
  (dolist (proc mysql-proc-list)
    (let ((mysql-process proc)) ; install a global variable for after-query function    
      (if (mysql-finished proc)
          (progn
            (setq mysql-proc-list
                  (remove proc mysql-proc-list))
            (unwind-protect
                (funcall (process-get proc 'after-query))
              (process-put proc 'after-query nil)))
        (process-put proc 'elapse-time
                     (+ (mysql-elapse-time proc) mysql-intrival)))
      (unless (< (mysql-elapse-time proc) mysql-timeout)
        (if (yes-or-no-p "Query exceed timeout, Do you want to continue")
            (process-put proc 'elapse-time 0)
          (interrupt-process proc)
          (setq mysql-proc-list
                (remove proc mysql-proc-list))))))
  (when (null mysql-proc-list)
    (cancel-timer mysql-timer)
    (setq mysql-timer nil)))

(defun mysql-query-cache-start (proc)
  (process-put proc 'cached-sql ""))

(defun mysql-query-cached (sql proc)
  "Query util get result. But if the time which mysql takes to process
the SQL longer than 0.5 sec, you will get no result.
If you are sure their is output, set WAIT non-nil"
  (setq sql (mysql-strip-sql sql))
  (process-put proc 'cached-sql (concat
                                 (mysql-cached-sql proc)
                                 sql)))

(defun mysql-query-cache-end (proc &optional backgroud func)
  (if backgroud
      (mysql-query-background (mysql-cached-sql proc) proc func)
    (mysql-query (mysql-cached-sql proc) proc)))

;;; query with shell command
(defun mysql-shell-query (sql &optional db)
  (with-temp-buffer
    (apply 'call-process mysql-program nil t nil
           (append ;; -s option inhibit header in output
            (remove "-s" mysql-options)
            (if (> (length mysql-user) 0)
                (list "-u" mysql-user))
            (if db (list db))
            (if (> (length mysql-password) 0)
                (list "-p" mysql-password))
            (list "-e" sql)))
    (mysql-output-table (buffer-string))))

;;; functions
(defun mysql-table-header (table)
  (let ((i -1))
    (mapcar (lambda (col)
              (cons col (setq i (1+ i))))
            (car table))))

(defun mysql-col (table field)
  (with-mysql-table
    table
    (let ((col (mysql-col-number field)))
      (mapcar (lambda (row)
                (nth col row))
              table))))

(defsubst mysql-col-number (field)
  (if (numberp field)
      field
    (cdr (assoc field mysql-header))))

(defsubst mysql-cell (row field)
  (nth (mysql-col-number field) row))

(defmacro with-mysql-table (table &rest body)
  (declare (indent 0) (debug t))
  `(let ((mysql-header (mysql-table-header ,table)))
     ,@body))

(defsubst mysql-quote (str)
  (if (and str (stringp str))
      (format "'%s'" (replace-regexp-in-string "'" "''" str))
    "''"))

;;; mysql.el ends here
