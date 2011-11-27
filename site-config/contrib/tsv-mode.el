;;; tsv-mode.el --- Major mode for edit table files

;; Copyright 2006 Ye Wenbin
;;
;; Author: wenbinye@163.com
;; Time-stamp: <Ye Wenbin 2007-12-07 21:30:07>
;; Version: $Id: tsv-mode.el,v 1.1.1.1 2007-03-13 13:16:10 ywb Exp $
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

;; This mode is not stable. Do backup file if you visit important data.

;; Put this file into your load-path and the following into your ~/.emacs:
;; (add-to-list 'load-path "/path/to/lib")
;; (autoload 'tsv-mode "tsv-mode" "A mode to edit table like file" t)
;; (autoload 'tsv-normal-mode "tsv-mode" "A minor mode to edit table like file" t)

;; Rename column: if `tsv-has-header-p', rename the column to a not
;; duplicate name. Also `tsv-first-line-as-header' have to check the
;; column names. xo

;; TODO:
;; 1. add some column command, such as move, add, delete.
;; 2. enable undo
;; 3. enable formula (this may need more time)
;; 
;;;_* Code:

(provide 'tsv-mode)
(eval-when-compile
  (require 'cl)
  (autoload 'format-spec "format-spec")
  (autoload 'format-spec-make "format-spec"))

(defgroup TSV nil
  "Major mode for editing tab separate files"
  :group 'convenience)

(defvar tsv-separator-list '("\t" ":" "," ";" " "))
(defvar tsv-comment-char "#"
  "Comment char for read file")
(defvar tsv-col-separator " " "Column is separated by this string")
(defvar tsv-elide-string ">")
(defvar tsv-elide-face 'italic)
(defvar tsv-write-annotation t
  "When not-nil, Insert the mode line to the file")
(defvar tsv-show-field-format "%d %n: %c\t%s"
  "Format specific as following:
%d -- real line number
%l -- current line number
%i -- index of the column
%n -- name of column
%s -- content of the field
%c -- content of the id column")
(defvar tsv-minmum-width 3
  "Minmum width for each column when read")
(defvar tsv-maxmum-width 20
  "Maxmum width for each column when read")

(defvar tsv-original-comment nil
  "comment lines in file")
(defvar tsv-id-column nil
  "A id column for display or other operation")
(defvar tsv-separator-char nil
  "Separate character for table file")
(defvar tsv-formatter nil
  "The CAR part of the element is the index of the column, and CDR
part is the column width")
(defvar tsv-column nil
  "A column index")
(defvar tsv-has-header-p nil)
(defvar tsv-table nil)
(defvar tsv-header nil
  "A list of column information.
 (COLUMN-NAME INDEX CURR-WIDTH MIN-WIDTH MAX-WIDTH AVER-WIDTH SHOW)
The column can have three state: SHOW, HIDE, DELETE. Only the columns
in SHOW state are displayed.")
(defvar tsv-real-header nil
  "For stringfy and write.

TSV has three type header, `tsv-header' contain the header when read
from the file. `tsv-real-header' contain the header that to write into
file, this header only contain the index information of `tsv-header',
so other column information is not update when you operate the table.
Only add, move, delete column will affect this variable. The function
`tsv-real-header' return a header contain current header information
and rearrage the index of column in according to `tsv-real-header'.
")

(defvar tsv-normal-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map global-map)
    (define-key map "\C-c\C-c" (lambda () (interactive) (tsv-normal-mode -1)))
    (define-key map "\t" 'tsv-normal-next-field)
    (define-key map (kbd "<backtab>") 'tsv-normal-prev-field)
    (define-key map (kbd "\C-e") 'tsv-normal-end-of-field/line)
    (define-key map (kbd "\C-a") 'tsv-normal-beginning-of-field/line)
    (define-key map (kbd "\C-v") 'tsv-scroll-up)
    (define-key map (kbd "\M-v") 'tsv-scroll-down)
    map)
  "Keymap for `tsv-normal-mode'")

(defvar tsv-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-v" 'tsv-scroll-up)
    (define-key map " " 'tsv-scroll-up)
    (define-key map "\M-v" 'tsv-scroll-down)
    (define-key map (kbd "DEL") 'tsv-scroll-down)
    (define-key map "j" 'next-line)
    (define-key map "k" 'previous-line)
    (define-key map "g" 'tsv-goto-line)
    (define-key map "\t" 'tsv-next-field)
    (define-key map (kbd "<backtab>") 'tsv-prev-field)
    (define-key map ">" 'tsv-widen-column)
    (define-key map "<" 'tsv-narrow-column)
    (define-key map (kbd "C-c C-<") 'tsv-narrow-to-min)
    (define-key map (kbd "C-c C->") 'tsv-widen-to-max)
    (define-key map "\C-c\C-n" 'tsv-set-column-width)
    (define-key map "\C-c\C-w" 'tsv-set-all-column-width)
    ;; (define-key map (kbd "C-<") 'tsv-move-column-left)
    ;; (define-key map (kbd "C->") 'tsv-move-column-right)
    (define-key map "s" 'tsv-show-field-at-point)
    (define-key map "w" 'tsv-copy-field-at-point)
    (define-key map "\C-m" 'tsv-edit-field-at-point)
    (define-key map "\C-k" 'tsv-kill-line)
    (define-key map "\C-y" 'tsv-yank)
    (define-key map "\C-w" 'tsv-kill-region)
    (define-key map "\M-w" 'tsv-kill-ring-save)
    (define-key map "H" 'tsv-hide-column)
    (define-key map "S" 'tsv-show-column)
    (define-key map "Q" 'tsv-exit)
    (define-key map "F" 'tsv-toggle-header-line)
    (define-key map "\C-c\C-e" 'tsv-normal-mode)
    (define-key map "\C-c\C-l" 'tsv-sort-lines-region)
    (define-key map "\C-c\C-s" 'tsv-sort-column)
    (define-key map "\C-c\C-u" 'tsv-revert-with-separator)
    (define-key map "\C-xnn" 'tsv-narrow-to-region)
    (define-key map "\C-xnw" 'tsv-widen)
    map)
  "Keymap for `tsv-mode'")

(put 'tsv-separator-char 'safe-local-variable 'stringp)
(put 'tsv-header 'safe-local-variable 'listp)
(put 'tsv-comment-char 'safe-local-variable 'stringp)
(put 'tsv-has-header-p 'safe-local-variable 'booleanp)

;;;###autoload 
(define-derived-mode tsv-mode nil "TSV"
  "Major mode for edit or view tsv file. If change major mode, don't
change it directly."
  (make-local-variable 'tsv-formatter)
  (make-local-variable 'tsv-id-column)
  (make-local-variable 'tsv-column)
  (make-local-variable 'tsv-write-annotation)
  (make-local-variable 'tsv-separator-char)
  (make-local-variable 'tsv-table)
  (set (make-local-variable 'tsv-original-comment) nil)
  (set (make-local-variable 'tsv-header) nil)
  (set (make-local-variable 'tsv-real-header) nil)
  (set (make-local-variable 'tsv-has-header-p) nil)
  (tsv-read)
  (setq buffer-read-only t)
  (setq buffer-undo-list t)
  (make-local-variable 'buffer-file-format)
  (add-to-list 'buffer-file-format 'tsv)
  (setq truncate-lines t))
               
(defun tsv-revert-with-separator (sep)
  (interactive "cWhich separator: ")
  (setq tsv-header nil
        tsv-has-header-p nil
        header-line-format nil)
  (unwind-protect
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert-file-contents-literally buffer-file-name)
        (tsv-read sep))
    (setq inhibit-read-only nil)))

;;;_. functions

;; detect separator:
;; if file has more than one line, accept the seperator which
;; split the line to same length.
;; else find the first which split line with more than one
;; column
(defun tsv-detect-separator ()
  "Detect `tsv-separator-char'"
  (save-excursion
    (let ((seps tsv-separator-list)
          (i 0)
          sep done line1 line2 len)
      (setq line1 (buffer-substring-no-properties
                   (line-beginning-position)
                   (line-end-position))
            line2 (progn
                    (forward-line 1)
                    (buffer-substring-no-properties
                     (point)
                     (line-end-position))))
      (if (string< "" line2)
          (while (and (not done) seps)
            (setq sep (car seps)
                  seps (cdr seps)
                  len1 (length (split-string line1 sep))
                  len2 (length (split-string line2 sep)))
            (if (and (= len1 len2) (> len1 1))
                (setq done t)))
        (while (and (not done) seps)
          (setq sep (car seps)
                seps (cdr seps))
          (if (> (length (split-string line1 sep)) 1)
              (setq done t))))
      (if done sep (car tsv-separator-list)))))

;;;_ , macros
(defmacro save-buffer-modified-p (&rest body)
  "Execute BODY, preserving buffer modified flag."
  `(let ((modified-p (buffer-modified-p)))
     (progn ,@body)
     (set-buffer-modified-p modified-p)))

(defmacro save-tsv-position (&rest body)
  "Execute BODY, preserving line position."
  `(let ((line (line-number-at-pos))
         (col (current-column)))
     (progn ,@body)
     (goto-line line)
     (move-to-column col)))

;;;_ , about column
(defsubst tsv-col-name (col &optional arg)
  (if arg
      (aset col 0 arg)
    (aref col 0)))
(defsubst tsv-col-index (col &optional arg)
  (if arg
      (aset col 1 arg)
    (aref col 1)))
(defsubst tsv-col-current-width (col &optional arg)
  (if arg
      (aset col 2 arg)
    (aref col 2)))
(defsubst tsv-col-min-width (col &optional arg)
  (if arg
      (aset col 3 arg)
    (aref col 3)))
(defsubst tsv-col-max-width (col &optional arg)
  (if arg
      (aset col 4 arg)
    (aref col 4)))
(defsubst tsv-col-average-width (col &optional arg)
  (if arg
      (aset col 5 arg)
    (aref col 5)))
(defsubst tsv-col-state (col &optional arg)
  (if arg
      (aset col 6 arg)
    (aref col 6)))
  
(defsubst tsv-col-show-p (col &optional arg)
  (eq (tsv-col-state col) 'show))
(defsubst tsv-col-del-p (col &optional arg)
  (eq (tsv-col-state col) 'delete))
(defsubst tsv-col-hide-p (col)
  "Indicate whether the column is hide"
  (eq (tsv-col-state col) 'hide))

(defsubst tsv-cols (&optional predicate)
  "The column names current displaying"
  (mapcar 'tsv-col-name
          (if predicate
              (remove-if-not predicate tsv-header)
            (remove-if 'tsv-col-del-p tsv-header))))

(defun tsv-col-find-index (name)
  "Return the index of the column with name NAME"
  (let ((cols tsv-header) index col)
    (while cols
      (setq col (car cols)
            cols (cdr cols))
      (if (string= (tsv-col-name col) name)
          (setq index (tsv-col-index col)
                cols nil)))
    index))

;;;_ , about rows
(defsubst tsv-delete-line ()
  "Delete current line"
  (delete-region (line-beginning-position) (1+ (line-end-position))))

(defun tsv-line-content ()
  "Return `split-string' result of the current line with separator `tsv-separator-char'."
  (split-string (buffer-substring-no-properties
                 (line-beginning-position)
                 (line-end-position))
                tsv-separator-char))

(defsubst tsv-line-number-at-pos (&optional pos)
  (or pos (setq pos (point)))
  (save-restriction
    (widen)
    (1- (line-number-at-pos pos))))

(defun tsv-row-set (row n elm)
  "Set the Nth element of ROW to elm. Adjust max or min column width
if necessary"
  (if (< n (length row))
      (let ((nrow (length tsv-table))
            (pos (nthcdr n row))
            (col (nth n tsv-header))
            (len (length elm))
            old oldlen)
        (setq old (car pos)
              oldlen (length pos))
        (unless (string= old elm)
          (set-buffer-modified-p t)
          ;; this may not exactly, if you modify the max or min record 
          (tsv-col-max-width col (max (tsv-col-max-width col) len))
          (tsv-col-min-width col (min (tsv-col-min-width col) len))
          (tsv-col-average-width col (/ (+ (- (* nrow (tsv-col-average-width col)) oldlen) len) nrow))
          (setcar pos elm)))
    (error "Args %d out of range: %S" n row)))

(defun tsv-update-at-point ()
  "Redisplay the line at point"
  (save-buffer-modified-p
   (let ((col (current-column))
         (row (tsv-row-at-point))
         (inhibit-read-only t))
     (tsv-delete-line)
     (tsv-format-line row)
     (forward-line -1)
     (move-to-column col))))

(defun tsv-rows-in-region (beg end)
  "All rows in the region"
  (let (rows)
    (save-excursion
      (goto-char beg)
      (while (< (point) end)
        (setq rows (cons (tsv-row-at-point) rows))
        (forward-line 1)))
    (nreverse rows)))

;;;_ , about format
(defun tsv-format-buffer ()
  "Insert contents in `tsv-table' to buffer"
  (interactive)
  (message "wait for formating...")
  (save-buffer-modified-p
   (save-tsv-position
    (let ((inhibit-read-only t))
      (erase-buffer)
      (mapc 'tsv-format-line tsv-table))))
  (message "format done!"))

(defun tsv-redisplay-table ()
  "Redisplay the rows in current buffer"
  (message "wait for formating...")
  (let (row)
    (save-buffer-modified-p
     (save-tsv-position
      (let ((inhibit-read-only t))
        (goto-char (point-min))
        (while (not (eobp))
          (setq row (tsv-row-at-point))
          (delete-region (line-beginning-position)
                         (1+ (line-end-position)))
          (tsv-format-line row))))))
  (message "format done!"))

(defsubst tsv-header-line ()
  "Format header line"
  (concat
   (make-string (if scroll-bar-mode 4 2) 32)
   (mapconcat
    (lambda (frt)
      (tsv-format-string (tsv-col-name (nth (car frt) tsv-header))
                         (cdr frt)))
    tsv-formatter tsv-col-separator)))
      
(defun tsv-compile-format ()
  "Compile `tsv-real-header'. This function set `tsv-column',
`tsv-formatter', header-line-format."
  (let (formatter (len 0) (col 0) (i 0) j)
    (setq tsv-formatter nil)
    (dolist (col tsv-real-header)
      (if (tsv-col-show-p col)
          (setq formatter (append formatter (list
                                             (cons
                                              (tsv-col-index col)
                                              (tsv-col-current-width col))))
                len (+ len (tsv-col-current-width col)))))
    (setq tsv-formatter formatter
          header-line-format (tsv-header-line)
          len (+ len (* (length tsv-col-separator) (- (length formatter) 1)))
          tsv-column (make-vector len 0))
    (dolist (frt formatter)
      (setq col (1+ col))
      (aset tsv-column i col)
      (dotimes (v (1- (cdr frt)))
        (aset tsv-column (+ i v 1) (- -1 v)))
      (setq i (+ i (cdr frt)))
      (when (< i len)
        (setq j (length tsv-col-separator))
        (aset tsv-column i 0)
        (dotimes (v (1- j))
          (aset tsv-column (+ i v 1) (- -1 v)))
        (setq i (+ i j))))))

(defun tsv-real-header ()
  "Return the header for write"
  (let ((header (apply 'vector
                       (mapcar 'copy-sequence tsv-header)))
        (i 0)
        col new)
    (dolist (h tsv-real-header)
      (setq col (aref header (tsv-col-index h)))
      (tsv-col-index col i)
      (setq i (1+ i))
      (setq new (cons col new)))
    (nreverse new)))

(defun tsv-format-string (str col)
  "If STR longer than COL, substitude exceed charactors with
`tsv-elide-string'. Other wise use blank to fill it.
"
  (if (> (length str) col)
      (propertize
       (concat (substring str 0 (- col (length tsv-elide-string)))
               tsv-elide-string)
       'face tsv-elide-face)
    (format (concat "%-" (number-to-string col) "s") str)))

(defsubst tsv-stringfy-row (row)
  "Convert ROW to raw format."
  (mapconcat (lambda (col)
               (nth (tsv-col-index col) row))
             tsv-real-header
             tsv-separator-char))

(defsubst tsv-format-line (row)
  "Convert ROW to the format to be display"
  (insert
   (propertize
    (concat
     " "
     (mapconcat (lambda (frt)
                  (tsv-format-string (nth (car frt) row)
                                     (cdr frt)))
                tsv-formatter tsv-col-separator) "\n")
    'row row)))

;;;_ , about list
(defun tsv-delete-list (list dels)
  "Delete DELS from LIST. DELS should have the same order as in the
LIST."
  (let (new)
    (dolist (elm list)
      (if (eq elm (car dels))
          (setq dels (cdr dels))
        (setq new (cons elm new))))
    (nreverse new)))

(defun tsv-splice-list (list from to replace)
  "Replace the list from FROM to TO with REPLACE by side effect. If FROM is 0,
the REPLACE is NOT done, therefore write
 `(setq foo (tsv-splice-list foo from to replace))'
to be sure of change the value of foo.
"
  (cond ((< from 0) (error "Args out of range"))
        ((= from 0) (append replace (nthcdr to list)))
        (t (setcdr (nthcdr (1- from) list) (append replace (nthcdr to list)))
           list)))

;;;_ , about read and write
(defun tsv-scan-table ()
  "Scan table to get max, min and average length of every column"
  (let ((tlen (make-vector (length tsv-header) 0))
        (nrow 0.0)
        tmph tmp len i)
    (dolist (row tsv-table)
      (setq tmph (cdr tsv-header)
            tmp (car tsv-header)
            i 0
            nrow (1+ nrow))
      (mapc (lambda (col)
              (setq len (length col))
              (tsv-col-min-width tmp (min (tsv-col-min-width tmp)
                                          len))
              (tsv-col-max-width tmp (max (tsv-col-max-width tmp)
                                          len))
              (aset tlen i (+ (aref tlen i) len))
              (setq i (1+ i)
                    tmp (car tmph)
                    tmph (cdr tmph)))
            row))
    (mapl (lambda (col len)
            (tsv-col-average-width (car col)
                                   (/ (car len) nrow)))
          tsv-header (append tlen nil))))

(defun tsv-write (from to buf)
  "Write the table to file or BUF. If `tsv-write-annotation' is
non-nil, the file variables will record at first line."
  (save-restriction
    (narrow-to-region from to)
    (erase-buffer)
    (insert
     (with-current-buffer buf
       (let ((header (tsv-real-header)))
         (concat
          (when tsv-write-annotation
            (format (concat "%s -*- mode: tsv; tsv-header: %S;"
                            " tsv-separator-char: %S; tsv-comment-char: %S;"
                            " tsv-has-header-p: %S -*-\n")
                    tsv-comment-char header
                    tsv-separator-char tsv-comment-char
                    tsv-has-header-p))
          tsv-original-comment
          (when tsv-has-header-p
            (format "%s\n"
                    (mapconcat 'tsv-col-name header tsv-separator-char)))
          (mapconcat 'tsv-stringfy-row tsv-table "\n")
          "\n"))))
    (point-max)))

(add-to-list 'format-alist
             '(tsv "Table mode"
                   ". -\\*- mode: tsv; tsv-hader:"
                   nil tsv-write t nil))

(defun tsv-read (&optional sep)
  (unwind-protect
      (let ((inhibit-read-only t)
            (i 0)
            table-start len row)
        (setq tsv-table nil)
        (save-buffer-modified-p
         (save-excursion
           (hack-local-variables)
           (goto-char (point-min))
           (if (looking-at ". -\\*- mode: tsv") (forward-line))
           (while (looking-at (concat "^" (regexp-quote tsv-comment-char)))
             (forward-line 1))
           (setq table-start (point-marker)
                 tsv-separator-char (if sep (string sep) (tsv-detect-separator)))
           (if (null tsv-header)
               (setq tsv-header (mapcar (lambda (col)
                                          (setq i (1+ i)
                                                len (length col))
                                          (if (< len tsv-minmum-width)
                                              (setq len tsv-minmum-width)
                                            (if (> len tsv-maxmum-width)
                                                (setq len tsv-maxmum-width)))
                                          (vector (format "col%d" i)
                                                  (1- i) len len len len 'show))
                                        (tsv-line-content))))
           (if tsv-has-header-p (forward-line))
           (setq len (length tsv-header))
           (while (not (eobp))
             (setq row (tsv-line-content))
             (when (/= len (length row))
               (error "Line columns not fit (default %d, current %d) at line %d: %s"
                      len (length row) (line-number-at-pos) (buffer-substring (point) (line-end-position))))
             (setq tsv-table (cons row tsv-table))
             (forward-line 1))
           (goto-char (point-min))
           (if (looking-at ". -\\*- mode: tsv") (tsv-delete-line))
           (setq tsv-original-comment (delete-and-extract-region
                                       (point) (marker-position table-start)))
           (if tsv-has-header-p (tsv-delete-line))
           (setq tsv-table (nreverse tsv-table))
           (tsv-scan-table)
           (setq tsv-real-header (copy-sequence tsv-header))
           (tsv-compile-format)
           (tsv-format-buffer))))
    (setq inhibit-read-only nil)))

;;;_. commands
;;;_ , about copy and kill
(defun tsv-kill-line (arg)
  (interactive "p")
  (let ((beg (line-beginning-position))
        (end (progn
               (if arg
                   (forward-visible-line (prefix-numeric-value arg))
                 (if (eobp)
                     (signal 'end-of-buffer nil))
                 (let ((end
                        (save-excursion
                          (end-of-visible-line) (point))))
                   (if (or (save-excursion
                             (unless show-trailing-whitespace
                               (skip-chars-forward " \t" end))
                             (= (point) end))
                           (and kill-whole-line (bolp)))
                       (forward-visible-line 1)
                     (goto-char end))))
               (point)))
        (inhibit-read-only t)
        rows)
    (if (< end beg)
        (let ((tmp beg))
          (setq beg end
                end tmp)))
    (setq rows (tsv-rows-in-region beg end))
    (setq tsv-table (tsv-delete-list tsv-table rows))
    (kill-new (mapconcat 'tsv-stringfy-row rows "\n"))
    (delete-region beg end)))

(defun tsv-kill-region (beg end)
  (interactive "r")
  (save-excursion
    (goto-char beg)
    (tsv-kill-line (- (line-number-at-pos end)
                      (line-number-at-pos beg)))))

(defun tsv-kill-ring-save (beg end)
  (interactive "r")
  (kill-new (mapconcat 'tsv-stringfy-row
                       (tsv-rows-in-region beg end) "\n"))
  (deactivate-mark))

;; yank function should redefined
(defun tsv-yank (arg)
  (interactive "P")
  (let ((rows (current-kill (cond ((listp arg) 0)
                                  ((eq arg '-) -2)
                                  (t (1- arg)))))
        (len (length tsv-real-header))
        (len1 (length tsv-header))
        (pos (save-restriction (widen) (tsv-line-number-at-pos)))
        col tmp new)
    (remove-text-properties 0 (length rows) nil rows)
    (setq rows (mapcar (lambda (row)
                         (setq row (split-string row tsv-separator-char))
                         (if (/= (length row) len)
                             (error "length of row not match: %S"
                                    row))
                         (setq new (make-vector len1 "")
                               tmp tsv-real-header)
                         (dolist (c row)
                           (setq col (car tmp)
                                 tmp (cdr tmp))
                           (aset new (tsv-col-index col) c))
                         (append new nil))
                       (split-string rows "\n")))
    (setq tsv-table (tsv-splice-list tsv-table pos pos rows))
    (beginning-of-line)
    (let ((inhibit-read-only t))
      (mapc 'tsv-format-line rows))))
                        
;;;_ , about field
(defsubst tsv-current-column ()
  "Return the current column of point. Adjust it if needed."
  (let ((col (1- (current-column))))
    (cond ((< col 0) 0)
          ((>= col (length tsv-column)) (1- (length tsv-column)))
          (t col))))

(defsubst tsv-row-at-point ()
  (get-text-property (point) 'row))

(defsubst tsv-col-name-at-point ()
  (tsv-col-name (nth (tsv-col-index-at-point) tsv-header)))

(defun tsv-col-index-at-point ()
  "Field name in the `tsv-formatter' according to current column"
  (let ((col (tsv-current-column))
        field)
    (setq field (aref tsv-column col))
    (if (< field 0)
        (setq field (aref tsv-column (+ col field))))
    (when (> field 0)
      (car (nth (1- field) tsv-formatter)))))

(defun tsv-show-field-at-point ()
  "Echo the field content."
  (interactive)
  (let ((idx (tsv-col-index-at-point))
        (row (tsv-row-at-point)))
    (when idx
      (message 
       (format-spec
        tsv-show-field-format
        (format-spec-make
         ?d (1+ (tsv-line-number-at-pos))
         ?l (line-number-at-pos)
         ?i idx
         ?n (tsv-col-name (nth idx tsv-header))
         ?s (nth idx row)
         ?c (nth (or tsv-id-column 0) row)))))))

(defun tsv-copy-field-at-point ()
  "Add the content of this column to kill ring"
  (interactive)
  (let ((field (tsv-col-index-at-point)))
    (when field
      (setq field (nth field (tsv-row-at-point)))
      (if (eq last-command 'kill-region)
          (kill-append field nil)
        (kill-new field))
      (message field))))

(defun tsv-edit-field-at-point ()
  (interactive)
  (let ((field (tsv-col-index-at-point))
        (row (tsv-row-at-point)))
    (tsv-row-set row field
                 (read-from-minibuffer "Set to: " (nth field row)))
    (tsv-update-at-point)))

(defun tsv-next-field (arg)
  "Jump the next ARG fields"
  (interactive "p")
  (if (< arg 0)
      (tsv-prev-field (- arg))
    (let ((col (tsv-current-column))
          (len (1- (length tsv-column)))
          field)
      (setq field (aref tsv-column col))
      (if (< field 0)
          (setq col (+ col field)))
      (while (> arg 0)
        (if (< col len)
            (setq col (1+ col))
          (forward-line)
          (setq col 0))
        (if (> (aref tsv-column col) 0)
            (setq arg (1- arg))))
      (move-to-column (1+ col)))))

(defun tsv-prev-field (arg)
  "Jump the previous ARG fields"
  (interactive "p")
  (if (< arg 0)
      (tsv-next-field (- arg))
    (let ((col (tsv-current-column))
          (len (1- (length tsv-column)))
          field)
      (setq field (aref tsv-column col))
      (if (< field 0)
          (setq col (+ col field)))
      (while (> arg 0)
        (if (> col 0)
            (setq col (1- col))
          (forward-line -1)
          (setq col len))
        (if (> (aref tsv-column col) 0)
            (setq arg (1- arg))))
      (move-to-column (1+ col)))))

(defun tsv-add-new-row (before arg)
  "Add ARG rows behind. With prefix add before"
  (interactive (list current-prefix-arg
                     (read-number "How many rows: " 1)))
  (let ((pos (save-restriction (widen) (tsv-line-number-at-pos)))
        (len (length tsv-header))
        rows)
    (if before
        (beginning-of-line)
      (setq pos (1+ pos))
      (forward-line 1))
    (setq rows (mapcar (lambda (i) (make-list len ""))
                       (number-sequence 1 arg)))
    (let ((inhibit-read-only t))
      (mapc 'tsv-format-line rows))
    (setq tsv-table (tsv-splice-list tsv-table pos pos rows))
    (forward-line (- arg))))

;;;_ , about column
(defun tsv-set-column-width (width)
  "Set current column width."
  (interactive "nSet to width: ")
  (let ((field (nth (tsv-col-index-at-point) tsv-header)))
    (tsv-col-current-width field width)
    (tsv-compile-format)
    (tsv-redisplay-table)))

(defun tsv-widen-column (arg)
  "Widen the current column by ARG."
  (interactive "p")
  (let ((field (nth (tsv-col-index-at-point) tsv-header)))
    (tsv-set-column-width (+ arg (tsv-col-current-width field)))))

(defun tsv-narrow-column (arg)
  "Narrow the current column by ARG."
  (interactive "p")
  (tsv-widen-column (- arg)))

(defun tsv-narrow-to-min ()
  "Set current column width to the min length of element is this column"
  (interactive)
  (let ((field (nth (tsv-col-index-at-point) tsv-header)))
    (tsv-set-column-width (tsv-col-min-width field))))

(defun tsv-widen-to-max ()
  "Set current column width to the max length of element is this column"
  (interactive)
  (let ((field (nth (tsv-col-index-at-point) tsv-header)))
    (tsv-set-column-width (tsv-col-max-width field))))

(defun tsv-set-all-column-width (width)
  "Set all column width to WIDTH"
  (interactive "nSet all column to width: ")
  (dolist (col tsv-header)
    (tsv-col-current-width col width))
  (tsv-compile-format)
  (tsv-redisplay-table))

(defun tsv-hide-column (col)
  (interactive (list (completing-read
                      "Hide column: "
                      (tsv-cols 'tsv-col-show-p)
                      nil t
                      (tsv-col-name-at-point))))
  (tsv-col-state (nth (tsv-col-find-index col) tsv-header) 'hide)
  (tsv-compile-format)
  (tsv-redisplay-table))

(defun tsv-delete-column (col)
  (interactive (list
                (completing-read
                 "Set this column as id: "
                 (tsv-cols) nil t
                 (tsv-col-name-at-point))))
  (let ((idx (tsv-col-find-index col)))
    (set-buffer-modified-p t)
    (tsv-col-state (nth idx tsv-header) 'delete)
    (setq tsv-real-header (remove-if (lambda (c)
                                       (= idx (tsv-col-index c)))
                                     tsv-real-header))
    (when (assoc idx tsv-formatter)
      (tsv-compile-format)
      (tsv-redisplay-table))))

(defun tsv-add-new-column (arg after)
  ""
  (interactive
   (list (prefix-numeric-value current-prefix-arg)
         (completing-read
          "After col(empty means at beginning): "
          (cons "" (tsv-cols)) nil t (tsv-col-name-at-point))))
  (tsv-add-colname (tsv-generate-colname arg) after)
  (tsv-insert-column-1 (lambda (i) (make-list arg ""))))

(defun tsv-add-colname (cols after)
  ""
  (let ((idx (length tsv-header))
        (len tsv-minmum-width))
    (setq cols (mapcar (lambda (col)
                         (setq idx (1+ idx))
                         (vector col (1- idx) len len len len 'show))
                       cols)
          tsv-header (append tsv-header cols)
          tsv-real-header
          (if (string= "" after)
              (tsv-splice-list tsv-real-header 0 0 cols)
            (let ((tmp tsv-real-header)
                  new col)
              (while tmp
                (setq col (car tmp)
                      tmp (cdr tmp)
                      new (cons col new))
                (if (string= (tsv-col-name col) after)
                    (setq new (append (nreverse new) cols tmp)
                          tmp nil)))
              new)))))

(defun tsv-generate-colname (&optional n)
  "Generate a not duplicate column name"
  (or n (setq n 1))
  (let ((names (mapcar 'tsv-col-name tsv-header))
        (idx (1+ (length tsv-header)))
        (i 0) new)
    (while (< i n)
      (setq new (format "col%d" idx))
      (if (member new names)
          (setq idx (1+ idx))
        (setq names (cons new names)
              i (1+ i))))
    (nreverse (butlast names (- idx n)))))

(defun tsv-insert-column-1 (col)
  (let (generator new (i 0))
    (setq generator
          (cond ((functionp col) col)
                ((listp col)
                 (lambda (i)
                   (let ((r (car col)))
                     (setq col (cdr col))
                     r)))
                (t (error "Wrong col"))))
    (dolist (row tsv-table)
      (setq new (cons (append row (funcall generator i)) new)
            i (1+ i)))
    (setq tsv-table (nreverse new))
    (save-excursion
      (save-restriction
        (widen)
        (tsv-compile-format)
        (tsv-format-buffer)))))

(defun tsv-yank-column (arg)
  ""
  (interactive "p")
  (let ((nrow (length tsv-table))
        cols nrowc after)
    (setq cols (split-string (current-kill (cond ((listp arg) 0)
                                                 ((eq arg '-) -2)
                                                 (t (1- arg))))
                             "\n")
          nrowc (length cols))
    (if tsv-has-header-p
        (setq nrowc (1- nrowc)))
    (unless (= nrow nrowc)
      (error "Row number is not match! table: %d but kill ring %d"
             nrow nrowc))
    (setq after
          (completing-read
           "After col(empty means at beginning): "
           (cons "" (tsv-cols)) nil t (tsv-col-name-at-point)))
    (setq cols (mapcar (lambda (r)
                         (split-string r tsv-separator-char))
                       cols))
    (if tsv-has-header-p
        (progn
          (tsv-add-colname (car cols) after)
          (tsv-insert-column-1 (cdr cols)))
      (tsv-add-colname (tsv-generate-colname (length (car cols)))
                       after)
      (tsv-insert-column-1 cols))))

(defun tsv-show-column (col)
  "Show the hided column"
  (interactive (list
                (let ((hides (tsv-cols 'tsv-col-hide-p)))
                  (if hides
                      (completing-read
                       "Add new field: "
                       hides  nil t (car hides))
                    'none))))
  (if (eq col 'none)
      (message "No hiden columns!")
    (tsv-col-state (nth (tsv-col-find-index col) tsv-header) 'show)
    (tsv-compile-format)
    (tsv-redisplay-table)))

(defun tsv-sort-lines-region (beg end col numericp desc)
  "Sort lines in the region in the column COL. The sort method can be
\"n\" if by numeric or \"a\" if by alpha. A customized function can be
specific by select \"o\". The function should accept two arguments
which type is string, for example `string<'. With a prefix to sort the
lines descendly."
  (interactive (list
                (region-beginning)
                (region-end)
                (completing-read
                 "Sort column: "
                 (tsv-cols 'tsv-col-show-p)
                 nil t
                 (tsv-col-name-at-point))
                (completing-read "Sorting method: [n]=numeric [a]=alpha [o]=other: "
                                 '("n" "a" "o")
                                 nil t "n")
                current-prefix-arg))
  (let ((rows (tsv-rows-in-region beg end))
        (idx (tsv-col-find-index col))
        func predicate)
    (setq func
          (cond ((string= numericp "o")
                 (intern-soft
                  (completing-read "Choose a sort function: " obarray
                                   (lambda (sym)
                                     (fboundp (intern-soft sym)))
                                   t)))
                ((string= numericp "n") (lambda (n1 n2)
                                          (< (string-to-number n1)
                                             (string-to-number n2))))
                ((string= numericp "a") 'string<)
                (t (error "Unknown sort method")))
          predicate
          (if desc
              (lambda (r2 r1)
                (funcall func (nth idx r1) (nth idx r2)))
            (lambda (r1 r2)
              (funcall func (nth idx r1) (nth idx r2)))))
    (setq rows (sort rows predicate))
    (setq tsv-table
          (tsv-splice-list tsv-table
                           (tsv-line-number-at-pos beg)
                           (tsv-line-number-at-pos end)
                           rows))
    (let ((inhibit-read-only t))
      (save-tsv-position
       (delete-region (progn
                        (goto-char beg)
                        (line-beginning-position))
                      (progn
                        (goto-char end)
                        (unless (looking-at "^")
                          (forward-line 1))
                        (point)))
       (mapc 'tsv-format-line rows)))))

(defun tsv-sort-column (col numericp desc)
  "Sort COL. The sort method can be \"n\" if by numeric or \"a\"
if by alpha. A customized function can be specific by select
\"o\". The function should accept two arguments which type is
string, for example `string<'. With a prefix to sort the lines
descendly.

See also `tsv-sort-lines-region'"
  (interactive
   (list (completing-read
          "Sort column: "
          (tsv-cols 'tsv-col-show-p)
          nil t (tsv-col-name-at-point))
         (completing-read "Sorting method: [n]=numeric [a]=alpha [o]=other: "
                          '("n" "a" "o")
                          nil t "n")
         current-prefix-arg))
  (tsv-sort-lines-region (point-min) (point-max) col numericp desc))

(defun tsv-rename-column ()
  "Rename current column"
  (interactive)
  (if tsv-has-header-p
      (let ((col (nth (tsv-col-index-at-point) tsv-header)))
        (tsv-col-name col
                      (read-from-minibuffer
                       "Rename: "
                       (tsv-col-name col)))
        (setq header-line-format (tsv-header-line)))
    (message "No use header line")))

(defun tsv-set-column-as-id (col)
  (interactive (list
                (completing-read
                 "Set this column as id: "
                 (tsv-cols) nil t (tsv-col-name-at-point))))
  (setq tsv-id-column (tsv-col-find-index col)))

(defun tsv-select-column ()
  "Prompt to select columns"
  (let ((header tsv-header)
        (bound (length tsv-header))
        valid-cols cols)
    (save-selected-window
      (with-temp-buffer
        (dolist (col header)
          (unless (tsv-col-del-p col)
            (setq valid-cols (cons (tsv-col-index col) valid-cols))
            (insert (format " %2d. %s\n"
                            (1+ (car valid-cols))
                            (tsv-col-name col)))))
        (pop-to-buffer (current-buffer))
        (setq cols
              (read-from-minibuffer "Select cols(separate by blank: "))))
    (when (and cols (string< "" cols))
      (remove-if 'null
                 (mapcar (lambda (num)
                           (setq num (floor (string-to-number num)))
                           (if (member num valid-cols)
                               (1- (floor num))))
                         (split-string cols))))))

(defun tsv-column-in-range (from to)
  "Return cols from column FROM to column TO"
  (let (cols col)
    (if (> from to)
        (let ((tmp to))
          (setq to from
                from tmp)))
    (if (< from 0) (setq from 0))
    (if (> to (length tsv-column)) (setq to (length tsv-column)))
    (setq col (aref tsv-column from))
    (if (< col 0)
        (setq col (aref tsv-column (+ from col))))
    (setq cols (list col))
    (dolist (col (number-sequence (1+ from) (1- to)))
      (if (> (aref tsv-column col) 0)
          (setq cols (cons (aref tsv-column col) cols))))
    (nreverse (mapcar '1- cols))))

(defun tsv-copy-column (cols)
  "Copy select rows"
  (interactive (list (tsv-select-column)))
  (kill-new
   (concat
    (if tsv-has-header-p
        (concat 
         (mapconcat (lambda (c)
                      (tsv-col-name (nth c tsv-header)))
                    cols tsv-separator-char)
         "\n"))
    (mapconcat (lambda (row)
                 (mapconcat
                  (lambda (col)
                    (nth col row)) cols tsv-separator-char))
               (tsv-rows-in-region (point-min) (point-max))
               "\n"))))

(defun tsv-copy-rectangle (beg end)
  "Copy the data in select rectangle"
  (interactive "r")
  (let ((cols (save-excursion
                (list
                 (progn
                   (goto-char beg)
                   (current-column))
                 (progn
                   (goto-char end)
                   (current-column))))))
    (setq cols (apply 'tsv-column-in-range (sort cols '<)))
    (kill-new
     (mapconcat (lambda (row)
                  (mapconcat
                   (lambda (col)
                     (nth col row)) cols tsv-separator-char))
                (tsv-rows-in-region beg end)
                "\n"))))

;; this is not implement, because it is a little difficult to handle
;; hide columns
;; (defun tsv-move-column-right (col arg)
;;   (interactive
;;    (list (completing-read
;;           "Move which column left: "
;;           (tsv-cols 'tsv-col-show-p)
;;           nil nil (tsv-col-name-at-point))
;;          current-prefix-arg))
;;   (or arg (setq arg 1))
;;   (if (member col (tsv-cols 'tsv-col-show-p))
;;       (let ((idx (tsv-col-find-index col))
;;             (header tsv-header)
;;             (pos 0) newfrt field)
;;         (while formatter
;;           (setq field (car formatter)
;;                 formatter (cdr formatter)
;;                 pos (1+ pos))
;;           (if (= (car field) idx)
;;               (setq newfrt (append newfrt formatter)
;;                     formatter nil)
;;             (setq newfrt (append newfrt (list field)))))
;;         (setq arg (+ arg pos -1))
;;         (setq newfrt
;;               (cond ((< arg 0) (cons field newfrt))
;;                     ((> arg (length newfrt))
;;                      (append newfrt (list field)))
;;                     (t (append (butlast newfrt (- (length newfrt) arg))
;;                                (list field)
;;                                (nthcdr arg newfrt)))))
;;         (unless (equal tsv-formatter newfrt)
;;           (tsv-redisplay-ref)))
;;     (message "No such column %s" col)))
;; (defun tsv-move-column-left (col arg)
;;   (interactive
;;    (list (completing-read
;;           "Move which column left: "
;;           (mapcar 'car (remove-if 'stringp tsv-formats))
;;           nil nil (tsv-col-index-at-point))
;;          current-prefix-arg))
;;   (or arg (setq arg 1))
;;   (tsv-move-column-right col (- arg)))

;;;_ , about narrow and widen
(defun tsv-narrow-to-region (beg end)
  "Narrow to select lines. This may useful when visit a large file.
Don't use `widen' to show narrowing region, use `tsv-widen' instead."
  (interactive "r")
  (goto-char beg)
  (setq beg (line-beginning-position))
  (goto-char end)
  (unless (looking-at "^")
    (forward-line 1)
    (setq end (point)))
  (narrow-to-region beg end))

(defalias 'tsv-widen 'tsv-format-buffer
  "Remove restrictions from current buffer. ")

;;;_ , about scroll
(defun tsv-scroll-up (&optional arg)
  "Scroll up without change current column"
  (interactive "P")
  (let ((col (current-column)))
    (scroll-up arg)
    (move-to-column col)))

(defun tsv-scroll-down (&optional arg)
  "Scroll down without change current column"
  (interactive "P")
  (let ((col (current-column)))
    (scroll-down arg)
    (move-to-column col)))

;;;_ , other
(defun tsv-show-comments ()
  "Show comments of current file"
  (interactive)
  (message (replace-regexp-in-string
            "\n*\\'" ""
            (replace-regexp-in-string "^#\\s-*" ""
                                      tsv-original-comment))))

(defun tsv-goto-line ()
  "Goto the Nth line"
  (interactive)
  (let ((col (current-column)))
    (call-interactively 'goto-line)
    (move-to-column col)))

(defun tsv-first-line-as-header ()
  "Typically, when you enter `tsv-mode' first time, the first
line is not use as header. So use this command when need. If you don't
want it, you can restore it by `tsv-restore-first-line'."
  (interactive)
  (if tsv-has-header-p
      (message "Already use first line as header!")
    (let ((header (car tsv-table))
          dup)
      (setq dup (catch 'not-valid
                  (tsv-check-header header)))
      (cond ((eq dup 'empty)
             (error "The header contain a empty column name"))
            ((null dup) nil)
            (t (error "Column name \"%s\" is duplicate" dup)))
      (save-buffer-modified-p
       (let ((tmp tsv-header)
             (inhibit-read-only t))
         (setq tsv-table (cdr tsv-table))
         (save-excursion
           (save-restriction
             (widen)
             (goto-char (point-min))
             (tsv-delete-line)))
         (while tmp
           (tsv-col-name (car tmp) (car header))
           (setq header (cdr header)
                 tmp (cdr tmp)))
         (setq tsv-has-header-p t)
         (setq header-line-format (tsv-header-line)))))))

(defun tsv-check-header (header)
  "Check header whether contain duplicate column"
  (let (new)
    (while header
      (cond ((string= (car header) "")
             (throw 'not-valid 'empty))
            ((member (car header) new)
             (throw 'not-valid (car header)))
            (t (setq new (cons (car header) new)
                     header (cdr header)))))))

(defun tsv-restore-first-line ()
  (interactive)
  (if tsv-has-header-p
      (let ((header (mapcar 'tsv-col-name tsv-header))
            (inhibit-read-only t)
            (i 0))
        (save-buffer-modified-p
         (save-excursion
           (save-restriction
             (widen)
             (goto-char (point-min))
             (tsv-format-line header)))
         (setq tsv-table (cons header tsv-table))
         (dolist (col tsv-header)
           (setq i (1+ i))
           (tsv-col-name col (format "col%d" i)))
         (setq header-line-format (tsv-header-line)))
        (setq tsv-has-header-p nil))
    (message "Not use first line as header!")))

(defun tsv-toggle-header-line ()
  (interactive)
  (if tsv-has-header-p
      (tsv-restore-first-line)
    (tsv-first-line-as-header)))

(defun tsv-exit ()
  (interactive)
  (when (eq major-mode 'tsv-mode)
    (let ((inhibit-read-only t))
      (save-buffer-modified-p
       (tsv-write (point-min) (point-max) (current-buffer))
       (setq header-line-format nil)
       (kill-all-local-variables)
       (setq buffer-undo-list nil)
       (setq buffer-file-format (delq 'tsv buffer-file-format))
       (setq buffer-read-only nil)))))

;;;_. normal minor mode
;;;###autoload 
(define-minor-mode tsv-normal-mode
  "A minor mode to edit table.
\\{tsv-normal-mode-map}"
  :lighter " Tn" :keymap tsv-normal-mode-map
  (when (and (eq major-mode 'tsv-mode) tsv-table)
    (if tsv-normal-mode
        (save-buffer-modified-p
         (let ((line (line-number-at-pos)))
           (setq buffer-read-only nil)
           (buffer-enable-undo)
           (erase-buffer)
           (if tsv-has-header-p
               (insert (mapconcat 'tsv-col-name tsv-header tsv-separator-char) "\n"))
           (insert (mapconcat 'tsv-stringfy-row tsv-table "\n"))
           (goto-line line))
         (setq header-line-format nil))
      (tsv-read (string-to-char tsv-separator-char))
      (setq buffer-read-only t))))

(defun tsv-normal-next-field (arg)
  (interactive "p")
  (if (< arg 0)
      (tsv-normal-prev-field (- arg))
    (while (and (not (eobp))
                (> arg 0))
      (setq arg (1- arg))
      (skip-chars-forward "^\t\n")
      (if (not (eobp))
          (forward-char 1)))))

(defun tsv-normal-prev-field (arg)
  (interactive "p")
  (if (< arg 0)
      (tsv-normal-next-field (- arg))
    (skip-chars-backward "^\t\n")
    (while (and (not (bobp))
                (> arg 0))
      (setq arg (1- arg))
      (if (not (bobp))
          (backward-char 1))
      (skip-chars-backward "^\t\n"))))

(defun tsv-normal-end-of-field/line ()
  "Move to the end of current field. If the position doesn't change,
move to the end of line."
  (interactive)
  (let ((pos (point)))
    (skip-chars-forward "^\t\n")
    (if (= pos (point))
        (end-of-line))))

(defun tsv-normal-beginning-of-field/line ()
  "Move to the beginning of current field. If the position doesn't change,
move to the beginning of line."
  (interactive)
  (let ((pos (point)))
    (skip-chars-backward "^\t\n")
    (if (= pos (point))
        (beginning-of-line))))

;;; tsv-mode.el ends here

;;;_. LocalVariable
;;; Local Variables: ***
;;; allout-layout:(1 : -1) ***
;;; End: ***
