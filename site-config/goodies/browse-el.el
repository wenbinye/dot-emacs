;;; browse-el.el.gz --- nevigate in an elisp source code

;; Copyright (C) 2004  Free Software Foundation, Inc.

;; Author: Charles Wang <charleswang@peoplemail.com.cn>
;; Keywords: lisp, tools

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; intallaltion, put the following lines in your .emacs
;;               (add-to-list 'load-path "path/where/to/browse.el")
;;               (require 'browse-el)
;;
;;; Code:


(require  'find-func)
;;;###autoload
(defvar browse-el-point-ring nil)
(defvar browse-el-point-ring-max-number 200)

;;;###autoload
(defun browse-el-push-point-mark()
  (if (eq (length browse-el-point-ring) browse-el-point-ring-max-number)
      (setq browse-el-point-ring (butlast browse-el-point-ring)))
  (let ((file (buffer-file-name)))
    (if file
        (push (cons  file (point-marker))
              browse-el-point-ring))))
;;;###autoload
(defun browse-el-pop-point-mark()
  (let ((val (pop browse-el-point-ring)))
    (if val
        (progn
          (find-file (car val))
          (goto-char (cdr val)))
      (error "reach buttome of ring"))))

(defun browse-el-find-funtion ()
  (interactive)
  (let ((fun (function-called-at-point)))
    (when fun
      (browse-el-push-point-mark)          
      (find-function-do-it fun nil 'switch-to-buffer))))

;;(defun foo ()
;;  (interactive)
;;  (print (format "function %s" (function-at-point))))
(defun browse-el-go-back()
  (interactive)
  (browse-el-pop-point-mark))
  

(defun browse-el-key-binding ()
  (interactive)
  (local-set-key (kbd "<f6>") 'browse-el-find-funtion)
  (local-set-key (kbd "<f5>") 'browse-el-go-back))

(provide 'browse-el)
;;; browse-el.el ends here
