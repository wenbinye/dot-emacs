;;; file-stat.el --- 

;; Copyright 2006 Ye Wenbin
;;
;; Author: wenbinye@163.com
;; Time-stamp: <Ye Wenbin 2006-09-13 16:25:28>
;; Version: $Id: file-stat.el,v 1.1.1.1 2007-03-13 13:16:10 ywb Exp $
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

;; 

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'file-stat)

;;; Code:

(provide 'file-stat)
(eval-when-compile
  (require 'cl))

(defsubst file-stat-type (file &optional id-format)
  (car (file-attributes file id-format)))
(defsubst file-stat-name-number (file &optional id-format)
  (cadr (file-attributes file id-format)))
(defsubst file-stat-uid (file &optional id-format)
  (nth 2 (file-attributes file id-format)))
(defsubst file-stat-gid (file &optional id-format)
  (nth 3 (file-attributes file id-format)))
(defsubst file-stat-atime (file &optional id-format)
  (nth 4 (file-attributes file id-format)))
(defsubst file-stat-mtime (file &optional id-format)
  (nth 5 (file-attributes file id-format)))
(defsubst file-stat-ctime (file &optional id-format)
  (nth 6 (file-attributes file id-format)))
(defsubst file-stat-size (file &optional id-format)
  (nth 7 (file-attributes file id-format)))
(defsubst file-stat-modes (file &optional id-format)
  (nth 8 (file-attributes file id-format)))
(defsubst file-stat-guid-changep (file &optional id-format)
  (nth 9 (file-attributes file id-format)))
(defsubst file-stat-inode-number (file &optional id-format)
  (nth 10 (file-attributes file id-format)))
(defsubst file-stat-system-number (file &optional id-format)
  (nth 11 (file-attributes file id-format)))

(defsubst file-attr-type (attr)
  (car attr))
(defsubst file-attr-name-number (attr)
  (cadr attr))
(defsubst file-attr-uid (attr)
  (nth 2 attr))
(defsubst file-attr-gid (attr)
  (nth 3 attr))
(defsubst file-attr-atime (attr)
  (nth 4 attr))
(defsubst file-attr-mtime (attr)
  (nth 5 attr))
(defsubst file-attr-ctime (attr)
  (nth 6 attr))
(defsubst file-attr-size (attr)
  (nth 7 attr))
(defsubst file-attr-modes (attr)
  (nth 8 attr))
(defsubst file-attr-guid-changep (attr)
  (nth 9 attr))
(defsubst file-attr-inode-number (attr)
  (nth 10 attr))
(defsubst file-attr-system-number (attr)
  (nth 11 attr))

;;; file-stat.el ends here
