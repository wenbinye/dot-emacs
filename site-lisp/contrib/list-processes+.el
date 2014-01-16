;;; list-processes+.el --- 

;; Copyright 2006 Ye Wenbin
;;
;; Author: wenbinye@163.com
;; Time-stamp: <Ye Wenbin 2014-01-16 09:27:23>
;; Version: $Id: list-processes+.el,v 1.1.1.1 2007-03-13 13:16:10 ywb Exp $
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
;;   (autoload "list-processes+" 'list-processes+
;;          "A enhance list processes command" t)

;;; Code:

(provide 'list-processes+)
(eval-when-compile
  (require 'cl))

(defvar list-processes-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-k" 'list-processes-kill-process)
    (define-key map "\C-m" 'list-processes-goto-buffer)
    map))

;;;###autoload 
(defun list-processes+ (&optional query-only)
  (interactive "P")
  (list-processes query-only)
  (pop-to-buffer (get-buffer "*Process List*"))
  (use-local-map list-processes-mode-map))

(defun list-processes-process-at-pos ()
  (let ((ln (line-number-at-pos)))
    (when (>= ln 1)
      (car (nth (- ln 1) tabulated-list-entries)))))

(defun list-processes-kill-process ()
  (interactive)
  (let ((proc (list-processes-process-at-pos)))
    (when (and proc
               (y-or-n-p (format "Kill process %s? " (process-name proc))))
      (delete-process proc)
      (list-processes+))))

(defun list-processes-goto-buffer ()
  (interactive)
  (let ((proc (list-processes-process-at-pos)))
    (when proc
      (if (and (process-buffer proc)
               (buffer-live-p (process-buffer proc)))
          (switch-to-buffer (process-buffer proc))
        (message "No associate buffer!")))))

;;; list-process-mode.el ends here
