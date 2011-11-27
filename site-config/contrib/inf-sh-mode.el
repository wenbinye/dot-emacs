;;; -*- lisp-interaction -*-
;;; inf-sh-mode.el --- A simple inferior shell mode

;; Copyright 2006 Ye Wenbin
;;
;; Author: wenbinye@163.com
;; Version: $Id: inf-sh-mode.el,v 1.1.1.1 2007-03-13 13:16:10 ywb Exp $
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
;;   (require 'inferior-sh-mode)

;;; Code:

(provide 'inf-sh-mode)
(require 'sh-script)
(eval-when-compile
  (require 'cl))

(defvar sh-set-inferior-hook nil
  "")

(defvar sh-inferior-buffer nil "")

(defun sh-inferior-buffer ()
  "If local variable `sh-inferior-buffer' valid, use it, elsewise, if
`sh-inferior-global-mode' enable, set local variable to global value."
  (or (and sh-inferior-buffer
           (buffer-live-p sh-inferior-buffer))
      (if (and sh-inferior-global-mode
               (and (default-value 'sh-inferior-buffer)
                    (buffer-live-p (default-value 'sh-inferior-buffer))))
          (setq sh-inferior-buffer (default-value 'sh-inferior-buffer)))))

(defun sh-switch-to-shell (arg)
  "show perl process buffer. With argument, just display it,
otherwise, switch to the buffer."
  (interactive "P")
  (if (sh-inferior-buffer)
      (if arg
          (display-buffer sh-inferior-buffer)
        (pop-to-buffer sh-inferior-buffer))
    (message "No SHELL process set!")))

(defun sh-switch-to-end-shell (arg)
  "show perl process buffer. With argument, just display it,
otherwise, switch to the buffer."
  (interactive "P")
  (if (sh-inferior-buffer)
      (let ((buf sh-inferior-buffer))
        (sh-switch-to-shell arg)
        (with-selected-window (get-buffer-window buf)
          (push-mark)
          (goto-char (point-max))))
    (message "No SHELL process set!")))

(defun sh-set-inferior-buffer (shell)
  ""
  (interactive (list
                (let ((bufs (mapcar 'buffer-name
                                    (save-excursion
                                      (remove-if-not (lambda (b)
                                                       (set-buffer b)
                                                       (eq major-mode 'shell-mode))
                                                     (buffer-list))))))
                  (completing-read "Set shell: " bufs nil t (car bufs)))))
  (set
   (make-local-variable 'sh-inferior-buffer)
   (get-buffer shell))
  (run-hooks 'sh-set-inferior-hook))

(defun sh-send-string (str)
  ""
  (interactive "sSend: ")
  (if (sh-inferior-buffer)
      (progn
        (comint-send-string sh-inferior-buffer str)
        (unless (string-match "\n$" str)
          (comint-send-string sh-inferior-buffer "\n"))
        (sh-send-empty-line)
        (message "Send string to buffer %s."
                 (buffer-name sh-inferior-buffer)))
    (message "No SHELL process set!")))

(defun sh-send-line ()
  ""
  (interactive)
  (sh-send-string
   (buffer-substring (line-beginning-position) (line-end-position))))

(defun sh-send-region (beg end)
  ""
  (interactive "r")
  (sh-send-string
   (concat "if [ 0 ]; then\n"
           (buffer-substring beg end)
           (unless (string-match "\n\\s-*$" (buffer-substring beg end))
             "\n")
           "fi\n")))

(defun sh-send-empty-line ()
  "put a empty input just for movement"
  (save-excursion
    (let* ((proc (get-buffer-process sh-inferior-buffer))
           (marker (process-mark proc)))
      (set-buffer sh-inferior-buffer)
      (goto-char marker)
      (insert (propertize "\n" 'field 'input))
      (set-marker marker (point)))))

(defun sh-turn-on-master ()
  ""
  (interactive)
  (if (sh-inferior-buffer)
      (progn
        (master-mode 1)
        (master-set-slave sh-inferior-buffer)
        (message "Set slave to %s" (buffer-name sh-inferior-buffer)))
    (message "No SHELL process set!")))

(define-minor-mode sh-inferior-global-mode
  "When turn on, use a common shell buffer"
  :global t
  (if sh-inferior-global-mode
      (unless (and (default-value 'sh-inferior-buffer)
                   (buffer-live-p (default-value 'sh-inferior-buffer)))
        (let ((bufs (mapcar 'buffer-name
                            (save-excursion
                              (remove-if-not (lambda (b)
                                               (set-buffer b)
                                               (eq major-mode 'shell-mode))
                                             (buffer-list)))))
              buf)
          (if bufs
              (progn
                (setq buf (get-buffer (completing-read "Set shell: " bufs nil t (car bufs))))
                (setq-default sh-inferior-buffer buf)
                (keep-end-watch-this buf))
            (setq sh-inferior-global-mode nil)
            (error "No shell buffer found!"))))
    (setq-default sh-inferior-buffer nil)))

(define-key sh-mode-map "\C-c\C-j" 'sh-send-line)
(define-key sh-mode-map "\C-x\C-e" 'sh-send-line)
(define-key sh-mode-map "\C-c\C-k" 'sh-send-region)
(define-key sh-mode-map "\C-c\C-e" 'sh-set-inferior-buffer)
(define-key sh-mode-map "\C-c\C-z" 'sh-switch-to-end-shell)
(define-key sh-mode-map "\C-c\C-y" 'sh-switch-to-shell)

;;; inf-sh-mode.el ends here
