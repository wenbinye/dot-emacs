;;; ywb-commands.el --- utility commands

;; Copyright (C) 2015 Free Software Foundation, Inc.
;;
;; Author: ywb <ywb@ywb-Inspiron-7520>
;; Maintainer: ywb <ywb@ywb-Inspiron-7520>
;; Created: 28 Nov 2015
;; Version: 0.01
;; Keywords: convenience

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
;;   (require 'ywb-util)

;;; Code:

(eval-when-compile
  (require 'cl))

;;;###autoload 
(defun ywb-clone-buffer (non-indirect)
  "If with prefix argument, clone buffer, other wise, clone indirect buffer"
  (interactive "P")
  (if non-indirect
      (call-interactively 'clone-buffer)
    (let ((indir-bufs (mapcar (lambda (buf) (cons buf (buffer-base-buffer buf)))
                              (remove-if-not 'buffer-base-buffer (buffer-list))))
          buf)
      (if (setq buf (assoc (current-buffer) indir-bufs))
          (select-window (display-buffer (cdr buf)))
        (if (setq buf (rassoc (current-buffer) indir-bufs))
            (select-window (display-buffer (car buf)))
          (setq current-prefix-arg nil)
          (call-interactively 'clone-indirect-buffer-other-window))))))

(defun ywb-camelcase-move-word (fw)
  (let ((case-fold-search nil)
        wordpos casepos)
    (save-excursion
      (forward-word fw)
      (setq wordpos (point)))
    (save-excursion
      (and (re-search-forward "\\w[A-Z][a-z]" nil t fw)
           (setq casepos (- (point) (if (> fw 0) 2 -1)))))
    (cond ((and wordpos casepos)
           (goto-char
            (if (< (abs (- casepos (point)))
                   (abs (- wordpos (point))))
                casepos
              wordpos)))
          (wordpos (goto-char wordpos))
          (casepos (goto-char casepos))
          (t (goto-char (if (> fw 0) (point-max) (point-min)))))))

;;;###autoload
(defun ywb-camelcase-forward-word (arg)
  (interactive "p")
  (let ((fw (signum arg)))
    (dotimes (i (abs arg))
      (ywb-camelcase-move-word fw))))

;;;###autoload
(defun ywb-camelcase-backward-word (arg)
  (interactive "p")
  (ywb-camelcase-forward-word (- arg)))

;;;###autoload
(defun ywb-get-column (start end)
  (interactive "r")
  (let ((cols (mapcar 'string-to-number
                      (split-string (read-from-minibuffer "cols(seperate by space): "))))
        (standard-output (get-buffer-create "*column*"))
        line)
    (with-current-buffer standard-output
      (erase-buffer))
    (save-excursion
      (goto-char start)
      (while (< (point) end)
        (setq line (split-string (buffer-substring-no-properties (line-beginning-position)
                                                                 (line-end-position))
                                 "\t"))
        (princ (mapconcat 'identity (mapcar (lambda (c) (nth (1- c) line)) cols) "\t"))
        (princ "\n")
        (forward-line 1)))
    (pop-to-buffer standard-output)))

(defvar ywb-scratch-buffer "*scratch*")
;;;###autoload
(defun ywb-create/switch-scratch (arg)
  (interactive "P")
  (when arg
    (setq ywb-scratch-buffer (read-buffer "Set scratch to: " (buffer-name))))
  (let ((buf (get-buffer ywb-scratch-buffer)))
    (if (null buf)
        (progn
          (or arg
              (setq ywb-scratch-buffer (if (y-or-n-p "The buffer no exists! Create *scratch*? ")
                                           "*scratch*"
                                         (read-buffer "Set scratch to: " (buffer-name)))))
          (switch-to-buffer ywb-scratch-buffer)
          (lisp-interaction-mode))
      (switch-to-buffer ywb-scratch-buffer))))

;;;###autoload
(defun ywb-delete-char-or-region ()
  (interactive)
  (if (and mark-active transient-mark-mode)
      (delete-region (region-beginning) (region-end))
    (call-interactively 'delete-char)))

;;;###autoload
(defun ywb-transpose-windows (arg)
  "Transpose the buffers shown in two windows."
  (interactive "p")
  (let ((selector (if (>= arg 0) 'next-window 'previous-window)))
    (while (/= arg 0)
      (let ((this-win (window-buffer))
            (next-win (window-buffer (funcall selector))))
        (set-window-buffer (selected-window) next-win)
        (set-window-buffer (funcall selector) this-win)
        (select-window (funcall selector)))
      (setq arg (if (plusp arg) (1- arg) (1+ arg))))))

;;;###autoload
(defun ywb-favorite-window-config (&optional percent)
  "Split window to proper portion"
  (interactive "P")
  (or percent (setq percent 50.5))
  (setq percent (/ percent 100.0))
  (let (buf)
    (if (> (length (window-list)) 1)
        (setq buf (window-buffer (next-window))))
    (delete-other-windows)
    (let ((maxwidth (window-width)))
      (split-window-horizontally (round (* maxwidth percent))))
    (if buf (save-selected-window
              (pop-to-buffer buf))))
  (call-interactively 'ywb-transpose-windows))

;;;###autoload
(defun ywb-set-paste ()
  (interactive)
  (fundamental-mode)
  (setq indent-line-function 'ignore))

(defvar ywb-switch-major-mode-history nil)
;;;###autoload
(defun ywb-switch-major-mode (mode)
  (interactive
   (list
    (intern
     (completing-read "Switch to mode: "
                      obarray (lambda (s)
                                (and (fboundp s)
                                     (string-match "-mode$" (symbol-name s))))
                      t nil 'ywb-switch-major-mode-history))))
  (setq ywb-switch-major-mode-history
        (cons (symbol-name major-mode) ywb-switch-major-mode-history))
  (funcall mode))


(defvar ywb-find-file-root-history nil
  "History list for files found using `find-file-root'.")

;;;###autoload 
(defun ywb-find-file-root ()
  "*Open a file as the root user.
   Prepends `find-file-root-prefix' to the selected file name so that it
   maybe accessed via the corresponding tramp method."
  (interactive)
  (require 'tramp)
  (let* ( ;; We bind the variable `file-name-history' locally so we can
         ;; use a separate history list for "root" files.
         (file-name-history find-file-root-history)
         (name (or buffer-file-name default-directory))
         (tramp (and (tramp-tramp-file-p name)
                     (tramp-dissect-file-name name)))
         (root-prefix "/sudo:root@localhost:")
         path dir file)

    ;; If called from a "root" file, we need to fix up the path.
;;     (when tramp
;;       (setq path (tramp-file-name-path tramp)
;;             dir (file-name-directory path)))

    (when (setq file (read-file-name "Find file (UID = 0): " dir path))
      (find-file (concat root-prefix file))
      ;; If this all succeeded save our new history list.
      (setq find-file-root-history file-name-history)
      ;; allow some user customization
      (run-hooks 'find-file-root-hook))))

;;;###autoload 
(defun ywb-ascii-table-show ()
  "Print the ascii table"
  (interactive)
  (with-current-buffer (get-buffer-create "*ASCII table*")
    (setq buffer-read-only nil)
    (erase-buffer)
    (let ((i   0)
          (tmp 0))
      (insert (propertize
               "                                [ASCII table]\n\n"
               'face font-lock-comment-face))
      (while (< i 32)
        (dolist (tmp (list i (+ 32 i) (+ 64 i) (+ 96 i)))
          (insert (concat
                   (propertize (format "%3d " tmp)
                               'face font-lock-function-name-face)
                   (propertize (format "[%2x]" tmp)
                               'face font-lock-constant-face)
                   "    "
                   (propertize (format "%3s" (single-key-description tmp))
                               'face font-lock-string-face)
                   (unless (= tmp (+ 96 i))
                     (propertize " | " 'face font-lock-variable-name-face)))))
        (newline)
        (setq i (+ i 1)))
      (goto-char (point-min)))
    (toggle-truncate-lines 1)
    (toggle-read-only 1)
    (display-buffer (current-buffer))))

(provide 'ywb-commands)

;;; ywb-commands.el ends here