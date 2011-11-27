;;; ws.el --- 

;; Copyright (C) 2009 Free Software Foundation, Inc.
;;
;; Author: Ye Wenbin <wenbinye@gmail.com>
;; Maintainer: Ye Wenbin <wenbinye@gmail.com>
;; Created: 31 Aug 2009
;; Version: 0.01
;; Keywords

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
;;   (require 'ws)
;;
;; Use M-x ws-mode to enable M-f (forward-word) and M-b (backward-word).

;;; Code:

(eval-when-compile
  (require 'cl))

(defvar ws-initialized nil)
(defvar ws-words-buffer nil)
(defvar ws-reverse-words-buffer nil)

(defvar ws-words-file "~/.emacs.d/ws/words.txt")
(defvar ws-reverse-words-file "~/.emacs.d/ws/rwords.txt")

(defun ws-create-words-file ()
  "Create words file from eim py.txt"
  (interactive)
  (let ((py (locate-file "py.txt" load-path))
        words ws)
    (when py
      (with-temp-buffer
        (setq words (make-hash-table :test 'equal))
        (insert-file-contents py)
        (re-search-forward "\\[Table\\]")
        (forward-line 1)
        (while (not (eobp))
          (mapc
           (lambda (w)
             (if (> (length w) 1)
                 (puthash w t words)))
           (cdr (split-string (buffer-substring-no-properties
                               (point) (progn (forward-line 1) (- (point) 1))))))))
      (with-temp-buffer
        (maphash (lambda (word ignore) (push word ws)) words)
        (mapc (lambda (word) (insert word "\n"))
              (setq ws (sort ws 'string<)))
        (write-region (point-min) (point-max) ws-words-file)
        (erase-buffer)
        (mapc (lambda (word) (insert word "\n"))
              (sort (mapcar 'ws-reverse-string ws) 'string<))
        (write-region (point-min) (point-max) ws-reverse-words-file)
        ))))

(defun ws-init ()
  (unless ws-initialized
    (save-excursion
      (set-buffer (generate-new-buffer " ws-words"))
      (insert-file-contents ws-words-file)
      (setq ws-words-buffer (current-buffer))
      (set-buffer (generate-new-buffer " ws-rwords"))
      (insert-file-contents ws-reverse-words-file)
      (setq ws-reverse-words-buffer (current-buffer))
      (setq ws-initialized t))))

(defun ws-bisearch-word (prefix start end)
  (let ((mid (/ (+ start end) 2))
        (len (length prefix))
        line)
    (goto-char mid)
    (beginning-of-line)
    (setq line (buffer-substring-no-properties (point) (+ (point) len)))
    (if (string= line prefix)
        (buffer-substring-no-properties (point) (line-end-position))
      (if (> mid start)
          (if (string< line prefix)
              (ws-bisearch-word prefix mid end)
            (ws-bisearch-word prefix start mid))))))

(defun ws-get-words (prefix &optional reverse)
  (ws-init)
  (with-current-buffer (if reverse ws-reverse-words-buffer ws-words-buffer)
    (when (ws-bisearch-word prefix (point-min) (point-max))
      (let ((len (length prefix))
            words pos)
        (save-excursion
          (while (and (not (bobp))
                      (progn
                        (setq pos (point))
                        (forward-line -1)
                        (string= (buffer-substring-no-properties (point) (+ (point) len))
                                 prefix)))
            (push (buffer-substring-no-properties (point) (- pos 1)) words)))
        (while (and (not (eobp))
                    (string= (buffer-substring-no-properties (point) (+ (point) len))
                             prefix))
          (push (buffer-substring-no-properties (point) (progn (forward-line 1) (- (point) 1))) words))
        words))))

(defun ws-forward-word ()
  (interactive)
  (if (looking-at "\\cC")
      (let ((words (ws-get-words (buffer-substring-no-properties (point) (+ (point) 1)))))
        (or (and words (re-search-forward (concat "\\=" (regexp-opt words)) nil t))
            (forward-char 1)))
    (let (pos)
      (save-excursion
        (if (re-search-forward "\\cC" nil t)
            (setq pos (- (point) 1))))
      (if pos
          (goto-char (min pos (save-excursion (forward-word) (point))))
        (forward-word)))))

(defun ws-reverse-string (str)
  (concat (reverse (append str nil))))

(defun ws-backward-word ()
  (interactive)
  (if (looking-back "\\cC")
      (let ((words (ws-get-words (buffer-substring-no-properties (- (point) 1) (point)) t)))
        (or (and words (re-search-backward (concat (regexp-opt (mapcar 'ws-reverse-string words)) "\\=") nil t))
            (backward-char 1)))
    (let (pos)
      (save-excursion
        (if (re-search-backward "\\cC" nil t)
            (setq pos (+ (point) 1))))
      (if pos
          (goto-char (max pos (save-excursion (backward-word) (point))))
        (backward-word)))))

(define-minor-mode ws-mode
  "Buffer-local minor mode to move word by chinese word."
  :group 'ws
  :global t
  :lighter " WS"
  :keymap
  `((,(kbd "M-f") . ws-forward-word)
    (,(kbd "M-b") . ws-backward-word))
  )

(provide 'ws)
;;; ws.el ends here
