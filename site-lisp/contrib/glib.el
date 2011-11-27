;;; glib.el --- 

;; Copyright 2007 Ye Wenbin
;;
;; Author: wenbinye@gmail.com
;; Version: $Id: glib.el,v 0.0 2007/09/24 16:44:04 ywb Exp $
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
;;   (require 'glib)

;;; Code:

(eval-when-compile
  (require 'cl))

(setq glib-src-directory-alist
      '(("glib" . "/home/ywb/softwares/source/gnome/glib-2.12.11/glib")
        ("gobject" . "/home/ywb/softwares/source/gnome/glib-2.12.11/gobject")
        ("gthread" . "/home/ywb/softwares/source/gnome/glib-2.12.11/gthread")
        ("gtk" . "/home/ywb/softwares/source/gnome/gtk+-2.10.11/gtk")
        ("cairo" . "/home/ywb/softwares/source/gnome/libcairo-1.4.2/src")
        ("goocanvas-1.0" . "/home/ywb/softwares/source/gnome/goocanvas-0.9/src/")))

(defun glib-catfile (&rest dirs)
  (let (name)
    (setq name (car dirs)
          dirs (cdr dirs))
    (while dirs
      (setq name (concat (file-name-as-directory name)
                         (car dirs))
            dirs (cdr dirs)))
    name))

(defun glib-splitdir (file &optional default-directory)
  (split-string (expand-file-name file (or default-directory "/"))
                "/" t))

(defun glib-find-symbol-1 (but)
  (glib-find-symbol (button-get but 'name) t))

(defun glib-find-file (file)
  (let ((dirs (last (glib-splitdir file) 2)))
    (glib-catfile
     (assoc-default (car dirs) glib-src-directory-alist)
     (concat (file-name-sans-extension (cadr dirs)) ".c"))))

(defun glib-find-symbol (sym &optional display)
  (interactive
   (let ((cur (thing-at-point 'symbol)))
     (unless (intern-soft cur clibpc-obarray)
       (setq cur nil))
     (list (completing-read (if (> (length cur) 0)
                                (format "Find (default %s): " cur)
                              "Find: ")
                            clibpc-obarray nil t nil nil cur)
           current-prefix-arg)))
  (setq sym (intern-soft sym clibpc-obarray))
  (when sym
    (let ((file (car (symbol-value sym)))
          name try)
      (setq file (glib-find-file file))
      (let (pos (marker (point-marker)))
        (when (file-exists-p file)
          (with-current-buffer (find-file-noselect file)
            (imenu (symbol-name sym))
            (if (assoc (symbol-name sym) (imenu--make-index-alist))
                (setq pos (point-marker)))))
        (unless pos
          (setq file (symbol-value sym))
          (with-current-buffer (find-file-noselect (car file))
            (goto-char (cdr file))
            (setq pos (point-marker))))
        (if display
            (with-selected-window (display-buffer (marker-buffer pos))
              (goto-char pos))
          (ring-insert find-tag-marker-ring marker)
          (switch-to-buffer (marker-buffer pos))
          (goto-char pos))))))

(setq clibpc-find-symbol-function 'glib-find-symbol-1)

(define-key c-mode-map "\en" 'glib-find-symbol)

(setq glib-html-doc-directory
  '(("" . "/usr/share/gtk-doc/html/")
    ("goocanvas-1.0" . "/usr/share/gtk-doc/html/goocanvas/")))

(defun glib-describe-function (func arg)
  (interactive
   (let ((cur (thing-at-point 'symbol)))
     (unless (intern-soft cur clibpc-obarray)
       (setq cur nil))
     (list (completing-read (if (> (length cur) 0)
                                (format "Describe function (default %s): " cur)
                              "Describe function: ")
                            clibpc-obarray nil t nil nil cur)
           current-prefix-arg)))
  (let ((sym (intern-soft func clibpc-obarray))
        file dirs dir index name try found)
    (setq file (car (symbol-value sym))
          dirs (last (glib-splitdir file) 2)
          dir (or (assoc-default (car dirs) glib-html-doc-directory)
                  (glib-catfile
                   (assoc-default "" glib-html-doc-directory)
                   (car dirs)))
          index (directory-files dir t "^ix"))
    (if index
        (while index
          (setq href (shell-command-to-string
                      (format "grep '<dt>%s,' %s" func (car index))))
          (if (= (length href) 0)
              (setq index (cdr index))
            (string-match "<a href=\"\\([^\"]+\\)\"" href)
            (browse-url-generic (concat (file-name-as-directory dir)
                                        (match-string 1 href)) arg)
            (setq index nil)))
      (setq href (shell-command-to-string
                  (format "grep '<a href=.*%s' %s/*.html" func dir)))
      (if (= (length href) 0)
          (message "No doc found!")
        (string-match "<a href=\"\\([^\"]+\\)\"" href)
        (browse-url-generic (concat (file-name-as-directory dir)
                                    (match-string 1 href)) arg)))))

(provide 'glib)
;;; glib.el ends here
