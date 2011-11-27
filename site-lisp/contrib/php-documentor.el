;;; php-documentor.el --- 

;; Copyright (C) 2010 Free Software Foundation, Inc.
;;
;; Author: Ye Wenbin <buzhi@taobao.com>
;; Maintainer: Ye Wenbin <buzhi@taobao.com>
;; Created: 10 Jan 2010
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
;;   (require 'php-documentor)

;;; Code:

(eval-when-compile
  (require 'cl))

(defvar php-documentor-scope-regexp (regexp-opt '("private" "protected" "public") t)
  "")

(defvar php-documentor-modifier-regexp (regexp-opt '("static" "abstract" "final") t)
  "")

(defvar php-documentor-func-regexp "^\\s-*\\([a-zA-Z ]*\\)function\\s-+\\([a-zA-Z0-9_]+\\)\\s-*(\\s-*\\(.*?\\)\\s-*)"
  "")

(defvar php-documentor-class-regexp
  (concat "^\\s-*\\([a-zA-Z]*\\)\\s-*" ;; modifier
          "\\(interface\\|class\\)\\s-*\\([a-zA-Z0-9_]+\\)" ;; class name
          "\\(?:\\s-+extends\\s-+\\([a-zA-Z0-9_]+\\)\\)?"
          "\\(?:\\s-+implements\\s-+\\([a-zA-Z0-9_ ,]+\\)\\)?")
  "")

(defvar php-documentor-var-regexp (concat "^\\s-*\\(\\(?:"
                                          (concat (regexp-opt '("private" "public" "protected" "static")) "\\s-+")
                                          "\\)+\\)\\(\\$[a-zA-Z0-9_]+\\)")
  "")

(defvar php-documentor-package nil
  "")

(defvar php-documentor-copyright nil
  "")

(defvar php-documentor-licence nil
  "")

(defvar php-documentor-version nil
  "")

(defun php-documentor-trim (str)
  (replace-regexp-in-string "\\(^[[:space:]\\n]*\\|[[:space:]\\n]*$\\)" "" str))

(defun php-documentor-dwim ()
  (interactive)
  (let ((type (php-documentor-context))
        function)
    (when type
      (setq function (intern-soft (format "php-documentor-%s" type)))
      (if (and function (commandp function))
          (call-interactively function)))))

(defun php-documentor-context ()
  (save-excursion
    (skip-syntax-forward "-b<>")
    (forward-line 0)
    (cond ((looking-at php-documentor-class-regexp) 'class)
          ((looking-at php-documentor-func-regexp) 'function)
          ((looking-at php-documentor-var-regexp) 'var))))

(defun php-documentor-function ()
  (interactive)
  (when (eq (php-documentor-context) 'function)
    (let ((modifier (match-string 1))
          (args (php-documentor-trim (match-string 3)))
          (tpl '(> "/**" n
                   "* " > p n
                   > "* " n)))
      (when (string-match php-documentor-scope-regexp modifier)
        (unless (string= (match-string 1 modifier) "public")
          (setq tpl (append tpl `(> "* @access " ,(match-string 1 modifier) n)))))
      (when (string-match php-documentor-modifier-regexp modifier)
        (setq tpl (append tpl `(> "* @" ,(match-string 1 modifier) n))))
      (when (> (length args) 0)
        (dolist (arg (split-string args "\\s-*,\\s-*"))
          (setq tpl (append tpl `(> "* @param " ,arg n)))))
      (setq tpl (append tpl '(> "* @return void" n
                                > "*/" n >)))
      (forward-line 0)
      (tempo-insert-template 'tpl nil))))

(defun php-documentor-class ()
  (interactive)
  (when (eq (php-documentor-context) 'class)
    (let ((modifier (match-string 1))
          (class-name (match-string 3))
          (extends (match-string 4))
          (implements (match-string 5))
          uses tpl)
      (setq tpl `(> "/**" n
                    "* " > p n
                    > "* " n))
      (when (string-match php-documentor-modifier-regexp modifier)
        (setq tpl (append tpl `(> "* @" ,(match-string 1 modifier) n))))
      (when (or extends implements)
        (setq tpl (append tpl `(> "* @uses " ,(mapconcat 'identity (cons extends (and implements (split-string implements "\\s-*,\\s-*"))) " ") n))))
      (setq tpl (append tpl `(> "* @package " ,php-documentor-package n
                                > "* @copyright " ,php-documentor-copyright n
                                > "* @author " ,user-full-name n
                                > "* @license " ,php-documentor-licence n
                                > "* @version " ,php-documentor-version n
                                > "*/" n >)))
      (when tpl
        (forward-line 0)
        (tempo-insert-template 'tpl nil)))))

(defun php-documentor-var ()
  (interactive)
  (when (eq (php-documentor-context) 'var)
    (let ((modifier (match-string 1))
          (var-name (match-string 2))
          tpl)
      (setq tpl `(> "/**" n
                    "* " > p n
                    > "* " n))
      (when (string-match php-documentor-modifier-regexp modifier)
        (setq tpl (append tpl `(> "* @" ,(match-string 1 modifier) n))))
      (when (string-match php-documentor-scope-regexp modifier)
        (setq tpl (append tpl `(> "* @access " ,(match-string 1 modifier) n))))
      (setq tpl (append tpl '(> "*/" n >)))
      (when tpl
        (forward-line 0)
        (tempo-insert-template 'tpl nil)))))

(provide 'php-documentor)
;;; php-documentor.el ends here
