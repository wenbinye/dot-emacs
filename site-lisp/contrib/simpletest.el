;;; simpletest.el --- Helper function for unittest with Simpletest

;; Copyright (C) 2009 Free Software Foundation, Inc.
;;
;; Author: Ye Wenbin <wenbinye@gmail.com>
;; Maintainer: Ye Wenbin <wenbinye@gmail.com>
;; Created: 21 Dec 2009
;; Version: 0.01
;; Keywords: languages, tools

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
;; Features privodes:
;;  1. automatic generate test
;;  2. run test for select function

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'simpletest)
;;   (add-hook 'php-mode-hook (lambda () (simpletest-mode 1)))

;;; Code:

(eval-when-compile
  (require 'cl))

(defvar simpletest-class-regexp
  "^\\s-*class\\s-+\\(\\(?:\\sw\\|\\s_\\)+\\)\\s-*"
  "Regexp that match class declaration")

(defvar simpletest-function-regexp
  "^\\s-*\\(\\(?:\\(?:abstract\\|final\\|private\\|protected\\|public\\|static\\)\\s-+\\)*\\)function\\s-+\\(\\(?:\\sw\\|\\s_\\)+\\)\\s-*("
  "Regexp that match function declaration")

(defvar simpletest-project-config
  '(
    (get-test-class-function   . simpletest-get-test-class-suffix)
    (get-source-class-function . simpletest-get-source-class-suffix)
    (get-source-file-function  . simpletest-get-source-file-annotated)
    (get-test-file-function    . simpletest-get-test-file-simple)
    (get-file-type-function    . simpletest-get-file-type-simple)
    (source-directory      . "lib")
    (test-directory        . "tests")
    (source-file-extension . ".php")
    (test-file-extension   . ".php")
    )
  "Project setting for misc operations.

 `get-test-class-function': get test class name from source class name
 `get-source-class-function': get source class name from test class name
 `get-source-file-function': get source file from source class name
 `get-test-file-function': get test file from test class name
 `get-file-type-function': detect whether current file is in source file or test file
 `source-directory': source file directory, default is 'lib'
 `test-directory': test file directory, default is 'tests'
 `source-file-extension': file extension for source file
 `test-file-extension': file extension for test file
")

(defvar simpletest-create-test-function 'simpletest-create-test-simple
  "Function to create test class code")

(defvar simpletest-config-file ".simpletest-config"
  "Project configuration file name")

(defvar simpletest-template-file "simpletest.tpl"
  "Template file name for test file")

(defvar simpletest-class-cache-file ".simpletest-classes"
  "Cache file name for lookup class-file")

(defun simpletest-class-ap ()
  "Get current class name"
  (save-excursion
    (if (re-search-backward simpletest-class-regexp nil t)
        (match-string-no-properties 1))))

(defun simpletest-function-ap ()
  "Get current function name"
  (save-excursion
    (goto-char (line-end-position))
    (if (re-search-backward simpletest-function-regexp nil t)
        (cons (match-string-no-properties 1) (match-string-no-properties 2)))))

(defun simpletest-file-class ()
  "Get class name for current file"
  (or (simpletest-class-ap)
      (save-excursion
        (goto-char (point-min))
        (if (re-search-forward simpletest-class-regexp nil t)
            (match-string-no-properties 1)))))

(defun simpletest-file-functions ()
  "Get all functions for current file"
  (let (functions)
    (save-excursion
      (goto-char (point-min))
      (while (and (not (eobp))
                  (re-search-forward simpletest-function-regexp nil t))
        (push (cons (match-string-no-properties 1) (match-string-no-properties 2))
              functions)))
    functions))

(defun simpletest-project-config (entry)
  (if (assoc entry simpletest-project-config)
      (cdr (assoc entry simpletest-project-config))
    (cdr (assoc entry (default-value 'simpletest-project-config)))))

(defun simpletest-get-test-class-suffix (class)
  "Add suffix 'Test' to get test class name"
  (concat class "Test"))

(defun simpletest-get-source-class-suffix (test-class)
  "Remove suffix 'Test' to get source class name"
  (replace-regexp-in-string "Test$" "" test-class))

(defun simpletest-get-test-class (class)
  "Get test class name for `class'"
  (funcall (simpletest-project-config 'get-test-class-function) class))

(defun simpletest-get-source-class (test-class)
  "Get source class name for `test-class'"
  (funcall (simpletest-project-config 'get-source-class-function) test-class))

(defun simpletest-find-top-directory (file &optional dir)
  "Find `file' in all parent directories of `dir'(include `dir')"
  (or dir (setq dir (expand-file-name default-directory)))
  (let ((thefile (expand-file-name file dir)))
    (if (file-exists-p thefile)
        thefile
      (setq pdir (directory-file-name (file-name-directory dir)))
      (if (string= pdir dir)
          nil
        (simpletest-find-top-directory file pdir)))))

;; (defun simpletest-find-file-cached (class)
;;   "Find class name from cached file"
;;   (let ((file (simpletest-find-top-directory simpletest-class-cache-file)))
;;     (unless file
;;       (let ((dir (read-directory-name "Project root directory: ")))
;;         (simpletest-build-cache-file dir)))))

(defun simpletest-get-source-file (class)
  "Get source file for `class'"
  (funcall (simpletest-project-config 'get-source-file-function) class))

(defun simpletest-get-test-file (test-class)
  "Get test file for `test-class'"
  (funcall (simpletest-project-config 'get-test-file-function) test-class))

(defun simpletest-get-test-file-simple (test-class)
  "Get file in `test-directory'.
The file name equals to test class name + `test-file-extension'."
  (let ((dir (simpletest-find-top-directory (simpletest-project-config 'test-directory))))
    (if dir
        (concat (file-name-as-directory dir) test-class
                (simpletest-project-config 'test-file-extension))
      (message "Can't not find test directory '%s'" (simpletest-project-config 'test-directory)))))

(defun simpletest-get-source-file-annotated (class)
  "Find source file from annotation in current buffer when edit test file"
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "@source\\s-+\\([--:\\\\$+<>@-Z_a-z~*?\x100-\xffff]+\\)" nil t)
        (match-string 1))))

(defun simpletest-get-test-file-pear (test-class)
  "Find test file using pear class name rule"
  (let ((dir (simpletest-find-top-directory (simpletest-project-config 'test-directory))))
    (if dir
        (concat (file-name-as-directory dir)
                (replace-regexp-in-string "_" "/" test-class)
                (simpletest-project-config 'test-file-extension))
      (message "Can't not find test directory '%s'" (simpletest-project-config 'test-directory)))))

(defun simpletest-get-source-file-pear (class)
  "Find source file using pear class name rule"
  (let ((dir (simpletest-find-top-directory (simpletest-project-config 'source-directory))))
    (if dir
        (concat (file-name-as-directory dir)
                (replace-regexp-in-string "_" "/" class)
                (simpletest-project-config 'source-file-extension))
      (message "Can't not find test directory '%s'" (simpletest-project-config 'source-directory)))))

(defun simpletest-get-file-type ()
  "Detect file whether is test class or source class"
  (funcall (simpletest-project-config 'get-file-type-function)))

(defun simpletest-get-file-type-simple ()
  "Detect php file type by check whether current class name end with 'Test'"
  (if (string-match "Test$" (simpletest-file-class))
      'test
    'source))

(defun simpletest-load-config (&optional reload)
  "Load simpletest config"
  (interactive "P")
  (when (or (not (local-variable-p 'simpletest-project-config)) reload)
    (make-local-variable 'simpletest-project-config)
    (let ((file (simpletest-find-top-directory simpletest-config-file)))
      (when file
        (load file t)))))

(defun simpletest-create-test-template (test-class test-file source-class source-file)
  "create test code using `template-simple'"
  (let ((file (locate-file simpletest-template-file template-directory-list)))
    (if file
        (template-simple-expand-template file)
      (message "Template file '%s' is not exists" simpletest-template-file))))

(defun simpletest-create-test-simple (test-class test-file source-class source-file)
  "Create test code"
  (insert (format "<?php
/**
 * TestCase for %s
 * @source %s
 */
require_once('simpletest/autorun.php');
class %s extends UnitTestCase
{
    function setUp()
    {
    }
}
"
                  source-class test-file test-class)))

(defun simpletest-create-test-1 (test-class test-file source-class source-file &optional other-window)
  (funcall (if other-window 'find-file-other-window 'find-file) test-file)
  (funcall simpletest-create-test-function test-class test-file source-class source-file)
  (save-buffer))

(defun simpletest-get-source-function (test-function)
  "Get function name from `test-function'"
  (let ((name (replace-regexp-in-string "^test" "" test-function)))
    (if (> (length name) 0)
        ;; downcase first letter
        (concat (downcase (substring name 0 1)) (substring name 1))
      name)))

(defun simpletest-get-test-function (function)
  "Get function name from `test-function'"
  (concat "test" (upcase-initials function)))

(defun simpletest-function-position (function)
  "Get point for `function'"
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward (concat "^\\s-*\\(\\(?:\\(?:abstract\\|final\\|private\\|protected\\|public\\|static\\)\\s-+\\)*\\)function\\s-+"
                                   (regexp-quote function)
                                   "\\s-*(") nil t)
        (+ (line-end-position) 1))))

(defun simpletest-goto-function (function)
  "Go to point for `function'"
  (let ((pos (simpletest-function-position function)))
    (if pos (goto-char pos))))

(defun simpletest-switch (&optional other-window)
  "Switch between test file and source file"
  (interactive "P")
  (simpletest-load-config)
  (let ((function (simpletest-function-ap))
        (class (simpletest-file-class))
        (find-file-function (if other-window 'find-file-other-window 'find-file))
        file)
    (if class
        (if (eq (simpletest-get-file-type) 'test)
            (progn
              (setq file (simpletest-get-source-file (simpletest-get-source-class class)))
              (if (and file (file-exists-p file))
                  (progn
                    (funcall find-file-function file)
                    (and function
                         (simpletest-goto-function (simpletest-get-source-function (cdr function)))))
                (message "Can't locate source file for class '%s'" class)))
          (let ((test-class (simpletest-get-test-class class)))
            (setq file (simpletest-get-test-file test-class))
            (if (and file (file-exists-p file))
                (progn
                  (funcall find-file-function file)
                  (and function
                       (simpletest-goto-function (simpletest-get-test-function (cdr function)))))
              (when (y-or-n-p (format "Test class '%s' not exists, create file %s" test-class file))
                (when (not (file-exists-p (file-name-directory file)))
                  (make-directory (file-name-directory file) t))
                (simpletest-create-test-1 test-class file
                                          class (file-relative-name buffer-file-name (file-name-directory file))
                                          other-window)))))
      (message "No class found in current buffer"))))

(defun simpletest-create-test (&optional all)
  "Create test method for current class.
With prefix argument, create all test function in current class"
  (interactive "P")
  (if (eq (simpletest-get-file-type) 'source)
      (let ((class (simpletest-file-class))
            (source-file buffer-file-name)
            functions class-end pos)
        (dolist (function
                 (if all
                     (simpletest-file-functions)
                   (list (simpletest-function-ap))))
          (when (and (consp function)
                     (not (string-match "protected\\|private" (car function))))
            (push (simpletest-get-test-function (cdr function)) functions)))
        (simpletest-switch)
        (if functions
            (save-excursion
              (goto-char (point-min))
              (when (not (re-search-forward simpletest-class-regexp nil t))
                (save-excursion 
                  (funcall simpletest-create-test-function
                           (simpletest-get-test-class class) buffer-file-name
                           class source-file))
                (re-search-forward simpletest-class-regexp nil t))
              (when (re-search-forward "{" nil t)
                (backward-char 1)
                (setq class-end (copy-marker (scan-sexps (point) 1)))
                (dolist (function functions)
                  (unless (simpletest-function-position function)
                    (goto-char class-end)
                    (backward-char 1)
                    (delete-region (progn (skip-chars-backward " \n\t") (point)) (1- class-end))
                    (insert (format "\n\n    function %s()\n    {\n        \n    }\n" function))
                    (setq pos (- (point) 7))))))
          (message "No function to create test"))
        (if pos (goto-char pos)))
    (message "Not in source file")))

(defun simpletest-run-test ()
  "Run test for current function."
  (interactive)
  (let ((class (simpletest-file-class))
        (function (simpletest-function-ap))
        test-file test-function compile-command)
    (if (and class function)
        (progn
          (if (eq (simpletest-get-file-type) 'source)
              (setq test-file (simpletest-get-test-file (simpletest-get-test-class class))
                    test-function (simpletest-get-test-function (cdr function)))
            (setq test-file buffer-file-name
                  test-function (cdr function)))
          (setq compile-command (format "php \"%s\" --test=%s" test-file test-function))
          (call-interactively 'compile))
      (message "No test found"))))

(define-minor-mode simpletest-mode
  "Toggle simpletest mode."
  :lighter " Simpletest"
  :keymap
  '(("\C-c\C-tb" . simpletest-switch)
    ("\C-c\C-tc" . simpletest-create-test)
    ("\C-c\C-tr" . simpletest-run-test)
    ("\C-c\C-t\C-b" . simpletest-switch)
    ("\C-c\C-t\C-c" . simpletest-create-test)
    ("\C-c\C-t\C-r" . simpletest-run-test)))

(provide 'simpletest)
;;; simpletest.el ends here
