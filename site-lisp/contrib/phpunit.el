;;; phpunit.el --- Helper function for unittest with Phpunit

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
;;   (require 'phpunit)
;;   (add-hook 'php-mode-hook (lambda () (phpunit-mode 1)))

;;; Code:

(eval-when-compile
  (require 'cl))

(defvar phpunit-class-regexp
  "^\\s-*\\(?:abstract\\s-+\\)?class\\s-+\\(\\(?:\\sw\\|\\s_\\)+\\)\\s-*"
  "Regexp that match class declaration")

(defvar phpunit-function-regexp
  "^\\s-*\\(\\(?:\\(?:abstract\\|final\\|private\\|protected\\|public\\|static\\)\\s-+\\)*\\)function\\s-+\\(\\(?:\\sw\\|\\s_\\)+\\)\\s-*("
  "Regexp that match function declaration")

(defvar phpunit-project-config
  '(
    (get-test-class-function   . phpunit-get-test-class-suffix)
    (get-source-class-function . phpunit-get-source-class-suffix)
    (get-source-file-function  . phpunit-get-source-file-annotated)
    (get-test-file-function    . phpunit-get-test-file-simple)
    (get-file-type-function    . phpunit-get-file-type-simple)
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

(defvar phpunit-create-test-function 'phpunit-create-test-simple
  "Function to create test class code")

(defvar phpunit-config-file ".phpunit-config"
  "Project configuration file name")

(defvar phpunit-template-file "phpunit.tpl"
  "Template file name for test file")

(defvar phpunit-class-cache-file ".phpunit-classes"
  "Cache file name for lookup class-file")

(defun phpunit-class-ap ()
  "Get current class name"
  (save-excursion
    (if (re-search-backward phpunit-class-regexp nil t)
        (match-string-no-properties 1))))

(defun phpunit-function-ap ()
  "Get current function name"
  (save-excursion
    (goto-char (line-end-position))
    (if (re-search-backward phpunit-function-regexp nil t)
        (cons (match-string-no-properties 1) (match-string-no-properties 2)))))

(defun phpunit-file-class ()
  "Get class name for current file"
  (or (phpunit-class-ap)
      (save-excursion
        (goto-char (point-min))
        (if (re-search-forward phpunit-class-regexp nil t)
            (match-string-no-properties 1)))))

(defun phpunit-file-functions ()
  "Get all functions for current file"
  (let (functions)
    (save-excursion
      (goto-char (point-min))
      (while (and (not (eobp))
                  (re-search-forward phpunit-function-regexp nil t))
        (push (cons (match-string-no-properties 1) (match-string-no-properties 2))
              functions)))
    functions))

(defun phpunit-project-config (entry)
  (if (assoc entry phpunit-project-config)
      (cdr (assoc entry phpunit-project-config))
    (cdr (assoc entry (default-value 'phpunit-project-config)))))

(defun phpunit-get-test-class-suffix (class)
  "Add suffix 'Test' to get test class name"
  (concat class "Test"))

(defun phpunit-get-source-class-suffix (test-class)
  "Remove suffix 'Test' to get source class name"
  (replace-regexp-in-string "Test$" "" test-class))

(defun phpunit-get-test-class (class)
  "Get test class name for `class'"
  (funcall (phpunit-project-config 'get-test-class-function) class))

(defun phpunit-get-source-class (test-class)
  "Get source class name for `test-class'"
  (funcall (phpunit-project-config 'get-source-class-function) test-class))

(defun phpunit-find-top-directory (file &optional dir)
  "Find `file' in all parent directories of `dir'(include `dir')"
  (or dir (setq dir (expand-file-name default-directory)))
  (let ((thefile (expand-file-name file dir)))
    (if (file-exists-p thefile)
        thefile
      (setq pdir (directory-file-name (file-name-directory dir)))
      (if (string= pdir dir)
          nil
        (phpunit-find-top-directory file pdir)))))

;; (defun phpunit-find-file-cached (class)
;;   "Find class name from cached file"
;;   (let ((file (phpunit-find-top-directory phpunit-class-cache-file)))
;;     (unless file
;;       (let ((dir (read-directory-name "Project root directory: ")))
;;         (phpunit-build-cache-file dir)))))

(defun phpunit-get-source-file (class)
  "Get source file for `class'"
  (funcall (phpunit-project-config 'get-source-file-function) class))

(defun phpunit-get-test-file (test-class)
  "Get test file for `test-class'"
  (funcall (phpunit-project-config 'get-test-file-function) test-class))

(defun phpunit-get-test-file-simple (test-class)
  "Get file in `test-directory'.
The file name equals to test class name + `test-file-extension'."
  (let ((dir (phpunit-find-top-directory (phpunit-project-config 'test-directory))))
    (if dir
        (concat (file-name-as-directory dir) test-class
                (phpunit-project-config 'test-file-extension))
      (message "Can't not find test directory '%s'" (phpunit-project-config 'test-directory)))))

(defun phpunit-get-source-file-annotated (class)
  "Find source file from annotation in current buffer when edit test file"
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "@source\\s-+\\([--:\\\\$+<>@-Z_a-z~*?\x100-\xffff]+\\)" nil t)
        (match-string 1))))

(defun phpunit-get-test-file-pear (test-class)
  "Find test file using pear class name rule"
  (let ((dir (phpunit-find-top-directory (phpunit-project-config 'test-directory))))
    (if dir
        (concat (file-name-as-directory dir)
                (replace-regexp-in-string "_" "/" test-class)
                (phpunit-project-config 'test-file-extension))
      (message "Can't not find test directory '%s'" (phpunit-project-config 'test-directory)))))

(defun phpunit-get-source-file-pear (class)
  "Find source file using pear class name rule"
  (let ((dir (phpunit-find-top-directory (phpunit-project-config 'source-directory))))
    (if dir
        (concat (file-name-as-directory dir)
                (replace-regexp-in-string "_" "/" class)
                (phpunit-project-config 'source-file-extension))
      (message "Can't not find test directory '%s'" (phpunit-project-config 'source-directory)))))

(defun phpunit-get-file-type ()
  "Detect file whether is test class or source class"
  (funcall (phpunit-project-config 'get-file-type-function)))

(defun phpunit-get-file-type-simple ()
  "Detect php file type by check whether current class name end with 'Test'"
  (if (string-match "Test$" (phpunit-file-class))
      'test
    'source))

(defun phpunit-load-config (&optional reload)
  "Load phpunit config"
  (interactive "P")
  (when (or (not (local-variable-p 'phpunit-project-config)) reload)
    (make-local-variable 'phpunit-project-config)
    (let ((file (phpunit-find-top-directory phpunit-config-file)))
      (when file
        (load file t)))))

(defun phpunit-create-test-template (test-class test-file source-class source-file)
  "create test code using `template-simple'"
  (let ((file (locate-file phpunit-template-file template-directory-list)))
    (if file
        (template-simple-expand-template file)
      (message "Template file '%s' is not exists" phpunit-template-file))))

(defun phpunit-create-test-simple (test-class test-file source-class source-file)
  "Create test code"
  (insert (format "<?php
/**
 * TestCase for %s
 * @source %s
 */
require_once('PHPUnit/Framework.php');
require_once('%s');
class %s extends PHPUnit_Framework_TestCase
{
    protected function setUp()
    {
    }
}
"
                  source-class source-file source-file test-class)))

(defun phpunit-create-test-1 (test-class test-file source-class source-file &optional other-window)
  (funcall (if other-window 'find-file-other-window 'find-file) test-file)
  (funcall phpunit-create-test-function test-class test-file source-class source-file)
  (save-buffer))

(defun phpunit-get-source-function (test-function)
  "Get function name from `test-function'"
  (let ((name (replace-regexp-in-string "^test" "" test-function)))
    (if (> (length name) 0)
        ;; downcase first letter
        (concat (downcase (substring name 0 1)) (substring name 1))
      name)))

(defun phpunit-get-test-function (function)
  "Get function name from `test-function'"
  (concat "test" (upcase-initials function)))

(defun phpunit-function-position (function)
  "Get point for `function'"
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward (concat "^\\s-*\\(\\(?:\\(?:abstract\\|final\\|private\\|protected\\|public\\|static\\)\\s-+\\)*\\)function\\s-+"
                                   (regexp-quote function)
                                   "\\s-*(") nil t)
        (+ (line-end-position) 1))))

(defun phpunit-goto-function (function)
  "Go to point for `function'"
  (let ((pos (phpunit-function-position function)))
    (if pos (goto-char pos))))

(defun phpunit-switch (&optional other-window)
  "Switch between test file and source file"
  (interactive "P")
  (phpunit-load-config)
  (let ((function (phpunit-function-ap))
        (class (phpunit-file-class))
        (find-file-function (if other-window 'find-file-other-window 'find-file))
        file)
    (if class
        (if (eq (phpunit-get-file-type) 'test)
            (progn
              (setq file (phpunit-get-source-file (phpunit-get-source-class class)))
              (if (and file (file-exists-p file))
                  (progn
                    (funcall find-file-function file)
                    (and function
                         (phpunit-goto-function (phpunit-get-source-function (cdr function)))))
                (message "Can't locate source file for class '%s'" class)))
          (let ((test-class (phpunit-get-test-class class)))
            (setq file (phpunit-get-test-file test-class))
            (if (and file (file-exists-p file))
                (progn
                  (funcall find-file-function file)
                  (and function
                       (phpunit-goto-function (phpunit-get-test-function (cdr function)))))
              (when (y-or-n-p (format "Test class '%s' not exists, create file %s" test-class file))
                (when (not (file-exists-p (file-name-directory file)))
                  (make-directory (file-name-directory file) t))
                (phpunit-create-test-1 test-class file
                                          class (file-relative-name buffer-file-name (file-name-directory file))
                                          other-window)))))
      (message "No class found in current buffer"))))

(defun phpunit-create-test (&optional all)
  "Create test method for current class.
With prefix argument, create all test function in current class"
  (interactive "P")
  (if (eq (phpunit-get-file-type) 'source)
      (let ((class (phpunit-file-class))
            (source-file buffer-file-name)
            functions class-end pos)
        (dolist (function
                 (if all
                     (phpunit-file-functions)
                   (list (phpunit-function-ap))))
          (when (and (consp function)
                     (not (string-match "protected\\|private" (car function))))
            (push (phpunit-get-test-function (cdr function)) functions)))
        (phpunit-switch)
        (if functions
            (save-excursion
              (goto-char (point-min))
              (when (not (re-search-forward phpunit-class-regexp nil t))
                (save-excursion 
                  (funcall phpunit-create-test-function
                           (phpunit-get-test-class class) buffer-file-name
                           class source-file))
                (re-search-forward phpunit-class-regexp nil t))
              (when (re-search-forward "{" nil t)
                (backward-char 1)
                (setq class-end (copy-marker (scan-sexps (point) 1)))
                (dolist (function functions)
                  (unless (phpunit-function-position function)
                    (goto-char class-end)
                    (backward-char 1)
                    (delete-region (progn (skip-chars-backward " \n\t") (point)) (1- class-end))
                    (insert (format "\n\n    function %s()\n    {\n        \n    }\n" function))
                    (setq pos (- (point) 7))))))
          (message "No function to create test"))
        (if pos (goto-char pos)))
    (message "Not in source file")))

(defun phpunit-run-test (&optional all)
  "Run test for current function."
  (interactive "P")
  (let ((class (phpunit-file-class))
        (function (phpunit-function-ap))
        (file-type (phpunit-get-file-type))
        test-class test-file test-function compile-command)
    (if class
        (progn
          (if (eq file-type 'source)
              (setq test-file (phpunit-get-test-file (phpunit-get-test-class class))
                    test-class (phpunit-get-test-class class))
            (setq test-file buffer-file-name
                  test-class class))
          (if all
              (setq compile-command (format "phpunit %s \"%s\"" class test-file))
            (setq test-function (if (eq file-type 'source) (phpunit-get-test-function (cdr function)) (cdr function)))
            (setq compile-command (format "phpunit --filter /::%s$/ %s \"%s\"" test-function class test-file)))
          (call-interactively 'compile))
      (message "No test found"))))

(define-minor-mode phpunit-mode
  "Toggle phpunit mode."
  :lighter " Phpunit"
  :keymap
  '(("\C-c\C-tb" . phpunit-switch)
    ("\C-c\C-tc" . phpunit-create-test)
    ("\C-c\C-tr" . phpunit-run-test)
    ("\C-c\C-t\C-b" . phpunit-switch)
    ("\C-c\C-t\C-c" . phpunit-create-test)
    ("\C-c\C-t\C-r" . phpunit-run-test)))

(provide 'phpunit)
;;; phpunit.el ends here
