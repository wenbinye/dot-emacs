(if load-file-name
    (let ((ywb-startup-dir (file-name-directory load-file-name)))
      ;; add site-lisp and subdirs to load-path
      (let ((default-directory (expand-file-name "site-lisp" ywb-startup-dir)))
        (add-to-list 'load-path default-directory)
        (load "subdirs.el"))
      ;; if no dot-emacs-helper.el, use this to inhibit load errors
      (unless (require 'dot-emacs-helper nil t)
        (defmacro deh-require-maybe (feature &rest forms)
          (declare (indent 1))
          `(progn (when (require ,feature nil t) ,@forms)))
        (defalias 'deh-require 'deh-require-maybe)
        (put 'deh-require 'lisp-indent-function 1)
        (defmacro deh-section (section &rest forms)
          (declare (indent 1))
          `(progn ,@forms)))
      ;; ready to load my configurations
      (mapc 'load (directory-files (expand-file-name "config" ywb-startup-dir) t "^[0-9]+-.*.el"))
      (server-start))
  (message "Load me by M-x load-file RET"))
