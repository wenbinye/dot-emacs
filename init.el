(if (not load-file-name)
    (error "Load me by M-x load-file RET"))

;; predefine directory:
;;  - ywb-startup-dir   : ~/.emacs.d
;;  - ywb-config-dir    : ~/.emacs.d/config
;;  - ywb-site-lisp-dir : ~/.emacs.d/site-lisp
(setq ywb-startup-dir (file-name-directory load-file-name)
      ywb-config-dir (expand-file-name "config" ywb-startup-dir)
      ywb-site-lisp-dir (expand-file-name "site-lisp" ywb-startup-dir))
(add-to-list 'load-path ywb-config-dir)
(add-to-list 'load-path ywb-site-lisp-dir)
(let ((default-directory ywb-site-lisp-dir))
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
    `(progn ,@forms))
  (defmacro deh-define-key (map &rest keypairs)
    "Define a batch of keys.

Example:
  (deh-define-key global-map
    (\"\\C-m\"        . 'newline-and-indent)
    (\"\\C-j\"        . 'newline))
"
    (declare (indent 1))
    (cons 'progn
          (mapcar (lambda (pair)
                    `(define-key ,map ,(car pair) ,(cdr pair)))
                  keypairs))))
;; ready to load my configurations
(mapc 'load (directory-files ywb-config-dir t "^[0-9]+-.*.el"))
(server-start)
