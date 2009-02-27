(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/mycontrib/"))
(unless (require 'dot-emacs-helper nil t)
  (defmacro deh-require-maybe (feature &rest forms)
    (declare (indent 1))
    `(progn (when (require ,feature nil t) ,@forms)))
  (defalias 'deh-require 'deh-require-maybe)
  (put 'deh-require 'lisp-indent-function 1)
  (defmacro deh-section (section &rest forms)
    (declare (indent 1))
    `(progn ,@forms)))

(mapc 'load (directory-files "~/.emacs.d/config/" t "^[0-9]+-.*.el"))

;; run lisp-interaction-mode-hook on *scratch*
(add-hook 'emacs-startup-hook
          (lambda ()
            (with-current-buffer (get-buffer-create "*scratch*")
              (setq buffer-offer-save nil)
              (auto-save-mode nil)
              (lisp-interaction-mode))))
(server-start)
