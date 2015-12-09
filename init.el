(defconst emacs-start-time (current-time))
(unless noninteractive
  (message "Loading %s..." load-file-name))
(setq message-log-max 16384)

(defvar ywb--init-directory (file-name-directory load-file-name))
(defvar ywb--config-directory (concat ywb--init-directory "config"))

(add-to-list 'load-path (concat ywb--init-directory "site-lisp"))

(require 'cl)
(setq package-archives
      '(("chaozhuo" . "http://mirrors.chaozhuo.net/elpa/packages/")))

(package-initialize)

(eval-and-compile
  (defvar use-package-verbose t)
  ;; (defvar use-package-expand-minimally t)
  (eval-after-load 'advice
    `(setq ad-redefinition-action 'accept))
  (require 'cl)
  (require 'use-package))

(defun ywb-generate-loaddefs ()
  (interactive)
  (require 'autoload)
  (let ((generated-autoload-file (expand-file-name "100-autoloads.el" ywb--config-directory)))
    (update-directory-autoloads (concat ywb--init-directory "site-lisp"))))
(unless (file-exists-p (expand-file-name "100-autoloads.el" ywb--config-directory))
  (ywb-generate-loaddefs))

(mapc 'load (directory-files ywb--config-directory t "^[0-9]+-.*.el$"))

(when (or window-system t)
  (let ((elapsed (float-time (time-subtract (current-time)
                                            emacs-start-time))))
    (message "Loading %s...done (%.3fs)" load-file-name elapsed))
  (add-hook 'after-init-hook
            `(lambda ()
               (let ((elapsed (float-time (time-subtract (current-time)
                                                         emacs-start-time))))
                 (message "Loading %s...done (%.3fs) [after-init]"
                          ,load-file-name elapsed))) t))
(server-start)
(setenv "EDITOR" "emacsclient")
(setenv "PAGER" "cat")
;; debug 
;; (setq user-init-file load-file-name)
;; (setq confirm-kill-emacs nil)
;; (desktop-save-mode 0)
