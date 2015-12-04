(setq custom-file (expand-file-name "02-private.el" ywb--config-directory))

(set-register ?, (cons 'file ywb--config-directory))

(defalias 'yes-or-no-p 'y-or-n-p)

(when window-system
  (tool-bar-mode 0))

(custom-set-variables
 '(auto-image-file-mode t)
 '(backup-directory-alist (quote (("." . "~/.emacs.d/backup"))))
 '(column-number-mode t)
 '(comint-input-ring-file-name "~/.emacs.d/shell-history")
 '(confirm-kill-emacs (quote y-or-n-p))
 '(delete-old-versions t)
 '(desktop-save-mode t)
 '(diff-switches "-ubB")
 '(dired-dwim-target t)
 '(dired-listing-switches "-alvh")
 '(dired-omit-files "^\\.")
 '(hippie-expand-try-functions-list
   (quote
    (try-complete-file-name-partially try-complete-file-name try-expand-all-abbrevs try-expand-list try-expand-dabbrev try-expand-line try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill try-complete-lisp-symbol-partially try-complete-lisp-symbol)))
 '(ibuffer-default-sorting-mode (quote major-mode))
 '(ido-enable-regexp t)
 '(ido-everywhere t)
 '(ido-mode (quote both) nil (ido))
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(kill-whole-line t)
 '(mouse-avoidance-mode (quote animate) nil (avoid))
 '(outline-minor-mode-prefix "^C^O")
 '(read-file-name-completion-ignore-case t)
 '(safe-local-variable-values (quote ((firestarter . ert-run-tests-interactively))))
 '(scroll-bar-mode nil)
 '(sentence-end-double-space nil)
 '(show-paren-mode t)
 '(tab-width 4)
 '(uniquify-buffer-name-style (quote post-forward-angle-brackets) nil (uniquify))
 '(version-control t)
 '(woman-cache-filename "~/.wmncach.el"))
