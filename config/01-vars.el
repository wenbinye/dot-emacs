(setq custom-file (expand-file-name "02-private.el" ywb--config-directory))

(set-register ?, (cons 'file ywb--config-directory))

(defalias 'yes-or-no-p 'y-or-n-p)

(when window-system
  (let (default-font zh-font)
    (cond
     ((eq window-system 'x)
      (setq default-font "Ubuntu Mono 16"
            zh-font (font-spec :family "Sans" :size 16)))
     ((eq window-system 'w32)
      (setq default-font "Consolas 11"
            zh-font (font-spec :family "Microsoft Yahei" :size 14))))
    (when default-font
      (set-default-font default-font)
      (setq fontset (frame-parameter nil 'font))
      (dolist (charset '(kana han symbol cjk-misc bopomofo))
        (set-fontset-font fontset charset zh-font))
      (add-to-list 'default-frame-alist `(font . ,fontset))))
  (tool-bar-mode 0))

(custom-set-variables
 '(auto-image-file-mode t)
 '(backup-directory-alist (quote (("." . "~/.emacs.d/backup"))))
 '(column-number-mode t)
 '(confirm-kill-emacs (quote y-or-n-p))
 '(delete-old-versions t)
 '(desktop-save-mode t)
 '(diff-switches "-ubB")
 '(dired-dwim-target t)
 '(dired-listing-switches "-alvh")
 '(dired-omit-files "^\\.")
 '(hippie-expand-try-functions-list
   (quote
    (try-expand-list try-expand-dabbrev try-expand-line try-expand-all-abbrevs try-expand-dabbrev-all-buffers try-expand-line-all-buffers try-expand-dabbrev-from-kill try-complete-lisp-symbol-partially try-complete-lisp-symbol)))
 '(ido-enable-regexp t)
 '(ido-everywhere t)
 '(ido-mode (quote both) nil (ido))
 '(imenu-tree-auto-update t)
 '(pde-extra-setting nil)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(kill-whole-line t)
 '(mouse-avoidance-mode (quote animate) nil (avoid))
 '(outline-minor-mode-prefix "^C^O")
 '(read-file-name-completion-ignore-case t)
 '(safe-local-variable-values (quote ((firestarter . ert-run-tests-interactively))))
 '(sentence-end "\\([。！？。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")
 '(scroll-bar-mode nil)
 '(sentence-end-double-space nil)
 '(show-paren-mode t)
 '(tab-width 4)
 '(uniquify-buffer-name-style (quote post-forward-angle-brackets) nil (uniquify))
 '(version-control t)
 '(winner-mode t)
 '(woman-cache-filename "~/.wmncach.el"))
