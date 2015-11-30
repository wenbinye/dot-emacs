(setq custom-file (expand-file-name "02-private.el" ywb--config-directory))

(set-register ?, (cons 'file ywb--config-directory))

(defalias 'yes-or-no-p 'y-or-n-p)

(when window-system
  (tool-bar-mode 0))
