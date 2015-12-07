(defun ywb-emacs-lisp-mode-hook ()
  (eldoc-mode t))

(eval-after-load "lisp-mode"
  '(progn
     (bind-key [?\t] 'completion-at-point lisp-mode-shared-map)
     (use-package browse-el
       :config
       (bind-key "M-." 'browse-el-find-funtion lisp-mode-shared-map)
       (bind-key "M-*" 'browse-el-go-back lisp-mode-shared-map))))

(add-hook 'emacs-lisp-mode-hook 'ywb-emacs-lisp-mode-hook)
(add-hook 'lisp-interaction-mode-hook 'ywb-emacs-lisp-mode-hook)

(use-package projectile
  :config
  (projectile-global-mode))

