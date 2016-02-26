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

(use-package pde-load
  :config
  (eval-after-load "imenu"
    '(defalias 'imenu--completion-buffer 'pde-ido-imenu-completion))
  (setq compilation-buffer-name-function 'pde-compilation-buffer-name))

(defun ywb-php-mode-hook ()
  (when (fboundp 'php-doc-eldoc-function)
    (set (make-local-variable 'eldoc-documentation-function)
         'php-doc-eldoc-function)
    (eldoc-mode 1))
  (when (boundp 'phpunit-mode)
    (phpunit-mode 1))
  (add-to-list 'php-imenu-generic-expression
               '(nil "^\\s-*\\(?:\\(?:abstract\\|final\\|private\\|protected\\|public\\|static\\)\\s-+\\)*function\\s-+\\(\\(?:\\sw\\|\\s_\\)+\\)\\s-*(" 1)))

(eval-after-load "php-mode"
  '(progn
     (bind-key "C-c C-v" 'ywb-geben-open-current-file php-mode-map)
     (use-package php-doc
       :config
       (bind-key [tab] 'php-doc-complete-function php-mode-map))
     (use-package phpunit
       :config
       (bind-key "C-c t b" 'phpunit-switch phpunit-mode-map)
       (bind-key "C-c t c" 'phpunit-create-test phpunit-mode-map)
       (bind-key "C-c t r" 'phpunit-run-test phpunit-mode-map))))
    
(add-hook 'php-mode-hook 'ywb-php-mode-hook)

(defun run-php ()
  (interactive)
  (let ((buffer (get-buffer-create "*php*"))
        (cmd "psysh"))
    (pop-to-buffer buffer)
    (unless (comint-check-proc buffer)
      (make-comint-in-buffer "psysh" buffer cmd))))

(use-package ggtags
  :bind ("M-." . ggtags-find-tag-dwim)
  :config
  (set-default 'completion-at-point-functions '(ggtags-completion-at-point)))

(use-package zephir-mode
  :commands zephir-mode)

(use-package web-mode
  :mode "\\.\\(volt\\|html\\)\\'"
  :bind ("C-c C-v" . browse-url-of-buffer))

(use-package groovy-mode
  :mode "\\.\\(groovy\\|gradle\\)\\'"
  :config
  (defun ywb-groovy-mode-hook()
    (setq c-basic-offset 4))
  (add-to-list 'groovy-mode-hook 'ywb-groovy-mode-hook))
