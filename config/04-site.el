(use-package expand-region
  :bind ("C-=" . er/expand-region))

(when window-system
  (use-package color-theme
    :config
    (use-package color-theme-solarized
      :config
      (color-theme-solarized))))

(use-package color-moccur
  :commands (moccur moccur-grep)
  :config
  (use-package moccur-edit))

(use-package browse-kill-ring
  :config
  (browse-kill-ring-default-keybindings))

(use-package bash-completion
  :config
  (add-hook 'shell-dynamic-complete-functions 'bash-completion-dynamic-complete))

(use-package chinese-wbim
  :commands chinese-wbim-use-package
  :init
  (setq chinese-wbim-use-tooltip nil)
  (setq default-input-method "chinese-wbim")
  (register-input-method
   "chinese-wbim" "euc-cn" 'chinese-wbim-use-package
   "五笔" "汉字五笔输入法" "wb.txt"))

(defun ywb-install-packages ()
  (interactive)
  (dolist (package '(
ag
auto-complete
bash-completion
browse-kill-ring
chinese-wbim
color-theme
color-theme-solarized
dockerfile-mode
expand-region
gradle-mode
geben
ggtags
helm
helm-projectile
markdown-mode
pde
php-mode
projectile
s
scss-mode
web-mode
yaml-mode
                     ))
    (unless (package-installed-p package)
      (package-install package))))

