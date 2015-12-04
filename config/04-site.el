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

