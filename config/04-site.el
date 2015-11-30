(use-package expand-region
  :bind ("C-=" . er/expand-region))

(when window-system
  (use-package color-theme
    :config
    (defun color-theme-gnome ()
      "Wheat on darkslategrey scheme.
From one version of Emacs in RH6 and Gnome, modified by Jonadab."
      (interactive)
      (color-theme-install
       '(color-theme-gnome
         ((foreground-color . "wheat")
          (background-color . "darkslategrey")
          (background-mode . dark))
         (default ((t (nil))))
         (region ((t (:foreground "cyan" :background "dark cyan"))))
         (underline ((t (:foreground "yellow" :underline t))))
         (modeline ((t (:foreground "dark cyan" :background "wheat"))))
         (modeline-buffer-id ((t (:foreground "dark cyan" :background "wheat"))))
         (modeline-mousable ((t (:foreground "dark cyan" :background "wheat"))))
         (modeline-mousable-minor-mode ((t (:foreground "dark cyan" :background "wheat"))))
         (italic ((t (:foreground "dark red" :italic t))))
         (bold-italic ((t (:foreground "dark red" :bold t :italic t))))
         (font-lock-comment-face ((t (:foreground "orange red"))))
         (bold ((t (:bold)))))))
    (color-theme-gnome)))

(use-package color-moccur
  :commands (moccur moccur-grep)
  :config
  (use-package moccur-edit))

(use-package browse-kill-ring
  :config
  (browse-kill-ring-default-keybindings))

