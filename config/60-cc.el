(deh-require 'xcscope
  (setq cscope-database-regexps
        '(("."
           (t)
           ("d:/Sources/postgresql-8.3.0/src" ("-d")))
          ("gedit"
           ( t )
           ("/home/ywb/softwares/source/gnome/gtk+-2.10.11/" ("-d"))
           ("/home/ywb/softwares/source/gnome/glib-2.12.11/" ("-d")))))
  (setq cscope-do-not-update-database t
        cscope-adjust nil))

(deh-section "ebrowse"
  (setq ebrowse-global-prefix-key "\C-z"))

(deh-section "c-mode"
  (defun my-c-mode-common-hook ()
    (my-mode-common-hook)
    (c-set-style "k&r")
    (setq c-basic-offset tab-width)
    (set (make-local-variable 'comment-style) 'indent)
    (local-set-key "*" 'self-insert-command)
    ;; (c-toggle-auto-hungry-state - 1)
    ;; (c-toggle-hungry-state t)
    (c-toggle-auto-newline t)
    (hs-minor-mode 1)
    (define-key c-mode-map "\t" 'clibpc-complete-function)
    (define-key c-mode-map "\C-c\C-v" 'gtk-find-doc)
    (eldoc-mode 1)
    (set (make-local-variable 'eldoc-documentation-function)
         'clibpc-eldoc-function)
    ;; (expand-add-abbrevs c-mode-abbrev-table expand-c-sample-expand-list)
    (tempo-use-tag-list 'tempo-c-tags))
  (add-hook 'c-mode-common-hook 'my-c-mode-common-hook))

(deh-section "c++-mode"
  (defun my-c++-mode-hook ()
    (my-c-mode-common-hook)
    (add-to-list 'c-style-alist
                 '("mine"
                   (c-basic-offset . 4)
                   (c-comment-only-line-offset . 0)
                   (c-hanging-braces-alist
                    (substatement-open after))
                   (c-offsets-alist
                    (topmost-intro . 0)
                    (substatement . +)
                    (substatement-open . 0)
                    (case-label . +)
                    (access-label . -)
                    (inclass . +)
                    (inline-open . 0))))
    (c-set-style "mine"))
  (add-hook 'c++-mode-hook 'my-c++-mode-hook)
  (add-hook 'c++-mode-hook
            (lambda ()
              (setq local-abbrev-table c-mode-abbrev-table))))

(deh-section "gud"
  (add-hook 'gud-mode-hook
            (lambda ()
              (define-key gud-mode-map (kbd "<M-up>") 'comint-previous-prompt)
              (set (make-local-variable 'paragraph-separate) "\\'"))))

(deh-section "gtk"
  (add-to-list 'ffap-c-path "/usr/include/gtk-2.0/")
  (add-to-list 'ffap-c-path "/usr/include/glib-2.0/")
  (require 'gtk-look nil t)
  (add-to-list 'PC-include-file-path "/usr/include/gtk-2.0/")

  (deh-section "glade-mode"
    (add-hook 'glade-mode-hook
              (lambda ()
                (defun glade-mode-eldoc-func ()
                  (let ((but (tree-mode-button-current-line)))
                    (and but
                         (widget-get but :help-echo))))
                (set (make-local-variable 'eldoc-documentation-function)
                     'glade-mode-eldoc-func)
                (eldoc-mode 1)))))

