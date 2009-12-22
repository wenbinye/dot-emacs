(deh-section "org"
  (setq org-CUA-compatible t)
  (add-hook 'org-load-hook
            (lambda ()
              ;; (let (org-CUA-compatible)
              ;;   (define-key org-mode-map (org-key 'S-return)   nil)
              ;;   (define-key org-mode-map (org-key 'S-up)       nil)
              ;;   (define-key org-mode-map (org-key 'S-down)     nil)
              ;;   (define-key org-mode-map (org-key 'S-left)     nil)
              ;;   (define-key org-mode-map (org-key 'S-right)    nil))
              (add-to-list 'org-link-frame-setup '(file . my-find-file-function))
              (define-key org-mode-map (kbd "C-c ^") 'ywb-org-table-sort-lines)
              (define-key org-mode-map (kbd "C-c $") nil)))
  (setq org-export-with-sub-superscripts nil)
  (defun my-find-file-function (file)
    "find file according to the file extension."
    (funcall (or (assoc-default file ywb-dired-guess-command-alist
                                'string-match)
                 'find-file) file))
  (setq org-file-apps-defaults-gnu '((t . emacs)))
  (org-remember-insinuate)
  (setq org-remember-templates
        '(("Todo" ?t "* TODO %?\n  %i\n  %a" "~/org/TODO.org" "Tasks")
          ("Journal" ?j "* %U %?\n\n  %i\n  %a" "~/org/JOURNAL.org")
          ("Idea" ?i "* %^{Title}\n  %i\n  %a" "~/org/JOURNAL.org" "New Ideas")))
  )
