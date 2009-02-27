(defun my-cperl-eldoc-documentation-function ()
  "Return meaningful doc string for `eldoc-mode'."
  (car
   (let ((cperl-message-on-help-error nil))
     (cperl-get-help))))

(defun my-perl-eletronic-eldoc ()
  (interactive)
  (call-interactively 'self-insert-command)
  (if eldoc-mode
      (message "%s" (funcall eldoc-documentation-function))))

(defun my-perl-append-doc ()
  (let ((myperldoc "~/.emacs.d/myperl-doc.txt"))
    (when (file-exists-p myperldoc)
      (let ((buf (get-buffer-create cperl-doc-buffer)))
        (save-excursion
          (set-buffer buf)
          (when (= (buffer-size) 0)
            (insert (documentation-property 'cperl-short-docs
                                            'variable-documentation))
            (insert-file-contents myperldoc)
            (setq buffer-read-only t)))))))

(defun ywb-gtk-perl-mode-hook ()
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "use Gtk2" 1000 t)
        (gtk-perl-mode 1))))

(deh-section "inf-perl"
  (setq inf-perl-start-file "~/.emacs.d/.psh_rc")
  (add-hook 'inf-perl-mode-hook
            (lambda ()
              (setq comint-input-ring-file-name "~/.emacs.d/.psh_history")
              (gtk-perl-mode 1)
              (keep-end-watch-this (buffer-name)))))

(deh-require 'pde-load)
(deh-section "perl"
  ;; (setq perlapi-file "e:/Programs/Perl/lib/pod/perlapi.pod")
  (defun ywb-cperl-mode-hook ()
    (tempo-use-tag-list 'tempo-perl-tags)
    (set (make-local-variable 'eldoc-documentation-function)
         'my-cperl-eldoc-documentation-function)
    (eldoc-mode 1)
    (local-set-key (kbd "C-c C-p") 'cperl-perldoc)
    (my-perl-append-doc))
  (add-hook 'cperl-mode-hook 'ywb-cperl-mode-hook)
  (add-hook 'cperl-mode-hook 'ywb-gtk-perl-mode-hook))

(setq perlapi-src-directory "/home/ywb/softwares/source/perl-5.8.7")

(add-hook 'xs-mode-hook
             (lambda () (setq c-electric-flag nil)))
