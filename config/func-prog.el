;; close paren
(defun ywb-insert-paren ()
  (interactive)
  (condition-case nil
      (progn
        (scan-sexps (point) -1)
        (insert ")")
        (ywb-insert-paren))
    (error (delete-char -1))))

(defun my-sourcepair-load (arg)
  "Call `sourcepair-load'.
With prefix argument, if not found create the file."
  (interactive "P")
  (let ((res (sourcepair-load)))
    (when (and (not (bufferp res)) arg)
      (let* ((files (sourcepair-analyze-filename (file-name-nondirectory (buffer-file-name))))
             (file (concat (file-name-as-directory (caar files))
                           (cadr files))))
        (find-file file)
        (message "Create %s" file)))))

;;{{{  Auto recompile for emacs-lisp
(defvar auto-recompile nil)
(put 'auto-recompile 'safe-local-variable 'booleanp)
(defvar auto-recompile-query t
  "if non-nil, ask user before byte compile.")
(defun auto-recompile-file-maybe ()
  (interactive)
  (when (and buffer-file-name (string-match "\\.el\\(\\.gz\\)?\\'" buffer-file-name))
    (let ((byte-file (concat buffer-file-name "c")))
      (if (and
           (file-exists-p byte-file)
           (file-newer-than-file-p buffer-file-name byte-file)
           (or auto-recompile
               (null auto-recompile-query)
               (called-interactively-p)
               (y-or-n-p (format "byte-compile %s" buffer-file-name))))
          (byte-compile-file buffer-file-name)))))
(defun auto-recompile-save-hook ()
  (add-hook 'kill-buffer-hook 'auto-recompile-file-maybe nil t))
(add-hook 'emacs-lisp-mode-hook 'auto-recompile-save-hook)
;;}}}

;;{{{  auto-pair
(setq skeleton-pair-default-alist
      '((?( _ ?)) (?\))
        (?[ _ ?]) (?\])
        (?{ _ ?}) (?\})
        (?` _ ?')))
(defun my-turn-on-pair-insert (&optional alist)
  "打开自动插入括号. see also `skeleton-pair-alist'"
  (interactive)
  (set (make-local-variable 'skeleton-pair-alist)
       (append alist
               skeleton-pair-default-alist))
  (setq skeleton-pair t)
  (dolist (pair skeleton-pair-alist)
    (and (and (> (length pair) 1) (integerp (car pair)))
         (local-set-key (string (car pair)) 'skeleton-pair-insert-maybe))))
(defun my-toggle-pair-insert ()
  "切换自动插入括号"
  (interactive)
  (setq skeleton-pair (not skeleton-pair)))
;;}}}

(defun ywb-html-insert-newline ()
  (interactive)
  (insert "<br />"))

(defun bibus-export-bib-1 ()
  (let ((ref (bibus-ref-at-point)))
    (format-spec
     "@ARTICLE{%i,
  AUTHOR = \"%a\",
  TITLE = \"%t\",
  Journal = \"%j\",
  PAGES = \"%p\",
  VOLUME = %v,
  NUMBER = %n,
  YEAR = %y,
}

"
     (format-spec-make
      ?i (bibus-ref-field ref "Identifier")
      ?a (replace-regexp-in-string ";" ". and " (bibus-ref-field ref "Author"))
      ?t (bibus-ref-field ref "Title")
      ?j (bibus-ref-field ref "Journal")
      ?p (let ((pages (bibus-ref-field ref "Pages")))
           (setq pages (split-string pages "-"))
           (mapconcat 'identity (list (car pages)
                                      (concat (substring (car pages) 0
                                                         (- (length (car pages))
                                                            (length (cadr pages))))
                                              (cadr pages))) "--"))
      ?v (bibus-ref-field ref "Volume")
      ?n (bibus-ref-field ref "Number")
      ?y (bibus-ref-field ref "Year")))))
(defun bibus-export-bib ()
  (interactive)
  (let (str)
    (if (bibus-has-marked-refp)
        (bibus-do-with-marked-ref
         (lambda ()
           (setq str (concat str (bibus-export-bib-1)))))
      (setq str (bibus-export-bib-1)))
    (if (eq last-command 'kill-region)
        (kill-append str nil)
      (kill-new str))
    (message "export lib")))
(defun apropos-function (pattern)
  (interactive (list (apropos-read-pattern "function")))
  (apropos-command pattern nil 'fboundp))
(defun ywb-html-preview-region (beg end)
  (interactive "r")
  (let ((file (make-temp-file "region-" nil ".html")))
    (write-region beg end file)
    (browse-url file)))