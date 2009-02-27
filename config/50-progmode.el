;; -*- mode: Emacs-Lisp -*-
;;{{{ etags, hideshow, tree-imenu, smart-compile
;; etags
(deh-section "etags"
  (setq tags-add-tables nil
        default-tags-table-function
        (lambda nil
          (ywb-find-top-directory "TAGS"))))

(deh-require 'help-dwim
  (help-dwim-register
   '(clibpc . ["a-zA-Z0-9_" clibpc-obarray nil
               (lambda (sym)
                 (clibpc-describe-function (symbol-name sym)))])
   t
   '((require 'clibpc nil t))))

(add-hook 'tree-mode-hook
          (lambda ()
            (toggle-truncate-lines t)))

;; Rebinding keys for hideshow
(deh-require 'hideshow
  (define-key hs-minor-mode-map "\C-c\C-o"
    (let ((map (lookup-key hs-minor-mode-map "\C-c@")))
      ;; C-h is help to remind me key binding
      (define-key map "\C-h" 'describe-prefix-bindings)
      (define-key map "\C-q" 'hs-toggle-hiding)
      ;; compatible with outline
      (define-key map "\C-c" 'hs-hide-block)
      (define-key map "\C-e" 'hs-show-block)
      map)))
;;}}}

(autoload 'gtags-mode "gtags" "" t)
;; Setting for common hook
(defun my-mode-common-hook ()
  (setq tab-width 4)
  (set (make-local-variable 'tab-stop-list) (number-sequence tab-width 80 tab-width))
  (abbrev-mode t)
  (set (make-local-variable 'comment-style) 'indent)
  (local-set-key "\t" 'hippie-expand)
  (setq c-basic-offset tab-width))

;;{{{ elisp
(deh-section "elisp"
  (deh-require 'browse-el
    (define-key lisp-mode-shared-map (kbd "M-.") 'browse-el-find-funtion)
    (define-key lisp-mode-shared-map (kbd "M-*") 'browse-el-go-back)
    (define-key lisp-mode-shared-map (kbd "<f6>") 'find-tag)
    (define-key lisp-mode-shared-map (kbd "<f5>") 'pop-tag-mark))

  (add-to-list 'ffap-alist '(lisp-interaction-mode . ffap-el-mode))
  (defun my-emacs-lisp-mode-hook ()
    (my-mode-common-hook)
    (define-key lisp-mode-shared-map (kbd "C-)") 'ywb-insert-paren)
    (local-set-key "\t" 'PC-lisp-complete-symbol)
    ;; (tempo-install "(?\\([^\\b]+\\)\\=" 'tempo-elisp-tags)
    (tempo-use-tag-list 'tempo-elisp-tags)
    (hs-minor-mode 1)
    (turn-on-eldoc-mode))
  (add-hook 'emacs-lisp-mode-hook 'my-emacs-lisp-mode-hook))
;;}}}

;;{{{ R
(deh-section "ess"
  (setq ess-etc-directory data-directory
        ess-etc-directory-list nil
        inferior-ess-font-lock-keywords  'inferior-ess-R-font-lock-keywords
        ess-directory "~/proj/RWork/")
  (autoload 'R "ess-site" "run R" t)
  (autoload 'inferior-ess-mode "ess-site")
  (autoload 'ess-mode "ess-site" "ess mode" t)
  (add-to-list 'ffap-alist '(inferior-ess-mode . ffap-ess-mode))
  (add-hook 'ess-mode-hook (lambda ()
                             (define-key ess-mode-map "\t" 'ess-complete-object-name)))
  (add-hook 'inferior-ess-mode-hook
            (lambda ()
              (keep-end-watch-this (current-buffer))))
  (defun ffap-ess-mode (name)
    (ffap-locate-file (concat name "/") t ffap-ess-path)))
;;}}}

;;=============================================================
;; Not use offen language
;;=============================================================

;; scheme
;;; (require 'quack)
(deh-section "scheme"
  (setq scheme-program-name "mzscheme")
  (setq scheme-mode-hook
        (lambda ()
          (defun switch-to-scheme (eob-p)
            "Switch to the scheme process buffer.
With argument, position cursor at end of buffer."
            (interactive "P")
            (if (or (and scheme-buffer (get-buffer scheme-buffer))
                    (scheme-interactively-start-process))
                (pop-to-buffer scheme-buffer t)
              (error "No current process buffer.  See variable `scheme-buffer'"))
            (when eob-p
              (push-mark)
              (goto-char (point-max))))
          (my-mode-common-hook))))

;; HTML
(deh-section "html"
  (setq sgml-xml-mode t)
  (add-hook 'sgml-mode-hook 'my-mode-common-hook)
  (set 'html-mode-hook
       (lambda ()
         (define-key html-mode-map (kbd "<C-return>") 'ywb-html-insert-newline)
         (let ((str '(""))
               (align '(("align" ("left") ("center") ("right")))))
           (setq sgml-tag-alist `(("style"
                                   ("href" ,str)
                                   ("type" "text/css"))
                                  ("meta"
                                   t
                                   ("http-equiv" ("Content-Type"))
                                   ("content" ("text/html; charset=utf-8" "text/plain") ("Copyright &#169;"))
                                   ("name" ,str))
                                  ("script"
                                   ("src" ,str)
                                   ("type" "text/javascript"))
                                  ("div" n
                                   ("class" ,str)
                                   ("src", str))
                                  ("object" ("id" ,str))
                                  ("code")
                                  ,@sgml-tag-alist))))))

;; sql
(deh-section "sql"
  (defun ywb-sql-send-line ()
    (interactive)
    (sql-send-region (line-beginning-position) (line-end-position)))
  (defun ywb-sql-completion-or-next-field ()
    (interactive)
    (if  (save-excursion
           (forward-line 0)
           (re-search-forward "mysql> " (line-end-position) t))
        (call-interactively 'comint-dynamic-complete)
      (re-search-forward "[\t|]\\(\n|\\)?[ ]*" nil t)))

  (defun ywb-sql-previous-field ()
    (interactive)
    (skip-chars-backward "\t| ")
    (if (re-search-backward "[\t|]" nil t)
        (goto-char (match-end 0))))

  (add-hook 'sql-mode-hook
            (lambda ()
              (font-lock-add-keywords 'sql-mode
                                      '(("^#.*" 0 font-lock-comment-face)))
              (define-key sql-mode-map (kbd "C-c C-j") 'ywb-sql-send-line)
              (define-key sql-mode-map (kbd "C-c C-z") 'sql-mysql-switch-buffer)))
  (setq sql-input-ring-file-name "~/.emacs.d/.sqlhist")
  (deh-require 'sql-completion 
    (add-to-list 'desktop-globals-to-save 'sql-mysql-schema)
    (add-hook 'sql-interactive-mode-hook
              (lambda ()
                (keep-end-watch-this (current-buffer))
                (toggle-truncate-lines 1)
                (define-key sql-interactive-mode-map "\t" 'ywb-sql-completion-or-next-field)
                (define-key sql-interactive-mode-map (kbd "<backtab>")
                  'ywb-sql-previous-field)
                (define-key sql-interactive-mode-map "\C-c\C-f" 'sql-mysql-current-syntax)
                (sql-mysql-completion-init)))))

;; sh-mode
(deh-section "sh-mode"
  (add-hook 'sh-mode-hook
            (lambda ()
              ;; (when buffer-file-name
              ;;   (executable-set-magic "bash" nil t t))
              (require 'inf-sh-mode)))
  (add-hook 'sh-set-inferior-hook
            (lambda ()
              (keep-end-watch-this
               (buffer-name sh-inferior-buffer)))))

;; gnuplot
(deh-section "gnuplot"
  (autoload 'gnuplot-mode "gnuplot" "gnuplot major mode" t)
  (autoload 'gnuplot-make-buffer "gnuplot" "open a buffer in gnuplot mode" t)
  (add-hook 'gnuplot-after-plot-hook
            (lambda ()
              (select-window (get-buffer-window gnuplot-comint-recent-buffer))))
  (add-hook 'gnuplot-comint-setup-hook
            (lambda ()
              (define-key comint-mode-map "\C-d" 'comint-delchar-or-maybe-eof))))

(deh-section "java"
  (add-hook 'java-mode-hook
            (lambda ()
              (c-set-style "java")
              (if (and (boundp 'msf-abbrev-mode-abbrevs)
                       (null (assoc 'c-mode msf-abbrev-mode-abbrevs)))
                  (msf-abbrev-scan-mode 'c-mode))
              (define-key java-mode-map "\t" 'hippie-expand))))

(deh-section "hla"
  (autoload 'hla-mode "hla-mode" "hla major mode" t)
  (add-hook 'hla-mode-hook
            (lambda ()
              (define-key hla-mode-map "\t" 'self-insert-command))))

(deh-section "po-mode"
  (defun ywb-po-mode-hook ()
    (when (re-search-forward "Content-Type: text/plain; charset=\\(CHARSET\\)" nil t)
      (let (( inhibit-read-only t))
        (replace-match "UTF-8" nil nil nil 1)
        (set-buffer-multibyte t)))
    (let ((dir (expand-file-name default-directory))
          (file ""))
      (while (and (not (string= file "locale"))
                  (not (string= dir "/")))
        (setq dir (directory-file-name dir)
              file (file-name-nondirectory dir)
              dir (file-name-directory dir)))
      (and (string= file "locale")
           (setq po-search-path (cons (list dir) po-search-path)))))
  (add-hook 'po-mode-hook 'ywb-po-mode-hook))

(deh-section "sml-mode"
  (add-hook 'sml-mode-hook
            (lambda ()
              (defun sml-send-line (&optional and-go)
                ""
                (interactive "P")
                (sml-send-region (line-beginning-position)
                                 (line-end-position)
                                 and-go))
              (define-key sml-mode-map "\C-c\C-j" 'sml-send-line)))
  (add-hook 'inferior-sml-mode-hook
            (lambda ()
              (keep-end-watch-this (current-buffer)))))

(deh-section "texinfo"
  (add-hook 'texinfo-mode-hook
            (lambda ()
              (local-set-key "\C-c\C-i" 'texinfo-insert-block))))

(deh-section "emboss"
  (add-to-list 'auto-mode-alist '("\\.acd$" . acd-mode))
  (autoload 'acd-mode "acd" "Major mode for emboss acd files" t)

  (setq emboss-src-diretory "/home/ywb/downloads/emboss/emboss/")
  (add-hook 'c-mode-hook 'emboss-c-mode-hook)
  (setq tempo-interactive t)
  (autoload 'emboss-c-mode-hook "emboss")
  (autoload 'emboss-describe-symbol "emboss"
    "Describe emboss data structure or functions." t)
  (eval-after-load "emboss"
    '(help-dwim-register
      `(emboss . [,emboss-symbol-chars emboss-obarray nil emboss-describe-symbol])
      t)))

(deh-section "vb"
  (setq visual-basic-mode-indent 4)
  )

(deh-section "autoloads"
  (autoload 'asy-mode "asy-mode.el" "Asymptote major mode." t)
  (autoload 'yaml-mode "yaml-mode" "YAML major mode" t)
  (autoload 'lasy-mode "asy-mode.el" "hybrid Asymptote/Latex major mode." t)
  (autoload 'asy-insinuate-latex "asy-mode.el" "Asymptote insinuate LaTeX." t)
  (autoload 'bat-mode "bat-mode" "Bat mode for Windows batch file" t)
  (autoload 'javascript-mode "javascript-mode" "JavaScript mode" t)
  (autoload 'css-mode "css-mode" "Mode for editing CSS files" t)
  (autoload 'python-mode "python" "Python editing mode." t)
  (autoload 'php-mode "php-mode" "php mode" t)
  (autoload 'visual-basic-mode "vb-mode" "Visual Basic Mode" t)
  (autoload 'pir-mode "pir-mode" nil t)
  (autoload 'pod-mode "pod-mode" "A major mode to edit pod" t)
  (autoload 'xs-mode "xs-mode" "Major mode for XS files" t)
  (autoload 'acd-mode "acd" "Major mode to edit acd files" t)
  (autoload 'sourcepair-load "sourcepair" nil t)
(autoload 'js2-mode "js2" nil t)
  (autoload 'oddmuse-mode "oddmuse" nil t))

(deh-section "auto-mode"
  (add-to-list 'auto-mode-alist '("\\.prc$" . sql-mode))
  (add-to-list 'auto-mode-alist '("\\.acd$" . acd-mode))
  (add-to-list 'auto-mode-alist '("\\.po\\'\\|\\.po\\." . po-mode))
  (add-to-list 'auto-mode-alist '("\\.i\\'" . swig-mode))
  (add-to-list 'auto-mode-alist '("\\.asy$" . asy-mode))
  (add-to-list 'auto-mode-alist '("\\.cls$" . LaTeX-mode))
  (add-to-list 'auto-mode-alist '("\\.css$" . css-mode))
  (add-to-list 'auto-mode-alist '("\\.\\(php[345]?\\|module\\|phtml\\|inc\\)$" . php-mode))
  (add-to-list 'auto-mode-alist '("\\.gp$" . gnuplot-mode))
  (add-to-list 'auto-mode-alist '("\\.\\(hla\\|hhf\\)$" . hla-mode))
  (add-to-list 'auto-mode-alist '("\\.\\(frm\\|bas\\)$" . visual-basic-mode))
  (add-to-list 'auto-mode-alist '("\\.\\(imc\\|pir\\)\\'" . pir-mode))
  (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
  (add-to-list 'auto-mode-alist '("apache2?/access" . apache-log-generic-mode))
  (add-to-list 'auto-mode-alist '("\\.fa\\|\\.gb\\|\\.embl$" . dna-mode))
  (add-to-list 'auto-mode-alist '("\.schemas" . xml-mode))
  (add-to-list 'auto-mode-alist '("\\.\\(p6\\|tdy\\|cgi\\|t\\)$" . perl-mode))
  (add-to-list 'auto-mode-alist '("\\.xs$" . c-mode))
  (add-to-list 'auto-mode-alist '("\\.pod$" . pod-mode))
  (add-to-list 'auto-mode-alist '("\\.pir$" . pir-mode))
  (add-to-list 'auto-mode-alist '("\\.xs$" . xs-mode))
  (add-to-list 'auto-mode-alist '("\\.muse$" . muse-mode))
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.twiki$" . oddmuse-mode)))

(deh-section "php"
  (setq php-imenu-generic-expression
        '(
          ("Private Methods"
           "^\\s-*\\(?:\\(?:abstract\\|final\\)\\s-+\\)?private\\s-+\\(?:static\\s-+\\)?function\\s-+\\(\\(?:\\sw\\|\\s_\\)+\\)\\s-*(" 1)
          ("Protected Methods"
           "^\\s-*\\(?:\\(?:abstract\\|final\\)\\s-+\\)?protected\\s-+\\(?:static\\s-+\\)?function\\s-+\\(\\(?:\\sw\\|\\s_\\)+\\)\\s-*(" 1)
          ("Public Methods"
           "^\\s-*\\(?:\\(?:abstract\\|final\\)\\s-+\\)?public\\s-+\\(?:static\\s-+\\)?function\\s-+\\(\\(?:\\sw\\|\\s_\\)+\\)\\s-*(" 1)
          ("Classes"
           "^\\s-*class\\s-+\\(\\(?:\\sw\\|\\s_\\)+\\)\\s-*" 1)
          (nil
           "^\\s-*\\(?:\\(?:abstract\\|final\\|private\\|protected\\|public\\|static\\)\\s-+\\)*function\\s-+\\(\\(?:\\sw\\|\\s_\\)+\\)\\s-*(" 1)
          ))
  (defun my-php-mode-hook ()
    (tempo-use-tag-list 'tempo-php-tags)
    (font-lock-add-keywords nil gtkdoc-font-lock-keywords)
    (when (featurep 'php-doc)
      (local-set-key "\t" 'php-doc-complete-function)
      (local-set-key (kbd "\C-c h") 'php-doc)
      (set (make-local-variable 'eldoc-documentation-function)
           'php-doc-eldoc-function)
      (eldoc-mode 1))
    (local-set-key (kbd "C-M-a") 'beginning-of-defun)
    (local-set-key (kbd "C-M-e") 'end-of-defun)
    (local-set-key (kbd "C-c s") 'compile-dwim-compile)
    )
  (add-hook 'php-mode-hook 'my-php-mode-hook))
