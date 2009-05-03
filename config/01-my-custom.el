;; 我的信息
(deh-section "private-info"
  (setq user-full-name "Ye Wenbin")
  (setq user-mail-address "wenbinye@gmail.com"))

;;{{{ customize
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(abbrev-mode t)
 '(appt-display-diary nil)
 '(appt-display-duration 5)
 '(appt-message-warning-time 0)
 '(auto-insert-directory "~/.templates/")
 '(comment-style (quote indent))
 '(compile-dwim-alist (quote ((python (or (name . "\\.py$") (mode . python-mode)) "%i \"%f\"" "%i \"%f%") (php (or (name . "\\.php$") (mode . php-mode)) "php \"%f\"" "php \"%f%") (perl (or (name . "\\.pl$") (mode . cperl-mode)) "%i -wc \"%f\"" "%i \"%f\"") (c (or (name . "\\.c$") (mode . c-mode)) ("gcc -o %n %f" "gcc -g -o %n %f") ("%n" "cint %f") "%n") (c++ (or (name . "\\.cpp$") (mode . c++-mode)) ("g++ -o %n %f" "g++ -g -o %n %f") "%n" "%n") (elisp (or (name . "\\.el$") (mode . emacs-lisp-mode) (mode . lisp-interaction-mode)) (emacs-lisp-byte-compile) (emacs-lisp-byte-compile) "%fc"))))
 '(confirm-kill-emacs (quote y-or-n-p))
 '(cperl-invalid-face nil t)
 '(csv-separators (quote ("," "	")))
 '(delete-selection-mode nil)
 '(delimit-columns-separator "  *")
 '(delimit-columns-str-after " ]")
 '(delimit-columns-str-before "[ ")
 '(delimit-columns-str-separator ", ")
 '(desktop-globals-to-save (quote (sql-mysql-schema bibus-formats desktop-missing-file-warning tags-file-name search-ring regexp-search-ring register-alist windata-named-winconf)))
 '(diff-switches "-ubB")
 '(dired-listing-switches "-alvh")
 '(doxymacs-browse-url-function (quote w3m-goto-url))
 '(doxymacs-use-external-xml-parser t)
 '(ecb-options-version "2.32")
 '(ediff-split-window-function (quote split-window-horizontally))
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(erc-autojoin-channels-alist (quote (("oftc.net" "#emacs-cn") ("llarian.net" "#dbix-class" "#tt" "#catalyst") ("co.uk" "#dbix-class" "#tt" "#catalyst") ("freenode.net" "#perl6" "#perl" "#ubuntu-cn" "#emacs"))))
 '(erc-fill-column 69)
 '(erc-modules (quote (autojoin button completion fill irccontrols list log match menu move-to-prompt netsplit networks noncommands readonly ring stamp spelling track)))
 '(erc-nick "yewenbin")
 '(erc-server-auto-reconnect nil)
 '(erc-track-exclude-types (quote ("JOIN" "KICK" "NICK" "PART" "QUIT" "MODE" "333" "353")))
 '(flymake-gui-warnings-enabled nil)
 '(folding-folding-on-startup nil)
 '(fringe-mode (quote (8 . 0)) nil (fringe))
 '(g-user-email "wenbinye@gmail.com")
 '(gblogger-user-email "wenbinye@gmail.com")
 '(generic-extras-enable-list (quote (alias-generic-mode apache-conf-generic-mode apache-log-generic-mode bat-generic-mode etc-fstab-generic-mode etc-modules-conf-generic-mode etc-passwd-generic-mode etc-services-generic-mode fvwm-generic-mode hosts-generic-mode inetd-conf-generic-mode java-manifest-generic-mode java-properties-generic-mode javascript-generic-mode mailagent-rules-generic-mode mailrc-generic-mode named-boot-generic-mode named-database-generic-mode prototype-generic-mode resolve-conf-generic-mode samba-generic-mode show-tabs-generic-mode vrml-generic-mode x-resource-generic-mode)))
 '(grep-command "grep -nHi \"\"")
 '(help-dwim-active-types (quote (phpdoc elisp-function elisp-variable woman clibpc perldoc)))
 '(hi-lock-file-patterns-policy (lambda (p) t))
 '(htmlize-convert-nonascii-to-entities nil)
 '(ibuffer-formats (quote ((mark modified read-only " " (name 30 30 :left :elide) " " (size 9 -1 :right) " " (mode 16 16 :right :elide) " " filename-and-process) (mark " " (name 16 -1) " " filename))))
 '(ido-enable-regexp t)
 '(ido-everywhere t)
 '(inferior-lisp-program "clisp")
 '(ispell-extra-args (quote ("--lang=en")))
 '(ispell-program-name "aspell")
 '(locate-command "slocate")
 '(max-mini-window-height nil)
 '(message-log-max 250)
 '(muse-colors-autogen-headings (quote outline))
 '(muse-colors-inline-image-method (quote muse-colors-use-publishing-directory))
 '(muse-colors-inline-images nil)
 '(muse-file-extension "muse")
 '(muse-file-regexp "[/?]\\|\\.\\(html?\\|pdf\\|mp3\\|el\\|zip\\|org\\|txt\\|tar\\)\\(\\.\\(gz\\|bz2\\)\\)?\\'")
 '(muse-html-charset-default "utf-8")
 '(muse-html-encoding-default (quote utf-8))
 '(muse-html-footer "~/.emacs.d/footer.html")
 '(muse-html-header "~/.emacs.d/header.html")
 '(muse-html-markup-functions (quote ((anchor . muse-html-markup-anchor) (table . muse-html-markup-table) (footnote . muse-html-markup-footnote))))
 '(muse-html-meta-content-encoding (quote utf-8))
 '(muse-html-style-sheet "<link rel=\"stylesheet\" type=\"text/css\" charset=\"utf-8\" media=\"all\" href=\"../css/core.css\" />")
 '(muse-latex-header "~/.emacs.d/header.tex")
 '(muse-publish-desc-transforms (quote (muse-wiki-publish-pretty-title muse-wiki-publish-pretty-interwiki muse-publish-escape-specials-in-string)))
 '(muse-wiki-publish-small-title-words (quote ("the" "and" "at" "on" "of" "for" "in" "an" "a" "page" "anime")))
 '(muse-xhtml-style-sheet "<style type=\"text/css\">
@import url(\"style.css\");
    </style>")
 '(org-agenda-files (quote ("d:/Docs/My Dropbox/Public/works/gtd.org")))
 '(org-emphasis-alist (quote (("*" bold "<b>" "</b>") ("/" italic "<i>" "</i>") ("_" underline "<u>" "</u>") ("=" shadow "<code>" "</code>"))))
 '(org-export-html-style "<style type=\"text/css\">
  html {
	font-size: 12pt;
  }
  .title { text-align: center; }
  .todo  { color: red; }
  .done { color: green; }
  .timestamp { color: grey }
  .timestamp-kwd { color: CadetBlue }
  .tag { background-color:lightblue; font-weight:normal }
  .target { background-color: lavender; }
  pre {
	border: 1pt solid #AEBDCC;
	background-color: #F3F5F7;
	padding: 5pt;
	font-family: courier, monospace;
  }
  table { border-collapse: collapse; }
  td, th {
	vertical-align: top;
	<!--border: 1pt solid #ADB9CC;-->
  }
</style>")
 '(org-file-apps (quote (("xls" . "gnumeric"))))
 '(outline-minor-mode-prefix "")
 '(outline-regexp "[*]+" t)
 '(perlapi-src-directory "d:/Sources/perl-5.8.8/" t)
 '(perldb-window-configuration (quote ((without-io (horizontal 0.5069 (vertical 0.4782 gud-comint-buffer perldb-stack-buffer) source-buffer) 1) (with-io (vertical 0.25 (horizontal 0.5 gud-comint-buffer perldb-locals-buffer) (vertical 0.5 (horizontal 0.5 source-buffer perldb-inferior-io) (horizontal 0.5 perldb-stack-buffer perldb-breakpoints-buffer))) 0 0))) t)
 '(pgg-default-user-id "Ye Wenbin")
 '(quickurl-format-function (lambda (url) (format "%s" (quickurl-url-url url))))
 '(quickurl-url-file "~/.emacs.d/.quickurls")
 '(safe-local-variable-values (quote ((view-mode . t) (byte-compile-warnings redefine callargs free-vars unresolved obsolete noruntime) (sgml-omittag) (sgml-shorttag . t) (sgml-general-insert-case . lower) (c-font-lock-extra-types "FILE" "bool" "language" "linebuffer" "fdesc" "node" "regexp") (outline-minor-mode . t) (c-indentation-style . bsd) (perl-indent-level . 4) (perl-continued-statement-offset . 4) (perl-continued-brace-offset . 0) (perl-brace-offset . -4) (perl-brace-imaginary-offset . 0) (perl-label-offset . -4) (cperl-continued-statement-offset . 2) (encoding . utf-8) (byte-compile-warnings free-vars unresolved callargs redefine) (add-log-time-zone-rule . t) (adaptive-fill-mode . t) (time-stamp-start . "def\\\\texinfoversion{") (time-stamp-format . "%:y-%02m-%02d.%02H") (time-stamp-end . "}") (outline-minor-mode) (life-universe-everything . 42) (symbolic-formulas ("Eastern area") ("West-district") ("North&South") ("Other")) (code . chinese-gbk-unix) (TeX-master . t) (todo-categories "life" "Todo" "Todo") (allout-layout 1 -1 1 1 1 -1 :) (folded-file . t) (folding-internal-margins) (cperl-indent-level . 4))))
 '(set-mark-command-repeat-pop t)
 '(sql-database "mysql")
 '(sql-indent-offset 4)
 '(sql-mysql-program "mysql")
 '(sql-postgres-program "/usr/bin/psql")
 '(sql-sqlite-program "sqlite3")
 '(sql-user "root")
 '(template-date-format "<%Y-%m-%d %H:%M:%S>")
 '(tooltip-frame-parameters (quote ((font . "-bitstream-bitstream vera sans mono-regular-roman-normal--12-*-*-*-*-*-fontset-startup") (name . "tooltip") (internal-border-width . 2) (border-width . 1))))
 '(tooltip-use-echo-area nil)
 '(view-read-only t)
 '(w3m-content-type-alist (quote (("text/plain" "\\.\\(?:txt\\|tex\\|el\\)\\'" nil nil) ("text/html" "\\.s?html?\\'" browse-url-generic nil) ("text/sgml" "\\.sgml?\\'" nil "text/plain") ("text/xml" "\\.xml\\'" nil "text/plain") ("image/jpeg" "\\.jpe?g\\'" ("/usr/bin/display" file) nil) ("image/png" "\\.png\\'" ("/usr/bin/display" file) nil) ("image/gif" "\\.gif\\'" ("/usr/bin/display" file) nil) ("image/tiff" "\\.tif?f\\'" ("/usr/bin/display" file) nil) ("image/x-xwd" "\\.xwd\\'" ("/usr/bin/display" file) nil) ("image/x-xbm" "\\.xbm\\'" ("/usr/bin/display" file) nil) ("image/x-xpm" "\\.xpm\\'" ("/usr/bin/display" file) nil) ("image/x-bmp" "\\.bmp\\'" ("/usr/bin/display" file) nil) ("video/mpeg" "\\.mpe?g\\'" nil nil) ("video/quicktime" "\\.mov\\'" nil nil) ("application/postscript" "\\.e?ps\\'" ("gv" file) nil) ("application/pdf" "\\.pdf\\'" ("xpdf" file) nil) ("application/xml" "\\.xml\\'" nil "text/plain") ("application/rdf+xml" "\\.rdf\\'" nil "text/plain") ("application/rss+xml" "\\.rss\\'" nil "text/plain") ("application/xhtml+xml" nil nil "text/html"))))
 '(woman-cache-filename "~/.wmncach.el")
 '(woman-manpath (quote ("/usr/man" "/usr/share/man" "/usr/X11R6/man" "/usr/local/man" "/home/ywb/proj/perl/man" "/usr/share/man/zh_TW" "/usr/share/man/zh_CN")))
 '(wtf-custom-alist nil)
 '(yow-file "~/.emacs.d/yow.lines"))

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(blank-line-face ((t (:background "violet"))))
 '(blank-space-face ((t (:background "LightYellow" :foreground "blue"))))
 '(cperl-array-face ((((class color) (background light)) (:foreground "Blue" :weight bold))))
 '(cperl-hash-face ((((class color) (background light)) (:foreground "Red" :slant italic :weight bold))))
 '(font-latex-verbatim-face ((((class color) (background light)) (:foreground "SaddleBrown"))))
 '(mcomplete-substr-method-alternative-part-face ((t (:foreground "BlueViolet"))))
 '(mcomplete-substr-method-fixed-part-face ((t (:foreground "BlueViolet" :weight bold))))
 '(moccur-edit-face ((t (:background "light coral" :weight bold))))
 '(moccur-edit-file-face ((t (:background "green yellow" :weight bold))))
 '(muse-link ((t (:foreground "blue" :underline "blue" :weight bold))))
 '(muse-link-face ((t (:foreground "blue" :underline "blue" :weight bold))))
 '(table-cell ((t (:background "moccasin" :foreground "black" :inverse-video nil))))
 '(variable-pitch ((t nil)))
 '(w3m-tab-unselected-face ((((type x w32 mac) (class color)) (:background "gray70" :foreground "gray20" :box (:line-width -1 :style released-button)))))
 '(widget-button ((t nil))))

(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)

;;}}}
