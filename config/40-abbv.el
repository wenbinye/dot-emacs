(require 'cc-mode)
(require 'cl)
(require 'tempo)

;;{{{ tempo
(deh-require 'tempo-x
  (setq tempo-interactive t)
  (global-set-key " " 'tempo-x-space)
  )
;;}}}

;;;;;;;;;;;;;;;;;;;; define mode abbrev ;;;;;;;;;;;;;;;;;;;;;;;;;
;;{{{ perl
(defvar tempo-perl-tags nil)
(deh-section "abbv-perl"
  (tempo-define-template
   "perl-dbi"
    '("use DBIx::Simple;"
      n> "my @cp = ('dbi:mysql:" (p "Database: ") "', 'ywb', 'pe');"
      n> "my ($db, $sql, $result);"
      n> "$db = DBIx::Simple->connect(@cp)"
      n "|| die DBIx::Simple->error;" >)
    "usedbi"
    "Use DBIx::Simple"
    'tempo-perl-tags)

  (tempo-define-template
   "perl-funcdesc"
   '("#==========================================================" n
     "# Input  : " p n
     "# Output : " p n
     "# Desc   :" p n
     "#==========================================================")
   "funcdesc"
   "Insert perl function description"
   'tempo-perl-tags)

  (tempo-define-template
   "perl-from2"
   '("from_to(" (m "OCT") ", " (m "FROM") ", "  (m "TO") ")")
   "from2"
   "Insert encode function from Encode module"
   'tempo-perl-tags))
;;}}}
(deh-section "php-abbv"
  (defvar tempo-php-tags nil)
  (tempo-define-template "php-docb"
                         '("/**" n
                           "* " > p n
                           > "*/")
                         "docb"
                         "Insert DocBlock"
                         'tempo-php-tags)
  (tempo-define-template "php-funb"
                         '("/**" n
                            "* " > p n
                           > "* " n
                           > "* @param " n
                           > "* @return " n
                           > "*/")
                         "funb"
                         "Insert Function DocBlock"
                         'tempo-php-tags)
  (tempo-define-template "php-fileb"
                         '("/**" n
                           "* " > p n
                           > "* " n
                           > "* @copyright Copyright (c) 2009, Taobao. inc" n
                           > "* @package " n
                           > "* @author Ye Wenbin<buzhi@taobao.com>" n
                           > "*/")
                         "fileb"
                         "Insert File DocBlock"
                         'tempo-php-tags)
  (tempo-define-template "php-classb"
                         '("/**" n
                           "* " > p n
                           > "* " n
                           > "* @package " n
                           > "*/")
                         "classb"
                         "Insert Class DocBlock"
                         'tempo-php-tags)
  )

;;{{{  c-mode
(deh-section "abbv-c-mode"
  (defvar tempo-c-tags nil
    "Tempo tags using in c-mode.")
  (tempo-define-template
   "c-if"
   '("if ( " p " ) {"
     n> p
     n "}" >)
   "ifx"
   "Expand if"
   'tempo-c-tags)
  (tempo-define-template
   "c-for"
   '((snippet
      "for ( " (S i) "=0; " (S i) "<" (S max) "; " (S i) "++ ) {"
      n> p
      n "}" >))
   "forx"
   "Expand for"
   'tempo-c-tags)
  (tempo-define-template
   "c-main"
   '("int main (int argc, char *argv[])"
     n "{" >
     n> p
     n "return 0;" >
     n "}" >)
   "mainx"
   "expand main"
   'tempo-c-tags))
;;}}}

;;{{{ emacs lisp
(deh-section "abbv-elisp"
  (defvar tempo-elisp-tags nil)
  (tempo-define-template "defun"
                         '("defun " p " (" p ")"
                           n> "\"" p "\""
                           n> ")")
                         "defun"
                         "Insert a defun expression"
                         'tempo-elisp-tags)

  (tempo-define-template "defvar"
                         '("defvar " p
                           n> "\"" p "\")")
                         "defvar"
                         "Insert a defvar expression"
                         'tempo-elisp-tags))
;;}}}

;;{{{ metapost
(deh-section "abbv-metapost"
  (tempo-define-template
   "meta-begfig"
   '("beginfig(" (read-from-minibuffer "fig id: "
                                       (if (save-excursion
                                             (re-search-backward "beginfig(\\([0-9]+\\))" nil t))
                                           (number-to-string
                                            (1+ (string-to-number (match-string 1))))
                                         "1")) ")"
                                         n> p
                                         n> "endfig;"
                                         n>)))
;;}}}

;; ;;{{{ scheme
;; (deh-section "abbv-scheme"
;;   (define-skeleton skeleton-scheme-mode-define
;;     "" nil
;;     "(define " _ ")")

;;   (define-skeleton skeleton-scheme-mode-lambda
;;     "" nil
;;     "(lambda (" (skeleton-read "Param: ") ") " _ ")"))
;; ;;}}}

;;{{{ msf-abbrev
(deh-require 'msf-abbrev
  (setq msf-abbrev-indent-after-expansion t)
  (setq msf-abbrev-root "~/.emacs.d/mode-abbrevs")
  (setq msf-abbrev-mode-alias
        '((c++-mode . c-mode)
          (cperl-mode . perl-mode)))
  (dolist (hook '(c++-mode-hook c-mode-hook
                                java-mode-hook
                                cperl-mode-hook))
    (add-hook hook 'msf-abbrev-mode)))
;;}}}
