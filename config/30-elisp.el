;;;;;;;;;;;;;;;;;;;;;;;;;; standard library ;;;;;;;;;;;;;;;;;;
;;{{{ Dired
(deh-require 'dired-x
  (add-to-list 'dired-font-lock-keywords
               (list dired-re-exe
                     '(".+" (dired-move-to-filename) nil (0 font-lock-type-face))) t)
  (deh-section "dired-omit"
    (add-hook 'dired-mode-hook
              (lambda ()
                (dired-omit-mode t)))
    (setq dired-omit-extensions
          '(".o" "~" ".bak" ".obj" ".ico" ".pif" ".lnk" ".a"
            ".ln" ".blg" ".bbl" ".drv" ".vxd" ".386" ".elc" ".lof"
            ".glo" ".lot" ".fmt" ".tfm" ".class" ".lib" ".mem" ".x86f"
            ".sparcf" ".fasl" ".ufsl" ".fsl" ".dxl" ".pfsl" ".dfsl"
            ".lo" ".la" ".gmo" ".mo" ".toc" ".aux" ".cp" ".fn" ".ky"
            ".tp" ".vr" ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs" ".pyc"
            ".pyo" ".idx" ".lof" ".lot" ".glo" ".blg" ".bbl" ".cps"
            ".fn" ".fns" ".ky" ".kys" ".pgs" ".tp" ".tps" ".vr" ".vrs"
            ".pdb" ".ilk"))
    (setq dired-omit-files
          (concat "^[.#]\\|^" (regexp-opt '(".." "." "CVS" "_darcs") t) "$")))
  ;; Setting for dired
  (setq dired-recursive-copies 'top)
  (setq dired-recursive-deletes 'top)
  (setq dired-dwim-target t)

  (deh-section "dired-assoc"
    (dolist (file '(("acroread" "\\.pdf$")
                    ("xpdf" "\\.pdf$")
                    ("xdvi" "\\.dvi$")
                    ("dvipdf" "\\.dvi$")
                    ("zxpdf" "\\.pdf.gz$")
                    ("ps2pdf" "\\.ps$" "\\.eps$")
                    ("gv" "\\.ps$" "\\.eps$")
                    ("unrar x" "\\.rar$")
                    ("kchmviewer" "\\.chm$")
                    ("mplayer -stop-xscreensaver" "\\.avi$" "\\.mpg" "\\.rmvb" "\\.rm$" "\\.flv$" "\\.wmv")
                    ("mplayer -playlist" "\\.list$")
                    ("gqview" "\\.gif$" "\\.jpe?g$" "\\.tif$" "\\.png$")
                    ("gthumb" "\\.gif$" "\\.jpe?g$" "\\.tif$" "\\.png$")
                    ("docview.pl" "\\.doc$")
                    ("ooffice -writer" "\\.ods$" "\\.doc$")
                    ("ooffice -calc"  "\\.xls$")
                    ("ooffice -impress" "\\.odt$" "\\.ppt$")
                    ("gnumeric" "\\.xls$")
                    ("7z x" "\\.7z$")
                    ("djview" "\\.djvu")
                    ("perl" "\\.pl$")
                    ("opera -newpage" "\\.xml$" "\\.html$" "\\.htm$" "\\.mht$")))
      (dolist (suf (cdr file))
        (let ((prg (assoc suf dired-guess-shell-alist-default)))
          (if prg
              (setcdr prg (delete-dups (cons (car file) (cdr prg))))
            (add-to-list 'dired-guess-shell-alist-default (list suf (car file))))))))
  ;; sort:directories first (emacswiki 上某君之作)
  (defun his-dired-sort ()
    "Dired sort hook to list directories first."
    (save-excursion
      (let (buffer-read-only)
        (forward-line 2) ;; beyond dir. header
        (sort-regexp-fields t "^.*$" "[ ]*." (point)
                            (point-max))))
    (set-buffer-modified-p nil))
  (add-hook 'dired-after-readin-hook 'his-dired-sort)
  (defun my-dired-long-lines ()
    (setq truncate-lines t))
  (add-hook 'dired-after-readin-hook 'my-dired-long-lines)
  ;; Dired Association
  ;;This allows "X" in dired to open the file using the explorer settings.      
  ;;From TBABIN(at)nortelnetworks.com
  ;;ToDo: adapt mswindows-shell-execute() for XEmacs or use tinyurl shell exec
  (if (eq system-type 'windows-nt)
      (progn
        (defun dired-custom-execute-file (&optional arg)
          (interactive "P")
          (mapcar #'(lambda (file)
                      (w32-shell-execute "open" (convert-standard-filename file)))
                  (dired-get-marked-files nil arg)))
        (defun dired-custom-dired-mode-hook ()
          (define-key dired-mode-map "X" 'dired-custom-execute-file))
        (add-hook 'dired-mode-hook 'dired-custom-dired-mode-hook))
    ;; Redefine of this function
    (defun dired-run-shell-command (command)
      (let ((handler
             (find-file-name-handler (directory-file-name default-directory)
                                     'shell-command)))
        (if handler
            (apply handler 'shell-command (list command))
          (start-process-shell-command "dired-run" nil command)))
      ;; Return nil for sake of nconc in dired-bunch-files.
      nil)))
;;}}}

;;{{{ ido
(deh-require 'ido
  (ido-mode 1)
  (setq ido-save-directory-list-file "~/.emacs.d/_ido_last")
  (setq read-file-name-function 'ido-read-file-name)
  ;; visit with dired also push the diretory to `ido-work-directory-list'
  (defadvice ido-file-internal (after ido-dired-add-work-directory)
    (when (eq ido-exit 'dired)
      (ido-record-work-directory (expand-file-name default-directory))))
  (ad-activate 'ido-file-internal)
  ;; push the most used directory to `ido-work-directory-list'
  (mapc (lambda (dir)
          (add-to-list 'ido-work-directory-list
                       (expand-file-name dir)))
        '("~/.emacs.d/"
          "~/.emacs.d/site-lisp/mycontrib/"
          "~/work/"
          "/home/ywb/.emacs.d/site-lisp/"
          "~/proj/lisp/"
          "~/proj/"
          "/usr/share/emacs/22.0.50/lisp/"
          "~/temp/"
          "~/bin/"
          "~/")))
;;}}}

;;{{{ Ibuffer
(deh-require 'ibuffer
  (require 'ibuf-ext nil t)
  (define-ibuffer-sorter file-name
    "Sort buffers by associated file name"
    (:description "file name")
    (apply 'string<
           (mapcar (lambda (buf)
                     (with-current-buffer (car buf)
                       (or buffer-file-name default-directory)))
                   (list a b))))

  (global-set-key (kbd "C-x C-b") 'ibuffer)
  (define-key ibuffer-mode-map "sf" 'ibuffer-do-sort-by-file-name)
  (defun ywb-ibuffer-rename-buffer ()
    (interactive)
    (call-interactively 'ibuffer-update)
    (let* ((buf (ibuffer-current-buffer))
           (name (generate-new-buffer-name
                  (read-from-minibuffer "Rename buffer(to new name): "
                                        (buffer-name buf)))))
      (with-current-buffer buf
        (rename-buffer name)))
    (call-interactively 'ibuffer-update))
  (defun ywb-ibuffer-find-file ()
    (interactive)
    (let ((default-directory (let ((buf (ibuffer-current-buffer)))
                               (if (buffer-live-p buf)
                                   (with-current-buffer buf
                                     default-directory)
                                 default-directory))))
      (call-interactively 'ido-find-file)))
  (define-key ibuffer-mode-map "r" 'ywb-ibuffer-rename-buffer)
  (define-key ibuffer-mode-map (kbd "C-x C-f") 'ywb-ibuffer-find-file)
  (define-key ibuffer-mode-map " " 'scroll-up)
  ;; group buffers
  (setq ibuffer-mode-hook
        (lambda ()
          (setq ibuffer-filter-groups
                '(
                  ("*buffer*" (name . "\\*.*\\*"))
                  ("dired" (mode . dired-mode))
                  ("perl" (or (mode . cperl-mode)
                              (mode . sepia-mode)
                              (mode . perl-mode)))
                  ("elisp" (or (mode . emacs-lisp-mode)
                               (mode . lisp-interaction-mode)))
                  ("prog" (or (mode . c++-mode)
                              (mode . c-mode)
                              (mode . java-mode)))
                  ("tags" (name . "^TAGS"))))))
  (setq ibuffer-saved-filters
        '(("t" ((or (mode . latex-mode)
                    (mode . plain-tex-mode))))
          ("c" ((or (mode . c-mode)
                    (mode . c++-mode))))
          ("p" ((mode . cperl-mode)))
          ("e" ((or (mode . emacs-lisp-mode)
                    (mode . lisp-interaction-mode))))
          ("d" ((mode . dired-mode)))
          ("s" ((mode . shell-mode)))
          ("i" ((mode . image-mode)))
          ("h" ((mode . html-mode)))
          ("gnus" ((or (mode . message-mode)
                       (mode . mail-mode)
                       (mode . gnus-group-mode)
                       (mode . gnus-summary-mode)
                       (mode . gnus-article-mode))))
          ("pr" ((or (mode . emacs-lisp-mode)
                     (mode . cperl-mode)
                     (mode . c-mode)
                     (mode . c++-mode)
                     (mode . php-mode)
                     (mode . java-mode)
                     (mode . idl-mode)
                     (mode . lisp-interaction-mode))))
          ("m" ((mode . muse-mode)))
          ("w" ((or (mode . emacs-wiki-mode)
                    (mode . muse-mode))))
          ("*" ((name . "*"))))))
;;}}}

;;{{{ shell
(deh-section "shell"
  (setenv "HISTFILE" "~/.emacs.d/.history")
  ;; 我做了一些修改，一是可以保留本次的命令到文件，二是可以在 Ibuffer 或
  ;; 者其它 buffer 中退出时不删除当前的 buffer
  (defun wcy-shell-mode-kill-buffer-on-exit (process state)
    (shell-write-history-on-exit process state)
    (kill-buffer (process-buffer process)))
  (defun ywb-shell-mode-hook ()
    (rename-buffer  (concat "*shell: " default-directory "*") t)
    (set-process-sentinel (get-buffer-process (current-buffer))
                          #'wcy-shell-mode-kill-buffer-on-exit)

    (ansi-color-for-comint-mode-on)
    (comint-send-string (get-buffer-process (current-buffer)) "set PERLIO=:unix\n")
    (setq-default
     comint-dynamic-complete-functions
     (let ((list (default-value 'comint-dynamic-complete-functions)))
       (add-to-list 'list 'shell-dynamic-complete-command t)))
    (abbrev-mode t))
  (add-hook 'shell-mode-hook 'ywb-shell-mode-hook))
;;}}}

;;{{{  session, org, ido, ibuffer and so on
;; session
(deh-require 'session
  (setq session-save-file "~/.emacs.d/_session")
  (setq session-save-file-coding-system 'chinese-gbk)
  (add-to-list 'session-globals-exclude 'org-mark-ring)
  (add-hook 'after-init-hook 'session-initialize))

(deh-section "org"
  (setq org-CUA-compatible t)
  (add-hook 'org-load-hook
            (lambda ()
              (dolist (key '(S-right S-left S-up S-down))
                (delq (assq key org-mode-map) org-mode-map))
              (let ((map (assq 3 org-mode-map)))
                (delq (assq ?\$ map) map))
              (define-key org-mode-map (kbd "C-c ^") 'ywb-org-table-sort-lines)))
  (setq org-export-with-sub-superscripts nil)
  (setq org-file-apps-defaults-gnu '((t . emacs))))

(deh-section "std-lib"
  (partial-completion-mode 1)
  (icomplete-mode 1)
  (winner-mode 1)
  ;; (auto-insert-mode 1)
  ;; view
  (setq view-mode-hook
        '((lambda ()
            (define-key view-mode-map "h" 'backward-char)
            (define-key view-mode-map "l" 'forward-char)
            (define-key view-mode-map "j" 'next-line)
            (define-key view-mode-map "k" 'previous-line))))

  ;; time-stamp
  (add-hook 'before-save-hook 'time-stamp)
  (setq time-stamp-active t)
  (setq time-stamp-warn-inactive t)

  (deh-section "formats"
    (setq display-time-format "%m月%d日 星期%a %R")
    (setq frame-title-format
          (list (format "emacs%d@%%b %%f" emacs-major-version)
                " -- "
                'display-time-string))
    (setq time-stamp-format "%U %:y-%02m-%02d %02H:%02M:%02S"))
  
  ;; desktop
  (deh-require 'desktop
    (setq desktop-globals-to-save
          (delq 'tags-table-list desktop-globals-to-save))
    (desktop-save-mode 1))

  (add-hook 'occur-mode-hook (lambda () (setq truncate-lines t)))

  ;; ffap
  ;; for windows path recognize
  (setq ffap-string-at-point-mode-alist
        '((file "--:\\\\$+<>@-Z_a-z~*?" "<@" "@>;.,!:")
          (url "--:=&?$+@-Z_a-z~#,%;*" "^A-Za-z0-9" ":;.,!?")
          (nocolon "--9$+<>@-Z_a-z~" "<@" "@>;.,!?")
          (machine "-a-zA-Z0-9." "" ".")
          (math-mode ",-:$+<>@-Z_a-z~`" "<" "@>;.,!?`:")))

  ;; tetris
  (setq tetris-update-speed-function (lambda (shapes rows) (/ 10.0 (+ 80.0 rows))))

  ;; uniquify
  (deh-require 'uniquify
    (setq uniquify-buffer-name-style 'post-forward-angle-brackets))

  ;; woman
  (setq woman-use-own-frame nil)
  (setq woman-fontify t)                ; dump emacs need this
  (autoload 'woman-mode "woman")
  (add-hook 'woman-mode-hook 'view-mode)
  (autoload 'woman-decode-buffer "woman")

  ;; change-log
  (add-hook 'change-log-mode-hook
            (lambda ()
              (auto-fill-mode t)
              (add-to-list 'change-log-font-lock-keywords
                           '("^[0-9-]+:? +\\|^\\(Sun\\|Mon\\|Tue\\|Wed\\|Thu\\|Fri\\|Sat\\) [A-z][a-z][a-z] [0-9:+ ]+"
                             (0 'change-log-date-face)
                             ("\\([^<(]+?\\)[ 	]*[(<]\\([A-Za-z0-9_.+-]+@[A-Za-z0-9_.-]+\\)[>)]" nil nil
                              (1 'change-log-name)
                              (2 'change-log-email))))))

  ;; generic-x
  (require 'generic-x)

  ;; bookmark
  (add-hook 'bookmark-bmenu-mode-hook
            (lambda ()
              (font-lock-add-keywords
               nil
               '(("^\\s-+\\(.*+\\)[ ]\\{2,\\}"
                  (1 (let ((file (split-string (buffer-substring-no-properties
                                                (line-beginning-position)
                                                (line-end-position)) " \\{2,\\}")))
                       (if (and (not (file-remote-p (nth 2 file)))
                                (file-directory-p (nth 2 file)))
                           font-lock-function-name-face
                         nil))))
                 ("^>.*" . font-lock-warning-face)
                 ("^D.*" . font-lock-type-face)))))

  (add-hook 'Info-mode-hook
            (lambda ()
              (define-key Info-mode-map "j" 'next-line)
              (define-key Info-mode-map "k" 'previous-line)))
  (filesets-init)
  (require 'ffap)
  (deh-section "hippie-expand"
    (setq hippie-expand-try-functions-list
          '(try-expand-line
            try-expand-dabbrev
            try-expand-line-all-buffers
            try-expand-list
            try-expand-list-all-buffers
            try-expand-dabbrev-visible
            try-expand-dabbrev-all-buffers
            try-expand-dabbrev-from-kill
            try-complete-file-name
            try-complete-file-name-partially
            try-complete-lisp-symbol
            try-complete-lisp-symbol-partially
            try-expand-whole-kill)))
  )
;;}}}

;;;;;;;;;;;;;;;;;;;;;;;;;; site-lisp library ;;;;;;;;;;;;;;;;;;

;;; recommend
;;{{{  w3m
(deh-require 'w3m-load
  (setq w3m-verbose t)                  ; log in *Messages*
  (add-hook 'w3m-mode-hook
            (lambda ()
              (defun ywb-w3m-goto-url (url)
                (if (and url (stringp url))
                    (w3m-goto-url url)))
              (local-unset-key "\C-xb")
              (local-unset-key (kbd "S-SPC"))
              (define-key w3m-mode-map "n" (lambda nil (interactive) (ywb-w3m-goto-url w3m-next-url)))
              (define-key w3m-mode-map "p" (lambda nil (interactive) (ywb-w3m-goto-url w3m-previous-url)))
              (define-key w3m-mode-map "t" (lambda nil (interactive) (ywb-w3m-goto-url w3m-contents-url)))
              ))

  (add-hook 'w3m-load-hook
            (lambda ()
              (add-to-list 'w3m-relationship-estimate-rules
                           `(w3m-relationship-simple-estimate
                             ""
                             ,(concat "<a\\s-+href=" w3m-html-string-regexp
                                      "\\s-*>.\\{,25\\}\\(?:next\\|后\\|下\\)")
                             ,(concat "<a\\s-+href=" w3m-html-string-regexp
                                      "\\s-*>.\\{,25\\}\\(?:prev\\|前\\|上\\)")
                             nil
                             ,(concat "<a\\s-+href=" w3m-html-string-regexp
                                      "\\s-*>.\\{,25\\}\\(?:index\\|目录\\)")
                             )))))
;;}}}

;;{{{ template, browse-kill-ring, wb-line, htmlize, moccur, recent-jump,  and so on
(deh-section "non-std-lib"
  ;; template
  ;; (deh-require 'template
  ;;   (template-initialize)
  ;;   (add-to-list 'template-default-expansion-alist
  ;;                '("ENDATE"
  ;;                  (let ((system-time-locale "C"))
  ;;                    (insert (format-time-string "%d %b %Y")))))
  ;;   (dolist (cmd '(ido-select-text ido-magic-forward-char
  ;;                                  ido-exit-minibuffer))
  ;;     (add-to-list 'template-find-file-commands cmd)))
  ;; browse-kill-ring
  (deh-require 'browse-kill-ring
    (browse-kill-ring-default-keybindings))
  ;; wb-line
  (autoload 'wb-line-number-toggle "wb-line-number" nil t)
  ;; htmlize
  (autoload 'htmlize-buffer "htmlize" "htmlize buffer" t)
  ;; moccur
  (autoload 'moccur-grep "moccur-edit" "Glob search file" t)
  (autoload 'moccur "moccur-edit" "moccur" t)
  ;; recent-jump
  (deh-require 'recent-jump
    (global-set-key (kbd "M-o") 'recent-jump-jump-backward)
    (global-set-key (kbd "M-i") 'recent-jump-jump-forward))

  ;; fold
  (setq fold-mode-prefix-key "\C-c\C-o")
  (deh-require 'fold
    (setq fold-autoclose-other-folds nil)
    (add-hook 'find-file-hook 'fold-find-file-hook t))
  ;; blank-mode
  (autoload 'blank-mode-on        "blank-mode"
    "Turn on blank visualization."   t)
  (autoload 'blank-mode-off       "blank-mode"
    "Turn off blank visualization."  t)
  (autoload 'blank-mode           "blank-mode"
    "Toggle blank visualization."    t)
  (autoload 'blank-mode-customize "blank-mode"
    "Customize blank visualization." t)
  ;; sdcv
  (deh-section "sdcv"
    (setq sdcv-dictionary-list '("DrEye4in1词典"))
    (autoload 'sdcv-search "sdcv-mode" "Search dictionary using sdcv" t))

  (deh-section "linum"
    (setq linum-format (concat (propertize "%6d " 'face 'default)
                               (propertize " " 'face 'fringe)))
    (autoload 'linum-mode "linum" "Display line number" t))
  ;; visible-line
  (require 'visible-lines nil t)
  ;; anything
  (autoload 'anything "anything" "" t)
  (eval-after-load "anything"
    '(progn
       (define-key anything-map "\C-n" 'anything-next-line)
       (define-key anything-map "\C-p" 'anything-previous-line)
       (define-key anything-map "\M-n" 'anything-next-source)
       (define-key anything-map "\M-p" 'anything-previous-source)))
  ;; ansit
  (autoload 'ansit-ansify-this "ansit"  "Ansi the region." t)
  ;; dna-mode
  (setq dna-cruft-regexp "[*-. 0-9\t\n]")
  (autoload 'dna-mode "dna-mode" "Major mode for dna sequence file")
  (add-hook 'dna-mode-hook
            (lambda ()
              (define-key dna-mode-map "\C-ch" 'hide-sublevels)
              (outline-minor-mode)))
  ;; rst-mode
  (autoload 'rst-mode "rst" "" t)
  ;; minibuf-isearch
  (autoload 'minibuf-isearch-next "minibuf-isearch" "" t)
  (autoload 'minibuf-isearch-prev "minibuf-isearch" "" t)
  (mapcar (lambda (keymap)
            (define-key keymap "\C-r" 'minibuf-isearch-prev)
            (define-key keymap "\C-s" 'minibuf-isearch-next))
          (delq nil (list (and (boundp 'minibuffer-local-map)
                               minibuffer-local-map)
                          (and (boundp 'minibuffer-local-ns-map)
                               minibuffer-local-ns-map)
                          (and (boundp 'minibuffer-local-completion-map)
                               minibuffer-local-completion-map)
                          (and (boundp 'minibuffer-local-must-match-map)
                               minibuffer-local-must-match-map))))

  ;; Add "CHARSET" for .po default charset
  (setq po-content-type-charset-alist
        '(("ASCII" . undecided)
          ("ANSI_X3.4-1968" . undecided)
          ("US-ASCII" . undecided)
          ("CHARSET" . undecided)))
  (autoload 'po-mode "po-mode"
    "Major mode for translators to edit PO files" t)

  (autoload 'muse-insert-list-item "muse-mode" t)
  (deh-section "oddmuse"
    (autoload 'oddmuse-mode "oddmuse" "" t)
    (add-hook 'oddmuse-mode-hook
              (lambda ()
                (set (make-local-variable 'outline-regexp) "---[+]+")
                (outline-minor-mode 1))))
  )
;;}}}

;;;;;;;;;;;;;;;;;;;;;;;;;; my contribute library ;;;;;;;;;;;;;;;;;;

;;{{{  My contribute library
(deh-section "mycontrib"
  ;; eight-puzzle
  (setq eight-puzzle-pics-path "~/.emacs.d/.8puzzle/")
  (setq eight-puzzle-use-image t)

  (deh-require 'template-simple)

  ;; shell-completion
  (deh-require 'shell-completion
    (setq shell-completion-sudo-cmd "\\(?:sudo\\|which\\)")
    (defvar my-lftp-sites (if (file-exists-p "~/.lftp/bookmarks")
                              (shell-completion-get-file-column "~/.lftp/bookmarks" 0 "[ \t]+")))
    (add-to-list 'shell-completion-options-alist
                 '("lftp" my-lftp-sites))
    (add-to-list 'shell-completion-prog-cmdopt-alist
                 '("lftp" ("help" "open" "get" "mirror" "bookmark")
                   ("open" my-lftp-sites)
                   ("bookmark" "add"))))

  ;; bibus
  (deh-section "bibus"
    (add-to-list 'desktop-globals-to-save 'bibus-formats)
    (setq bibus-file-app
          '(("pdf" . "acroread \"%f\"")))
    (add-hook 'bibus-ref-mode-hook
              (lambda ()
                (deh-define-key bibus-ref-mode-map
                  ("E" . 'bibus-export-bib))))
    ;; (setq bibus-default-file-directory "~/work/mypaper/")
    )

  ;; tsv-mode
  (setq tsv-elide-string ""
        tsv-elide-face 'italic)
  (add-hook 'tsv-mode-hook (lambda () (hl-line-mode 1)))

  ;; incr
  (setq incr-enable-feature
        '(number rotate roman date han-number))

  (deh-section "eim"
    ;; eim
    (autoload 'eim-use-package "eim" "Another emacs input method")
    (register-input-method
     "eim-wb" "euc-cn" 'eim-use-package
     "五笔" "汉字五笔输入法" "wb.txt")
    (setq default-input-method "eim-wb")
    (register-input-method
     "eim-py" "euc-cn" 'eim-use-package
     "拼音" "汉字拼音输入法" "py.txt")
    (setq eim-use-tooltip nil)
    (require 'eim-extra nil t)
    (when (featurep 'eim-extra)
      (global-set-key ";" 'eim-insert-ascii)))

  (autoload 'hexl-mode "hexl+" "Edit a file in a hex dump format" t)
  ;; (deh-section "mule-menu"
  ;;   (require 'english-menu nil t)
  ;;   (require 'chinese-menu nil t)
  ;;   (require 'mule-menu nil t))
  (deh-section "dot-emacs-helper"
    (setq deh-custom-file "~/.emacs.d/config/110-deh.el"))
  )
;;}}}

;;;;;;;;;;;;;;;;;;;;;;;;;; Extra library ;;;;;;;;;;;;;;;;;;
;; Tricks to load feature when needed
(defun muse-mode ()
  (require 'muse-init)
  (muse-mode-choose-mode))

(defun emms ()
  (interactive)
  (require 'emms-init))

(setq
 deh-enable-list
 '(("flymake"
    (require 'flymake)
    (defun my-flymake-find-file-hook ()
      (condition-case nil
          (flymake-find-file-hook)
        (error nil)))
    (add-hook 'find-file-hooks 'my-flymake-find-file-hook t))
   ("bbdb"
    (deh-require 'bbdb
      (bbdb-initialize 'gnus 'message)
      (setq bbdb-north-american-phone-numbers-p nil)
      (setq bbdb-user-mail-names
            (regexp-opt '("wenbinye@163.com"
                          "wenbinye@gmail.com")))
      (setq bbdb-complete-name-allow-cycling t)
      (setq bbdb-use-pop-up nil)))
   ("latex"
    (load "preview-latex.el" t t t)
    (load "auctex.el" t t t)
    (autoload 'CJK-insert-space "cjkspace"
      "Insert tildes appropriately in CJK document." t)
    (defun cjk-toggle-space-tilde (arg)
      (interactive "P")
      (setq CJK-space-after-space
            (if (null arg)
                (not CJK-space-after-space)
              (> (prefix-numeric-value arg) 0)))
      (message "Now SPC will insert %s" (if CJK-space-after-space "SPC" "~")))
    (setq TeX-electric-escape t)
    (defun my-tex-mode-hook ()
      (auto-fill-mode 1)
      (defun TeX-arg-input-file (optionel &optional prompt local)
        "Prompt for a tex or sty file.

First optional argument is the prompt, the second is a flag.
If the flag is set, only complete with local files."
        (unless (or TeX-global-input-files local)
          (message "Searching for files...")
          (setq TeX-global-input-files
                (mapcar 'list (TeX-search-files (append TeX-macro-private
                                                        TeX-macro-global)
                                                TeX-file-extensions t t))))
        (let ((file (if TeX-check-path
                        (completing-read
                         (TeX-argument-prompt optionel prompt "File")
                         (unless local
                           TeX-global-input-files))
                      (read-file-name
                       (TeX-argument-prompt optionel prompt "File")))))
          (if (null file)
              (setq file ""))
          (if (not (string-equal "" file))
              (TeX-run-style-hooks file))
          (TeX-argument-insert file optionel)))
      (my-turn-on-pair-insert '((?$ _ ?$)))
      (define-key LaTeX-mode-map " " 'CJK-insert-space)
      (define-key LaTeX-mode-map "\C-c\C-a" 'cjk-toggle-space-tilde))
    (add-hook 'TeX-mode-hook 'my-tex-mode-hook)
    )
   ("emboss"
    (add-to-list 'PC-include-file-path "/home/ywb/local/share/EMBOSS/acd")
    (autoload 'acd-mode "acd" "Major mode for acd files" t)
    (add-to-list 'auto-mode-alist '("\\.acd$" . acd-mode))

    (defvar emboss-wossname-obarray
      (let ((db (make-vector 199 0)))
        (with-temp-buffer
          (call-process "wossname"
                        nil t nil "wossname" "-search" "")
          (goto-char (point-min))
          (forward-line 1)
          (while (not (eobp))
            (forward-line 1)
            (while (looking-at "\\sw+")
              (intern (current-word) db)
              (forward-line 1))
            (forward-line 2)))
        db))

    (defun emboss-wossname (cmd)
      (interactive
       (list (intern (completing-read "Show manual: " emboss-wossname-obarray nil t))))
      (let ((inhibit-read-only t))
        (with-current-buffer (get-buffer-create "*wossname*")
          (erase-buffer)
          (call-process "tfm" nil t nil "tfm" "-program" (symbol-name cmd))
          (view-mode 1)
          (with-selected-window (display-buffer (current-buffer))
            (goto-char (point-min))))))
    (help-dwim-register
     `(wossname . [ "a-z" emboss-wossname-obarray nil emboss-wossname ])
     t)
    (setq emboss-src-diretory "/home/ywb/downloads/emboss/emboss")
    (require 'emboss nil t)
    (defalias 'eds 'emboss-describe-symbol)
    (help-dwim-register
     `(emboss . [ ,emboss-symbol-chars emboss-obarray nil emboss-describe-symbol ]) nil))
   ("cedet"
    ;; (setq semantic-load-turn-useful-things-on t)
    (load "/home/ywb/softwares/emacs/sources/cedet-1.0pre4/common/cedet.el")
    (semantic-load-enable-minimum-features)
    (setq semanticdb-project-roots 
          (list
           (expand-file-name "/")))
    (autoload 'senator-try-expand-semantic "senator")
    (add-to-list 'hippie-expand-try-functions-list 'senator-try-expand-semantic)
    (defvar ywb-semantic-imenu-function (symbol-function 'semantic-create-imenu-index))
    (defun ywb-toggle-semantic-imenu ()
      (interactive)
      (if (eq (symbol-function 'semantic-create-imenu-index)
              (symbol-function 'imenu-default-create-index-function))
          (progn
            (fset 'semantic-create-imenu-index ywb-semantic-imenu-function)
            (message "Using semantic-create-imenu-index"))
        (fset 'semantic-create-imenu-index
              (symbol-function 'imenu-default-create-index-function))
        (message "Using imemu default"))))
   ("sepia"
    (autoload 'sepia-repl "sepia" "Sepia" t)
    (autoload 'sepia-mode "sepia" "Sepia" t)
    (defalias 'perl-mode 'sepia-mode)
    (add-hook 'sepia-repl-mode-hook
              (lambda ()
                (sepia-rebuild)))
    (add-hook 'sepia-mode-hook
              (lambda ()
                (setq beginning-of-defun-function nil
                      end-of-defun-function nil)
                (add-to-list 'ffap-alist  '(sepia-mode . my-cperl-ffap-locate))
                (setq add-log-current-defun-function
                      (lambda ()
                        (if (re-search-backward "^sub[ \t]+\\([^({ \t\n]+\\)" nil t)
                            (match-string-no-properties 1)))))))
   ))
