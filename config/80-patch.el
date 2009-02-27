;;; * patch for some elisp
;; (setq woman-rebuild-exclude-path
;;       (remove-if-not (lambda (path) (string-match "^/usr" path))
;;                      woman-manpath))
(eval-after-load "woman"
  '(progn
     (defun ywb-directory-files (dir &optional full match nosort)
       (let ((files
              (split-string
               (shell-command-to-string
                (format "perl -e 'opendir(DIR, $ARGV[0]); print join(qq{\n}, grep {/$ARGV[1]/} readdir(DIR))' '%s' '%s'"
                        dir (or match ".")))
               "\n" t)))
         (if full
             (setq files (mapcar (lambda (f) (expand-file-name f dir)) files)))
         (or nosort
             (setq files (sort files 'string<)))
         files))
     (defun woman-file-name-all-completions (topic)
       "Return an alist of the files in all man directories that match TOPIC."
       ;; Support 3 levels of caching: each element of
       ;; woman-topic-all-completions is a list of one of the forms:
       ;;   (topic)
       ;;   (topic (path-index) (path-index) ... )
       ;;   (topic (path-index filename) (path-index filename) ... )
       ;; where the are no duplicates in the value lists.
       ;; Topic must match first `word' of filename, so ...
       (let ((topic-regexp
              (concat "^" (regexp-quote topic)
                      "(\\..+)*\\.([0-9lmnt]\\w*)(\\.(g?z|bz2))?$"))
             (topics woman-topic-all-completions)
             (path woman-expanded-directory-path)
             dir files)
         (if (cdr (car topics))
             ;; Use cached path-info to locate files for each topic:
             (let ((path-info (cdr (assoc topic topics)))
                   filename)
               (while path-info
                 (setq dir (nth (car (car path-info)) path)
                       filename (car (cdr (car path-info)))
                       path-info (cdr path-info)
                       files (nconc files
                                    ;; Find the actual file name:
                                    (if filename
                                        (list (concat dir "/" filename))
                                      (ywb-directory-files dir t topic-regexp)
                                      )))))
           ;; Search path for the files for each topic:
           (while path
             (setq dir (car path)
                   path (cdr path))
             (if (woman-not-member dir path) ; use each directory only once!
                 (setq files (nconc files
                                    (directory-files dir t topic-regexp))))
             ))
         (mapcar 'list files)
         ))))

;; add w3m-contents-url
(eval-after-load "w3m"
  '(progn
     (defun w3m-fontify-anchors ()
       "Fontify anchor tags in the buffer which contains halfdump."
       (let ((help (w3m-make-help-echo w3m-href-anchor))
             (balloon (w3m-make-balloon-help w3m-href-anchor))
             prenames start end)
         (goto-char (point-min))
         (setq w3m-max-anchor-sequence 0) ;; reset max-hseq
         (while (re-search-forward "<_id[ \t\r\f\n]+" nil t)
           (setq start (match-beginning 0))
           (setq prenames (get-text-property start 'w3m-name-anchor))
           (w3m-parse-attributes (id)
             (delete-region start (point))
             (w3m-add-text-properties start (point-max)
                                      (list 'w3m-name-anchor
                                            (cons
                                             (w3m-url-transfer-encode-string
                                              id)
                                             prenames)))))
         (goto-char (point-min))
         (while (re-search-forward "<a[ \t\r\f\n]+" nil t)
           (setq start (match-beginning 0))
           (setq prenames (get-text-property start 'w3m-name-anchor2))
           (w3m-parse-attributes (href name id charset
                                       (rel :case-ignore) (hseq :integer))
             (unless name
               (setq name id))
             (when rel
               (setq rel (split-string rel))
               (cond
                ((member "next" rel) (setq w3m-next-url href))
                ((or (member "prev" rel) (member "previous" rel))
                 (setq w3m-previous-url href))
                ((member "start" rel) (setq w3m-start-url href))
                ((or (member "contents" rel)
                     (member "up" rel))
                 (setq w3m-contents-url href))))
             (delete-region start (point))
             (cond
              (href
               (when (re-search-forward "[ \t\r\f\n]*\\(</a>\\)" nil t)
                 (setq end (match-beginning 0))
                 (delete-region (match-beginning 1) (match-end 1))
                 (setq href (w3m-expand-url (w3m-decode-anchor-string href)))
                 (unless (w3m-url-local-p href)
                   (w3m-string-match-url-components href)
                   (setq href (if (match-beginning 8)
                                  (let ((tmp (match-string 9 href)))
                                    (concat (w3m-url-transfer-encode-string
                                             (substring href 0 (match-beginning 8))
                                             (w3m-charset-to-coding-system charset))
                                            "#" tmp))
                                (w3m-url-transfer-encode-string
                                 href
                                 (w3m-charset-to-coding-system charset)))))
                 (setq hseq (or (and (null hseq) 0) (abs hseq)))
                 (setq w3m-max-anchor-sequence (max hseq w3m-max-anchor-sequence))
                 (w3m-add-face-property start end (if (w3m-arrived-p href)
                                                      'w3m-arrived-anchor-face
                                                    'w3m-anchor-face))
                 (w3m-add-text-properties start end
                                          (list 'w3m-href-anchor href
                                                'mouse-face 'highlight
                                                'w3m-anchor-sequence hseq
                                                'help-echo help
                                                'balloon-help balloon))
                 (when (w3m-imitate-widget-button)
                   (require 'wid-edit)
                   (let ((widget-button-face (if (w3m-arrived-p href)
                                                 'w3m-arrived-anchor-face
                                               'w3m-anchor-face))
                         (widget-mouse-face 'highlight)
                         w)
                     (setq w (widget-convert-button 'default start end
                                                    :button-keymap nil
                                                    :help-echo href))
                     (w3m-static-unless (featurep 'xemacs)
                       (overlay-put (widget-get w :button-overlay) 'evaporate t))))
                 (when name
                   (w3m-add-text-properties start (point-max)
                                            (list 'w3m-name-anchor2
                                                  (cons
                                                   (w3m-url-transfer-encode-string
                                                    name)
                                                   prenames))))))
              (name
               (w3m-add-text-properties start (point-max)
                                        (list 'w3m-name-anchor2
                                              (cons
                                               (w3m-url-transfer-encode-string
                                                name)
                                               prenames)))))))
         (when w3m-icon-data
           (setq w3m-icon-data (cons (w3m-expand-url (car w3m-icon-data))
                                     (or (w3m-image-type (cdr w3m-icon-data))
                                         'ico))))
         (when w3m-next-url
           (setq w3m-next-url (w3m-expand-url w3m-next-url)))
         (when w3m-previous-url
           (setq w3m-previous-url (w3m-expand-url w3m-previous-url)))
         (when w3m-start-url
           (setq w3m-start-url (w3m-expand-url w3m-start-url)))
         (when w3m-contents-url
           (setq w3m-contents-url (w3m-expand-url w3m-contents-url)))))))

(eval-after-load "filesets"
  '(progn
     (defun filesets-add-buffer (&optional name buffer)
       "Add BUFFER (or current-buffer) to the fileset called NAME.
User will be queried, if no fileset name is provided."
       (interactive)
       (let* ((buffer (or buffer
                          (current-buffer)))
              (file (or (buffer-file-name buffer)
                        (if (eq major-mode 'dired-mode)
                            (buffer-local-value 'default-directory buffer)
                          (error "No file name found for the buffer"))))
              (name   (or name
                          (completing-read
                           (format "Add '%s' to fileset: " buffer)
                           filesets-data nil)))
              (entry  (or (assoc name filesets-data)
                          (when (y-or-n-p
                                 (format "Fileset %s does not exist. Create it? "
                                         name))
                            (progn
                              (add-to-list 'filesets-data (list name '(:files)))
                              (message
                               "Fileset %s created.  Call `M-x filesets-save-config' to save."
                               name)
                              (car filesets-data))))))
         (if entry
             (let* ((files  (filesets-entry-get-files entry))
                    (this   (or (buffer-file-name buffer)
                                (substring (buffer-local-value 'default-directory buffer) 0 -1)))
                    (inlist (filesets-member this files
                                             :test 'filesets-files-equalp)))
               (cond
                (inlist
                 (message "Filesets: '%s' is already in '%s'" this name))
                ((and (equal (filesets-entry-mode entry) ':files)
                      this)
                 (filesets-entry-set-files entry (cons this files) t)
                 (filesets-set-config name 'filesets-data filesets-data))
                (t
                 (message "Filesets: Can't add '%s' to fileset '%s'" this name)))))))
     (defun filesets-open-file ()
       (interactive)
       (let (set file)
         (setq set (completing-read "Open file in set: " filesets-data nil t))
         (setq set
               (delq nil
                     (mapcar (lambda (file)
                               (if (and (vectorp file)
                                        (eq (car (aref file 1)) 'filesets-file-open))
                                   (cons (aref file 0) (aref file 1))))
                             (cdr (cadr (member set filesets-submenus))))))
         (find-file (cadr (nth 2 (assoc-default (ido-completing-read "file: " set) set))))))
     ))

(when (= emacs-major-version 23)
  (eval-after-load "descr-text"
    '(progn
       (defun describe-char-display (pos char)
         (if (display-graphic-p (selected-frame))
             (let ((display (internal-char-font pos char)))
               (and display
                    (setcar display (string-as-multibyte (car display)))
                    display))
           (let* ((coding (terminal-coding-system))
                  (encoded (encode-coding-char char coding)))
             (if encoded
                 (encoded-string-description encoded coding))))))))

(defun PC-look-for-include-file ()
  (if (string-match "[\"<]\\([^\"<>]*\\)[\">]?$" (buffer-file-name))
      (let ((name (substring (buffer-file-name)
			     (match-beginning 1) (match-end 1)))
	    (punc (aref (buffer-file-name) (match-beginning 0)))
	    (path nil)
	    new-buf)
	(kill-buffer (current-buffer))
	(if (equal name "")
	    (with-current-buffer (car (buffer-list))
	      (save-excursion
		(beginning-of-line)
		(if (looking-at
		     "[ \t]*#[ \t]*include[ \t]+[<\"]\\(.+\\)[>\"][ \t]*[\n/]")
		    (setq name (buffer-substring (match-beginning 1)
						 (match-end 1))
			  punc (char-after (1- (match-beginning 1))))
		  ;; Suggested by Frank Siebenlist:
		  (if (or (looking-at
			   "[ \t]*([ \t]*load[ \t]+\"\\([^\"]+\\)\"")
			  (looking-at
			   "[ \t]*([ \t]*load-library[ \t]+\"\\([^\"]+\\)\"")
			  (looking-at
			   "[ \t]*([ \t]*require[ \t]+'\\([^\t )]+\\)[\t )]"))
		      (progn
			(setq name (buffer-substring (match-beginning 1)
						     (match-end 1))
			      punc ?\<
			      path load-path)
			(if (string-match "\\.elc$" name)
			    (setq name (substring name 0 -1))
			  (or (string-match "\\.el$" name)
			      (setq name (concat name ".el")))))
		    (error "Not on an #include line"))))))
    ;; remove it because I need typemap
	;; (or (string-match "\\.[[:alnum:]]+$" name)
	;;     (setq name (concat name ".h")))
	(if (eq punc ?\<)
	    (let ((path (or path (PC-include-file-path))))
	      (while (and path
			  (not (file-exists-p
				(concat (file-name-as-directory (car path))
					name))))
		(setq path (cdr path)))
	      (if path
		  (setq name (concat (file-name-as-directory (car path)) name))
		(error "No such include file: <%s>" name)))
	  (let ((dir (with-current-buffer (car (buffer-list))
		       default-directory)))
	    (if (file-exists-p (concat dir name))
		(setq name (concat dir name))
	      (error "No such include file: `%s'" name))))
	(setq new-buf (get-file-buffer name))
	(if new-buf
	    ;; no need to verify last-modified time for this!
	    (set-buffer new-buf)
	  (set-buffer (create-file-buffer name))
	  (erase-buffer)
	  (insert-file-contents name t))
	;; Returning non-nil with the new buffer current
	;; is sufficient to tell find-file to use it.
	t)
    nil))

(eval-after-load "sourcepair"
  '(progn
(defun sourcepair-load ()
  "Load the corresponding C/C++ header or source file for the current buffer.

This function can be invoked by \\[sourcepair-load].  It will load the the
corresponding header or source file for the current buffer.  For example, if
you are looking at the file FooParser.cpp and press \\[sourcepair-load], the
file FooParser.h will be loaded.  It also works the other way as well.

There are five global variables that can be used to adjust how the function
works:

 `sourcepair-source-extensions'
 `sourcepair-header-extensions'
 `sourcepair-source-path'
 `sourcepair-header-path'
 `sourcepair-recurse-ignore'

See the documentation for these variables for more info.
"

  (interactive)
  (catch 'found-matching-file
	(let* ((temp (sourcepair-analyze-filename (file-name-nondirectory (buffer-file-name))))
		   (search-path (car temp))
		   (possible-filenames (cdr temp)))
	  (if (= (length possible-filenames) 0)
		  (message "%s is not a recognized source or header file (consider \
updating sourcepair-source-extensions or sourcepair-header-extensions)"
                   (buffer-name))
		(progn
		  (while search-path
			(let ((path-to-check (car search-path))
				  (matching-filename nil))
			  (if (and (> (length path-to-check) 3)
					   (equal (substring path-to-check -2) "/*"))
				  (setq matching-filename (sourcepair-find-one-of (substring path-to-check 0 -2)
																  possible-filenames
																  t))
				(setq matching-filename 
					  (sourcepair-find-one-of path-to-check possible-filenames nil)))
			  
			  (if (eq matching-filename nil)
				  (setq search-path (cdr search-path))
				(throw 'found-matching-file (find-file matching-filename)))))
          (if (y-or-n-p "No matching file found. Create one? ")
              (find-file (completing-read "file name: " possible-filenames))))))))
     ))

(defadvice occur (around occur-mark-region)
  (save-restriction
    (if (and mark-active transient-mark-mode)
        (narrow-to-region (region-beginning) (region-end)))
    ad-do-it))
(ad-activate 'occur)
(defadvice browse-url-generic (before ywb-browse-url-generic)
  (setq url (replace-regexp-in-string "\\cC" 'url-hexify-string url)))
(ad-activate 'browse-url-generic)
