;; w3m-visit
(defun ywb-dired-w3m-visit ()
  (interactive)
  (w3m-goto-url (concat "file://" (dired-get-filename nil t))))

;; kill-fullname
(if (eq system-type 'windows-nt)
    (defun ywb-dired-copy-fullname-as-kill (&optional arg)
      "In dired mode, use key W to get the full name of the file"
      (interactive "P")
      (let (file)
        (setq file (dired-get-filename nil t))
        (or (not arg)
            (cond ((= arg 1)
                   (setq file (convert-standard-filename file)))
                  ((= arg 2)
                   (setq file (ywb-convert-to-cygwin-path file)))
                  ((= arg 3)
                   (setq file (convert-standard-filename (file-name-directory file))))))
        (if (eq last-command 'kill-region)
            (kill-append file nil)
          (kill-new file))
        (message "%s" file)))
  (defun ywb-dired-copy-fullname-as-kill ()
    (interactive)
    (dired-copy-filename-as-kill 0)))

;; quickview
(defvar ywb-dired-quickview-buffer nil)
(defun ywb-dired-quickview ()
  (interactive)
  (if (buffer-live-p ywb-dired-quickview-buffer)
      (kill-buffer ywb-dired-quickview-buffer))
  (setq ywb-dired-quickview-buffer
        (find-file-noselect (dired-get-file-for-visit)))
  (display-buffer ywb-dired-quickview-buffer))

;; compress
(defun ywb-dired-compress-dir ()
  (interactive)
  (let ((files (dired-get-marked-files t)))
    (if (and (null (cdr files))
             (string-match "\\.\\(tgz\\|tar\\.gz\\)" (car files)))
        (shell-command (concat "tar -xvf " (car files)))
      (let ((cfile (concat (file-name-nondirectory
                            (if (null (cdr files))
                                (car files)
                              (directory-file-name
                               default-directory))) ".tgz"))
            proc)
        (setq cfile
              (read-from-minibuffer "Compress file name: " cfile))
        (setq proc
              (apply 'start-process (append (list "diredz" nil "tar"
                                                  "-hzcvf" cfile) files)))
        (set-process-sentinel proc
                              (lambda (&rest args)
                                (message "Compress finished. Press g to flush directory!")))))))
;; jump
(defun ywb-dired-jump-to-file ()
  "Quick jump to the file in dired-mode"
  (interactive)
  (goto-char (point-min))
  (dired-next-line 1)
  (let ((name "")
        input)
    (while (progn
             (setq input (read-char (format "Jump to: %s" name)))
             (if (and (< input ? ) (not (member input (list ?\d ?\^n ?\^P))))
                 (progn (setq unread-command-events (list input)) nil)
               (cond ((= input ?\d)
                      (progn (setq name (substring name 0 -1))
                             (goto-char (point-min))
                             (dired-next-line 1)))
                     ((= input ?\^N)
                      (dired-next-line 1))
                     ((= input ?\^P)
                      (while (progn
                               (dired-previous-line 1)
                               (not (or (looking-at name) (bobp))))))
                     (t
                      (setq name (concat name (char-to-string input)))))
               (while (not (or (looking-at name) (eobp)))
                 (dired-next-line 1))
               t)))))
;; count size
(defun ywb-dired-count-dir-size ( arg )
  (interactive "P")
  (let ((dir (dired-get-filename nil t))
        proc)
    (when (file-directory-p dir)
      (with-current-buffer (get-buffer-create "*Shell Command Output*")
        (setq default-directory "~/")
        (erase-buffer)
        (setq proc        
              (start-process-shell-command "dirsize" (current-buffer)
                                           ;; "/home/ywb/bin/dirsize"
                                           "du" "-h"
                                           (if arg "-s" "")
                                           (format "\"%s\"" dir)))
        (set-process-sentinel proc
                              (lambda (proc event)
                                (let ((buf (process-buffer proc)))
                                  (with-selected-window
                                      (get-buffer-window buf)
                                    (goto-char (point-min))))))
        (display-buffer (current-buffer))))))

;; list file
(defun ywb-list-files ()
  (interactive)
  (let ((files '("files-list"))
        buffer)
    (goto-char (point-min))
    (while (not (eobp))
      (setq files (append files (list
                                 (buffer-substring-no-properties
                                  (point) (line-end-position)))))
      (forward-line 1))
    (setq buffer (get-buffer (car files)))
    (if (and (buffer-live-p buffer)
             (eq (buffer-local-value 'major-mode buffer) 'dired-mode))
        (kill-buffer buffer))
    (display-buffer (dired-noselect files))))

(defun ywb-insert-directory-recursive (dir reg filter)
  (let ((max-lisp-eval-depth 10000)
        (max-specpdl-size 20000))
    (mapc
     (lambda (c)
       (when (funcall filter c)
         (insert c "\n"))
       (when (file-directory-p c)
         (ywb-insert-directory-recursive c reg filter)))
     (directory-files dir t reg))))

(defun ywb-list-directory-recursive (dir)
  (interactive "DList Directory: ")
  (let ((buffer (get-buffer-create "*files-list*")))
    (with-current-buffer buffer
      (erase-buffer)
      (ywb-insert-directory-recursive dir "^[^.]+" 'file-regular-p)
      (local-set-key (kbd "C-c C-c") 'ywb-list-files)
      (display-buffer (current-buffer))
      (message "Press C-c C-c to convert to dired"))))
;; filter
(defvar my-dired-omit-regexp nil
  "*Default global omit regexp")

(defun my-dired-omit-expunge (&optional regexp)
  "Omit file not match the regexp."
  (interactive "sOmit file not match(regexp): ")
  (if (and dired-omit-mode
           (or (interactive-p)
               (not dired-omit-size-limit)
               (< (buffer-size) dired-omit-size-limit)
               (progn
                 (message "Not omitting: directory larger than %d characters."
                          dired-omit-size-limit)
                 (setq dired-omit-mode nil)
                 nil)))
      (if (and regexp (string= regexp ""))
          ;; if regexp is empty, remove the omit regexp
          (progn
            (setq my-dired-omit-regexp (default-value 'my-dired-omit-regexp))
            (revert-buffer))
        (let ((old-modified-p (buffer-modified-p))
              (omit-re (if regexp
                           (set (make-local-variable 'my-dired-omit-regexp) regexp)
                         my-dired-omit-regexp))
              count)
          (or (null omit-re)
              (string= omit-re "")
              (let ((dired-marker-char dired-omit-marker-char))
                (message "Omitting...")
                (if (dired-mark-if
                     (and
                      ;; not already marked
                      (looking-at " ")
                      ;; uninteresting
                      (let ((fn (dired-get-filename dired-omit-localp t)))
                        (and fn (not (string-match omit-re fn)))))
                     nil)
                    (progn
                      (setq count (dired-do-kill-lines nil "Omitted %d line%s."))
                      (force-mode-line-update))
                  (message "(Nothing to omit)"))))
          ;; Try to preserve modified state of buffer.  So `%*' doesn't appear
          ;; in mode-line of omitted buffers.
          (set-buffer-modified-p (and old-modified-p
                                      (save-excursion
                                        (goto-char (point-min))
                                        (re-search-forward dired-re-mark nil t))))
          count))))
(add-hook 'dired-after-readin-hook 'my-dired-omit-expunge)
(defun ywb-dired-filter-regexp (regexp &optional arg)
  (interactive
   (list (dired-read-regexp
          (concat (if current-prefix-arg "Exclude" "Exclude not")
                  " match (regexp): "))
         current-prefix-arg))
  (dired-mark-files-regexp regexp)
  (or arg (dired-toggle-marks))
  (dired-do-kill-lines))

(defun ywb-dired-filter-extension (extension &optional arg)
  "Filter by file extension. Multiple extension seperated by space, such as \"c cpp h\"."
  (interactive
   (list (read-from-minibuffer
          (concat "Exclude extension is "
                  (if current-prefix-arg "" "not") ": "))
         current-prefix-arg))
  (ywb-dired-filter-regexp
   (concat "\\.\\(" (regexp-opt (split-string extension)) "\\)\\'")
   arg))
;; add description
(defvar ywb-description-file "desc.html")
(defun ywb-add-description (desc)
  (interactive (list
                (let ((fn (dired-get-filename 'no-dir)))
                  (if (file-exists-p ywb-description-file)
                      (with-temp-buffer
                        (insert-file-contents ywb-description-file)
                        ;; if the file has description, edit it and save
                        (if (re-search-forward "<ul>" nil t)
                            (read-from-minibuffer
                             (format "Description for %s: " fn)
                             (if (re-search-forward (concat "href=\"" (regexp-quote fn)
                                                            "\">\\(.*\\)</a>") nil t)
                                 (match-string 1) ""))
                          (error "The %s is not a description file! Please rename it."
                                 ywb-description-file)))
                    ;; if file not exits create it 
                    (with-current-buffer (find-file-noselect ywb-description-file)
                      (insert
                       "<html>\n\t<body>\n\t\t<ul>\n\t\t</ul>\n\t</body>\n</html>")
                      (save-buffer)
                      (kill-buffer (current-buffer)))
                    (read-from-minibuffer
                     (format "Description for %s: " fn))))))
  (let ((fn (dired-get-filename 'no-dir))
        (buflist (buffer-list)))
    (setq desc (replace-regexp-in-string "\n ?" " " desc))
    (with-current-buffer (find-file-noselect ywb-description-file)
      (save-excursion
        ;; if already has description, replace it
        (goto-char (point-min))
        (re-search-forward "<ul>")
        (if (re-search-forward (concat "href=\"" (regexp-quote fn)
                                       "\">\\(.*\\)</a>") nil t)
            (replace-match desc nil nil nil 1)
          (insert (format "\n<li><a href=\"%s\">%s</a></li>" fn desc)))
        (save-buffer)
        (unless (member (current-buffer) buflist)
          (kill-buffer (current-buffer)))))))
;; mark-bad-link
(defun ywb-dired-mark-bad-link ()
  (interactive)
  (dired-mark-if
   (let* ((fn (dired-get-filename nil t))
          (link (and fn (file-symlink-p fn))))
     (and link (not (file-exists-p link))))
   nil))
;; convmv
(defun ywb-dired-convmv (from to &optional arg)
  "Convert file name coding system on all marked (or next arg) files. 

For example, a file with name encoded by gbk, use
M-x ywb-dired-convmv RET gbk RET RET, then the file will display with
right coding system. Also you can convert a file to gbk coding
system when locale is utf-8 by M-x ywb-dired-convmv RET RET gbk RET."
  (interactive "zFrom coding: \nzTo coding: \nP")
  (or from (setq from file-name-coding-system))
  (or to (setq to file-name-coding-system))
  (dolist (file (dired-map-over-marks (cons (dired-get-filename)
                                            (point)) arg))
    (setq file (car file))
    (rename-file file (concat (string-as-unibyte (file-name-directory file))
                              (encode-coding-string
                               (decode-coding-string (encode-coding-string (file-name-nondirectory file) file-name-coding-system)
                                                     from)
                               to))))
  (revert-buffer))
