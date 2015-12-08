;;;###autoload
(defun ywb-dired-copy-full-filename ()
  (interactive)
  (dired-copy-filename-as-kill 0))

;;;###autoload
(defun ywb-dired-w3m-visit (file)
  (interactive (list (dired-get-filename nil t)))
  (w3m-goto-url (concat "file://" file)))

(defvar ywb-dired-quickview-buffer nil)
;;;###autoload
(defun ywb-dired-quickview ()
  (interactive)
  (if (buffer-live-p ywb-dired-quickview-buffer)
      (kill-buffer ywb-dired-quickview-buffer))
  (setq ywb-dired-quickview-buffer
        (find-file-noselect (dired-get-file-for-visit)))
  (display-buffer ywb-dired-quickview-buffer))

;;;###autoload
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
                               default-directory))) ".tar.gz"))
            proc)
        (setq cfile
              (read-from-minibuffer "Compress file name: " cfile))
        (setq proc
              (apply 'start-process (append (list "diredz" nil "tar"
                                                  "-hzcvf" cfile) files)))
        (set-process-sentinel proc
                              (lambda (&rest args)
                                (message "Compress finished. Press g to flush directory!")))))))

;;;###autoload
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
         (insert (file-relative-name c) "\n"))
       (when (file-directory-p c)
         (ywb-insert-directory-recursive c reg filter)))
     (directory-files dir t reg))))

;;;###autoload
(defun ywb-dired-list-directory (dir)
  (interactive "DList Directory: ")
  (let ((buffer (get-buffer-create "*files-list*")))
    (with-current-buffer buffer
      (setq default-directory dir)
      (erase-buffer)
      (ywb-insert-directory-recursive dir "^[^.]+" 'file-regular-p)
      (local-set-key (kbd "C-c C-c") 'ywb-list-files)
      (display-buffer (current-buffer))
      (message "Press C-c C-c to convert to dired"))))

;; filter
(defvar my-dired-omit-regexp nil
  "*Default global omit regexp")

;;;###autoload
(defun ywb-dired-filter-regexp (regexp &optional arg)
  (interactive
   (list (dired-read-regexp
          (concat (if current-prefix-arg "Exclude" "Exclude not")
                  " match (regexp): "))
         current-prefix-arg))
  (dired-mark-files-regexp regexp)
  (or arg (dired-toggle-marks))
  (dired-do-kill-lines))

;;;###autoload
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

;;;###autoload
(defun ywb-dired-ediff (file)
  "Compare file at point with file FILE using `diff'.
FILE defaults to the file at the mark.  (That's the mark set by
\\[set-mark-command], not by Dired's \\[dired-mark] command.)
The prompted-for file is the first file given to `diff'.
With prefix arg, prompt for second argument SWITCHES,
which is options for `diff'."
  (interactive
   (let ((current (dired-get-filename t))
         (default (if (mark t)
                      (save-excursion (goto-char (mark t))
                                      (dired-get-filename t t)))))
     (if (or (equal default current)
             (and (not (equal (dired-dwim-target-directory)
                              (dired-current-directory)))
                  (not mark-active)))
         (setq default nil))
     (require 'ediff)
     (list (read-file-name (format "Ediff %s with%s: "
                                   current
                                   (if default
                                       (concat " (default " default ")")
                                     ""))
                           (if default
                               (dired-current-directory)
                             (dired-dwim-target-directory))
                           default t))))
  (ediff-files file (dired-get-filename t)))
