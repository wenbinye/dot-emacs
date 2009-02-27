;; get text with property
(defun ywb-get-line ()
  (ywb-get-region (line-beginning-position) (line-end-position)))
(defun ywb-get-region (start end arg)
  (interactive "r\nP")
  (let ((line  (format "%S" (funcall (if arg
                                         'buffer-substring-no-properties
                                       'buffer-substring)
                                     start end))))
    (if (eq last-command 'kill-region)
        (kill-append line nil)
      (kill-new line))
    line))

;; dump hash content
(defun ywb-dump-hash (hash)
  (when (hash-table-p hash)
    (maphash (lambda (key val)
               (insert (format "%S => %S\n" key val)))
             hash)))
(defun ywb-hash-table-keys (hash)
  (let (keys)
    (maphash (lambda (k v) (setq keys (cons k keys))) hash)
    keys))

;; message anything
(defvar ywb-debug t)
(defsubst ywb-message (&rest args)
  "If `ywb-debug' is non-nil, message the ARGS"
  (if ywb-debug (message "%S" args)))

;; complete symbol
;;;_ , completion variable and function
(defun ywb-complete-var (f)
  (dolist (var (all-completions f obarray (lambda (s)
                                  (boundp s))))
    (insert (format "%s\n" var))))
(defun ywb-complete-fun (f)
  (dolist (var (all-completions f obarray (lambda (s)
                                  (fboundp s))))
    (insert (format "(%s)\n" var))))
;; unload
(defun ywb-unload (feature-prefix)
  "Unload a feature according to 'FEATURE-PREFIX'"
  (dolist (sym (all-completions feature-prefix obarray))
    (unintern sym)))

(defun primer-sequence (from to)
  (delq nil (mapcar (lambda (n) (and (primerp n) n))
                      (number-sequence from to))))
(defun primerp (n)
  (cond ((< n 2) nil)
        ((= n 2) t)
        ((= (% n 2) 0) nil)
        (t (let ((max (1+ (ceiling (sqrt n))))
                 (i 3)
                 (flag t))
             (while (and (< i max) flag)
               (if (= (% n i) 0)
                   (setq flag nil))
               (setq i (+ i 2)))
             flag))))
;; toggle-display-point
(defun ywb-toggle-display-point (&optional arg)
  (interactive "P")
  (if (null arg)
      (setq arg 0))
  (let* ((point-format '(:eval (format " %d " (point))))
         (off (null (member point-format global-mode-string))))
    (cond ((> arg 0)
           (when off
             (setq global-mode-string
                   (append global-mode-string (list point-format)))))
          ((< arg 0)
           (when (not off)
             (setq global-mode-string
                   (remove point-format global-mode-string))))
          ;; if arg 0, if off turn on, else turn off
          (off
           (ywb-toggle-display-point 1))
          (t
           (ywb-toggle-display-point -1)))))
(defun ywb-find-not-encodable-char ()
  (interactive)
  (let* ((from (point-min))
         (to (point-max))
         (codings (find-coding-systems-region from to))
         (unsafe (list buffer-file-coding-system))
         (rejected nil))
    (if (member (coding-system-base buffer-file-coding-system)
                codings)
        (message "Current coding system is work!")
      (setq unread-command-events (list ?\^G))
      (select-safe-coding-system-interactively
       from to codings unsafe rejected (car codings)))))
(defun ywb-generate-loaddefs ()
  (interactive)
  (require 'autoload)
  (with-temp-buffer
    (dolist (file
             (append
              (directory-files "~/.emacs.d/config/" t "func-.*.el")
              (directory-files "~/.emacs.d/site-lisp/mycontrib/" t ".*.el")))
      (unless (file-directory-p file)
        (generate-file-autoloads file)))
    (write-region (point-min) (point-max) "~/.emacs.d/config/100-loaddefs.el")))
