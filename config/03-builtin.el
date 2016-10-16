(add-hook 'dired-load-hook
          (lambda ()
            (add-to-list 'dired-font-lock-keywords
               (list dired-re-exe
                     '(".+" (dired-move-to-filename) nil (0 font-lock-type-face))) t)
            (bind-keys :map dired-mode-map
                       ("z" . ywb-dired-compress-dir)
                       ("b" . ywb-dired-list-directory)
                       ("E" . ywb-dired-w3m-visit)
                       ("W" . ywb-dired-copy-full-filename)
                       ("j" . ido-find-file)
                       ("J" . woman-dired-find-file)
                       ("M-o" . dired-omit-mode)
                       ([? ] . ywb-dired-count-dir-size)
                       ("r" . wdired-change-to-wdired-mode)
                       ("C-q" . ywb-dired-quickview)
                       ("/r" . ywb-dired-filter-regexp)
                       ("/." . ywb-dired-filter-extension))
            (load "dired-x")))

(defun ywb-dired-mode-hook ()
  (dired-omit-mode 1))
(add-hook 'dired-mode-hook 'ywb-dired-mode-hook)

(defun ywb-dired-after-readin-hook ()
  (save-excursion
    (let (buffer-read-only)
      (forward-line 2) ;; beyond dir. header
      (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max))))
  (setq truncate-lines t))
(add-hook 'dired-after-readin-hook 'ywb-dired-after-readin-hook)

(defun ywb-shell-mode-hook ()
  (rename-buffer (concat "*shell: " default-directory "*") t)
  (set-process-sentinel (get-buffer-process (current-buffer))
                        (lambda (process event) (kill-buffer))))
(add-hook 'shell-mode-hook 'ywb-shell-mode-hook)
(eval-after-load "shell"
  '(progn
     (defadvice shell-cd (after rename-buffer)
       (rename-buffer (concat "*shell: " default-directory "*") t))
     (ad-activate 'shell-cd)))

(add-hook 'occur-mode-hook
          (lambda ()
            (setq truncate-lines t)))

(require 'uniquify)

(defun ywb-bookmark-bmenu-mode-hook ()
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
     ("^D.*" . font-lock-type-face))))
(add-hook 'bookmark-bmenu-mode-hook 'ywb-bookmark-bmenu-mode-hook)

(require 'windmove)
(windmove-default-keybindings)

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer)
  :config
  (use-package ibuf-ext)
  (define-ibuffer-sorter filename/process
    "Sort buffers by associated file name"
    (:description "file name")
    (apply 'string<
           (mapcar (lambda (buf)
                     (with-current-buffer (car buf)
                       (or buffer-file-name default-directory)))
                   (list a b))))
  (setq ibuffer-saved-filter-groups
        '(("default"
           ("shell" (mode . shell-mode))
           ;;("*buffer*" (name . "\\*.*\\*"))
           ("dired" (mode . dired-mode))
           ("perl" (or (mode . cperl-mode)
                       (mode . sepia-mode)
                       (mode . perl-mode)))
           ("php" (mode . php-mode))
           ("elisp" (or (mode . emacs-lisp-mode)
                        (mode . lisp-interaction-mode)))
           ("prog" (or (mode . c++-mode)
                       (mode . c-mode)
                       (mode . java-mode))))
          ("php"
           ("php" (mode . php-mode))
           ("dired" (mode . dired-mode))
           ("perl" (or (mode . cperl-mode)
                       (mode . sepia-mode)
                       (mode . perl-mode)))
           ("elisp" (or (mode . emacs-lisp-mode)
                        (mode . lisp-interaction-mode)))
           ("prog" (or (mode . c++-mode)
                       (mode . c-mode)
                       (mode . java-mode)))
           ("*buffer*" (name . "\\*.*\\*")))))
  (add-hook 'ibuffer-mode-hook
            (lambda ()
              (ibuffer-switch-to-saved-filter-groups "default"))))

(eval-after-load "image-mode"
  '(define-key image-mode-map "I" 'ywb-image-display-info))

(setenv "PATH" (mapconcat 'identity exec-path ":"))
