;; sdcv search
(defvar ywb-dictionary-list '("DrEye4in1词典"))
(defun ywb-dictionary-search-wordap (&optional read)
  "Use perl script dict to look up word under point"
  (interactive "P")
  (let (word)
    (setq word 
          (if read (read-from-minibuffer "Word: ")
            (current-word)))
    (shell-command (format "sdcv -n %s %s"
                           (mapconcat (lambda (dict)
                                        (concat "-u " dict))
                                      ywb-dictionary-list " ")
                           word))))

;;;###autoload
(define-minor-mode ywb-dict-mode
  "add a key bind to `ywb-dictionary-search-append'"
  :lighter " Dict"
  :keymap '(("\M-3" . ywb-dictionary-search-append)))

(defun ywb-dictionary-search-append (arg)
  "append the explaination of the word to current line"
  (interactive "P")
  (or arg (setq arg 15))
  (call-interactively 'ywb-dictionary-search-wordap)
  (let (exp)
    (with-current-buffer"*Shell Command Output*"
      (goto-char (point-min))
      (while (re-search-forward "^[0-9]+ ?\\." nil t)
        (setq exp (cons (buffer-substring
                         (match-end 0)
                         (line-end-position)) exp)))
      (unless exp
        (setq exp (save-excursion
                    (goto-char (point-max))
                    (when
                        (re-search-backward "^\\cc" nil t)
                      (list
                       (buffer-substring
                        (line-beginning-position)
                        (line-end-position))))))))
    (when exp
      (move-to-column arg t)
      (insert (mapconcat 'identity (nreverse exp) "; ") "\n"))))
