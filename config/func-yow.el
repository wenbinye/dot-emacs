;;{{{ yow
(defun ywb-yow ()
  (interactive)
  (unless (minibuffer-window-active-p (selected-window))
    (yow nil nil)))
;;;###autoload 
(defun ywb-toggle-yow (arg)
  (interactive "P")
  (let ((timer (remove-if-not (lambda (timer)
                                (eq (aref timer 5) 'ywb-yow))
                              timer-idle-list)))
    (mapc 'cancel-timer timer)
    (if (or (and (null arg) (null timer))
            (and arg (> (prefix-numeric-value arg) 0)))
        (progn
          (message "Turn on yow!")
          (run-with-idle-timer 5 t 'ywb-yow))
      (message "Turn off yow!"))))
;;}}}
