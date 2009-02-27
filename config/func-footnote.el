(defun ywb-footnote-detect-style (start end)
  "Detect `footnote-style' by counting match regexp of each style in
`footnote-style-alist', and select the style matches most."
  (let ((case-fold-search nil))
    (caar
     (sort
      (mapcar
       (lambda (footnote-style)
         (cons footnote-style
               (count-matches
                (concat "\n"
                        (regexp-quote footnote-start-tag)
                        (Footnote-current-regexp)
                        (regexp-quote footnote-end-tag))
                start end nil)))
       (mapcar 'car footnote-style-alist))
      (lambda (s1 s2) (> (cdr s1) (cdr s2)))))))

;;;###autoload
(defun ywb-footnote-rescan (&optional force)
  "Rescan footnote position in the file."
  (interactive "P")
  (when (or force
            (not (and footnote-text-marker-alist
                      footnote-pointer-marker-alist)))
    (setq footnote-text-marker-alist nil
          footnote-pointer-marker-alist nil)
    (let ((arg 1)                       ; footnote index
          (modified (buffer-modified-p))
          old-point start end match count pointer)
      (save-excursion
        (Footnote-goto-char-point-max)
        (setq end (point))
        (when (re-search-backward (concat "^" footnote-section-tag-regexp) nil t)
          (setq start (point)
                footnote-style (ywb-footnote-detect-style start end))
          (while (and (< (point) end)
                      (re-search-forward (concat "\n\\("
                                                 (regexp-quote footnote-start-tag)
                                                 (Footnote-current-regexp)
                                                 (regexp-quote footnote-end-tag)
                                                 "\\)")
                                         nil t))
            (setq match (match-string 1))
            (forward-line 0)
            (setq old-point (point))
            ;; find footnote position in text, if the index appear
            ;; more than once, select interactively
            (save-excursion
              (goto-char (point-min))
              (setq count 0)
              (while (re-search-forward (regexp-quote match) start t)
                (setq pointer (point)
                      count (1+ count))))
            (cond ((= count 0)
                   (setq footnote-text-marker-alist nil
                         footnote-pointer-marker-alist nil)
                   (error "No footnote found for index %s" match))
                  ((> count 1)
                   (setq pointer (ywb-footnote-select-pointer start match))))
            ;; renumber footnote
            (delete-region (point)
                           (progn (re-search-forward (regexp-quote footnote-end-tag))
                                  (point)))
            (Footnote-insert-numbered-footnote arg nil)
            (Footnote-insert-text-marker arg old-point)
            (goto-char pointer)
            (delete-region (point)
                           (progn (re-search-backward (regexp-quote footnote-start-tag))
                                  (point)))
            (Footnote-insert-pointer-marker arg (point))
            (Footnote-insert-numbered-footnote arg t)
            (setq arg (1+ arg))
            (goto-char old-point)
            (end-of-line))))
      (set-buffer-modified-p modified))))

(defun ywb-footnote-select-pointer (end index)
  "Set message position for INDEX."
  (let ((line (buffer-substring (line-beginning-position)
                                (min (line-end-position)
                                     (+ 30 (line-beginning-position)))))
        (overlay (make-overlay (point-min) (1+ (point-min))))
        point)
    (overlay-put overlay 'face 'match)
    (save-excursion
      (goto-char (point-min))
      (while (and (null point)
                  (re-search-forward (regexp-quote index) end t))
        (move-overlay overlay (match-beginning 0) (match-end 0))
        (if (y-or-n-p (format "Set point of \"%s\" here? " line))
            (setq point (point)))))
    (delete-overlay overlay)
    point))

(add-hook 'footnote-mode-hook 'ywb-footnote-rescan)