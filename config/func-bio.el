(defun ywb-tr (string table)
  "translate STRING according to TABLE"
  (let (newstr tr)
    (dolist (char (append string nil))
      (setq newstr (append newstr (list
                                   (if (setq tr (assoc char table))
                                       (cdr tr) char)))))
    (concat newstr)))

(defun ywb-build-tr-table (from to)
  (let (table)
    (dotimes (i (length from))
      (add-to-list 'table (cons (aref from i) (aref to i))))
    table))

(defvar ywb-dna-tr-table (ywb-build-tr-table "atcgATCG" "tagcTAGC"))
(defvar ywb-rna-tr-table (ywb-build-tr-table "aucgAUCG" "uagcUAGC"))

(defun ywb-rna-seqp (seq)
  (string-match "[uU]" seq))

(defun ywb-complement-string (seq)
  (if (> (length seq) 20)
      (with-temp-buffer
        (insert seq)
        (apply 'call-process-region (point-min) (point-max)
               "tr" t t t
               (if (ywb-rna-seqp seq)
                   (list "aucgAUCG" "uagcUAGC")
                 (list "atcgATCG" "tagcTAGC")))
        (buffer-string))
    (if (ywb-rna-seqp seq)
        (ywb-tr seq ywb-rna-tr-table)
      (ywb-tr seq ywb-dna-tr-table))))

(defun ywb-reverse-string (str)
  (concat (reverse (append str nil))))

;;;###autoload 
(defun ywb-reverse-complement-region (beg end)
  (interactive "r")
  (let ((seq (delete-and-extract-region beg end)))
    (insert (ywb-reverse-string (ywb-complement-string seq)))))

;; complement-seq
(defun ywb-complement-seq (beg end)
  (interactive "r")
  (insert (ywb-complement-string (delete-and-extract-region beg end))))
;; reverse-seq
(defun ywb-reverse-seq (beg end)
  (interactive "r")
  (insert (ywb-reverse-string (delete-and-extract-region beg end))))

;; fill-seq
;;;###autoload 
(defun ywb-fill-seq (beg end)
  (interactive "r")
  (let ((len (or fill-column 75))
        (seq (replace-regexp-in-string "[-\\ \n\t0-9]+" "" (delete-and-extract-region beg end)))
        (pos 0))
    (if (< (length seq) len)
        (insert seq)
      (while (progn
               (insert (substring seq pos (+ pos len)) "\n")
               (setq pos (+ pos len))
               (< (+ pos len) (length seq))))
      (insert (substring seq pos))
      t)))

;; sort mirna
(defun ywb-sort-mirna (m1 m2)
  (if (and (string-match "-" m1)
           (string-match "-" m2))
      (let ((p1 (split-string m1 "-"))
            (p2 (split-string m2 "-")))
        (if (string= (nth 1 p1) (nth 1 p2))
            (let ((n1 (string-to-number (nth 2 p1)))
                  (n2 (string-to-number (nth 2 p2))))
              (if (= n1 n2)
                  (string< (nth 2 p1) (nth 2 p2))
                (< n1 n2)))
          (string< (nth 1 p1) (nth 1 p2))))
    (string< m1 m2)))
  
;;;###autoload 
(defun ywb-sort-mirna-lines (reverse beg end)
  (interactive "P\nr")
  (ywb-sort-lines-1 reverse beg end
                    (lambda (pos1 pos2)
                      (ywb-sort-mirna
                       (buffer-substring-no-properties (car pos1) (cdr pos1))
                       (buffer-substring-no-properties (car pos2) (cdr
                                                                   pos2))))))

;; clustalw seq
(defvar ywb-clustalw-cmd "clustalw")
;;;###autoload 
(defun ywb-clustalw-region (beg end)
  "Clustalw fasta sequences in region"
  (interactive "r")
  (let ((file (make-temp-file "clustalw-"))
        proc)
    (write-region beg end file)
    (setq proc
          (start-process-shell-command "clustalw" nil ywb-clustalw-cmd
                                       file))
    (process-put proc 'result (concat file ".aln"))
    (set-process-sentinel proc (lambda (proc &rest ignore)
                                 (display-buffer (find-file-noselect
                                                  (process-get proc 'result)))))))
