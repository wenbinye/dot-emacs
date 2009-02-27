;;; muse-init.el ---
;;{{{  基本配置
(require 'muse-colors)
(require 'muse-mode)                    ; load authoring mode
(require 'muse-html)                    ; load publishing styles I use
(require 'muse-latex)
;; (require 'muse-texinfo)
(require 'muse-journal)
;; (require 'muse-docbook)
(require 'muse-wiki)
(require 'muse-project)                 ; publish files in projects
(require 'muse-tree nil t)
(require 'tempo)

(add-to-list 'auto-mode-alist '("\\.muse\\'" . muse-mode-choose-mode))
(add-to-list 'magic-mode-alist '("#title " . muse-mode))

(setq muse-completing-read-function 'ido-completing-read
      muse-latexcjk-encoding-default "{UTF8}{song}"
      muse-publish-date-format "%Y 年 %m 月 %d 日"
      muse-journal-date-format "%Y 年 %m 月 %d 日，%A"
      muse-colors-evaluate-lisp-tags nil
      muse-wiki-ignore-bare-project-names t
      muse-wiki-ignore-implicit-links-to-current-page t
      ywb-muse-recentchanges-page "RecentChanges")

(defvar ywb-muse-publish-root-path "~/public_html/")
(defun ywb-muse-relative-path (file)
  (concat
   (file-relative-name
    ywb-muse-publish-root-path
    (file-name-directory muse-publishing-current-output-path))
   file))
(defun ywb-define-muse-project (dir)
  `(,dir (,(concat "~/Muse/" dir) :default "index"
          :force-publish (,ywb-muse-recentchanges-page "WikiIndex"))
         (:base "html" :path ,(concat "~/public_html/" (downcase dir)))))
(setq muse-project-alist
      `(,@(mapcar 'ywb-define-muse-project
                  '("Emacs" "StdLib" "Perl" "GtkPerl" "Programming" "Other" "Catalyst" "Work"))
        (,@(ywb-define-muse-project "ElispIntro")
         (:base "latexbook" :path "~/Muse/latex/elispintro"
                :exclude ,(regexp-opt '("index" "RecentChanges" "WikiIndex"))))
        ("Journal" ("~/Muse/Journal/" :default ,(format-time-string "%Y%m"))
         (:base "journal-html" :path "~/public_html/journal")
         (:base "journal-rss" :path "~/public_html/journal"))
        ("Website" ("~/Muse/" :default "index"
                    :force-publish (,ywb-muse-recentchanges-page "WikiIndex"))
         (:base "html" :path "~/public_html"))
        ;; 使用特殊模板的项目
        ("PDE"
         ("~/proj/darcs/pde/misc/")
         (:base "myhtml" :path "~/proj/darcs/pde/lisp/doc/"))))

(autoload 'sgml-tag "sgml-mode" "" t)
(autoload 'sgml-close-tag "sgml-mode" "" t)
(defvar muse-tag-alist
  '(("example")
    ("literal")
    ("lisp" n)
    ("src" ("lang" ("emacs-lisp") ("perl") ("sql") ("c++") ("sh")) n))
  "Tag list for `sgml-tag'.")

(defun my-muse-mode-hook ()
  ;; 与高亮源代码有关
  (add-hook 'font-lock-mode-hook
            (lambda ()
              (when (and (boundp 'muse-colors-overlays)
                         muse-colors-overlays
                         (null font-lock-mode))
                (mapcar 'delete-overlay muse-colors-overlays)))
            nil t)
  (make-local-variable 'muse-colors-overlays)
  ;; 新建文件时自动插入，特别是 WikiIndex 和 RecentChanges
  (when (= (buffer-size) 0)
    (let ((page (muse-page-name)))
      (cond ((string= page "WikiIndex")
             (insert "#title 目录\n"
                     "<lisp>(ywb-muse-index-as-string t t t)</lisp>\n"))
            ((string= page "RecentChanges")
             (insert "#title 最近更新\n"
                     "<lisp>(ywb-muse-generate-recentchanges)</lisp>\n"))
            (t (insert "#title ")))))
  (outline-minor-mode 1)
  (auto-fill-mode 1)
  (tempo-use-tag-list 'tempo-muse-tags)
  ;; 实用的按键
  (define-key muse-mode-map (kbd "C-c C-c") 'ywb-muse-preview-source)
  (define-key muse-mode-map (kbd "C-c C-m") 'ywb-muse-preview-with-w3m)
  (set (make-local-variable 'sgml-tag-alist) muse-tag-alist)
  (modify-syntax-entry ?> ")" muse-mode-syntax-table)
  (modify-syntax-entry ?< "(" muse-mode-syntax-table)
  (define-key muse-mode-map (kbd "C-c /") 'sgml-close-tag)
  (define-key muse-mode-map (kbd "C-c t") 'sgml-tag))
(add-hook 'muse-mode-hook 'my-muse-mode-hook)
;;}}}

;;{{{  使用 css 来着色
(require 'htmlize)
(defun htmlize-region-for-paste (beg end)
  "Htmlize the region and return just the HTML as a string.
This forces the `inline-css' style and only returns the HTML body,
but without the BODY tag.  This should make it useful for inserting
the text to another HTML buffer."
  (let ((htmlbuf (htmlize-region beg end)))
    (unwind-protect
        (with-current-buffer htmlbuf
          (buffer-substring (plist-get htmlize-buffer-places 'content-start)
                            (plist-get htmlize-buffer-places 'content-end)))
      (kill-buffer htmlbuf))))
;;}}}

;;{{{  产生最近更新页面
(require 'file-stat nil t)
(when (not (fboundp 'file-stat-mtime))
  (defsubst file-stat-mtime (file &optional id-format)
    (nth 5 (file-attributes file id-format))))

(defvar ywb-muse-recentchanges-format "%Y年%m月%d日")

(defun ywb-muse-generate-recentchanges (&optional show-proj &rest project-list)
  "Generate recent changes of project files.

If SHOW-PROJ is non-nil, the index will add project name.
If PROJECT-LIST is given, all changes in these projects will display"
  (or project-list (setq project-list (list (car (muse-project)))))
  (let ((curr-file (muse-current-file))
        (curr-buf (current-buffer))
        (content "")
        last files header current changed beg pos)
    ;; if this file is not save yet, just return an empty string
    (if (not (file-exists-p curr-file))
        ""
      (with-temp-buffer
        (insert-file-contents curr-file)
        ;; search for last change time stamp.
        ;; if there is one, get it and update the time stamp
        ;; if didn't have one, insert current time stamp after directives
        (goto-char (point-min))
        (if (re-search-forward "^; last time stamp: \\([0-9]+\\(\\.[0-9]+\\)?\\)"
                               nil t)
            (progn
              (setq last (seconds-to-time (string-to-number (match-string 1))))
              ;; (re-search-forward "^; last time stamp: \\([0-9.]+\\)" nil t)
              (replace-match (number-to-string (float-time)) nil nil nil 1))
          (re-search-forward "^[^#]" nil t)
          (backward-char 1)
          (insert (format "; last time stamp: %d\n" (float-time))))
        ;; get all file in the project-list newer than last
        (setq files (ywb-muse-get-rc-page project-list (or last '(0 0))))
        (when files
          (re-search-forward "</lisp>")
          (forward-line 1)
          (setq beg (point))
          ;; insert href for the pages. Pages are collected under the
          ;; same header generated by `ywb-muse-recentchanges-format'
          (setq header (format-time-string
                        ywb-muse-recentchanges-format
                        (nth 2 (car files))))
          (insert "* " header "\n")
          (dolist (file files)
            (setq current (format-time-string
                           ywb-muse-recentchanges-format
                           (nth 2 file)))
            (unless (string= current header)
              (insert "\n* " current "\n")
              (setq header current))
            (insert " - [[" (car file) "#" (cadr file) "]["
                    (if show-proj (concat (car file) "-") "")
                    (cadr file) "]]"
                    ;; if the page is not register in the this
                    ;; recentchange, a new tag will add
                    (save-excursion
                      (if (re-search-forward
                           (regexp-quote (concat "[" (car file) "#" (cadr file) "]"))
                           nil t) "" " *(new)*"))
                    "\n"))
          (setq pos (point))
          (if (re-search-forward header nil t)
              ;; if we update this file in the same peroid, the duplicate
              ;; line should removed. 
              (progn
                (re-search-forward "^[*]" nil t)
                (setq content (mapconcat 'identity 
                                         (delete-dups (split-string (delete-and-extract-region beg (point)) "\n")) "\n"))
                (insert content)
                ;; make change in publishing buffer
                (save-excursion
                  (set-buffer curr-buf)
                  (save-restriction
                    (widen)
                    (goto-char (point-min))
                    (re-search-forward "^[*]" nil t)
                    (delete-region (point)
                                   (progn
                                     (re-search-forward "^[*]" nil t)
                                     (point))))))
            (setq content (buffer-substring beg (point))))
          (write-region (point-min) (point-max) curr-file)
          (message "Use M-x revert-buffer to update current buffer")))
      content)))

(defun ywb-muse-get-rc-page (project-list newer)
  (let (files mtime)
    (dolist (proj project-list)
      (dolist (file (muse-project-file-alist (muse-project proj)))
        (when (file-exists-p (cdr file))
          (setq mtime (file-stat-mtime (cdr file)))
          (if (and (time-less-p newer mtime)
                   ;; autosave file
                   (not (string-match "^\.#" (car file)))
                   ;; the page itself
                   (not (string= ywb-muse-recentchanges-page (car file))))
              (setq files (cons (list proj (car file) mtime) files))))))
    (sort files (lambda (f1 f2) (time-less-p (nth 2 f2) (nth 2 f1))))))
;;}}}

;;{{{  使用标题产生索引页面
(defun ywb-muse-page-title (file)
  (when (and file (file-exists-p file))
    (let ((max-size 200))           ; the max size to search for title
      (with-temp-buffer
        (insert-file-contents file nil 0 max-size)
        (goto-char (point-min))
        (if (re-search-forward "^#title\\s-*" nil t)
            (buffer-substring (point) (line-end-position)))))))

(defvar ywb-muse-index-exclude-pages
  '("index" "WikiIndex" "RecentChanges")
  "Exclude pages when publish index")

(defun ywb-muse-index-as-string (&optional as-list exclude-private exclude-current &rest project-list)
  "Generate the index of all wiki file using title.
See also `muse-index-as-string'.
PROJECT-LIST is the index of projects to insert.
"
  (unless project-list
    (setq project-list (list (car (muse-project)))))
  (let ((current (muse-page-name))
        files title)
    (with-temp-buffer
      (dolist (project project-list)
        (setq files
              (sort (copy-alist (muse-project-file-alist project))
                    (function
                     (lambda (l r)
                       (string-lessp (car l) (car r))))))
        (when (and exclude-current current)
          (setq files (delete (assoc current files) files)))
        (unless (= (length project-list) 1)
          (insert "* " project "\n"))
        (dolist (file files)
          (when (and (file-exists-p (cdr file))
                     (not (member (car file) ywb-muse-index-exclude-pages))
                     (not (and exclude-private
                               (muse-project-private-p (cdr file)))))
            (insert " - [[" project "#" (car file) "]["
                    (or (ywb-muse-page-title (cdr file)) (car file))
                    "]]\n"))))
      (insert "\n")
      (buffer-string))))
;;}}}

;;{{{  打开 html 文件和使用 w3m 打开的命令
(defun ywb-muse-output-file ()
  (let ((style (muse-style
                (muse-project-get-applicable-style buffer-file-name
                                                   (cddr muse-current-project)))))
    (muse-publish-output-file buffer-file-name
                              (muse-style-element :path style) style)))
(defun ywb-muse-preview-with-w3m ()
  "Preview the html file"
  (interactive)
  (muse-project-publish-this-file)
  (let ((file (ywb-muse-output-file)))
    (w3m-goto-url (if (string-match "^[a-zA-Z]:" file)
                      (ywb-convert-to-cygwin-path file)
                    (concat "file://" file)))))
(defun ywb-muse-preview-source ()
  "Find the html file"
  (interactive)
  (muse-project-publish-this-file)
  (find-file (ywb-muse-output-file)))
;;}}}

;;{{{  高亮文件中的源代码及新增代码行号功能
(defvar muse-colors-overlays nil)
(defun muse-colors-src-tag (beg end)
  "Strip properties and mark as literal."
  (let (face)
    (muse-unhighlight-region beg end)
    (save-excursion
      (goto-char beg)
      (let ((fs 1) content face-list fe mode attrs number ov ovs
            (font-lock-verbose nil))
        (when (re-search-forward "<src\\(.*\\)>" nil t)
          (setq beg (match-end 0)
                attrs (mapcar
                       (lambda (pair)
                         (setq pair (split-string pair "="))
                         (setcdr pair (substring (cadr pair) 1 -1))
                         pair)
                       (split-string (match-string 1)))
                mode (and (assoc "lang" attrs)
                          (intern-soft (concat (cdr (assoc "lang" attrs))
                                               "-mode"))))
          (when (and mode (fboundp mode))
            (goto-char end)
            (setq end
                  (if (re-search-backward "</src>" nil t)
                      (match-beginning 0)
                    (point-max))
                  content (buffer-substring-no-properties beg end))
            (with-current-buffer (get-buffer-create "*muse-temp*")
              (funcall mode)
              (insert content)
              (font-lock-fontify-buffer)
              (htmlize-ensure-fontified)
              (or (get-text-property fs 'face)
                  (setq fs (next-single-property-change fs 'face)))
              (while (and fs (< fs (point-max)))
                (setq fe (or (next-single-property-change fs 'face)
                             (point-max))
                      face (get-text-property fs 'face))
                (and face fe (setq face-list (cons (list (1- fs) (1- fe) face) face-list)))
                (setq fs fe))
              (kill-buffer (current-buffer)))
            (when face-list
              ;; (message "%S" face-list)
              (dolist (f (nreverse face-list))
                (put-text-property (+ beg (car f)) (+ beg (cadr f))
                                   'face (nth 2 f)))))
          (when (and (assoc "number" attrs)
                     (setq number (string-to-number (cdr (assoc "number" attrs)))))
            (mapc (lambda (o)
                    (let ((pos (overlay-start o)))
                      (if (or (null pos)
                              (and (> pos beg) (< pos end)))
                          (delete-overlay o)
                        (push o ovs))))
                  muse-colors-overlays)
            (setq muse-colors-overlays ovs)
            (goto-char beg)
            (forward-line 1)
            (while (and (not (eobp)) (< (point) end))
              (when (not (looking-at "</src>"))
                (setq ov (make-overlay (point) (point)))
                (push ov muse-colors-overlays)
                (overlay-put ov 'before-string (format "%4d  " number))
                (setq number (1+ number)))
              (forward-line 1))))))))
(add-to-list 'muse-colors-tags '("src" t nil nil muse-colors-src-tag))
(defun muse-html-src-tag (beg end attrs)
  "Publish the region using htmlize.
The language to use may be specified by the \"lang\" attribute.

Muse will look for a function named LANG-mode, where LANG is the
value of the \"lang\" attribute.

This tag requires htmlize 1.34 or later in order to work."
  (if (condition-case nil
          (progn
            (require 'htmlize)
            (if (fboundp 'htmlize-region-for-paste)
                nil
              (muse-display-warning
               (concat "The `htmlize-region-for-paste' function was not"
                       " found.\nThis is available in htmlize.el 1.34"
                       " or later."))
              t))
        (error nil t))
      ;; if htmlize.el was not found, treat this like an example tag
      (muse-publish-example-tag beg end)
    (muse-publish-ensure-block beg)
    (let* ((mode (and (assoc "lang" attrs)
                      (intern (concat (cdr (assoc "lang" attrs))
                                      "-mode"))))
           (text (delete-and-extract-region beg end))
           (number (and (assoc "number" attrs)
                        (string-to-number (cdr (assoc "number" attrs)))))
           htmltext)
      (with-current-buffer (get-buffer-create "*muse-temp*")
        (insert text)
        (if (functionp mode)
            (funcall mode)
          (fundamental-mode))
        (font-lock-fontify-buffer)
        ;; silence the byte-compiler
        (when (fboundp 'htmlize-region-for-paste)
          ;; transform the region to HTML
          (setq htmltext (htmlize-region-for-paste (point-min) (point-max))))
        (kill-buffer (current-buffer)))
      (save-restriction
        (narrow-to-region (point) (point))
        (insert htmltext)
        (goto-char (point-min))
        (re-search-forward "<pre\\([^>]*\\)>\n?" nil t)
        (replace-match "<pre class=\"src\">")
        (when number
          (forward-line 1)
          (while (not (eobp))
            (insert (format "%4d  " number))
            (setq number (1+ number))
            (forward-line 1))
          (forward-line 0)
          (if (looking-at "^\\s-*[0-9]+  </pre>")
              (delete-region (point) (+ (point) 6))))
        (goto-char (point-max))
        (muse-publish-mark-read-only (point-min) (point-max))))))
;;}}}

;;{{{  增加 publish-project 的 --all 选项
(defun muse-project-batch-publish ()
  "Publish Muse files in batch mode."
  (let ((muse-batch-publishing-p t)
        force)
    (if (string= "--force" (or (car command-line-args-left) ""))
        (setq force t
              command-line-args-left (cdr command-line-args-left)))
    (if (string= "--all" (or (car command-line-args-left) ""))
        (setq command-line-args-left (nconc (cdr command-line-args-left)
                                            (mapcar 'car muse-project-alist))))
    (if command-line-args-left
        (dolist (project (delete-dups command-line-args-left))
          (message "Publishing project %s ..." project)
          (muse-project-publish project force))
      (message "No projects specified."))))
;;}}}

;;{{{  移除 html 文件中中文字符之间的换行
(defun ywb-remove-html-cjk-space ()
  (when (string= (muse-style-element :base muse-publishing-current-style) "html")
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "\\(\\cc\\)\n\\(\\cc\\)" nil t)
        (unless (get-text-property (match-beginning 0) 'read-only)
          (replace-match "\\1\\2"))))))
(add-hook 'muse-after-publish-hook 'ywb-remove-html-cjk-space)
;;}}}

;;{{{  生成简单 html 数学公式，格式参考 org 
(defun ywb-muse-publish-math-tag (beg end attrs)
  (require 'org)
  (let ((tag (or (cdr (assoc "tag" attrs)) "span")))
    (insert (concat "<" tag " class=\"math\">"
                    (org-export-html-convert-sub-super
                     (delete-and-extract-region beg end))
                    "</" tag ">"))
    (muse-publish-mark-read-only beg (point))))
(add-to-list 'muse-html-markup-tags
             '("math" t t t ywb-muse-publish-math-tag))
;;}}}

;;{{{  只对我个人有用的代码

;;{{{  创建 wikisource 符号链接
(defun ywb-muse-create-wikisource ()
  "Create all wikisource directory using file symbol link"
  (interactive)
  (dolist (proj muse-project-alist)
    (let ((source (expand-file-name (car (cadr proj))))
          (wikisource
           (expand-file-name (concat (muse-get-keyword :path (nth 2 proj)) "/" "wikisource"))))
      (when (and (file-exists-p source)
                 (not (file-exists-p wikisource)))
        (message "Create link %s" wikisource)
        (call-process "ln" nil nil nil "-s" source wikisource)))))
;;}}}

;;{{{  自定义图片 html 标记代码
(setcdr (assoc 'image-with-desc muse-html-markup-strings)
        "<div class=\"figure\">
		<div class=\"photo\">
	<img src=\"%1%.%2%\" alt=\"%3%\"/>	</div>
		<p>%3%</p>
	</div>")
;;}}}

;;{{{  隐藏 literal 标签
(defun muse-colors-literal-tag (beg end)
  "Strip properties and mark as literal."
  (muse-unhighlight-region beg end)
  (let ((multi (save-excursion
                 (goto-char beg)
                 (forward-line 1)
                 (> end (point)))))
    (when (string= (buffer-substring beg (+ beg 9)) "<literal>")
      (add-text-properties beg (+ beg 9) '(invisible muse intangible t))
      (add-text-properties (- end 10) end '(invisible muse intangible t)))
    (add-text-properties beg end `(face muse-verbatim
                                        font-lock-multiline ,multi))))
;;}}}

;;{{{  为 shell-completion 增加补全选项
(when (featurep 'shell-completion)
  (add-to-list 'shell-completion-options-alist
               `("publish-project" "--force" "--all" ,@(mapcar 'car muse-project-alist))))
;;}}}

;;{{{  修正一个 table regexp 错误
(let ((table-el (assoc 2300 muse-publish-markup-regexps)))
  (unless (= (aref (cadr table-el) 0) ?^)
    (setcar (cdr table-el)
            (concat "^" (cadr table-el)))))
(let ((emdash (assoc 2500 muse-publish-markup-regexps)))
  (setcar (cdr emdash) "\\(^\\|[[:blank:]]*\\)---\\($\\|[[:blank:]]*\\)"))
;;}}}

;;{{{  增加一个标签，在 elispintro 中有用
(defun ywb-muse-publish-desc-tag (beg end)
  (let (muse-publish-inhibit-style-hooks)
    (muse-publish-ensure-block beg)
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (insert (muse-markup-text 'begin-dl))
      (insert (muse-markup-text 'begin-ddt))
      (forward-line 1)
      (muse-publish-markup-region (point) (line-end-position))
      (insert (muse-markup-text 'end-ddt))
      (forward-line 1)
      (insert (muse-markup-text 'begin-dde))
      (muse-publish-markup-region (point) (point-max))
      (goto-char (point-max))
      (insert (muse-markup-text 'end-dde)
              (muse-markup-text 'end-dl))
      (muse-publish-mark-read-only (point-min) (point-max)))))
(add-to-list 'muse-publish-markup-tags
             '("description" t nil nil ywb-muse-publish-desc-tag))
;;}}}

;;{{{  在 tutorial 系列中自动生成导航链接
(defun ywb-muse-tutor-link ()
  (if (or (null muse-publishing-current-file)
          (string= (muse-style-element :base muse-publishing-current-style) "myhtml"))
      (let* ((project (muse-project))
             (page (muse-page-name))
             (files (muse-project-file-alist))
             (default (assoc
                       (muse-get-keyword :default (cadr project))
                       files))
             (wikiword-re "\\<\\(\\(?:[[:upper:]]+[[:lower:]]*\\)\\(?:[[:upper:]]+[[:lower:]]*\\)*\\)")
             index prev next)
        (with-temp-buffer
          (insert-file-contents (cdr default))
          (goto-char (point-min))
          (when (re-search-forward (concat "^\\s-+[0-9]+.\\s-*\\(\\[\\[\\)?"
                                           (regexp-quote page)) nil t)
            (save-excursion
              (forward-line 0)
              (if (re-search-backward (concat "^\\s-+[0-9]+.\\s-*\\(\\[\\[\\)?"
                                              wikiword-re "\\]") nil t)
                  (setq prev (match-string 2))))
            (forward-line 1)
            (if (re-search-forward (concat "^\\s-+[0-9]+.\\s-*\\(\\[\\[\\)?"
                                           wikiword-re "\\]") nil t)
                (setq next (match-string 2)))
            (ywb-muse-tutor-format-anchor (car default) prev next files))))))

(defun ywb-muse-tutor-format-anchor (content prev next file-alist)
  (concat
   "<literal>
   <div class=\"tutorNav\"><ul>
"
   (mapconcat
    'identity
    (delq nil
          (list (if content (format "<li><a href=\"%s.html\">目录</a></li>" content))
                (if prev (format "<li><a href=\"%s.html\">上一节：%s</a></li>" prev
                                 (ywb-muse-page-title (cdr (assoc prev file-alist)))))
                (if next (format "<li><a href=\"%s.html\">下一节：%s</a></li>" next
                                 (or (ywb-muse-page-title (cdr (assoc next file-alist)))
                                     next)))))
    "\n")
   "
</ul></div>
</literal>"))
;;}}}

;;{{{  在第一段文字上加上 first-para 类属性，用于调整网页间距
(defun ywb-add-first-para ()
  (when (string= (muse-style-element :base muse-publishing-current-style) "html")
    (save-excursion
      (goto-char (point-min))
      (when
          (re-search-forward (regexp-quote "<!-- Page published by Emacs Muse begins here -->"))
        (forward-line 1)
        (if (or (looking-at "<p>")
                (and (looking-at (regexp-quote "<div class=\"contents\">"))
                     (re-search-forward "</div>\\s-+")
                     (looking-at "<p>")))
            (replace-match "<p class=\"first-para\">"))))))
(add-hook 'muse-after-publish-hook 'ywb-add-first-para)
;;}}}

;;{{{  用于更新来自文件中的源代码
(defun ywb-muse-src-update-from (file &optional lang)
  (if (not (file-exists-p file))
      (error "File %s is not exists!" file)
    (let (beg end)
      (save-excursion
        (save-restriction
          (skip-chars-forward "[\t \n]")
          (setq beg (point))
          (if (looking-at "<src")
              (re-search-forward "^</src>")
            (insert "<src lang=\"" (or lang "") "\">\n\n</src>"))
          (narrow-to-region beg (point))
          (goto-char (point-min))
          (forward-line 1)
          (delete-region (point)
                         (progn (goto-char (point-max))
                                (forward-line 0)
                                (point)))
          (insert-file-contents file))))))
;;}}}

;;{{{  我喜欢的 latex 表格产生格式
(defun muse-latex-markup-table ()
  (let* ((table-info (muse-publish-table-fields (match-beginning 0)
                                                (match-end 0)))
         (row-len (car table-info))
         (field-list (cdr table-info)))
    (when table-info
      (muse-insert-markup "\\begin{tabular}{" (make-string row-len ?l) "}\n")
      (unless (eq (caar field-list) 'hline)
        (muse-insert-markup "\\hline\n"))
      (dolist (fields field-list)
        (let ((type (car fields)))
          (setq fields (cdr fields))
          (if (eq type 'hline)
              (muse-insert-markup "\\hline\n")
            (when (= type 3)
              (muse-insert-markup "\\hline\n"))
            (insert (car fields))
            (setq fields (cdr fields))
            (dolist (field fields)
              (muse-insert-markup " & ")
              (insert field))
            (muse-insert-markup " \\\\\n")
            (when (= type 2)
              (muse-insert-markup "\\hline\n")))))
      (unless (eq (caar (last field-list)) 'hline)
        (muse-insert-markup "\\hline\n"))
      (muse-insert-markup "\\end{tabular}"))))
;;}}}

;;{{{  Extra command
(defun ywb-muse-project-publish-with-style (project style force)
  (interactive
   (let ((project (muse-read-project "Publish project: " nil t))
         styles style)
     (setq styles (cddr (muse-project project)))
     (if (= (length styles) 1)
         (setq style (car styles))
       (setq style (completing-read "With style: "
                                    (mapcar 'cadr styles) nil t))
       (while styles
         (if (string= style (cadr (car styles)))
             (setq style (car styles)
                   styles nil)
           (setq styles (cdr styles)))))
     (list project style current-prefix-arg)))
  (setq style (list style))
  (muse-project-save-buffers project)
  ;; run hook before publishing begins
  (run-hook-with-args 'muse-before-project-publish-hook project)
  ;; run the project-level publisher
  (let ((fun (or (muse-get-keyword :publish-project (cadr project) t)
                 'muse-project-publish-default)))
    (funcall fun project style force)))
(defun ywb-muse-clean-src ()
  (interactive)
  (save-excursion
    (forward-line 0)
    (unless (looking-at "<src")
      (re-search-backward "<src"))
    (forward-line 1)
    (let ((blanks most-positive-fixnum)
          (start (point)))
      (while (not (looking-at "</src>"))
        (skip-chars-forward " \t")
        (if (> (current-column) 0)
            (setq blanks (min blanks (current-column))))
          ;; (delete-region (1- (point)) (point)))
        (forward-line 1))
      (forward-line -1)
      (move-to-column blanks t)
      (delete-rectangle start (point)))))
;;}}}
;;}}}

(defvar tempo-muse-tags nil
  "")

(tempo-define-template
 "muse-src"
 '("<src lang=\""
   (pi ("Mode: " ("perl" "emacs-lisp" "c" "c++" "sql" "sh"))) "\">" n
   p n
   "</src>")
 "srcx"
 "Insert src tag"
 'tempo-muse-tags)
                       
(defadvice muse-follow-name-at-point (before ywb-push-mark)
  "push mark before follow the link"
  (push-mark-command t))
(ad-activate 'muse-follow-name-at-point)
                       
(provide 'muse-init)
