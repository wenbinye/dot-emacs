;; -*- coding: utf-8 -*-
;;; customization
(setq custom-file (expand-file-name "01-my-custom.el" ywb-config-dir))

;;{{{ 一般设置
;; 语法高亮
(global-font-lock-mode t)
;; fill 相关
(setq default-justification 'full)
(setq adaptive-fill-mode nil)
(setq fill-column 70)
;; 关闭开机画面
(setq inhibit-startup-message t)
;; 没有提示音
(setq ring-bell-function 'ignore)
;; (setq visible-bell t)
;; 折行
(setq truncate-partial-width-windows nil) 
;; 显示列号
(setq column-number-mode t)
;; 删除整行
(setq-default kill-whole-line t)
(setq kill-ring-max 50) 
;; 不用TAB来缩进
(setq-default indent-tabs-mode nil) 
(setq default-tab-width 4)
(setq tab-stop-list nil)
(setq display-time-mail-file "~/.emacs.d/mail")

;; 设置 sentence-end, 可以识别中文
(setq sentence-end "\\([。！？。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")
;; 不要在 fill 时在句号后加空格
(setq sentence-end-double-space nil)
;; 滚动边界
(setq scroll-margin 0
      scroll-conservatively 10000)
;; 显示括号匹配, 而不是跳到另一个括号
(show-paren-mode t)
(setq show-paren-style 'parentheses)
;; 光标靠近鼠标时鼠标跳开
(mouse-avoidance-mode 'animate)
;; 可以显示图片
(auto-image-file-mode t)
;; 显示选中区域
(transient-mark-mode t)
;; 备份和版本控制
(setq version-control t)
(setq kept-new-versions 3)
(setq delete-old-versions t)
(setq kept-old-versions 2)
(setq dired-kept-versions 1)
;;; DO NOT depends on the backup, it is not really useful
(setq backup-directory-alist '(("." . "~/.emacs.d/backup")))
(setq backup-by-copying t)
;; 默认目录
(setq default-directory "~/")
;; 文件名补全忽略大小写
(setq read-file-name-completion-ignore-case t)
(setq completion-ignore-case t)

;; diary, todo, calendar
(deh-section "calendar"
  (setq diary-file "~/.emacs.d/diary")
  (setq todo-file-do "~/.emacs.d/todo-do")
  (setq todo-file-done "~/.emacs.d/todone-done")
  (setq todo-file-top "~/.emacs.d/todone-top")
  (add-hook 'initial-calendar-window-hook (lambda () (toggle-truncate-lines 1)))
  ;; calendar
  ;; for calendar-sunrise-sunset
  (setq calendar-longitude 114
        calendar-latitude 22.3
        calendar-location-name "深圳"))

;; set my file register
(deh-section "register" 
  (set-register ?. '(file . "~/.emacs"))
  (set-register ?, '(file . "~/.emacs.d"))
  (set-register ?t '(file . "~/temp"))
  (set-register ?d '(file . "~/downloads")))

;; prevent no response if click the memu in File
(fset 'print-buffer 'ignore)
(setq lpr-command "")
(setq printer-name "")
;; abbrev
(setq abbrev-file-name "~/.emacs.d/abbrev_defs")
(put 'define-abbrev-table 'lisp-indent-function 1)
;;}}}

(custom-set-variables
 '(confirm-kill-emacs (quote y-or-n-p))
 '(cperl-invalid-face nil)
 '(desktop-globals-to-save (quote (sql-mysql-schema bibus-formats desktop-missing-file-warning search-ring regexp-search-ring register-alist windata-named-winconf)))
 '(diff-switches "-ubB")
 '(dired-listing-switches "-alvh")
 '(ediff-split-window-function (quote split-window-horizontally))
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(flymake-gui-warnings-enabled nil)
 '(folding-folding-on-startup nil)
 '(ido-enable-regexp t)
 '(ido-everywhere t)
 '(outline-minor-mode-prefix "")
 '(tooltip-use-echo-area nil)
 '(view-read-only t)
 '(woman-cache-filename "~/.wmncach.el")
 '(woman-manpath (quote ("/usr/man" "/usr/share/man" "/usr/X11R6/man" "/usr/local/man" "/home/ywb/proj/perl/man" "/usr/share/man/zh_TW" "/usr/share/man/zh_CN"))))

(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)

