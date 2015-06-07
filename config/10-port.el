;; -*- coding: utf-8 -*-
;; This config is for portable. The platform relate configuration
;; should appear here.

(deh-section "window-system"
  (when (eq window-system 'x)
    ;; no scroll bar
    (set-scroll-bar-mode nil)       
    ;; 关闭按钮
    (tool-bar-mode -1)))
    
(deh-section "coding-system"
  (unless (coding-system-p 'gbk)
    (define-coding-system-alias 'gbk 'chinese-iso-8bit))
  (unless (coding-system-p 'chinese-gbk)
    (define-coding-system-alias 'chinese-gbk 'chinese-iso-8bit))
  (prefer-coding-system 'gbk)
  (prefer-coding-system 'utf-8)

  (add-to-list 'auto-coding-alist '("\\.nfo\\'" . cp437))
  (dolist (char (append
                 "、。．，·ˉˇ¨〃々―～‖…’”）〕〉》」』〗\
】；：？！±×÷∶°′″℃／＼＂＿￣｜ㄥ"  nil))
    (modify-syntax-entry char "." (standard-syntax-table))))

(deh-section "ffap"
  (setq ffap-string-at-point-mode-alist
        '((file "--{}:\\\\$+<>@-Z_a-z~*?\x100-\xffff" "<@" "@>;.,!:")
          (url "--:=&?$+@-Z_a-z~#,%;*" "^A-Za-z0-9" ":;.,!?")
          (nocolon "--9$+<>@-Z_a-z~" "<@" "@>;.,!?")
          (machine "-a-zA-Z0-9." "" ".")
          (math-mode ",-:$+<>@-Z_a-z~`" "<" "@>;.,!?`:"))))

(deh-section "win32"
  (when (eq system-type 'windows-nt)
    (setq file-name-coding-system 'gbk)
    (set-terminal-coding-system 'gbk)
    (set-keyboard-coding-system 'gbk)
    (setq locale-coding-system 'gbk)
    (set-selection-coding-system 'gbk)
    (set-clipboard-coding-system 'ctext)
    (set-clipboard-coding-system 'gbk)
    (set-terminal-coding-system 'gbk)
    (set-buffer-file-coding-system 'gbk)
    (modify-coding-system-alist 'process "*" 'gbk)
    (setq default-process-coding-system '(gbk . gbk))
    ;; (set-language-environment 'Chinese-GB)
    (setq w32-charset-info-alist
          (cons '("gbk" w32-charset-gb2312 . 936)
                w32-charset-info-alist))
    (setq abbreviated-home-dir nil)

    (defun ywb-convert-to-cygwin-path (path)
      (concat "file:///cygdrive/" (substring path 0 1) (substring path 2)))

    (defun ywb-convert-cygwin-path (path)
      (setq path (substring path 17))
      (concat (substring path 0 1) ":" (substring path 1)))))

(deh-section "linux"
  (when (eq system-type 'gnu/linux)
    (make-variable-buffer-local 'compile-command)
    (defvar ywb-emacs-lisp-path
      (expand-file-name (concat data-directory "../site-lisp/")))
    (setq x-select-enable-clipboard t)

    (dolist (dir '("/usr/lib/info"
                   "/usr/gnu/info"
                   "/usr/gnu/lib/info"
                   "/opt/gnu/info"
                   "/usr/share/lib/info"
                   "/usr/local/share/lib/info"
                   "/usr/gnu/lib/emacs/info"
                   "~/info"
                   "~/info/perlinfo"
                   "~/local/info"
                   "~/local/share/info"))
      (add-to-list 'Info-default-directory-list dir))

    (if (= emacs-major-version 23)
        (setq find-function-C-source-directory "/home/ywb/softwares/sources/emacs-23.1/src")
      (setq find-function-C-source-directory "/home/ywb/downloads/cvs.savannah.gnu.org/emacs-22/src"))))

(deh-section "emacs23"
  ;; (require 'fenc nil t)
  (when (eq window-system 'x)
    (load (expand-file-name "my-fontset.el" ywb-config-dir))))
