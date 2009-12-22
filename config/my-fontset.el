;;; The setting blow works well in GNU Emacs 23.0.60.1 built on 2008-10-14
;;; shipped with Ubuntu 8.10.
;;;
;;; 三个预定义的 fontset：
;;;  fontset-standard, fontset-startup, fontset-default
;;;  用 C-h r m Fontsets <RET> 查看 fontset 的说明
;;;  用 M-x describe-fontset <RET> fontset-default 查看 fontset-default 的定义
;;;  用 M-x describe-char 或者 C-u C-x = 查看光标下字符的字体信息
;;;  (frame-parameter nil 'font) 取得当前 frame 的 font
;;;  (frame-parameter nil 'font-backend) 取得当前 frame 字体渲染后端
;;;  Shift + LeftClick 弹出字体选择对话框
;;;
;;; 设置默认字体的三种办法：
;;; 1) 在 .Xdefaults 中设置 Emacs.Font 等资源，或者在命令用 -fn 指定
;;;    字体，这都会修改 fontset-startup;
;;; 2) 在 .emacs 用 set-fontset-font 修改 fontset-default;
;;; 3) 在 .emacs 用 create-fontset-from-fontset-spec 或者
;;;    create-fontset-from-ascii-font 创建新的 fontset，并用
;;;    set-default-font 设置当前 frame 的字体(Emacs 23.1 此函数
;;;    被废弃，成为 set-frame-font 的别名);
;;;
;;; 创建新的 fontset 有两种办法：
;;; 1) 用 create-fontset-from-fontset-spec 一次性设置好所有字符集的字体;
;;; 2) 用 create-fontset-from-fontset-spec 或者
;;;    create-fontset-from-ascii-font 设置好 ascii 字符集的字体，再用
;;;    set-fontset-font 针对各个字符集调整字体;
;;;
;;; set-fontset-font 有三种用法：(参考 C-h f set-fontset-font)
;;; 1) 用 font-spec 函数指定字体
;;; 2) 用 (FAMILY . REGISTRY) 指定
;;; 3) 用 字体名字指定，见 C-h r m Font X <RET>
;;;
;;; 另外，设置新建 frame 的默认字体有两种办法：
;;; 1) 修改 default-frame-alist
;;; 2) 在每个 frame 创建时自动执行某段设置代码(有的 Emacs 版本里第一种
;;;    办法不灵)
;;;
;;; 小问题:
;;; 1) 有的 Emacs 版本里这些设置不能用于 emacs -nw，这需要用 window-system
;;; 是否设置来区分 Emacs 的图形和字符界面，有的 Emacs 版本这个变量没有
;;; 定义，安全的办法是：
;;;    (if (and (boundp 'window-system) window-system)
;;;      (...字体设置代码...))
;;; 2) 似乎字体里的 MAKER 为 unknown 有特殊含义，-*-DejaVu Sans Mono-...
;;; 就没有 -unknown-DejaVu Sans Mono-... 的效果好。
;;;

;; use `alias emacs='emacs -fn "DejaVu Sans Mono:size=14"` to avoid
;; frame size switching on startup.

;; Way 1
;; ;(let ((zh-font "-unknown-AR PL UMing CN-*-*-*-*-16-*-*-*-*-*-*-*")
;; (let ((zh-font "WenQuanYi Bitmap Song:pixelsize=13")
;;       (fontset "fontset-my"))
;;   (create-fontset-from-fontset-spec
;;     (concat
;;       "-unknown-DejaVu Sans Mono-*-*-*-*-12-*-*-*-*-*-" fontset
;;       ",kana:"          zh-font
;;       ",han:"           zh-font
;;       ",symbol:"        zh-font
;;       ",cjk-misc:"      zh-font
;;       ",bopomofo:"      zh-font))
;;   (set-default-font fontset)
;;   (add-to-list 'default-frame-alist `(font . ,fontset)))
  ;; or set font for new frame like this:
  ;(add-to-list 'after-make-frame-functions
  ;             (lambda (new-frame)
  ;               (select-frame new-frame)
  ;               (if window-system
  ;                 (set-frame-font "fontset-my"))))


;; Way 2
(let ((fontset nil)
      (zh-font (font-spec :family "WenQuanYi Bitmap Song" :size 12)))
  (set-default-font "DejaVu Sans Mono:pixelsize=12:foundry=unknown")
  (setq fontset (frame-parameter nil 'font))
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font fontset charset zh-font))
  (add-to-list 'default-frame-alist `(font . ,fontset)))
 
