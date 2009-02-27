(provide 'emms-init)

;;{{{  Basic settings
(deh-section "emms-setup"
  (require 'emms-setup)
  (emms-standard)
  (setq emms-lyrics-coding-system nil   ; automatic decode
        emms-lyrics-dir "/home/ywb/music/lrc/"
        emms-source-file-default-directory "~/music/playlist/" ; source directory
        emms-volume-amixer-control "PCM"
        emms-playlist-buffer-name " *EMMS*"
        emms-playlist-mode-window-width 35
        emms-lyrics-scroll-timer-interval 1
        emms-player-list '(emms-player-mplayer
                           emms-player-timidity
                           emms-player-mpg321
                           emms-player-ogg123)
        ;; never use ogg
        emms-score-file "~/.emacs.d/.emms-score")

  (setq emms-playlist-default-major-mode
        'emms-playlist-mode)

  (add-hook 'emms-playlist-mode-hook
            (lambda ()
              (toggle-truncate-lines 1)))

  (require 'emms-mode-line)
  (emms-mode-line 1)
  (require 'emms-playing-time)
  (emms-playing-time 1)
  (require 'emms-lyrics)
  (emms-lyrics 1)
  (require 'emms-volume)

  (require 'emms-mark)
  (require 'emms-tag-editor)
  (require 'emms-history)
  (setq emms-history-start-playing t)
  (require 'emms-score)
  (emms-score 1)
  (require 'emms-i18n))
;;}}}

;;{{{  find lyric in emms-lyrics-dir more relax
(defun ywb-emms-current-track-title ()
  (let ((title
         (emms-track-get
          (emms-playlist-current-selected-track)
          'info-title))
        file)
    (if (string-match ".*\\s-*[-_]\\s-*\\(.+\\)\\s-*$" title)
        (setq title (match-string 1 title)))
    (if (string-match "\\(.*\\)\\s*-(" title)
        (setq title (match-string 1 title)))
    (replace-regexp-in-string "^[0-9._]+" "" title)))

(defun ywb-emms-lyrics-find-lyric (file)
  "Return full path of found lrc FILE, or nil if not found.
Use `emms-source-file-gnu-find' to find lrc FILE under current directory
and `emms-lyrics-dir'.
e.g., (emms-lyrics-find-lyric \"abc.lrc\")"
  (let* ((track (emms-playlist-current-selected-track))
         (dir (file-name-directory (emms-track-get track 'name)))
         lrc)
    (if (catch 'found
          (when (and (eq 'file (emms-track-get track 'type))
                     (file-exists-p (concat dir file))) ; same directory?
            (setq lrc (concat dir file))
            (throw 'found t))
          (dolist (dir (list emms-lyrics-dir "/media/sda1/Program Files/TTPlayer/Lyrics/"))
            (setq lrc (concat (file-name-as-directory dir)
                              (emms-track-get track 'info-artist)
                              " - "
                              (emms-track-get track 'info-title) ".lrc"))
            (if (file-exists-p lrc)
                (throw 'found t)))
          (when (and (emms-track-get track 'info-title)
                     (not (string= emms-lyrics-dir "")))
            (setq lrc
                  (car
                   (split-string
                    (shell-command-to-string
                     (concat emms-source-file-gnu-find " "
                             emms-lyrics-dir " -iname "
                             "'*"  ; wrap up whitespaces, FIXME, '->\'
                             (emms-replace-regexp-in-string "'" "*" (ywb-emms-current-track-title))
                             "*'"))
                    "\n" t)))
            (throw 'found t)))
        lrc)))

(setq emms-lyrics-find-lyric-function 'ywb-emms-lyrics-find-lyric)
(defun ywb-emms-trueice-get-lrc ()
  (let ((track (emms-playlist-current-selected-track)))
    (when (and (eq (emms-track-get track 'type) 'url)
               (string-match "http://iris.trueice.net" (emms-track-name track)))
      (start-process
       "lrcsearch" nil "lrcbaidu.pl"
       "-t" (emms-track-get track 'info-title)))))
(add-hook 'emms-player-started-hook 'ywb-emms-trueice-get-lrc)

;;}}}

;;{{{ automatically add m3u file download from http://music.trueice.net
(defvar ywb-emms-stream-m3u-list '((file . "/tmp/play.m3u")
                                   (regexp "/tmp/" ".*\\.m3u")))
(defvar ywb-emms-stream-timer nil)
(defun ywb-emms-stream-handler ()
  (when (buffer-live-p emms-playlist-buffer)
    (let (files tmp)
      (dolist (file ywb-emms-stream-m3u-list)
        (cond ((and (eq (car file) 'file)
                    (file-exists-p (cdr file)))
               (setq files (cons (cdr file) files)))
              ((eq (car file) 'regexp)
               (setq files (append (directory-files (nth 1 file)
                                                    t (nth 2 file))
                                   files)))
              ((eq (car file) 'function)
               (setq files (append (funcall (cdr file)) files)))))
      (dolist (file files)
        (setq tmp (make-temp-file "emms-"))
        (rename-file file tmp t)
        (with-current-emms-playlist
          (emms-add-playlist tmp))))))
(define-minor-mode ywb-emms-stream-mode
  "If turn on, the m3u file include in `ywb-emms-stream-m3u-list'
will automatic load when exists."
  :global t
  (if ywb-emms-stream-timer
      (cancel-timer ywb-emms-stream-timer))
  (if ywb-emms-stream-mode
      (setq ywb-emms-stream-timer
            (run-at-time nil 2 'ywb-emms-stream-handler))))
;;}}}

;;{{{ set mp3 info from file name
(require 'url-util)
(defun ywb-emms-info-file-name (track)
  "Use the file name as the track information. Use parent
directory name as artist and current file name as title. Add this
to the end of `emms-info-functions' for not rewrite exists
information when needed."
  (let ((name (url-unhex-string (emms-track-name track)))
        (artist (emms-track-get track 'info-artist))
        (title (emms-track-get track 'info-title)))
    (when (or (null artist)
              (string= artist "")
              (string-match "unknown" artist))
      (emms-track-set track 'info-artist
                      (file-name-nondirectory
                       (substring (file-name-directory name) 0 -1))))
    (when (or (null title)
              (string= title ""))
      (emms-track-set track 'info-title
                      (file-name-sans-extension
                       (file-name-nondirectory name))))))
(add-to-list 'emms-info-functions 'ywb-emms-info-file-name t)
;;}}}

;;{{{ coding system handle
;;;; redefine of this function for encoding support.
;;;; So that emms can play stream music that from http://music.trueice.net
(defun emms-source-playlist-parse-m3u ()
  "Parse the native EMMS playlist in the current buffer."
  (let ((coding buffer-file-coding-system))
    (mapcar (lambda (file)
              (if (string-match "\\`http://" file)
                  (let ((track (emms-track 'url
                                           (replace-regexp-in-string "\r$" "" file))))
                    (emms-track-set track 'coding buffer-file-coding-system)
                    track)
                (emms-track 'file file)))
            (emms-source-playlist-m3u-files))))
(defun emms-player-mplayer-start (track)
  (let ((default-process-coding-system
          (copy-tree default-process-coding-system))
        (filename (emms-track-name track))
        process)
    (if (emms-track-get track 'coding)
        (setcdr default-process-coding-system
                (emms-track-get track 'coding)))
    
    (setq process (apply 'start-process
                         emms-player-simple-process-name
                         nil
                         emms-player-mplayer-command-name
                         ;; splice in params here
                         (append emms-player-mplayer-parameters
                                 (list filename))))
    ;; add a sentinel for signaling termination
    (set-process-sentinel process 'emms-player-simple-sentinel))
  (emms-player-started emms-player-mplayer))
(defun emms-player-mplayer-playable-p (track)
  "Return non-nil when we can play this track."
  (and (executable-find emms-player-mplayer-command-name)
       (memq (emms-track-type track) '(file url))
       (string-match (emms-player-get 'emms-player-mplayer 'regexp)
                     (emms-track-name track))))
(emms-player-set 'emms-player-mplayer
                 'regexp
                 (regexp-opt '(".ogg" ".mp3" ".wav" ".mpg" ".mpeg" ".wmv"
                               ".wma" ".mov" ".avi" ".divx" ".ogm" ".asf"
                               ".mkv" "http://" "mms://" ".rm" ".rmvb" ".mp4"
                               ".flac" ".vob" ".m4a" ".flv")))
;;}}}

;;{{{  Extra commands
(defun ywb-emms-playlist-clear-track-and-next ()
  "Clear current track and play next"
  (interactive)
  (with-current-emms-playlist
    (goto-char emms-playlist-selected-marker)
    (emms-playlist-mode-kill-entire-track)
    (if (emms-playlist-track-at)
        (emms-playlist-mode-play-current-track)
      (emms-next))))

(defvar ywb-emms-music-download-dir
  (let ((dir (expand-file-name
              (concat emms-source-file-default-directory "../download/"))))
    (or (file-exists-p dir)
        (make-directory dir t))
    dir))
(defun ywb-emms-download-file (&optional track)
  (let ((default-process-coding-system (cons locale-coding-system
                                             locale-coding-system)))
    (setq track (or track (emms-playlist-current-selected-track)))
    (when (and (string-match "http://iris.trueice.net" (emms-track-name track))
               (featurep 'emms-score)
               (> (emms-score-get-score (emms-track-name track)) 0))
      (let* ((file (emms-track-name track))
             (artist (emms-track-get track 'info-artist))
             (title (emms-track-get track 'info-title))
             (dir (concat ywb-emms-music-download-dir artist "/"))
             (dest (concat dir title ".mp3")))
        (unless (file-exists-p dest)
          (or (file-exists-p dir)
              (make-directory dir))
          (start-process "wget" nil "wget" file "-O" dest))))))
(add-hook 'emms-player-started-hook 'ywb-emms-download-file)

(defun ywb-emms-toggle-amxier-control ()
  (interactive)
  (if (string= emms-volume-amixer-control "PCM")
      (setq emms-volume-amixer-control "Master")
    (setq emms-volume-amixer-control "PCM")))

(defun ywb-emms-restart ()
  (interactive)
  (emms-stop)
  (emms-start))

(defun ywb-emms-select-track-ap ()
  "Select next track to play without stop current"
  (interactive)
  (emms-playlist-select (save-excursion
                          (forward-line -1)
                          (when (bobp)
                            (goto-char (point-max))
                            (forward-line 0))
                          (point))))

(defun emms-playlist-mode-go ()
  "Switch to the current emms-playlist buffer and use emms-playlist-mode."
  (interactive)
  (if (or (null emms-playlist-buffer)
          (not (buffer-live-p emms-playlist-buffer)))
      (error "No current Emms buffer")
    (switch-to-buffer emms-playlist-buffer)
    (when emms-playlist-mode-center-when-go
      (emms-playlist-mode-center-current))))
;;{{{  change volume
(defvar emms-volume-amixer-raise-commands
  '(?=))
(defvar emms-volume-amixer-lower-commands
  '(?-))

(defun emms-volume-amixer-raise (&optional arg)
  (interactive "P")
  (if arg
      (emms-volume-amixer-change -2)
    (emms-volume-amixer-change 2))
  (let (command)
    (while (progn
             (setq command (read-event))
             (cond ((member command emms-volume-amixer-raise-commands)
                    (emms-volume-amixer-change 2))
                   ((member command emms-volume-amixer-lower-commands)
                    (emms-volume-amixer-change -2)))))
    (setq unread-command-events (list command))))

(defun emms-volume-amixer-lower ()
  (interactive)
  (emms-volume-amixer-raise -1))
;;}}}

;;}}}

;;{{{  mode-line and header-line
(setq frame-title-format `(,(format "emacs%d@%%b %%f --" emacs-major-version)
                           (vc-mode vc-mode)
                           mode-line-modes
                           (which-func-mode
                            ("" which-func-format
                             "--"))))
(setq-default mode-line-format 
              '(#("-" 0 1
                  (auto-composed t help-echo "mouse-1: select (drag to resize), mouse-2: delete others, mouse-3: delete this"))
                mode-line-mule-info mode-line-modified mode-line-frame-identification
                mode-line-buffer-identification
                mode-line-position
                (global-mode-string
                 (#("--" 0 2
                    (help-echo "mouse-1: select (drag to resize), mouse-2: delete others, mouse-3: delete this"))
                  global-mode-string))))
;; (setq global-mode-string
;;       (append (remove-if (lambda (s)
;;                            (and (symbolp s) (string-match "emms" (symbol-name s))))
;;                          global-mode-string)
;;               '(emms-mode-line-string emms-playing-time-string emms-lyrics-mode-line-string)))
;;}}}

;;{{{  play method
(defvar ywb-emms-play-random nil)
(defadvice emms-playlist-select-next (before ywb-emms-play-random)
  (emms-playlist-ensure-playlist-buffer)
  (when ywb-emms-play-random
    (unless (and emms-playlist-selected-marker
                 (marker-position emms-playlist-selected-marker))
      (setq emms-playlist-selected-marker (make-marker)))
    (set-marker emms-playlist-selected-marker (random (1- (point-max))))))
(ad-activate 'emms-playlist-select-next)
(defvar ywb-emms-play-method-stack nil)
(defvar ywb-emms-play-method-stack-length 10)
(defvar ywb-emms-play-method-list
  '(ywb-emms-play-random
    emms-repeat-track
    emms-repeat-playlist))
(defun ywb-emms-toggle-play-method (arg method)
  (set method
       (if (null arg) (not (symbol-value method))
         (> (prefix-numeric-value arg) 0)))
  (dolist (method (remove method ywb-emms-play-method-list))
    (set method nil))
  (if (symbol-value method)
      (progn
        (setq ywb-emms-play-method-stack (cons method ywb-emms-play-method-stack))
        (if (> (length ywb-emms-play-method-stack)
               ywb-emms-play-method-stack-length)
            (setq ywb-emms-play-method-stack
                  (butlast ywb-emms-play-method-stack
                           (- (length ywb-emms-play-method-stack)
                              ywb-emms-play-method-stack-length)))))
    (setq ywb-emms-play-method-stack (cdr ywb-emms-play-method-stack))
    (and (car ywb-emms-play-method-stack)
         (set (car ywb-emms-play-method-stack) t)))
  (emms-mode-line-alter)
  (force-mode-line-update)
  (message "Emms now will %s%s"
           (if (symbol-value method) "" "not ")
           (mapconcat 'identity
                      (last (split-string (symbol-name method) "-") 2) " ")))
(defun ywb-emms-toggle-repeat-playlist (arg)
  (interactive "P")
  (ywb-emms-toggle-play-method arg 'emms-repeat-playlist))
(defun ywb-emms-toggle-repeat-track (arg)
  (interactive "P")
  (ywb-emms-toggle-play-method arg 'emms-repeat-track))
(defun ywb-emms-toggle-play-random (arg)
  (interactive "P")
  (ywb-emms-toggle-play-method arg 'ywb-emms-play-random))
;;}}}

;;{{{  Key bindings
(deh-section "emms-kbd"
  (deh-define-key emms-volume-minor-mode-map
    ("=" . 'emms-volume-mode-plus))

  (deh-define-key emms-playlist-mode-map
    ("=" . 'emms-volume-raise)
    ("A" . 'emms-score-up-file-on-line)
    ("X" . 'emms-score-down-file-on-line)
    ("N" . 'emms-playlist-new)
    ("S" . 'ywb-emms-select-track-ap)
    ("j" . 'ywb-emms-toggle-play-random)
    ("r" . 'ywb-emms-toggle-repeat-playlist)
    ("R" . 'ywb-emms-toggle-repeat-track))

  (set-keymap-parent ctl-ce-map emms-playlist-mode-map)
  (deh-define-key ctl-ce-map
    ("m" . 'emms-playlist-mode-go-popup)
    ("u" . 'emms-score-up-playing)
    ("d" . 'ywb-emms-playlist-clear-track-and-next)
    ("s" . 'emms-start)
    ("x" . 'emms-stop)
    ("-" . 'emms-volume-amixer-lower)
    ("g" . 'ywb-emms-restart)
    ("t" . 'emms-lyrics-visit-lyric)
    ("=" . 'emms-volume-amixer-raise)))
;;}}}
(emms-history-load)
