(bind-key "C-d" 'ywb-delete-char-or-region)
(bind-key "C-2" 'set-mark-command)
(bind-key "C-m" 'newline-and-indent)
(bind-key "C-j" 'newline)
(bind-key "M-/" 'hippie-expand)
(bind-key "M-f" 'ywb-camelcase-forward-word)  
(bind-key "M-b" 'ywb-camelcase-backward-word)
(bind-key "M-'" 'just-one-space)

(defvar ywb-ctrl-c-map (lookup-key global-map "\C-c"))
(bind-keys :map ywb-ctrl-c-map
           ("$" . toggle-truncate-lines)
           ("b" . ywb-create/switch-scratch)
           ("h" . help-dwim)
           ("i" . imenu)
           ("j" . ffap)
           ("l" . ywb-transpose-windows)
           ("m" . ywb-switch-major-mode)
           ("o" . browse-url-at-point)
           ("r" . compile-dwim-run)
           ("s" . compile-dwim-compile)
           ("v" . imenu-tree)
           ("u" . revert-buffer)
           ("x" . incr-dwim)
           ("w" . ywb-favorite-window-config))

(defvar ywb-ctrl-x-map (lookup-key global-map "\C-x"))
(bind-keys :map ywb-ctrl-x-map
           ("C-t" . transpose-sexps)
           ("C-r" . ywb-find-file-root)
           ("t" . template-simple-expand-template)
           ("c" . ywb-clone-buffer)
           ("j" . jump-to-register))

(bind-keys :prefix-map ywb-ctrl-c-c-prefix-map
           :prefix "C-c c"
           ("f" . find-library)
           ("v" . view-mode))

(bind-key [?\t] 'comint-dynamic-complet minibuffer-local-map)
