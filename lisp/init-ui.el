;; -*- coding: utf-8; lexical-binding: t; -*-

(when (display-graphic-p)
  ;; 更改光标样式
  ;; (set-default 'cursor-type 'box)
  ;; (set-default 'cursor-type 'bar)
  ;; (setq cursor-type 'bar)
  (setq cursor-type 'box)
  (if (and (string-equal "A12969" system-name) (equal system-type 'windows-nt))
      ;; 使用(print (current-frame-configuration))来确认大小
      (progn
        (set-frame-position (selected-frame) 350 0)
        (set-frame-width (selected-frame) 150)
        (set-frame-height (selected-frame) 44)
        )
    ;; (progn
    ;;   (set-frame-position (selected-frame) 0 0)
    ;;   (add-to-list 'default-frame-alist '(fullscreen . maximized)))
    )
  )

;; 高亮当前行
(global-hl-line-mode 1)
;; menu bar
(menu-bar-mode -1)
;; 关闭工具栏
(if (boundp 'tool-bar-mode)
    (tool-bar-mode -1))
;; no scroll bar
(if (fboundp 'set-scroll-bar-mode)
    (set-scroll-bar-mode nil))
;; 关闭启动画面
(setq inhibit-splash-screen 1)

;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Displaying-Messages.html
;; 设置mini-window高度，例如echo area
;; Maximum height for resizing mini-windows (the minibuffer and the echo area).
;; If a float, it specifies a fraction of the mini-window frame’s height.
;; If an integer, it specifies a number of lines.
(setq max-mini-window-height 1.0) 

;; https://github.com/MatthewZMD/.emacs.d/blob/master/README.md#smooth-scrolling
;; {{ 平滑滚动
;; Vertical Scroll
;; (setq scroll-step 1)
;; (setq scroll-margin 1)
;; (setq scroll-conservatively 101)
;; (setq scroll-up-aggressively 0.01)
;; (setq scroll-down-aggressively 0.01)
;; (setq auto-window-vscroll nil)
;; (setq fast-but-imprecise-scrolling nil)
;; (setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
;; (setq mouse-wheel-progressive-speed nil)
;; ;; Horizontal Scroll
;; (setq hscroll-step 1)
;; (setq hscroll-margin 1)
;; }}

(provide 'init-ui)
