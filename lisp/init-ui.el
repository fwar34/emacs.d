;; -*- coding: utf-8; lexical-binding: t; -*-

(when (display-graphic-p)
  ;; 更改光标样式
  ;; (set-default 'cursor-type 'box)
  ;; (set-default 'cursor-type 'bar)
  ;; (setq cursor-type 'bar)
  (setq cursor-type 'box)
  (if (string-equal "A12969" system-name)
      ;; 使用(print (current-frame-configuration))来确认大小
      (progn
        (set-frame-position (selected-frame) 350 0)
        (set-frame-width (selected-frame) 150)
        (set-frame-height (selected-frame) 44)
        )
    (progn
      (set-frame-position (selected-frame) 0 0)
      (add-to-list 'default-frame-alist '(fullscreen . maximized))))



  ;; {{{ font settings
  ;; http://zhuoqiang.me/torture-emacs.html同样在YoudaoNote中保存
  ;; (x-list-fonts "*")
  ;; (print (font-family-list)) 打印字体

  ;; (require 'cl-lib) ;; find-if is in common list package

  ;; (defun fwar34/font-exist-p (font)
  ;;   (if (null (x-list-fonts font))
  ;;       nil
  ;;     t))

  ;; windows: 底下这3个字体可以表格对齐
  ;; 1. "Sarasa Fixed SC"
  ;; 2. "Sarasa Term SC"
  ;; 3. "Sarasa Fixed Slab SC"
  ;; linux: 使用iosevka字体可对齐表格

  (let ((english-font "nil")
        (font-list (font-family-list)))
    (cond
     ((and (string-equal system-type "gnu/linux") (member "Iosevka Curly Slab" font-list))
      ;; Iosevka Slab, Iosevka
      (setq english-font "Iosevka Curly Slab"))
     ((member "Sarasa Fixed Slab SC" font-list)
      (setq english-font "Sarasa Fixed Slab SC")))

    ;; Auto generated by cnfonts
    ;; <https://github.com/tumashu/cnfonts>
    (set-face-attribute
     'default nil
     :font (font-spec :name english-font
                      :weight 'normal
                      :slant 'normal
                      :size (if (string= "A12969" system-name) 24 25))))
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font
     (frame-parameter nil 'font)
     charset
     ;; (font-spec :name (if (member "Microsoft YaHei UI" (font-family-list)) "Microsoft YaHei UI" "nil")
     (font-spec :name (if (member "Sarasa Fixed Slab SC" (font-family-list)) "Sarasa Fixed Slab SC" "nil")
                :weight 'normal
                :slant 'normal
                :size 24)))
  ;; }}}
  )

;; https://github.com/tumashu/cnfonts
(use-package cnfonts
  :ensure t
  :if (display-graphic-p)
  :commands (cnfonts-edit-profile cnfonts-insert-fontname cnfonts-insert-fonts-configure)
  ;; :config
  ;; (cnfonts-enable)

  ;; 1. 在scratch执行后，就会在 scratch 中插入当前可用字体的名称列表，这是一个很有用的技巧。
  ;; (cl-prettyprint (font-family-list))
  ;; (cl-prettyprint (x-list-fonts "*"))
  ;; 2. 命令：`cnfonts-insert-fontname', 可以让用户选择一个可用字体插入到当前光 标处。
  ;; 3. 使用命令: `describe-char' 可以了解光标处字符使用什么字体。
  )
;; font setting end============================================================================================

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
(setq scroll-step 1)
(setq scroll-margin 1)
(setq scroll-conservatively 101)
(setq scroll-up-aggressively 0.01)
(setq scroll-down-aggressively 0.01)
(setq auto-window-vscroll nil)
(setq fast-but-imprecise-scrolling nil)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)
;; Horizontal Scroll
(setq hscroll-step 1)
(setq hscroll-margin 1)
;; }}

;; {{
;; (global-prettify-symbols-mode 1)
;; (defun add-pretty-lambda ()
;;   "Make some word or string show as pretty Unicode symbols.  See https://unicodelookup.com for more."
;;   (setq prettify-symbols-alist
;;         '(
;;           ("lambda" . 955)
;;           ("delta" . 120517)
;;           ("epsilon" . 120518)
;;           ("->" . 8594)
;;           ("<=" . 8804)
;;           (">=" . 8805)
;;           )))
;; (add-hook 'prog-mode-hook 'add-pretty-lambda)
;; (add-hook 'org-mode-hook 'add-pretty-lambda)
;; }}

;; https://github.com/casouri/valign
;; 这个包能对齐 Org Mode、Markdown和table.el 的表格。它能对齐包含不等宽字体、中日韩字符、图片的表格。valign 不会影响 Org Mode（或 Markdown mode）基于等宽字符的对齐。）
;; (use-package valign
;;   :ensure t
;;   :hook
;;   (org-mode . valign-mode)
;;   )

(provide 'init-ui)
