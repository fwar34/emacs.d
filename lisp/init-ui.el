;; -*- coding: utf-8; lexical-binding: t; -*-
(when window-system
  ;; 更改光标样式
  ;; (set-default 'cursor-type 'box)
  ;; (set-default 'cursor-type 'bar)
  ;; (setq cursor-type 'bar)
  (setq cursor-type 'box)
  ;;设置窗口位置为屏库左上角(0,0)
  ;; (set-frame-position (selected-frame) 200 50)
  ;;设置宽和高
  ;; (set-frame-width (selected-frame) 100)
  ;; (set-frame-height (selected-frame) 35)
  (add-to-list 'default-frame-alist '(fullscreen . maximized))
  ;; set a default font
  (cond
   ;; ((member "Hack" (font-family-list)) (set-face-attribute 'default nil :font "Hack-12" :slant 'Oblique))
   ;; ((member "Courier New" (font-family-list)) (set-face-attribute 'default nil :font "Courier New-12" :slant 'Oblique))
   
   ((member "Hack" (font-family-list))
    (if (equal (system-name) "feng-desktop")
        (set-face-attribute 'default nil :font "Hack-14" :slant 'Oblique)
      (set-face-attribute 'default nil :font "Hack-16" :slant 'Oblique)))
   ((member "Courier 10 Pitch" (font-family-list)) (set-face-attribute 'default nil :height 120 :slant 'Oblique :font "Courier 10 Pitch"))
   ((member "Courier New" (font-family-list))
    (if (equal system-type 'windows-nt)
        (set-face-attribute 'default nil :font "Courier New-12")
      ;; (set-face-attribute 'default nil :font "Courier New-13" :slant 'Oblique :weight 'bold)))
      (set-face-attribute 'default nil :font "Courier New-13")))
   ;; ((member "DejaVu Sans Mono" (font-family-list)) (set-face-attribute 'default nil :height 165 :font "DejaVu Sans Mono"))
   ((member "Roboto Mono" (font-family-list)) (set-face-attribute 'default nil :height 165 :font "Roboto Mono"))
   ((member "DejaVu Sans Mono" (font-family-list)) (set-face-attribute 'default nil :height 165 :font "DejaVu Sans Mono"))
   )

  ;; 中文字体的设置，同时解决中英文字体宽度不一致的问题（org-mode的表格可以中英文对齐）。
  ;; 而且解决了中文字体导致emacs卡的现象。
  (when (equal system-type 'windows-nt)
    (dolist (charset '(kana han cjk-misc bopomofo))
      ;; 台式电脑
      (if (equal (system-name) "DESKTOP-LL8PBC8")
          (set-fontset-font (frame-parameter nil 'font) charset (font-spec :family "微软雅黑" :size 18))
        (set-fontset-font (frame-parameter nil 'font) charset (font-spec :family "微软雅黑" :size 15)))))

  (when (equal system-type 'gnu/linux)
    (set-fontset-font "fontset-default" 'gb18030 '("Noto Sans CJK SC" . "unicode-bmp")))
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

(provide 'init-ui)
