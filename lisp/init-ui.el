;; -*- coding: utf-8; lexical-binding: t; -*-
(setq print-font nil)

(when window-system
  ;; 更改光标样式
  ;; (set-default 'cursor-type 'box)
  ;; (set-default 'cursor-type 'bar)
  ;; (setq cursor-type 'bar)
  (setq cursor-type 'box)
  ;; 设置窗口位置为屏库左上角(0,0)
  ;; (set-frame-position (selected-frame) 200 50)
  (set-frame-position (selected-frame) 0 0)
  ;; 设置宽和高
  ;; (set-frame-width (selected-frame) 100)
  ;; (set-frame-height (selected-frame) 35)
  (add-to-list 'default-frame-alist '(fullscreen . maximized))

  ;; font setting start============================================================================================
  ;; http://zhuoqiang.me/torture-emacs.html同样在YoudaoNote中保存
  ;; (x-list-fonts "*")
  ;; (print (font-family-list)) 打印字体

  (require 'cl-lib) ;; find-if is in common list package

  (defun fwar34/font-exist-p (font)
    (if (null (x-list-fonts font))
        nil
      t))

  (defun fwar34/set-fonts (english-fonts englist-font-size &optional chinese-fonts chinese-font-size)
    ;; 英文字体设置
    (set-face-attribute 'default nil
                        :height englist-font-size
                        ;; :weight 'bold
                        ;; :slant 'Oblique
                        :font (find-if #'fwar34/font-exist-p english-fonts)) 
    (message (format "set englist font: %s size: %d" (find-if #'fwar34/font-exist-p english-fonts) englist-font-size))

    ;; 中文字体设置
    (when chinese-fonts
        (dolist (charset '(kana han symbol cjk-misc bopomofo))
          ;; (set-fontset-font (frame-parameter nil 'font) charset (font-spec :family "微软雅黑" :size 18))
          (set-fontset-font (frame-parameter nil 'font)
                            charset (font-spec :family (find-if #'fwar34/font-exist-p chinese-fonts)
                                               :size chinese-font-size)))
        (message (format "set chinese font: %s size: %d" (find-if #'fwar34/font-exist-p chinese-fonts) chinese-font-size)))
    )

  (defvar englist-font-list '("Courier 10 Pitch"
                              "RobotoMono Nerd Font"
                              "PragmataPro Mono"
                              "RobotoMono Nerd Font Mono"
                              "Courier New"
                              "Hack"
                              "RobotoMono NF"
                              "DejaVu Sans Mono"))
  (defvar chinese-font-list '("黑体"
                              "STHeiti"
                              "STFangsong"
                              "Microsoft Yahei"
                              "文泉驿等宽微米黑"
                              "新宋体"
                              "宋体"))

  (let ((is-set-chinese nil))
    (if (file-readable-p "/etc/os-release")
        (with-temp-buffer
           (insert-file-contents "/etc/os-release")
           (if (string-match "ID=arch" (buffer-string))
               (setq is-set-chinese t))))
    ;; (if is-set-chinese
    ;;     ;; 设置英文和中文
    ;;     (fwar34/set-fonts englist-font-list 130 chinese-font-list 25)
    ;;   ;; 设置英文
    ;;   ;; (fwar34/set-fonts englist-font-list 120)
    ;;   (fwar34/set-fonts englist-font-list 130 chinese-font-list 25))
    )

  ;; (if (equal window-system 'ns)
  ;;     (fwar34/set-fonts englist-font-list 160 chinese-font-list 16)
  ;;   (if (string= system-name "Taishiji")
  ;;       (fwar34/set-fonts englist-font-list 130 chinese-font-list 33)
  ;;     (if (string= system-name "FL-Notebook")
  ;;         (progn
  ;;           (fwar34/set-fonts englist-font-list 120 chinese-font-list 25)
  ;;           (fwar34/set-fonts '("RobotoMono Nerd Font") 120))
  ;;       (fwar34/set-fonts englist-font-list 120 chinese-font-list 16))))
  )

(defvar font-flag nil)
(defun my-change-englist-font ()
  (interactive)
  (if font-flag
      (fwar34/set-fonts '("PragmataPro Mono") 130)
    (fwar34/set-fonts '("RobotoMono Nerd Font") 120))
  (setq font-flag (not font-flag)))

;; (use-package emacs-pragmatapro-ligatures
;;   ;; :defer t
;;   :straight
;;   (:host github :repo "lumiknit/emacs-pragmatapro-ligatures")
;;   :config
;;   ;; Enable pragmatapro-lig-mode for specific modes
;;   (add-hook 'text-mode-hook 'pragmatapro-lig-mode)
;;   (add-hook 'prog-mode-hook 'pragmatapro-lig-mode)
;;   ;; or globally
;;   ;;(pragmatapro-lig-global-mode)
;;   )

;; https://github.com/tumashu/cnfonts
(use-package cnfonts
  :ensure t
  :if window-system
  :defer t
  :config
  ;; (cnfonts-enable)

  ;; 1. 在scratch执行后，就会在 scratch 中插入当前可用字体的名称列表，这是一个很有用的技巧。
  ;; (cl-prettyprint (font-family-list))
  ;; (cl-prettyprint (x-list-fonts "*"))
  ;; 2. 命令：`cnfonts-insert-fontname', 可以让用户选择一个可用字体插入到当前光 标处。
  ;; 3. 使用命令: `describe-char' 可以了解光标处字符使用什么字体。
  )

  ;; --------------------------------------------------------------------------------------------------
  ;; (cond
  ;;  ((member "RobotoMono Nerd Font" (font-family-list))
  ;;   (progn
  ;;     (set-face-attribute 'default nil :height 120 :font "RobotoMono Nerd Font")
  ;;     (when print-font (message "RobotoMono Nerd Font")))))
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

(when (display-graphic-p)
  ;; Auto generated by cnfonts
  ;; <https://github.com/tumashu/cnfonts>
  (set-face-attribute
   'default nil
   :font (font-spec :name "Sarasa Mono Slab K"
                    :weight 'normal
                    :slant 'normal
                    :size 25))
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font
     (frame-parameter nil 'font)
     charset
     (font-spec :name "nil"
                :weight 'normal
                :slant 'normal
                :size 12.5))))

(provide 'init-ui)
