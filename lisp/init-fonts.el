;; -*- coding: utf-8; lexical-binding: t; -*-

;; {{{ font settings
;; http://zhuoqiang.me/torture-emacs.html同样在YoudaoNote中保存
;; (x-list-fonts "*")
;; (print (font-family-list)) 打印字体

;; (require 'cl-lib) ;; find-if is in common list package

;; (defun fwar34/font-exist-p (font)
;;   (if (null (x-list-fonts font))
;;       nil
;;     t))

(use-package faces
  :if (display-graphic-p)
  :config
  (let ((font-list (font-family-list))
        (english-font "nil")
        (english-font-size 24)
        (chinese-font "Sarasa Fixed Slab SC")
        (chinese-font-size 24))
    
    ;; pcase的用法https://emacs-china.org/t/pcase-pattern/15111
    (pcase (upcase system-name)
      ;; -----------------------------------------------------------------------------
      ;; 我的笔记本
      ("FL-NOTEBOOK"
       (pcase system-type
         ;; Windows
         ('windows-nt (when (member "Sarasa Fixed Slab SC" font-list)
                        (setq english-font "Sarasa Fixed Slab SC"
                              english-font-size 25
                              chinese-font-size 26)))
         ;; WSL
         ('gnu/linux (when (member "Iosevka Curly Slab" font-list)
                       (setq english-font "Iosevka Curly Slab"
                             english-font-size 25
                             chinese-font-size 26)))))
      ;; -----------------------------------------------------------------------------
      ;; 公司电脑
      ("A12969"
       (pcase system-type
         ;; Windows
         ('windows-nt (when (member "Sarasa Fixed Slab SC" font-list)
                        (setq english-font "Sarasa Fixed Slab SC"
                              english-font-size 24
                              chinese-font-size 24)))
         ;; WSL
         ('gnu/linux (when (member "Iosevka Curly Slab" font-list)
                       (setq english-font "Iosevka Curly Slab"
                             english-font-size 25
                             chinese-font-size 26))))))
    ;; -----------------------------------------------------------------------------
    (message "english-font %s" english-font)
    (message "english-font-size %s" english-font-size)
    (message "chinese-font %s" chinese-font)
    (message "chinese-font-size %s" chinese-font-size)
    ;; 设置英文字体
    (set-face-attribute
     'default nil
     :font (font-spec :name english-font
                      :weight 'normal
                      :slant 'normal
                      :size english-font-size))
    ;; 设置中文字体
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font
       (frame-parameter nil 'font)
       charset
       (font-spec :name chinese-font
                  :weight 'normal
                  :slant 'normal
                  :size chinese-font-size))))


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
    ;; 2. 命令：`cnfonts-insert-fontname', 可以让用户选择一个可用字体插入到当前光标处。
    ;; 3. 使用命令: `describe-char' 可以了解光标处字符使用什么字体。
    )
  )

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

(provide 'init-fonts)
