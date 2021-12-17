;; -*- coding: utf-8; lexical-binding: t; -*-

;; 字体设置参考 https://www.gnu.org/software/emacs/manual/html_node/emacs/Fonts.html 和 (describe-function set-face-attribute 或者 font-spec)
;; 可以使用 fc-list : family 来显示所有的字体 family

;; {{{ font settings
;; http://zhuoqiang.me/torture-emacs.html同样在YoudaoNote中保存
;; (x-list-fonts "*")
;; (print (font-family-list)) 打印字体

;; (require 'cl-lib) ;; find-if is in common list package

;; (defun fwar34/font-exist-p (font)
;;   (if (null (x-list-fonts font))
;;       nil
;;     t))

(defun fwar34-set-fonts ()
  "My change fonts"
  (interactive)
  (let ((font-list (font-family-list))
        ;; (english-font "Ubuntu Mono")
        (english-font "JetBrains Mono")
        (english-font-size 24)
        ;; (chinese-font "Sarasa Fixed Slab SC")
        (chinese-font "Noto Sans CJK SC")
        ;; (chinese-font "Noto Serif CJK SC")
        ;; (chinese-font "Noto Sans SC")
        ;; (chinese-font "Source Han Sans CN")
        (chinese-font-size 24)
        (setlight 'semi-light))
    
    ;; (print font-list)
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
      ("DESKTOP-ARCHLINUX"
       (pcase system-type
         ;; Windows
         ('gnu/linux (when (member "Noto Sans CJK SC" font-list)
                       (setq ;;english-font "JetBrainsMono Nerd Font"
                        english-font "Iosevka"
                        english-font-size 32
                        chinese-font "Noto Sans CJK SC"
                        ;; chinese-font "Sarasa Mono SC Nerd"
                        chinese-font-size 32)))
         ;; WSL
         ;; ('gnu/linux (when (member "Iosevka Curly Slab" font-list)
         ;;               (setq english-font "Iosevka Curly Slab"
         ;;                     english-font-size 24
         ;;                     chinese-font-size 24)))
         ))
      ;; -----------------------------------------------------------------------------
      ;; 公司虚拟机 lxd
      ((or "FENG-ARCHLINUX" "UBUNTU-AWESOME")
       ;; (when (member "JetBrains Mono" font-list)
       ;;                 (setq english-font "JetBrainsMono Nerd Font"
       ;;                       english-font-size 24
       ;;                       chinese-font-size 24))
       ;; (when (member "Iosevka Curly Slab" font-list)
       ;;                 (setq english-font "Iosevka Curly Slab" ;; Sarasa Mono SC Nerd
       ;;                       english-font-size 24
       ;;                       chinese-font-size 24))
       ;; (when (member "Sarasa Mono SC Nerd" font-list)
       ;;                 (setq english-font "Sarasa Mono SC Nerd" ;; Sarasa Mono SC Nerd
       ;;                       english-font-size 24
       ;;                       chinese-font-size 24))
       (when (and (member "Iosevka" font-list) (member "Noto Sans SC" font-list))
                       (setq english-font "Iosevka" ;; Sarasa Mono SC Nerd
                             english-font-size 24
                             chinese-font "Noto Sans SC"
                             chinese-font-size 24))
       (when (and (member "Iosevka" font-list) (member "Noto Sans CJK SC" font-list))
         (setq english-font "Iosevka" ;; Sarasa Mono SC Nerd
               english-font-size 24
               chinese-font "Noto Sans CJK SC"
               chinese-font-size 24
               setlight nil))
       )
      )
      ;; -----------------------------------------------------------------------------
    (message "english-font %s" english-font)
    (message "english-font-size %s" english-font-size)
    (message "chinese-font %s" chinese-font)
    (message "chinese-font-size %s" chinese-font-size)
    (message "setlight %s" setlight)

    ;; 单独设置 org-table 字体
    ;; Org table font
    ;; (custom-set-faces
    ;;  '(org-table ((t (:family "Ubuntu Mono derivative Powerline")))))

    ;; 单独设置 markdown-code-face
    (when (equal system-type 'gnu/linux) (display-graphic-p)
          (custom-set-faces
           '(markdown-code-face ((t (:family english-font)))))
          (custom-set-faces
           '(markdown-preview-face ((t (:family english-font)))))
          (custom-set-faces
           '(org-table ((t (:family english-font)))))
          )
    ;; (markdown-pre-face markdown-code-face)

    ;; 设置英文字体
    (set-face-attribute
     'default nil
     :font (font-spec :name english-font
                      ;; :weight 'normal
                      :weight 'semi-light
                      :slant 'normal
                      :size english-font-size))

    ;; (setq chinese-font "Noto Sans SC")
    ;; (setq chinese-font "Noto Sans CJK SC")
    ;; (setq chinese-font-size 24)
    ;; 设置中文字体
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font
       ;; (frame-parameter nil 'font)
       t
       charset
       (font-spec :name chinese-font
                  ;; :weight 'normal
                  ;; :weight 'semi-light ;; 字体没有安装 weight 相关的字体的时候设置会失败
                  :weight setlight
                  :slant 'normal
                  :size chinese-font-size)))

    ;; (dolist (charset '(kana han symbol cjk-misc bopomofo))
    ;;   (set-fontset-font t charset chinese-font))
    ;; (setq face-font-rescale-alist
    ;;       (mapcar (lambda (item) (cons item 1.2)) chinese-font))
    )
  )

(use-package faces
  :if (display-graphic-p)
  :config
  (fwar34-set-fonts)

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

(when (display-graphic-p)
  (use-package ligature
    ;; :disabled
    :ensure t
    :straight
    (:host github :repo "mickeynp/ligature.el")
    :config
    ;; (ligature-set-ligatures 'go-mode
    ;;                         '("->" "->>" "<=" ">=" "!=" "<-" ":="))
    ;; Enable the "www" ligature in every possible major mode
    (ligature-set-ligatures 't '("www"))
    ;; Enable traditional ligature support in eww-mode, if the
    ;; `variable-pitch' face supports it
    (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
    ;; Enable all Cascadia Code ligatures in programming modes
					; (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
					;                                      ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
					;                                      "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
					;                                      "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
					;                                      "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
					;                                      "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
					;                                      "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
					;                                      "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
					;                                      ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
					;                                      "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
					;                                      "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
					;                                      "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
					;                                      "\\\\" "://"))
    ;; (ligature-set-ligatures 'prog-mode '("<=" ">=" "!=" "|=" "||" ":=" "->" "->>" "&&" ">>>" "<<<" "?:"))
    (ligature-set-ligatures 'prog-mode '("<=" ">=" "!=" ":=" "&&" "->" "::" "<-"))
    ;; Enables ligature checks globally in all buffers. You can also do it
    ;; per mode with `ligature-mode'.
    (global-ligature-mode t)
    ))

(provide 'init-fonts)
