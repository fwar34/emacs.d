;;; init-input.el --- Useful preset transient commands  -*- coding:utf-8; lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(when (and (display-graphic-p) (>= emacs-major-version 26))
  (use-package posframe))

(defun my-pyim/rime-toggle-input-method-advice (&rest _)
  "Load pyim before `toggle-input-method'."
  (unless (featurep 'pyim)
    (advice-remove 'toggle-input-method #'my-pyim/rime-toggle-input-method-advice)
    (message "input unless xxxxxxx")

    (use-package pyim
      :init
      (defun my-pyim-predicate-org-in-src-block-p ()
        "Whether point is in an org-mode's code source block."
        (and (derived-mode-p 'org-mode)
             (org-in-src-block-p)))

      (defun my-pyim-predicate-in-doc-string-p ()
        "Whether point is in the doc string."
        (or
         (eq (plist-get (text-properties-at (point)) 'face) 'font-lock-doc-face)
         (eq (plist-get (text-properties-at (point)) 'face) 'font-lock-string-face)))
      :config
      (message "pyim xxxxxx")
      ;; 激活 basedict 拼音词库，五笔用户请继续阅读 README
      (use-package pyim-basedict
        :after pyim
        :config
        (pyim-basedict-enable))

      ;; (setq default-input-method "pyim")
      (defun my-chinese-setup ()
        "Set up my private Chinese environment."
        (setq default-input-method "pyim"))
      (add-hook 'set-language-environment-hook 'my-chinese-setup)

      ;; 按 "C-<return>" 将光标前的 regexp 转换为可以搜索中文的 regexp.
      ;; (define-key minibuffer-local-map (kbd "C-<return>") 'pyim-cregexp-convert-at-point)
      ;; (define-key ivy-minibuffer-map (kbd "C-<return>") 'pyim-cregexp-convert-at-point)
      
      ;; 我使用全拼
      ;; (setq pyim-default-scheme 'quanpin)
      ;; (setq pyim-default-scheme 'pyim-shuangpin)
      (setq pyim-default-scheme 'xiaohe-shuangpin)

      (defun my-englist-p ()
        "disable pyim in modes"
        (equal major-mode 'eshell-mode))

      ;; 设置 pyim 探针设置，这是 pyim 高级功能设置，可以实现 *无痛* 中英文切换 :-)
      ;; 我自己使用的中英文动态切换规则是：
      ;; 1. 光标只有在注释里面时，才可以输入中文。
      ;; 2. 光标前是汉字字符时，才能输入中文。
      ;; 3. 使用 M-j 快捷键，强制将光标前的拼音字符串转换为中文。
      (setq-default pyim-english-input-switch-functions
                    '(;; pyim-probe-dynamic-english
                      pyim-probe-auto-english
                      pyim-probe-isearch-mode
                      pyim-probe-program-mode
                      pyim-probe-org-structure-template
                      my-englist-p
                      my-pyim-predicate-in-doc-string-p
                      my-pyim-predicate-org-in-src-block-p
                      meow-normal-mode-p)) ;; pyim-probe-dynamic-english 和 pyim-probe-auto-english 二选一

      ;;根据环境自动切换到半角标点输入模式
      (setq-default pyim-punctuation-half-width-functions
                    '(pyim-probe-punctuation-line-beginning
                      pyim-probe-punctuation-after-punctuation))
      
      ;; 开启拼音搜索功能
      ;; (pyim-isearch-mode 1)

      ;; 使用 pupup-el 来绘制选词框, 如果用 emacs26, 建议设置
      ;; 为 'posframe, 速度很快并且菜单不会变形，不过需要用户
      ;; 手动安装 posframe 包。
      (if (and (display-graphic-p) (>= emacs-major-version 26))
          (setq pyim-page-tooltip 'posframe)
        (setq pyim-page-tooltip 'popup))

      ;; 选词框显示9个候选词
      (setq pyim-page-length 9)

      (if (and (file-exists-p (expand-file-name "~/.emacs.d/pyim-bigdict.pyim"))
               (file-exists-p (expand-file-name "~/.emacs.d/pyim-mine.pyim")))
          (progn
            ;; 个人词库
            (setq pyim-dicts
                  `((:name "pyim-bigdict" :file ,(expand-file-name "~/.emacs.d/pyim-bigdict.pyim"))
                    (:name "pyim-mine" :file ,(expand-file-name "~/.emacs.d/pyim-mine.pyim"))))
            ;; (pyim-import "~/.emacs.d/pyim-mine.cipin")
            ;; 让 Emacs 启动时自动加载 pyim 词库
            (add-hook 'emacs-startup-hook #'(lambda () (pyim-restart-1 t)))
            )
        (message "你的pyim词库文件没有找到，请先去安装"))
      ;; (add-hook 'kill-emacs-hook (lambda ()
      ;;                              (pyim-export "~/.emacs.d/pyim-mine.cipin")
      ;;                              (pyim-export-personal-words "~/.emacs.d/pyim-mine.pyim")))

      ;; 如何使用其它字符翻页
      (define-key pyim-mode-map "." 'pyim-page-next-page)
      (define-key pyim-mode-map "," 'pyim-page-previous-page)

      ;; (general-define-key
      ;;  :keymaps 'insert
      ;;  ;; :prefix "C-i"
      ;;  ;; 转换前面的英文字符为中文
      ;;  "M-j" 'pyim-convert-string-at-point
      ;;  ;; 使用C-i或者C-\来进行中英文输入法切换
      ;;  "C-i" 'pyim-toggle-input-ascii)
      ;; (general-define-key
      ;;  :jump t
      ;;  :keymaps 'insert
      ;;  :predicate '(not (equal major-mode term-mode))
      ;;  :prefix "C-i"
      ;;  ;; 转换前面的英文字符为中文
      ;;  "C-i" 'pyim-convert-string-at-point
      ;;  ;; 使用C-i或者C-\来进行中英文输入法切换
      ;;  ;; "C-i" 'pyim-toggle-input-ascii
      ;;  )


      ;; {{{
      ;; 当前没有输入内容的时候直接使用evil-escape的按键（；g）的直接返回到normal模式
      (defun my-pyim-self-insert-command (orig-func key)
        (let ((fkey (elt meow-two-char-escape-sequence 0))
              (skey (elt meow-two-char-escape-sequence 1))
              (last-char (if (and (local-variable-p 'my-last-char) (numberp my-last-char))
                             my-last-char
                           nil)))
          (set (make-local-variable 'my-last-char) key)
          (if (char-equal key fkey)
              (progn
                (set (make-local-variable 'last-event-time) (float-time))
                (funcall orig-func key))
            (progn
              (if (and (numberp last-char)
                       (char-equal last-char fkey)
                       (char-equal key skey)
                       (and (local-variable-p 'last-event-time)
                            (floatp last-event-time)
                            (< (- (float-time) last-event-time) meow-two-char-escape-delay)))
                  (progn
                    (backward-delete-char 1)
                    ;; (toggle-input-method)
                    (meow-normal-mode))
                (if (numberp key)
                    (funcall orig-func key)
                  (setq unread-command-events (append unread-command-events (list key))))))))
        )
      (advice-add 'pyim-input-method :around 'my-pyim-self-insert-command)
      ;; (advice-remove 'pyim-input-method 'my-pyim-self-insert-command)
      ;; }}}
      )

    (use-package rime
      ;; pacman -S librime
      :if (equal system-type 'gnu/linux)
      :init
      (defun my-rime-predicate-in-doc-string-p ()
        "Whether point is in the doc string."
        (eq (plist-get (text-properties-at (point)) 'face) 'font-lock-doc-face))
      :custom
      (rime-show-candidate 'posframe)
      (rime-disable-predicates '(my-rime-predicate-in-doc-string-p
                                 rime-predicate-in-code-string-p
                                 rime-predicate-in-code-string-after-ascii-p
                                 meow-normal-mode-p
                                 rime-predicate-prog-in-code-p
                                 rime-predicate-hydra-p
                                 rime-predicate-space-after-cc-p
                                 rime-predicate-org-in-src-block-p
                                 rime-predicate-punctuation-line-begin-p
                                 ;; rime-predicate-current-input-punctuation-p
                                 rime-predicate-punctuation-after-ascii-p
                                 rime-predicate-after-alphabet-char-p
                                 rime-predicate-current-uppercase-letter-p
                                 ))
      ;; (rime-inline-predicates '(rime-predicate-space-after-cc-p))
      :bind
      (:map rime-mode-map
            ("C-`" . 'rime-send-keybinding)
            ("M-j" . 'rime-force-enable))
      (:map rime-active-mode-map
            ("M-j" . 'rime-inline-ascii))
      :config
      (message "rime xxxxxx")
      ;; (setq default-input-method "rime")
      (defun my-chinese-setup ()
        "Set up my private Chinese environment."
        (setq default-input-method "rime"))
      (add-hook 'set-language-environment-hook 'my-chinese-setup)

      ;; support shift-l, shift-r, control-l, control-r
      (setq rime-inline-ascii-trigger 'shift-l)

      ;; {{{
      (defun my-rime-self-insert-command (orig-func key)
        (let ((fkey (elt meow-two-char-escape-sequence 0))
              (skey (elt meow-two-char-escape-sequence 1))
              (last-char (if (and (local-variable-p 'my-last-char) (numberp my-last-char))
                             my-last-char
                           nil)))
          (set (make-local-variable 'my-last-char) key)
          (if (char-equal key fkey)
              (progn
                (set (make-local-variable 'last-event-time) (float-time))
                (funcall orig-func key))
            (progn
              (if (and (numberp last-char)
                       (char-equal last-char fkey)
                       (char-equal key skey)
                       (and (local-variable-p 'last-event-time)
                            (floatp last-event-time)
                            (< (- (float-time) last-event-time) meow-two-char-escape-delay)))
                  (progn
                    (backward-delete-char 1)
                    ;; (toggle-input-method)
                    (meow-normal-mode))
                (if (numberp key)
                    (funcall orig-func key)
                  (setq unread-command-events (append unread-command-events (list key))))))))
        )
      (advice-add 'rime-input-method :around 'my-rime-self-insert-command)
      ;; (advice-remove 'rime-input-method 'my-rime-self-insert-command)
      ;; }}}
      )))
(advice-add #'toggle-input-method :before #'my-pyim/rime-toggle-input-method-advice)

;; 使用 help `pyim-activate', 判断变量 input-method
;; 1. evil-insert entry hook
;; 2. addvice C-\


(provide 'init-input)
;;; init-input.el ends here
