;; -*- coding: utf-8; lexical-binding: t; -*-
(when (>= emacs-major-version 24)
  (require 'package)
  ;; (setq package-archives '(("gnu"   . "https://elpa.emacs-china.org/gnu/")
  ;;                         ("melpa" . "https://elpa.emacs-china.org/melpa/"))))
  (setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                           ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
  ;; (setq package-archives '(("gnu"   . "http://mirrors.cloud.tencent.com/elpa/gnu/")
  ;;                        ("melpa" . "http://mirrors.cloud.tencent.com/elpa/melpa/")))
  ;; (setq package-archives '(("gnu" . "http://mirrors.ustc.edu.cn/elpa/gnu/")
  ;;                          ("melpa" . "http://mirrors.ustc.edu.cn/elpa/melpa/")))

  ;; (add-to-list 'package-archives '("myelpa" . "https://github.com/fwar34/myelpa/"))
  )

;; 使用本地的备份直接打开这个注释
;; myelpa is the ONLY repository now, dont forget trailing slash in the directory
;; (setq package-archives '(("myelpa" . "~/.myelpa/")))

(require 'cl)

;; (defun require-package (package)
;;   "refresh package archives, check package presence and install if it's not installed"
;;   (if (null (require package nil t))
;;       (progn (let* ((ARCHIVES (if (null package-archive-contents)
;;                                   (progn (package-refresh-contents)
;;                                          package-archive-contents)
;;                                 package-archive-contents))
;;                     (AVAIL (assoc package ARCHIVES)))
;;                (if AVAIL
;;                    (package-install package)))
;;              (require package))))

;; ;; use-package
;; (require-package 'use-package)

;; Initialize packages
;; (unless (bound-and-true-p package--initialized) ; To avoid warnings in 27
;;   (setq package-enable-at-startup nil)          ; To prevent initializing twice
;;   (package-initialize))

;; Setup `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
;; (setq use-package-always-ensure t)

;; install manually
;; C-x C-f ~/gnu-elpa-keyring-update-2019.3.tar
;; M-x package-install-from-buffer
;; http://elpa.gnu.org/packages/gnu-elpa-keyring-update.html
(use-package gnu-elpa-keyring-update
  :ensure t)

;; straight.el
;; https://github.com/raxod502/straight.el#integration-with-use-package
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; chords
(use-package use-package-chords
  :ensure t
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Theme
(use-package monokai-theme
  :unless (display-graphic-p)
  :custom-face
  ;; 可以使用counsel-describe-face来查找face
  (org-block ((t (:extend t))))
  (org-block-begin-line ((t (:extend t))))
  (org-block-end-line ((t (:extend t))))
  :ensure t
  :config
  (if (and (equal system-type 'windows-nt) (> emacs-major-version 24))
      (add-hook 'window-setup-hook '(lambda () (load-theme 'monokai t)))
    (add-hook 'after-init-hook '(lambda () (load-theme 'monokai t))))
  (setq monokai-height-minus-1 0.8
        monokai-height-plus-1 1.0
        monokai-height-plus-2 1.0
        monokai-height-plus-3 1.0
        monokai-height-plus-4 1.0)
  ;; If you would like to use variable-pitch-mode you can enable it with:
  (setq monokai-user-variable-pitch t)
  ;; (custom-set-faces '(hl-line ((t (:extend t)))))
  )

;; (use-package monokai-pro-theme
;;   :disabled
;;   :ensure t
;;   :config
;;   (load-theme 'monokai-pro t)
;;   )

;; (use-package darkokai-theme
;;   :disabled
;;   :ensure t
;;   :config
;;   (load-theme 'darkokai t)
;;   )

(use-package zenburn-theme
  :if (display-graphic-p)
  :ensure t
  :config
  (load-theme 'zenburn t)
  )

;; (use-package doom-themes
;;   :disabled
;;   :ensure t
;;   :config
;;   ;; (load-theme 'doom-theme t)
;;   )

;; (use-package monokai-alt-theme
;;   :disabled
;;   :ensure t
;;   :config
;;   )

;; (use-package color-theme-almost-monokai
;;   :disabled
;;   :load-path "lisp"
;;   :config
;;   (color-theme-almost-monokai)
;;   )

;; (use-package color-theme-molokai
;;   :straight
;;   (:host github :repo "alloy-d/color-theme-molokai")
;;   :config
;;   (color-theme-molokai)
;;   )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (and (display-graphic-p) (>= emacs-major-version 26))
  (use-package posframe :ensure t))

(use-package pyim
  :ensure t
  :if (and (display-graphic-p) (string-equal "A12969" system-name) (not (equal system-type 'windows-nt)))
  :demand t
  :config
  ;; 激活 basedict 拼音词库，五笔用户请继续阅读 README
  (use-package pyim-basedict
    :if window-system
    :ensure t
    :config (pyim-basedict-enable))

  (setq default-input-method "pyim")

  ;; 我使用全拼
  ;; (setq pyim-default-scheme 'quanpin)
  ;; (setq pyim-default-scheme 'pyim-shuangpin)
  (setq pyim-default-scheme 'xiaohe-shuangpin)

  ;; 设置 pyim 探针设置，这是 pyim 高级功能设置，可以实现 *无痛* 中英文切换 :-)
  ;; 我自己使用的中英文动态切换规则是：
  ;; 1. 光标只有在注释里面时，才可以输入中文。
  ;; 2. 光标前是汉字字符时，才能输入中文。
  ;; 3. 使用 M-j 快捷键，强制将光标前的拼音字符串转换为中文。
  (setq-default pyim-english-input-switch-functions
                '(;; pyim-probe-dynamic-english
                  ;; pyim-probe-auto-english
                  pyim-probe-isearch-mode
                  pyim-probe-program-mode
                  pyim-probe-org-structure-template))

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

  ;; 选词框显示5个候选词
  (setq pyim-page-length 7)

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
  (general-define-key
   :keymaps 'insert
   :prefix "C-i"
   ;; 转换前面的英文字符为中文
   "C-i" 'pyim-convert-string-at-point
   ;; 使用C-i或者C-\来进行中英文输入法切换
   ;; "C-i" 'pyim-toggle-input-ascii
   )
  ) 

;; https://github.com/emacs-evil/evil-collection
;; evil
(use-package evil
  :ensure t
  :hook
  (after-init . evil-mode)
  :init
  ;; (setq evil-want-keybinding nil) must put before load evil
  ;; See https://github.com/emacs-evil/evil-collection/issues/60 for more details.
  (setq evil-want-keybinding nil)
  ;; disable status in echo area
  (setq evil-echo-state nil)
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-C-i-jump t)
  (setq evil-want-fine-undo "Yes")
  (setq evil-want-Y-yank-to-eol t)
  ;; (setq evil-no-display t)              ;; not display evil state in echo area
  :bind
  (:map isearch-mode-map
        ("<up>" . 'isearch-ring-retreat)
        ("<down>" . isearch-ring-advance))
  (:map evil-normal-state-map
        ("C-a" . evil-first-non-blank)
        ("C-e" . evil-end-of-line))
  :config
  ;; 设置光标样式
  (setq evil-motion-state-cursor 'box)  ; 
  (setq evil-visual-state-cursor 'box)  ; 
  (setq evil-normal-state-cursor 'box)  ; 
  (setq evil-insert-state-cursor 'bar)  ; 
  (setq evil-emacs-state-cursor  'hbar) ; _
  (setq evil-insert-state-cursor '((hbar . 5) "yellow")
        evil-normal-state-cursor '(box "purple"))

  (define-key evil-ex-search-keymap (kbd ";g") #'keyboard-quit)
  ;; for quit shell-command output buffer
  ;; (defun my-quit-window (&rest _)
  ;;   (with-current-buffer "*Shell Command Output*"
  ;;     (evil-local-set-key 'normal (kbd "q") #'quit-window)))
  ;; (advice-add 'shell-command :after #'my-quit-window)
  ;; (defadvice shell-command (after advice-find-file activate)
  ;;   (with-current-buffer "*Shell Command Output*"
  ;;     ;; (evil-local-set-key 'normal (kbd "q") #'quit-window)))
  ;;     (evil-local-set-key 'normal (kbd "q") #'kill-this-buffer)))
  (evil-ex-define-cmd "q[uit]" 'kill-this-buffer)

  ;; https://emacs.stackexchange.com/questions/31334/history-of-search-terms-for-evil-mode
  ;; (custom-set-variables '(evil-search-module 'evil-search))

  ;; (when (< emacs-major-version 28)
  ;;   (use-package undo-fu :ensure t))

  ;; https://emacs-china.org/t/customize-evil-undo-system-for-redo-functionality/14969/3
  (when (fboundp 'evil-set-undo-system)
    (evil-set-undo-system (if (>= emacs-major-version 28) 'undo-redo 'undo-tree))
    ;; (evil-set-undo-system (if (>= emacs-major-version 28) 'undo-redo 'undo-fu))
    )

  (use-package avy :ensure t)

  :custom
  ;; undo will never freeze my Emacs
  ;; (evil-undo-system (if (>= emacs-major-version 28) 'undo-redo 'undo-fu))
  ;; https://emacs-china.org/t/evil-insert-state-or-evil-emacs-state/16710/6?u=fwar34
  (evil-ex-interactive-search-highlight 'selected-window)
  (evil-want-C-g-bindings t)
  )

(use-package undo-tree
  :ensure t
  :hook
  (after-init . global-undo-tree-mode))

(use-package goto-chg
  ;; Goto Last Change
  ;; Goto the point of the most recent edit in the buffer.
  :after evil
  :ensure t
  )

;; evil-collection
(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init)
  )

(use-package hungry-delete
  :ensure t
  :hook
  (evil-mode . global-hungry-delete-mode)
  )

(use-package expand-region
  :ensure t
  :defer t
  )

(use-package counsel
  ;; counsel repository contains:
  ;; Ivy, a generic completion mechanism for Emacs.
  ;; Counsel, a collection of Ivy-enhanced versions of common Emacs commands.
  ;; Swiper, an Ivy-enhanced alternative to isearch.
  :ensure t
  :bind
  (([remap switch-to-buffer] . ivy-switch-buffer)
   ([remap isearch-forward] . swiper)
   ([remap describe-function] . counsel-describe-function)
   ([remap describe-variable] . counsel-describe-variable)
   ([remap describe-symbol] . counsel-describe-symbol)
   ("M-x" . counsel-M-x)
   :map ivy-minibuffer-map
   ("M-l" . ivy-restrict-to-matches))
  :config
  (general-define-key
   :states 'emacs
   :keymaps 'ivy-occur-grep-mode-map
   "q" 'quit-window
   "gs" 'evil-avy-goto-char
   "gw" 'ivy-wgrep-change-to-wgrep-mode
   "n" 'ivy-occur-next-error
   )

  ;; 调整 counsel 搜索的方式: 忽略单词顺序
  (setq ivy-re-builders-alist
        '((counsel-rg . ivy--regex-plus)
          (swiper . ivy--regex-plus)
          (swiper-isearch . ivy--regex-plus)
          (t . ivy--regex-ignore-order)))

  (setq ivy-initial-inputs-alist nil
        ivy-wrap t
        ivy-height 15
        ivy-fixed-height-minibuffer t
        ivy-format-function #'ivy-format-function-line
        ivy-use-virtual-buffers t
        ivy-count-format "%d/%d ")
  (ivy-mode 1) ;; M-j ivy-yank-word，将光标的word读入minibuffer，很像vim中的功能
  ;; C-y yank，可以在minibuffer中粘贴
  ;; 默认就是fancy
  ;; (setq ivy-display-style 'fancy)
  ;; (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)
  (setq enable-recursive-minibuffers t)
  ;; (define-key ivy-minibuffer-map (kbd "C-i") 'counsel-evil-registers)
  ;; (define-key isearch-mode-map (kbd "C-i") 'counsel-evil-registers)
  ;; (define-key isearch-mode-map (kbd "C-n") 'ivy-next-line)
  ;; (define-key isearch-mode-map (kbd "C-p") 'ivy-previous-line)
  ;; (define-key ivy-minibuffer-map (kbd "C-r") 'counsel-minibuffer-history)
  ;; (global-set-key (kbd "C-h f") #'counsel-describe-function)
  ;; (global-set-key (kbd "C-h v") #'counsel-describe-variable)

  ;; (setq counsel-fzf-cmd "fd -I --exclude={site-lisp,etc/snippets,themes,/eln-cache,/var,/elpa,quelpa/,/url,/auto-save-list,.cache,doc/} --type f | fzf -f \"%s\" --algo=v1")

  ;; 默认的rg配置
  ;; (setq counsel-rg-base-command "rg -M 240 --with-filename --no-heading --line-number --color never %s")
  (setq counsel-rg-base-command '("rg"
                                  ;; "--max-columns" "240"
                                  "--with-filename" "--no-heading" "--line-number" "--color"
                                  "never" "%s"
                                  "--path-separator"
                                  "/"
                                  "--iglob" "!tags"
                                  "--iglob" "!makefile"
                                  "--iglob" "!makefile.*"
                                  "--iglob" "!*.lo"
                                  "."))
  ;; (setq counsel-rg-base-command '("rg"
  ;;                                 "-M" "240"
  ;;                                 "--with-filename" "--no-heading" "--line-number" "--color"
  ;;                                 "never" "%s"
  ;;                                 "-g" "!package-config.org"
  ;;                                 "-g" "!TAGS"
  ;;                                 "-g" "!tags"
  ;;                                 "-g" "!site-lisp/**"
  ;;                                 "-g" "!doc/**"
  ;;                                 "-g" "!themes/**"
  ;;                                 "-g" "!mysnippets/**"
  ;;                                 "-g" "!debian/**"
  ;;                                 "-g" "!auxdir/**"
  ;;                                 "-g" "!m4/**"
  ;;                                 ))

  ;; (setq counsel-ag-base-command '("ag"
  ;;                                 "--vimgrep"
  ;;                                 "%s"
  ;;                                 "--smart-case"
  ;;                                 "--ignore" "tags"
  ;;                                 "--ignore" "TAGS"
  ;;                                 "--ignore" "Makefile.*"
  ;;                                 "--ignore" "Makefile"
  ;;                                 "--ignore" "*.lo"))

  ;; https://emacs-china.org/t/emacs-helm-ag/6764
  ;; 支持中文搜索，但是只有两个汉字以上才能搜索到结果，还不清楚原因
  ;; (when (equal system-type 'windows-nt)
  ;; win10如果默认改成了utf8编码则不需要底下这个配置
  ;;   (modify-coding-system-alist 'process "ag" '(utf-8 . chinese-gbk-dos))
  ;;   (modify-coding-system-alist 'process "rg" '(utf-8 . chinese-gbk-dos)))

  (general-define-key
   :keymaps 'ivy-minibuffer-map
   :prefix ","
   "," 'self-insert-command
   "s" 'ivy-restrict-to-matches
   "d" 'swiper-avy
   "c" 'ivy-occur
   "a" 'ivy-beginning-of-buffer
   "e" 'ivy-end-of-buffer
   )
  

  (use-package ivy-posframe
    :disabled
    :ensure t
    :if (display-graphic-p)
    :config
    ;; The following example displays swiper on 20 lines by default for ivy,
    ;; and displays other functions in posframe at the location specified on 40 lines.
    ;; (setq ivy-posframe-height-alist '((swiper . 20)
    ;;                                   (t      . 40)))

    ;; How to show fringe to ivy-posframe
    (setq ivy-posframe-parameters
          '((left-fringe . 8)
            (right-fringe . 8)))

    ;; Per-command mode.
    ;; Different command can use different display function.
    (setq ivy-posframe-display-functions-alist
          '((swiper          . ivy-display-function-fallback)
            (complete-symbol . ivy-posframe-display)
            (counsel-M-x     . ivy-posframe-display-at-window-center)
            (t               . ivy-posframe-display)))
    (ivy-posframe-mode 1)
    )

  (use-package amx
    :ensure t
    :defer t
    :config
    (amx-mode)
    )

  (use-package ivy-xref
    :ensure t
    :defer t
    :init
    ;; xref initialization is different in Emacs 27 - there are two different
    ;; variables which can be set rather than just one
    (when (>= emacs-major-version 27)
      (setq xref-show-definitions-function #'ivy-xref-show-defs))
    ;; Necessary in Emacs <27. In Emacs 27 it will affect all xref-based
    ;; commands other than xref-find-definitions (e.g. project-find-regexp)
    ;; as well
    (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

  ;; {{{
  ;; https://emacs-china.org/t/ivy-occur/12083
  (defvar ivy-occur-filter-prefix ">>> ")

;;;###autoload
  (defun ivy-occur/filter-lines ()
    (interactive)
    (unless (string-prefix-p "ivy-occur" (symbol-name major-mode))
      (user-error "Current buffer is not in ivy-occur mode"))

    (let ((inhibit-read-only t)
          (regexp (read-regexp "Regexp(! for flush)"))
          (start (save-excursion
                   (goto-char (point-min))
                   (re-search-forward "[0-9]+ candidates:"))))
      (if (string-prefix-p "!" regexp)
          (flush-lines (substring regexp 1) start (point-max))
        (keep-lines regexp start (point-max)))
      (save-excursion
        (goto-char (point-min))
        (let ((item (propertize (format "[%s]" regexp) 'face 'ivy-current-match)))
          (if (looking-at ivy-occur-filter-prefix)
              (progn
                (goto-char (line-end-position))
                (insert item))
            (insert ivy-occur-filter-prefix item "\n"))))))

;;;###autoload
  (defun ivy-occur/undo ()
    (interactive)
    (let ((inhibit-read-only t))
      (if (save-excursion
            (goto-char (point-min))
            (looking-at ivy-occur-filter-prefix))
          (undo)
        (user-error "Filter stack is empty"))))

  (defun ivy|occur-mode-setup ()
    (local-set-key "/" #'ivy-occur/filter-lines)
    (local-set-key (kbd "M-/") #'ivy-occur/undo))

  (add-hook 'ivy-occur-mode-hook 'ivy|occur-mode-setup)
  (add-hook 'ivy-occur-grep-mode-hook 'ivy|occur-mode-setup)
  ;; }}}
  )

(use-package ivy-rich
  :ensure t
  :after ivy
  :config
  (ivy-rich-mode 1)
  (setq ivy-format-function #'ivy-format-function-line)
  ;; To abbreviate paths using abbreviate-file-name (e.g. replace “/home/username” with “~”)
  (setq ivy-rich-path-style 'abbrev)
  )

(use-package wgrep
  :ensure t
  :defer t
  :config
  (general-define-key
   :states 'emacs
   :keymaps 'wgrep-mode-map
   :prefix ","
   "qq" 'wgrep-abort-changes
   "zz" 'wgrep-finish-edit
   "gs" 'evil-avy-goto-char
   "," 'self-insert-command
   )
  )



;; smartparens setting
(use-package smartparens
  :disabled
  :ensure t
  :hook
  (after-init . smartparens-mode)
  :config
  (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
  (sp-local-pair 'lisp-interaction-mode "'" nil :actions nil)

  )

;; js2-mode setting
(use-package js2-mode
  :ensure t
  :after js2-mode
  :preface
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  )

(use-package web-mode
  :ensure t
  :preface
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  :after web-mode
  :config
  (define-key web-mode-map (kbd "C-n") 'web-mode-tag-match)
  )

;; (use-package emmet-mode
;;   :ensure t
;;   :hook
;;   ((sgml-mdoe . emmet-mode)    ;; Auto-start on any markup modes
;;    (html-mode . emmet-mode)    ;; enable Emmet's css abbreviation.
;;    (web-mode . emmet-mode)
;;    (css-mode . emmet-mode))
;;   )

;; popwin setting
(use-package popwin
  :ensure t
  :hook
  (evil-mode . popwin-mode)
  )

(use-package winum
  ;; Navigate windows and frames using numbers.
  :ensure t
  :hook
  (evil-mode . winum-mode)
  :config
  (setq winum-auto-setup-mode-line nil)
  )

;; (use-package powershell
;;   :ensure t
;;   :if (equal system-type 'windows-nt)
;;   :defer t
;;   )


(use-package fzf
  :ensure t
  :unless (equal system-type 'windows-nt)
  :defer t
  )

(use-package magit
  :ensure t
  :commands magit
  :config
  ;; https://www.helplib.com/GitHub/article_131559
  ;; (evil-define-key evil-magit-state magit-mode-map "?"'evil-search-backward)
  )

;; yasnippet setting
(use-package yasnippet
  :ensure t
  :config
  (run-with-idle-timer 0.5 nil #'yas-global-mode)
  (setq yas-snippet-dirs '("~/.emacs.d/mysnippets"))

  (use-package ivy-yasnippet
    :ensure t
    :after yasnippet
    :config
    (setq ivy-yasnippet-expand-keys nil)
    )
  )

(use-package youdao-dictionary
  :ensure t
  :commands
  (youdao-dictionary-search-at-point
   youdao-dictionary-search-at-point+
   youdao-dictionary-search-from-input)
  :config
  ;; Enable Cache
  (setq url-automatic-caching t)
  ;; Integrate with popwin-el (https://github.com/m2ym/popwin-el)
  (push "*Youdao Dictionary*" popwin:special-display-config)
  ;; Set file path for saving search history
  (setq youdao-dictionary-search-history-file "~/.emacs.d/.youdao")
  ;; Enable Chinese word segmentation support (支持中文分词)
  (setq youdao-dictionary-use-chinese-word-segmentation t)

  ;; press 'q' to quit youdao output buffer
  ;; (defun my-quit-window (&rest _)
  ;;   (with-current-buffer "*Youdao Dictionary*"
  ;;     (evil-local-set-key 'normal (kbd "q") #'quit-window)))
  ;; (advice-add 'youdao-dictionary-search-at-point :after #'my-quit-window)
  ;; (defadvice youdao-dictionary-search-at-point (after advice-youdao-point activate)
  ;;   (with-current-buffer "*Youdao Dictionary*"
  ;;     (evil-local-set-key 'normal (kbd "q") #'quit-window)))
  (evil-define-key 'normal youdao-dictionary-mode-map "q" #'kill-this-buffer)
  ;; (add-hook 'youdao-dictionary-mode-hook
  ;;           (lambda ()
  ;;             (define-key evil-normal-state-local-map (kbd "q") 'quit-window)))
  )

(use-package go-translate
  :disabled
  :ensure t
  :custom
  (go-translate-base-url "https://translate.google.cn")
  (go-translate-local-language "zh-CN")
  (go-translate-buffer-follow-p t) ;;翻译完成后总是将光标切换到翻译窗口
  :config
  ;; https://emacs-china.org/t/google/14407/48?u=fwar34
  (setq go-translate-token-current (cons 430675 2721866130))
  )

(use-package go-mode
  :ensure t
  :preface
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))
  :after go-mode
  :config
  (autoload 'go-mode "go-mode" nil t)
  )

(use-package rust-mode
  :ensure t
  :preface
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
  :after rust-mode
  :config
  (autoload 'rust-mode "rust-mode" nil t)
  ;; The rust-format-buffer function will format your code with rustfmt if installed.
  ;; By default, this is bound to C-c C-f.
  ;; Placing (setq rust-format-on-save t) in your ~/.emacs will enable automatic
  ;; running of rust-format-buffer when you save a buffer.
  ;; (setq rust-format-on-save t)
  )

(use-package which-key
  :ensure t
  :hook
  (evil-mode . which-key-mode)
  :init
  (setq which-key-allow-imprecise-window-fit t) ; performance
  (setq which-key-separator ":")
  (setq which-key-show-docstrings t)
  (which-key-setup-minibuffer)
  )

(use-package projectile
  :ensure t
  :hook
  (evil-mode . projectile-mode)
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c C-p") 'projectile-command-map)
  (setq projectile-completion-system 'ivy)
  )

;; https://www.emacswiki.org/emacs/NeoTree
(use-package neotree
  :ensure t
  :defer t
  :config
  ;; Note: For users who want to use the icons theme. Pls make sure you have
  ;; installed the all-the-icons package and its fonts.
  ;; (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  ;; (setq neo-theme (when (display-graphic-p) 'icons))
  (setq neo-theme 'arrow)
  ;; Every time when the neotree window is opened, let it find current file and jump to node.
  (setq neo-smart-open t)
  )

(use-package all-the-icons
  :disabled
  :ensure t
  :defer t
  :if (display-graphic-p)
  )

;; (use-package lispy
;;   :ensure t
;;   :disabled
;;   :hook
;;   (emacs-lisp-mode . lispy-mode)
;;   :config
;;   ;; (define-key lispy-mode-map (kbd “<delete>”) #'lispy-delete)
;;   ;; (define-key lispy-mode-map (kbd “C-d”) #'lispy-delete-backward)
;;   ;; (define-key lispy-mode-map (kbd “C-k”) #'lispy-kill)
;;   ;; (define-key lispy-mode-map (kbd “C-y”) #'lispy-yank)
;;   ;; (define-key lispy-mode-map (kbd “C-e”) #'lispy-move-end-of-line) 
;;   )

;; https://github.com/noctuid/lispyville
(use-package lispyville
  :ensure t
  ;; :disabled
  ;; :hook
  ;; lispyvill used with lispy
  ;; (lispy-mode . lispyville-mode)

  ;; Lispyville can also be used without lispy:
  :hook
  (emacs-lisp-mode . lispyville-mode)
  (lisp-mode . lispyville-mode)
  :config
  (lispyville-set-key-theme
   ;; his is probably the simplest method of improving things. By default,
   ;; pressing escape after using something like lispy-mark from special will
   ;; enter normal state but won’t cancel the region. Lispyville provides lispyville-normal-state
   ;; to deactivate the region and enter normal state in one step. You can map it manually or
   ;; use the escape key theme (e.g. (lispyville-set-key-theme '(... (escape insert emacs)))).
   ;; '((escape insert emacs) 
   ;;   additional-movement prettify atom-motions slurp/barf-cp additional additional-wrap))
   '((escape insert emacs) 
     additional-movement slurp/barf-cp additional commentary text-objects wrap)))

;; linum-relative
;; (use-package linum-relative
;;   ;; emacs26 builtin
;;   :disabled
;;   :config
;;   ;; (linum-relative-toggle)
;;   )

(use-package rainbow-delimiters
  :ensure t
  :hook
  (prog-mode . rainbow-delimiters-mode)
  )

;; beacon
(use-package beacon
  :ensure t
  :hook
  (evil-mode . beacon-mode)
  )

;; end https://github.com/jojojames/evil-collection
;; (require 'evil-leader)
(use-package evil-escape
  :ensure t
  :after evil
  )

(use-package evil-surround
  :ensure t
  :after evil
  )

(use-package evil-nerd-commenter
  :ensure t
  :after evil
  :config
  ;; must put before (evilnc-default-hotkeys t t)
  (setq evilnc-use-comment-object-setup nil)
  (evilnc-default-hotkeys t t)
  )

(use-package evil-easymotion
  :ensure t
  :after evil
  :config
  (evilem-default-keybindings (kbd "-"))
  ;; (evilem-default-keybindings (kbd "\\"))
  )

(use-package evil-matchit
  :ensure t
  :after evil
  :config
  (global-evil-matchit-mode 1)
  )

(use-package evil-exchange
  :ensure t
  :after evil
  :config
  ;; change default key bindings (if you want) HERE
  ;; (setq evil-exchange-key (kbd "zx"))
  (evil-exchange-install)
  )

;; {{ @see https://github.com/syl20bnr/spacemacs/blob/master/doc/DOCUMENTATION.org#replacing-text-with-iedit
;; same keybindgs as spacemacs:
;;  - "SPC s e" to start `iedit-mode'
;;  - "TAB" to toggle current occurrence
;;  - "n" next, "N" previous (obviously we use "p" for yank)
;;  - "gg" the first occurence, "G" the last occurence
;;  - Please note ";;" or `avy-goto-char-timer' is also useful
(use-package evil-iedit-state
  :ensure t
  :after evil
  )

(use-package general
  :ensure t
  :after evil
  :config
  (general-evil-setup t)
  )


(use-package highlight-symbol
  :ensure t
  :defer t
  )

(use-package transient
  :ensure t)

;; https://github.com/wolray/symbol-overlay/
;; https://github.com/wolray/symbol-overlay/issues/59
(use-package symbol-overlay
  ;;默认n,p,i,q在高亮的地方点击为下一个，上一个，取消所有的高亮，替换
  :ensure t
  :after transient
  :commands symbol-overlay
  :config
  (transient-define-prefix symbol-overlay-transient ()
    "Symbol Overlay transient"
    ["Symbol Overlay"
     ["Overlays"
      ("." "Add/Remove at point" symbol-overlay-put)
      ("k" "Remove All" symbol-overlay-remove-all)
      ]
     ["Move to Symbol"
      ("n" "Next" symbol-overlay-switch-forward)
      ("p" "Previous" symbol-overlay-switch-backward)
      ]
     ["Other"
      ("m" "Hightlight symbol-at-point" symbol-overlay-mode)
      ]
     ]
    )
  ;; Or you may prefer to overwrite the keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "r") 'symbol-overlay-query-replace)
    (define-key map (kbd "x") 'symbol-overlay-remove-all)
    (define-key map (kbd "n") 'symbol-overlay-jump-next)
    (define-key map (kbd "p") 'symbol-overlay-jump-prev)
    (setq symbol-overlay-map map))
  )

;; rainbow-mode
(use-package rainbow-mode
  :ensure t
  :after evil
  :config
  (rainbow-mode 1)
  )

(use-package fix-word
  :ensure t
  :after evil
  )

(use-package browse-kill-ring
  :ensure t
  :defer t
  :config
  (evil-define-key 'normal browse-kill-ring-mode-map "q"
    #'browse-kill-ring-quit)
  )

(use-package function-args
  ;; GNU Emacs package for showing an inline arguments hint for the C/C++ function at point
  :disabled
  :ensure t
  :after evil
  :config
  (fa-config-default)
  (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
  (add-to-list 'auto-mode-alist '("\\.hpp\\'" . c++-mode))
  )

(use-package symon
  ;; tiny graphical system monitor 
  ;; https://github.com/zk-phi/symon
  :disabled
  :ensure t
  :after evil
  :config
  (symon-mode)
  )

(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown")
  )

(use-package rainbow-identifiers
  :ensure t
  :after evil
  :hook
  (prog-mode . rainbow-identifiers-mode)
  )

;; highlight-numbers
(use-package highlight-numbers
  :ensure t
  :unless window-system
  :after evil
  :hook
  (prog-mode . highlight-numbers-mode)
  )

;; highlight-quoted
(use-package highlight-quoted
  :ensure t
  :hook
  (emacs-lisp-mode . highlight-quoted-mode)
  )

;; highlight-defined
(use-package highlight-defined
  :ensure t
  :hook
  (emacs-lisp-mode . highlight-defined-mode)
  )

(use-package evil-snipe
  :disabled
  :ensure t
  :after evil
  ;; :hook
  ;; (magit-mode . turn-off-evil-snipe-override-mode)
  :config
  (evil-snipe-mode +1)
  ;; and disable in specific modes
  (push 'dired-mode evil-snipe-disabled-modes)
  (push 'package-menu-mode evil-snipe-disabled-modes)
  (push 'global-mode evil-snipe-disabled-modes)
  ;; To map : to a python function (but only in python-mode):
  (add-hook 'python-mode-hook
            (lambda ()
              (make-variable-buffer-local 'evil-snipe-aliases)
              (push '(?: "def .+:") evil-snipe-aliases)))

  ;; Integration into avy/evil-easymotion
  ;; This will allow you to quickly hop into avy/evil-easymotion right after a snipe.
  (define-key evil-snipe-parent-transient-map (kbd "C-;")
    (evilem-create 'evil-snipe-repeat
                   :bind ((evil-snipe-scope 'buffer)
                          (evil-snipe-enable-highlight)
                          (evil-snipe-enable-incremental-highlight))))

  ;; Evil-snipe can override evil-mode's native motions with 1-char sniping:
  ;; https://github.com/hlissner/evil-snipe
  (evil-snipe-override-mode +1)

  ;; https://github.com/hlissner/evil-snipe#conflicts-with-other-plugins
  ;; It seems evil-snipe-override-mode causes problems in Magit buffers, to fix this:
  (add-hook 'magit-mode-hook 'turn-off-evil-snipe-override-mode)
  )

;; evil-smartparens
(use-package evil-smartparens
  :disabled
  :after evil
  :ensure t
  :hook
  (smartparens-enabled . evil-smartparens-mode)
  ;; :config
  ;; (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode)
  )

;; evil-visualstar
(use-package evil-visualstar
  :ensure t
  :after evil
  :config
  (global-evil-visualstar-mode)
  ;; (setq evil-visualstar/persistent t)
  ;; (custom-set-variables '(evil-visualstar/persistent t))
  )

;; evil-indent-plus
(use-package evil-indent-plus
  :disabled
  :ensure t
  :after evil
  :config
  ;; This is a continuation of evil-indent-textobject. It provides six new text objects to evil based on indentation:
  ;; ii: A block of text with the same or higher indentation.
  ;; ai: The same as ii, plus whitespace.
  ;; iI: A block of text with the same or higher indentation, including the first line above with less indentation.
  ;; aI: The same as iI, plus whitespace.
  ;; iJ: A block of text with the same or higher indentation, including the first line above and below with less indentation.
  ;; aJ: The same as iJ, plus whitespace.
  (define-key evil-inner-text-objects-map "i" 'evil-indent-plus-i-indent)
  (define-key evil-outer-text-objects-map "i" 'evil-indent-plus-a-indent)
  (define-key evil-inner-text-objects-map "I" 'evil-indent-plus-i-indent-up)
  (define-key evil-outer-text-objects-map "I" 'evil-indent-plus-a-indent-up)
  (define-key evil-inner-text-objects-map "J" 'evil-indent-plus-i-indent-up-down)
  (define-key evil-outer-text-objects-map "J" 'evil-indent-plus-a-indent-up-down)
  )

(use-package highlight-parentheses
  :ensure t
  :after evil
  :config
  ;; (add-hook 'prog-mode-hook 'highlight-parentheses-mode)
  (define-globalized-minor-mode global-highlight-parentheses-mode
    highlight-parentheses-mode
    (lambda () (highlight-parentheses-mode t)))
  (global-highlight-parentheses-mode t)
  )

;; imenu-list
(use-package imenu-list
  :ensure t
  :after evil)

(use-package taglist
  :defer t
  :straight
  (:host github :repo "liugang/taglist")
  :config
  (evil-define-key 'normal taglist-mode-map "q" #'kill-this-buffer)
  (evil-define-key 'normal taglist-mode-map "s" #'swiper)
  (evil-define-key 'normal taglist-mode-map (kbd "RET") #'taglist-jump-to-tag)
  ;; (add-hook 'taglist-mode-hook #'read-only-mode)
  )

;; (use-package fringe-helper
;;   ;; helper functions for fringe bitmaps
;;   :ensure t
;;   :init
;;   :delight
;;   :config
;;   )

(use-package git-gutter
  ;; :disabled        
  ;; :bind
  ;; (("SPC c n" . git-gutter:next-hunk)
  ;;  ("SPC c p" . git-gutter:previous-hunk)) 
  ;; :if (not (display-graphic-p))
  :ensure t
  :after evil
  ;; :if (display-graphic-p)
  :config
  ;; If you enable global minor mode
  (global-git-gutter-mode t)
  ;; If you would like to use git-gutter.el and linum-mode
  ;; (unless (display-graphic-p) (git-gutter:linum-setup))
  ;; (git-gutter:linum-setup)
  ;; Use for 'Git'(`git`), 'Mercurial'(`hg`), 'Bazaar'(`bzr`), and 'Subversion'(`svn`) projects
  ;; (custom-set-variables '(git-gutter:handled-backends '(git hg bzr svn)))
  (custom-set-variables '(git-gutter:handled-backends '(git svn)))
  ;; inactivate git-gutter-mode in asm-mode and image-mode
  (custom-set-variables '(git-gutter:disabled-modes '(asm-mode image-mode)))
  ;; Hide gutter when there are no changes if git-gutter:hide-gutter is non-nil. (Default is nil)
  (custom-set-variables '(git-gutter:hide-gutter t))
  ;; If you set git-gutter :update-interval seconds larger than 0,
  ;; git-gutter updates diff information in real-time by idle timer.
  (custom-set-variables '(git-gutter:update-interval 0))
  (custom-set-variables '(git-gutter:visual-line t))

  ;; console not display, because git-gutter has bug in emacs26 no window
  ;; (unless window-system (custom-set-variables '(git-gutter:display-p nil)))
  ;; diff information is updated at hooks in git-gutter:update-hooks.
  (add-to-list 'git-gutter:update-hooks 'focus-in-hook)
  ;; diff information is updated after command in git-gutter:update-commands executed.
  (add-to-list 'git-gutter:update-commands 'other-window)
  ;; (custom-set-variables
  ;;  '(git-gutter:modified-sign "~") ;; two space
  ;;  '(git-gutter:added-sign "++")    ;; multiple character is OK
  ;;  '(git-gutter:deleted-sign "--"))
  ;; (set-face-background 'git-gutter:modified "purple") ;; background color
  ;; (set-face-foreground 'git-gutter:added "green")
  ;; (set-face-foreground 'git-gutter:deleted "red")
  ;; (set-face-background 'git-gutter:modified "purple") ;; background color
  ;; (set-face-background 'git-gutter:added "green")
  ;; (set-face-background 'git-gutter:deleted "red")
  ;; Jump to next/previous hunk
  ;; (define-key evil-normal-state-map (kbd "[ c") 'git-gutter:previous-hunk)
  ;; (define-key evil-normal-state-map (kbd "] c") 'git-gutter:next-hunk)
  ;; (define-key evil-normal-state-map (kbd "] s") 'git-gutter:stage-hunk)

  ;; https://github.com/noctuid/evil-guide
  ;; you could use this to have git-gutter’s commands for navigating hunks save the current location before jumping:
  (evil-add-command-properties #'git-gutter:next-hunk :jump t)
  (evil-add-command-properties #'git-gutter:previous-hunk :jump t)
  )

(use-package git-gutter-fringe
  :ensure t
  :after git-gutter
  :if (display-graphic-p)
  :config
  (set-face-foreground 'git-gutter-fr:modified "purple")
  (set-face-foreground 'git-gutter-fr:added    "green")
  (set-face-foreground 'git-gutter-fr:deleted  "red")
  )

;; (use-package diff-hl
;;   :disabled
;;   :ensure t
;;   :after evil
;;   ;; :if (not (display-graphic-p))
;;   :config
;;   (global-diff-hl-mode)
;;   (diff-hl-margin-mode) 
;;   (advice-add 'svn-status-update-modeline :after #'diff-hl-update)
;;   (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
;;   (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
;;   (evil-define-key 'normal 'magit-mode-map "q" #'kill-buffer-and-window)
;;   )

(use-package ace-popup-menu
  :ensure t
  :after evil
  :config
  (ace-popup-menu-mode 1)
  )

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :after evil
  :ensure t
  :config
  (exec-path-from-shell-initialize)
  )

(use-package volatile-highlights
  :ensure t
  :after evil
  :config
  (volatile-highlights-mode t)
  ;;-----------------------------------------------------------------------------
  ;; Supporting evil-mode.
  ;;-----------------------------------------------------------------------------
  (vhl/define-extension 'evil 'evil-paste-after 'evil-paste-before
                        'evil-paste-pop 'evil-move)
  (vhl/install-extension 'evil)
  ;;-----------------------------------------------------------------------------
  ;; Supporting undo-tree.
  ;;-----------------------------------------------------------------------------
  (vhl/define-extension 'undo-tree 'undo-tree-yank 'undo-tree-move)
  (vhl/install-extension 'undo-tree)
  )

;; (use-package vterm
;;     :ensure t
;; )

;; (use-package vterm-toggle
;;     :ensure t
;;     :config
;;     (global-set-key [f2] 'vterm-toggle)
;; )

;; (use-package terminal-toggle
;;     :ensure t
;;     :config
;; )

(use-package multi-term
  :ensure t
  :after evil
  :config
  (cond
   ((equal system-type 'windows-nt) (setq multi-term-program "eshell"))
   ((equal system-type 'gnu/linux) (setq multi-term-program "/usr/bin/zsh")))

  ;; (define-key term-mode-map "\e\C-l" 'evil-buffer)
  ;; (define-key term-raw-map ";bb" 'evil-buffer)

  ;; (cl-dolist (element term-bind-key-alist)
  ;;   (setq bind-key (car element))
  ;;   (setq bind-command (cdr element))
  ;;   (cond
  ;;    ((stringp bind-key) (setq bind-key (read-kbd-macro bind-key)))
  ;;    ((vectorp bind-key) nil)
  ;;    (t (signal 'wrong-type-argument (list 'array bind-key))))
  ;;   (define-key term-raw-map bind-key bind-command))

  ;; https://www.jianshu.com/p/2c1ac913d2cb
  ;; 如果想保留自己在其他mode下的快捷键，将快捷键添加到 term-bind-key-alist这个列表中
  (add-to-list 'term-bind-key-alist '("M-l" . evil-buffer))
  (add-to-list 'term-bind-key-alist '("M-y" . term-paste))
  (add-to-list 'term-bind-key-alist '("C-x C-x" . (lambda () (interactive) (term-send-raw-string "\C-x"))))
  ;; 修改快捷键的map,如果你发你定义自己的快捷键与该major-mode的冲突，可以直接修改它的key-map
  ;; (define-key term-mode-map (kbd "C-=") 'evil-buffer)
  ;; (define-key term-raw-map (kbd "C-=") 'evil-buffer)
  (evil-define-key 'insert term-raw-map ";tm" 'evil-buffer)
  (evil-define-key 'insert term-raw-map ";;" (lambda () (interactive) (term-send-raw-string ";")))
  (evil-define-key 'insert term-raw-map ";g" 'evil-normal-state)
  
  (defun my-multi-term ()
    (interactive)
    (if (equal 'windows-nt system-type)
        (aweshell-toggle)
      (if (string-match "*terminal<[0-9]\\{1,2\\}>*" (buffer-name))
          (evil-buffer nil)
        (let ((index 1)
              (term-buffer))
          (catch 'break
            (while (<= index 10)
              (setq target-buffer (format "*%s<%s>*" multi-term-buffer-name index))
              (when (buffer-live-p (get-buffer target-buffer))
                (setq term-buffer target-buffer)
                (throw 'break nil))
              (setq index (1+ index))))
          (if term-buffer
              (switch-to-buffer term-buffer)
            (multi-term))
          )))
    )

  ;; (with-parsed-tramp-file-name default-directory path
  ;;       (let ((method (cadr (assoc `tramp-login-program (assoc path-method tramp-methods)))))
  ;;         (message (concat method " " (when path-user (concat path-user "@")) path-host "\C-m"))
  ;;         (message (concat "cd " path-localname "\C-m"))))

  ;; (with-parsed-tramp-file-name default-directory path
  ;;   (message path-user)
  ;;   )

  )

;; https://github.com/purcell/disable-mouse
(use-package disable-mouse
  :ensure t
  :if (and (display-graphic-p) (not (string-equal "A12969" system-name)))
  :config
  (global-disable-mouse-mode)
  (with-eval-after-load 'evil
    (mapc #'disable-mouse-in-keymap
          (list evil-motion-state-map
                evil-normal-state-map
                evil-visual-state-map
                evil-insert-state-map)))
  )

(use-package thrift
  :ensure t
  :defer t
  )

(use-package lua-mode
  :ensure t
  :defer t
  :config
  (add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
  (add-to-list 'interpreter-mode-alist '("lua" . lua-mode))
  )

;; (use-package modern-cpp-font-lock
;;   :ensure t)

(use-package cider
  :ensure t
  :defer t
  :hook
  (clojure-mode . cider-mode)
  ;; :config
  ;; (add-hook 'clojure-mode-hook #'cider-mode)
  )

;; (use-package inf-clojure
;;   :disabled
;;   :ensure t
;;   ;; :defer t
;;   :config
;;   (add-hook 'clojure-mode-hook #'inf-clojure-minor-mode)
;;   )

(use-package god-mode
  :ensure t
  :init
  (general-define-key
   :states 'normal
   "," 'god-execute-with-current-bindings)
  :commands god-execute-with-current-bindings
  :config
  ;; which-key support god-mode
  (which-key-enable-god-mode-support)
  ;; You can change the entire modeline's foreground and background to indicate whether God mode is active as follows:
  (defun my-god-mode-update-modeline ()
    (let ((limited-colors-p (> 257 (length (defined-colors)))))
      (cond (god-local-mode (progn
                              ;; (set-face-background 'mode-line (if limited-colors-p "red" "#e9e2cb"))
                              (set-face-background 'mode-line (if limited-colors-p "red" "orange red"))
                              ;; (set-face-background 'mode-line-inactive (if limited-colors-p "red" "orange red"))
                              ))
            (t (progn
                 ;; (set-face-background 'mode-line (if limited-colors-p "black" "#2B2B2B"))
                 ;; (set-face-background 'mode-line-inactive (if limited-colors-p "black" "#2B2B2B"))))
                 ;; 使用counsel-color-emacs可以查看
                 (set-face-background 'mode-line (if limited-colors-p "black" "gray26"))
                 ;; (set-face-background 'mode-line-inactive (if limited-colors-p "black" "gray26"))
                 )))))

  (add-hook 'god-mode-enabled-hook #'my-god-mode-update-modeline)
  (add-hook 'god-mode-disabled-hook #'my-god-mode-update-modeline)

  ;; For running occasional and single commands in God mode
  ;; (evil-define-key 'normal global-map "gm" (lambda (&optional called-interactively) (interactive "d")
  ;;                                            (if god-local-mode
  ;;                                                (keyboard-quit)
  ;;                                              (god-execute-with-current-bindings called-interactively))))
  ;; (evil-define-key 'normal global-map "gm" #'god-execute-with-current-bindings)
  (define-key god-local-mode-map (kbd ".") #'repeat)
  (define-key god-local-mode-map (kbd ",") #'keyboard-quit)
  )

(use-package indent-guide
  :ensure t
  :hook
  (after-init . indent-guide-global-mode))

(use-package caps-lock
  :commands caps-lock-mode
  :ensure t)

;; (use-package eyebrowse
;;   :ensure t
;;   :hook
;;   (after-init . eyebrowse-mode)
;;   :config
;;   (eyebrowse-setup-opinionated-keys)
;;   ;; (eyebrowse-setup-evil-keys)
;;   )

(use-package perspective
  :ensure t
  ;; :init
  ;; (setq persp-mode-prefix-key (kbd "C-M-g"))
  :hook
  (after-init . persp-mode)
  :bind
  (("C-x C-b" . persp-ivy-switch-buffer) ; or use a nicer switcher, see below
   ("C-x b" . persp-switch-to-buffer*)
   ("C-x k" . persp-kill-buffer*)
   ([remap switch-to-buffer] . persp-switch-to-buffer*)
   ([remap kill-buffer] . persp-kill-buffer*)
   )
  :custom
  (persp-interactive-completion-function 'ivy-completing-read)
  (persp-sort 'created)
  (persp-mode-prefix-key (kbd "C-x C-i"))
  (persp-state-default-file "~/.emacs.d/perspective.save")
  ;; (persp-show-modestring 'header)
  (persp-modestring-short t)
  :config
  (add-hook 'kill-emacs-hook #'persp-state-save)
  )

(use-package workgroups2
  :disabled
  :ensure t
  :hook
  (after-init . workgroups-mode)
  :init
  ;; Change prefix key (before activating WG)
  ;; ;; (setq wg-prefix-key (kbd "M-p"))
  (setq wg-prefix-key (kbd "C-c C-c"))
  :config
  ;; Mode Line changes
  ;; Display workgroups in Mode Line?
  (setq wg-mode-line-display-on t)          ; Default: (not (featurep 'powerline))
  (setq wg-flag-modified t)                 ; Display modified flags as well
  (setq wg-mode-line-decor-left-brace "<"
        wg-mode-line-decor-right-brace ">"  ; how to surround it
        wg-mode-line-decor-divider ":")

  ;; ;; What to do on Emacs exit / workgroups-mode exit?
  ;; ;; (setq wg-emacs-exit-save-behavior           'save)      ; Options: 'save 'ask nil
  ;; ;; (setq wg-workgroups-mode-exit-save-behavior 'save)      ; Options: 'save 'ask nil

  ;; ;; Change workgroups session file
  (setq wg-session-file "~/.emacs.d/.emacs_workgroups")
  )

(use-package info-colors
  :ensure t
  :hook
  (Info-selection . info-colors-fontify-node)
  ;; :config
  ;; (add-hook 'Info-selection-hook 'info-colors-fontify-node)
  )

;; Display ^L glyphs as horizontal lines
;; https://depp.brause.cc/form-feed/
(use-package form-feed
  :ensure t
  ;; :hook
  ;; (help-mode . form-feed-mode)
  :config
  (global-form-feed-mode)
  )

;; 文字生成拼写的大字
(use-package figlet
  :commands figlet
  :if (executable-find "figlet")
  :ensure t)

(use-package benchmark-init
  :ensure t
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate)
  )

(use-package quickrun
  :commands quickrun
  :unless (string-equal "windows-nt" system-type)
  :ensure t)

(use-package rg
  :ensure t
  :defer t
  :bind
  (("C-c s" . rg-menu)
   :map rg-mode-map
   ("w" . wgrep-change-to-wgrep-mode)
   ("j" . compilation-next-error)
   ("k" . compilation-previous-error))
  :bind*
   ("C-c C-s" . rg-menu)
  :config
  (rg-enable-menu)
  (add-hook 'rg-mode-hook (lambda () (evil-set-initial-state 'rg-mode 'emacs)))

  (general-define-key
   :states 'emacs
   :prefix ","
   :keymaps 'rg-mode-map
   "," 'self-insert-command
   "gs" 'evil-avy-goto-char)
  )

(use-package elpa-mirror
  :ensure t
  :config
  (setq elpamr-default-output-directory "~/.myelpa"))

(use-package powershell
  :ensure t
  :defer t
  )

(provide 'init-packages)
