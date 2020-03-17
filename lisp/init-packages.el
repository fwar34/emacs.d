;; -*- coding: utf-8; lexical-binding: t; -*-
(when (>= emacs-major-version 24)
  (require 'package)
  (if (< emacs-major-version 27) (package-initialize))
  ;; (setq package-archives '(("gnu"   . "https://elpa.emacs-china.org/gnu/")
  ;;                         ("melpa" . "https://elpa.emacs-china.org/melpa/"))))
  (setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                           ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
  )

(defun require-package (package)
  "refresh package archives, check package presence and install if it's not installed"
  (if (null (require package nil t))
      (progn (let* ((ARCHIVES (if (null package-archive-contents)
                                  (progn (package-refresh-contents)
                                         package-archive-contents)
                                package-archive-contents))
                    (AVAIL (assoc package ARCHIVES)))
               (if AVAIL
                   (package-install package)))
             (require package))))

;; use-package
(require-package 'use-package)
(require 'use-package)
;; (setq use-package-always-ensure t)

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
(when (and (display-graphic-p) (>= emacs-major-version))
  (use-package posframe :ensure t))

(use-package pyim
  :ensure t
  :if window-system
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
                  ;; pyim-probe-program-mode
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
  (setq pyim-page-length 5)

  ;; 让 Emacs 启动时自动加载 pyim 词库
  ;; (add-hook 'emacs-startup-hook
  ;;           #'(lambda() (pyim-restart-1 t)))

  :bind
  (;; ("M-o ;" . pyim-delete-word-from-personal-buffer))
   ;; ("M-o o" . pyim-convert-string-at-point)
   ("M-j" . pyim-convert-string-at-point)
   ("M-i" . toggle-input-method)   ;; defualt key bind: C-\
   ;; ("M-j" . pyim-toggle-input-ascii)
   ("C-\\" . pyim-toggle-input-ascii)
   )) ;;与 pyim-probe-dynamic-english 配合

;; (setq evil-want-keybinding nil) must put before load evil
;; See https://github.com/emacs-evil/evil-collection/issues/60 for more details.
(setq evil-want-keybinding nil)

;; https://github.com/emacs-evil/evil-collection
;; evil
(use-package evil
  :ensure t
  :hook
  (after-init . evil-mode)
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-C-i-jump t)
  (setq evil-want-fine-undo "Yes")
  (setq evil-want-Y-yank-to-eol t)
  ;; (setq evil-no-display t)              ;; not display evil state in echo area
  :config

  (define-key evil-ex-search-keymap (kbd ";g") #'keyboard-quit)
  ;; for quit shell-command output buffer
  ;; (defun my-quit-window (&rest _)
  ;;   (with-current-buffer "*Shell Command Output*"
  ;;     (evil-local-set-key 'normal (kbd "q") #'quit-window)))
  ;; (advice-add 'shell-command :after #'my-quit-window)
  (defadvice shell-command (after advice-find-file activate)
    (with-current-buffer "*Shell Command Output*"
      ;; (evil-local-set-key 'normal (kbd "q") #'quit-window)))
      (evil-local-set-key 'normal (kbd "q") #'kill-this-buffer)))
  )

(use-package undo-tree
  :ensure t
  :hook
  (after-init . global-undo-tree-mode))

(use-package goto-chg
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
  :bind ([remap switch-to-buffer] . #'ivy-switch-buffer)
  :after evil
  :config
  (setq ivy-initial-inputs-alist nil
        ivy-wrap t
        ivy-height 15
        ivy-fixed-height-minibuffer t
        ivy-format-function #'ivy-format-function-line
        ivy-use-virtual-buffers t)
  (ivy-mode 1)
  (setq enable-recursive-minibuffers t)
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)
  (global-set-key (kbd "C-h f") #'counsel-describe-function)
  (global-set-key (kbd "C-h v") #'counsel-describe-variable))

(use-package smex
  ;; I use this package to display history for M-x
  :ensure t
  :after evil
  :config
  (smex-initialize)
  )

(use-package ivy-xref
  :ensure t
  :after ivy
  :init
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs)
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

(use-package emmet-mode
  :ensure t
  :hook
  ((sgml-mdoe . emmet-mode)    ;; Auto-start on any markup modes
   (html-mode . emmet-mode)    ;; enable Emmet's css abbreviation.
   (web-mode . emmet-mode)
   (css-mode . emmet-mode))
  )

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

(use-package powershell
  :ensure t
  :if (equal system-type 'windows-nt)
  :defer t
  )

;; 开启全局company
(use-package company
  :ensure t
  :hook
  (after-init . global-company-mode)
  :config
  ;; (add-hook 'after-init-hook 'global-company-mode)
  ;; 显示候选项的数字号。根据数字号选择候选项
  (setq company-show-numbers t)
  ;; make previous/next selection in the popup cycles
  (setq company-selection-wrap-around t) 
  ;; Some languages use camel case naming convention,
  ;; so company should be case sensitive.
  (setq company-dabbrev-ignore-case nil)
  (setq company-idle-delay 0.2)
  )

(use-package fzf
  :ensure t
  :unless (equal system-type 'windows-nt)
  :defer t
  )

(use-package ack
  :ensure t
  :defer t
  :config
  ;; (add-hook 'ack-minibuffer-setup-hook 'ack-skel-vc-grep t)
  ;; (add-hook 'ack-minibuffer-setup-hook 'ack-yank-symbol-at-point t)
  (evil-define-key 'normal ack-mode-map "q"
    (lambda ()
      (interactive)
      (if (= (length (window-list-1)) 1)
          (quit-window)
        (delete-window))))
  (if (fboundp 'make-thread)
      (add-hook 'ack-mode-hook
                (lambda ()
                  (make-thread (lambda ()
                                 (while (not (get-buffer-window "*ack*"))
                                   (sleep-for 0 100))
                                 (select-window (get-buffer-window "*ack*")))))))
  )

(use-package magit
  :ensure t
  :after evil
  )

(use-package evil-magit
  :ensure t
  :after (magit evil) 
  :config
  ;; https://www.helplib.com/GitHub/article_131559
  ;; (evil-define-key evil-magit-state magit-mode-map "?"'evil-search-backward)
  )

;; yasnippet setting
;; (use-package yasnippet
;;   :disabled
;;   :ensure t
;;   :defer t
;;   )

(use-package youdao-dictionary
  :ensure t
  :defer t
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
  (setq rust-format-on-save t)
  )

(use-package which-key
  :ensure t
  :hook
  (evil-mode . which-key-mode)
  :init
  (setq which-key-allow-imprecise-window-fit t) ; performance
  (setq which-key-separator ":")
  )

(use-package projectile
  :ensure t
  :hook
  (evil-mode . projectile-mode)
  :config
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  )

;; https://www.emacswiki.org/emacs/NeoTree
(use-package neotree
  :ensure t
  :defer t
  :config
  (global-set-key [f8] 'neotree-toggle)
  ;; Note: For users who want to use the icons theme. Pls make sure you have
  ;; installed the all-the-icons package and its fonts.
  ;; (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (setq neo-theme (when (display-graphic-p) 'icons))
  ;; Every time when the neotree window is opened, let it find current file and jump to node.
  (setq neo-smart-open t)
  ;; When running ‘projectile-switch-project’ (C-c p p), ‘neotree’ will change root automatically.
  (setq projectile-switch-project-action 'neotree-projectile-action)
  ;; Similar to find-file-in-project, NeoTree can be opened (toggled) at projectile project root as follows:
  (defun neotree-project-dir ()
    "Open NeoTree using the git root."
    (interactive)
    (let ((project-dir (projectile-project-root))
          (file-name (buffer-file-name)))
      (neotree-toggle)
      (if project-dir
          (if (neo-global--window-exists-p)
              (progn
                (neotree-dir project-dir)
                (neotree-find file-name)))
        (message "Could not find git project root."))))
  ;; If you use evil-mode, by default some of evil key bindings conflict with neotree-mode keys. For example,
  ;; you cannot use q to hide NeoTree. To make NeoTree key bindings in effect, you can bind those keys
  ;; in evil-normal-state-local-map in neotree-mode-hook, as shown in below code:
  (add-hook 'neotree-mode-hook
            (lambda ()
              (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
              (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-quick-look)
              (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
              (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)
              (define-key evil-normal-state-local-map (kbd "g") 'neotree-refresh)
              (define-key evil-normal-state-local-map (kbd "n") 'neotree-next-line)
              (define-key evil-normal-state-local-map (kbd "p") 'neotree-previous-line)
              (define-key evil-normal-state-local-map (kbd "A") 'neotree-stretch-toggle)
              (define-key evil-normal-state-local-map (kbd "H") 'neotree-hidden-file-toggle)))
  )

(use-package all-the-icons
  :ensure t
  :defer t
  :unless window-system
  )

(use-package lispy
  :ensure t
  :disabled
  :hook
  (emacs-lisp-mode . lispy-mode)
  :config
  ;; (define-key lispy-mode-map (kbd “<delete>”) #'lispy-delete)
  ;; (define-key lispy-mode-map (kbd “C-d”) #'lispy-delete-backward)
  ;; (define-key lispy-mode-map (kbd “C-k”) #'lispy-kill)
  ;; (define-key lispy-mode-map (kbd “C-y”) #'lispy-yank)
  ;; (define-key lispy-mode-map (kbd “C-e”) #'lispy-move-end-of-line) 
  )

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
     additional-movement slurp/barf-cp additional)))

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
  (evilem-default-keybindings "M-m")
  )

(use-package ace-jump-mode
  :ensure t
  :defer t
  :config
  (eval-after-load "ace-jump-mode"
    '(ace-jump-mode-enable-mark-sync))
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
  )

(use-package function-args
  ;; GNU Emacs package for showing an inline arguments hint for the C/C++ function at point
  :disabled
  :ensure t
  :after evil
  :config
  (fa-config-default)
  (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
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

(use-package ace-window
  :disabled
  :ensure t
  :defer t
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
  :ensure t
  :after evil
  ;; :if (display-graphic-p)
  :config
  ;; If you enable global minor mode
  (global-git-gutter-mode t)
  ;; If you would like to use git-gutter.el and linum-mode
  (unless (display-graphic-p) (git-gutter:linum-setup))
  ;; Use for 'Git'(`git`), 'Mercurial'(`hg`), 'Bazaar'(`bzr`), and 'Subversion'(`svn`) projects
  ;; (custom-set-variables '(git-gutter:handled-backends '(git hg bzr svn)))
  (custom-set-variables '(git-gutter:handled-backends '(git svn)))
  ;; inactivate git-gutter-mode in asm-mode and image-mode
  (custom-set-variables '(git-gutter:disabled-modes '(asm-mode image-mode)))
  ;; Hide gutter when there are no changes if git-gutter:hide-gutter is non-nil. (Default is nil)
  (custom-set-variables '(git-gutter:hide-gutter t))
  ;; If you set git-gutter :update-interval seconds larger than 0,
  ;; git-gutter updates diff information in real-time by idle timer.
  (custom-set-variables '(git-gutter:update-interval 2))
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

(use-package diff-hl
  :disabled
  :ensure t
  :after evil
  :if (not (display-graphic-p))
  :config
  (global-diff-hl-mode)
  (diff-hl-margin-mode) 
  (advice-add 'svn-status-update-modeline :after #'diff-hl-update)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (evil-define-key 'normal 'magit-mode-map "q" #'kill-buffer-and-window)
  )

(use-package company-statistics
  :ensure t
  :after after-init
  :config
  (add-hook 'after-init-hook 'company-statistics-mode)
  )

(use-package company-c-headers
  :ensure t
  :after evil
  :config
  (add-to-list 'company-backends 'company-c-headers)
  )

(use-package ace-popup-menu
  :ensure t
  :after evil
  :config
  (ace-popup-menu-mode 1)
  )

(use-package company-jedi
  :ensure t
  :after python
  :config
  (defun my/python-mode-hook ()
    (add-to-list 'company-backends 'company-jedi))
  (add-hook 'python-mode-hook 'my/python-mode-hook)
  )

(use-package exec-path-from-shell
  :disabled
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

(use-package company-english-helper
  ;; write by lazycat
  :after evil
  :straight
  (:host github :repo "manateelazycat/company-english-helper")
  :config
  ;; toggle-company-english-helper
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
  :config

  (cond
   ((equal system-type 'windows-nt) (setq multi-term-program "eshell"))
   ((equal system-type 'gnu/linux) (setq multi-term-program "/usr/bin/zsh")))

  ;; (setq mykey (read-kbd-macro "C-l"))
  ;; (define-key term-mode-map mykey 'evil-buffer)
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

  (add-to-list 'term-bind-key-alist '("M-l" . evil-buffer))
  (add-to-list 'term-bind-key-alist '("M-y" . term-paste))

  (defun my-multi-term ()
    (interactive)
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
          (progn
            (setq orig-default-directory default-directory)
            (switch-to-buffer term-buffer)
            (term-send-raw-string (concat "cd " orig-default-directory "\C-m"))
            )
        (multi-term)))
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
  :if window-system
  :config
  (global-disable-mouse-mode)
  (with-eval-after-load 'evil
    (mapc #'disable-mouse-in-keymap
          (list evil-motion-state-map
                evil-normal-state-map
                evil-visual-state-map
                evil-insert-state-map)))
  )

;; (use-package modern-cpp-font-lock
;;   :ensure t)

(provide 'init-packages)
