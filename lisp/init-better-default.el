;; -*- coding: utf-8; lexical-binding: t; -*-

;; -----------------------------------------------------------------
;; https://blog.csdn.net/yo746862873/article/details/52291780
;;;; 设置编辑环境
;; 设置为中文简体语言环境
;; (set-language-environment 'Chinese-GBK)
;; ;; (set-language-environment 'UTF-8)
;; (set-default-coding-systems 'utf-8-unix)
;; ;; 设置emacs 使用 utf-8-unix
;; (setq locale-coding-system 'utf-8-unix)
;; ;; 设置键盘输入时的字符编码
;; (set-keyboard-coding-system 'utf-8-unix)
;; (set-selection-coding-system 'utf-8-unix)
;; ;; 文件默认保存为 utf-8-unix
;; (set-buffer-file-coding-system 'utf-8-unix)
;; (set-default buffer-file-coding-system 'utf-8-unix)
;; (set-default-coding-systems 'utf-8-unix)
;; ;; 解决粘贴中文出现乱码的问题
;; (set-clipboard-coding-system 'utf-8-unix)
;; ;; 终端中文乱码
;; (set-terminal-coding-system 'utf-8-unix)
;; (modify-coding-system-alist 'process "*" 'utf-8-unix)
;; (setq default-process-coding-system '(utf-8-unix . utf-8-unix))
;; ;; 解决文件目录的中文名乱码
;; (setq-default pathname-coding-system 'utf-8-unix)
;; (set-file-name-coding-system 'utf-8-unix)

;; -----------------------------------------------------------------------------
;; https://emacs-china.org/t/emacs/7814/7
;; 今天解决了一个困扰了许久的问题，那就是emacs的文件编码问题，个人一直以来都是默认使用utf-8来编辑文件，但是总会遇到别的同事使用gbk来编码，导致查看别人写的代码时显示乱码，之前遇到这种问题，一般都是 C-x ret r revert编码成gbk，很不方便。
;; 还有一段时间是直接(set-language-environment "Chinese-GB18030")设置编码环境到gbk ，这样看别人的文件是没问题了，但是自己的文件也变成gbk了，更糟糕的是，一些输出会变成乱码，比如 mu4e-view模式下转换邮件到浏览器查看，就会变成乱码。
;; 目前找到的解决办法 设置emacs 的prefer-coding-system
;; ;; set coding config, last is highest priority.
;; ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Recognize-Coding.html#Recognize-Coding
;; (prefer-coding-system 'cp950)
;; (prefer-coding-system 'gb2312)
;; (prefer-coding-system 'cp936)
;; (prefer-coding-system 'gb18030)
;; (prefer-coding-system 'utf-16)
;; (prefer-coding-system 'utf-8-dos)
;; (prefer-coding-system 'utf-8-unix)
;; 这样emacs会按照顺序优先编码，需要注意的是，放在最后的会被最优先选择。上面的设置就是最优先选择utf-8-unix
;; 这样设置在一些场景下还有一些不够智能，比如magit 看 log信息，会出现 log现实中文正常，进入查看修改内容如果遇到中文会变成乱码

;; https://stackoverflow.com/questions/63644928/emacs-failed-quit-with-error-of-utf-8-cannot-encode
;; utf-8-unix cannot encode these可以使用下面的代码来看具体是什么文字
;; (decode-coding-string (unibyte-string #o326 #o334 #o301 #o371) 'chinese-gbk)

;; 自动检测文件编码
;; https://emacs-china.org/t/emacs/7814/7
(use-package unicad
  :disabled
  :ensure t
  :config
  (unicad-mode))
;; -----------------------------------------------------------------------------
;; https://stackoverflow.com/questions/2901541/which-coding-system-should-i-use-in-emacs
(setq utf-translate-cjk-mode nil) ; disable CJK coding/encoding (Chinese/Japanese/Korean characters)
(set-language-environment 'utf-8)
(set-keyboard-coding-system 'utf-8-mac) ; For old Carbon emacs on OS X only
(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system
 (if (eq system-type 'windows-nt)
     'utf-16-le  ;; https://rufflewind.com/2014-07-20/pasting-unicode-in-emacs-on-windows
   'utf-8))
;; (prefer-coding-system 'utf-8)

(prefer-coding-system 'cp950)
(prefer-coding-system 'gb2312)
(prefer-coding-system 'cp936)
(prefer-coding-system 'gb18030)
;; (prefer-coding-system 'utf-16)
;; (prefer-coding-system 'utf-8-dos)
(prefer-coding-system 'utf-8)
;; -----------------------------------------------------------------------------

;; 使用英文 day-name, 而不是中文： “星期XX”，在org-capture中windows中文里面一直乱码
(when (equal system-type 'windows-nt)
  (setq system-time-locale "C"))

;; {{{
;; https://github.com/syl20bnr/spacemacs/issues/192
;; https://andrewjamesjohnson.com/suppressing-ad-handle-definition-warnings-in-emacs/
;; Suppressing ad-handle-definition Warnings in Emacs
(setq ad-redefinition-action 'accept)
;; }}}

;; 自动刷新被修改过的文件
(global-auto-revert-mode +1)

;; 随时重新加载发生修改过的文件
(setq load-prefer-newer t)

;; 关闭GUI功能
(setq use-file-dialog nil
      use-dialog-box nil
      inhibit-startup-screen t
      inhibit-startup-message t)

;; 关闭字体缓存gc
(setq inhibit-compacting-font-caches nil)

;; 最大单行字符数量
(setq-default fill-column 80)
;; -----------------------------------------------------------------
(use-package recentf
  :after evil
  ;; :defer 2
  :config
  (recentf-mode 1)
  ;; 补充一下，recentf 展示时，可以对文件名预处理，比如把家目录替换为 ~
  (add-to-list 'recentf-filename-handlers 'abbreviate-file-name)
  (setq recentf-max-menu-item 10)
  (global-prettify-symbols-mode t)
  ;; 禁用响铃
  (setq ring-bell-function 'ignore)
  ;; 光标靠近鼠标指针时，让鼠标指针自动让开，别挡住视线
  ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Mouse-Avoidance.html
  (mouse-avoidance-mode 'animate)
  ;; (mouse-avoidance-mode 'banish)
  ;; 当光标在行尾上下移动的时候，始终保持在行尾
  (setq track-eol t)
  ;; display time in modeline
  (display-time-mode 1)
  ;; 在使用emacs时，一行文字如果不按回车键，那么它就会一直往右延伸，不会自动换行。这是很不方便的。
  (setq work-wrap 'off)
  ;; 禁用备份文件
  (setq make-backup-files nil)
  (setq auto-save-default nil)

  ;;-------------------------------------------------------------
  ;; paren settings
  ;;-------------------------------------------------------------
  ;; https://www.emacswiki.org/emacs/ShowParenMode#toc1
  (setq show-paren-delay 0)
  (show-paren-mode)
  ;; https://stackoverflow.com/questions/22951181/emacs-parenthesis-match-colors-styling
  ;; "C-u C-x =(aka C-u M-x what-cursor-position)" with the cursor on the highlighted parenthesis,
  ;; you will know what the highlighting face is.

  ;; reference from spacemacs
  (defun true-color-p ()
    (or
     (display-graphic-p)
     (= (tty-display-color-cells) 16777216)))
  ;; (setq variant 'light)
  (setq variant 'dark)
  (setq mat (if (eq variant 'dark) (if (true-color-p) "#86dc2f" "#86dc2f") (if (true-color-p) "#ba2f59" "#af005f")))
  (setq weight-value (if (window-system) 'normal 'extra-bold))
  ;;-------------------------------------------------------------
  ;; set-face-attribute 这个要延时调用才能起作用，没搞清楚原因，难道会被覆盖？
  ;;-------------------------------------------------------------
  ;; (defun custom-face ()
  ;;   (set-face-attribute
  ;;    'show-paren-match
  ;;    nil
  ;;    :foreground mat
  ;;    :underline t
  ;;    :background nil
  ;;    :inverse-video nil
  ;;    :weight weight-value))
  ;; (run-with-idle-timer 2 nil 'custom-face)
  ;;-------------------------------------------------------------
  ;; custom-set-faces
  ;;-------------------------------------------------------------
  (custom-set-faces
   `(show-paren-match ((t (:foreground ,mat :underline t :background nil :inverse-video nil :weight ,weight-value)))))
  ;;-------------------------------------------------------------
  ;; custom-theme-set-faces
  ;;-------------------------------------------------------------
  ;; (custom-theme-set-faces
  ;;  'monokai
  ;;  `(show-paren-match ((t (:foreground ,mat :underline t :background nil :inverse-video nil :weight ,weight-value)))))

  ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Matching.html
  ;; causes highlighting also when point is on the inside of a parenthesis. 
  ;; (setq show-paren-when-point-inside-paren t)

  ;;-------------------------------------------------------------
  ;; 高亮光标增强 advice-add和define-advice,只有延时调用或者禁用evil才起作用,还没搞清楚原因。
  ;; defadvice在windows下就不用延时
  ;;-------------------------------------------------------------
  ;; (define-advice show-paren-function (:around (fn) fix-show-paren-function)
  ;;   "Highlight enclosing parens."
  ;;   (cond ((looking-at-p "\\s(") (funcall fn))
  ;;         (t (save-excursion
  ;;              (ignore-errors (backward-up-list))
  ;;              (funcall fn)))))
  ;;-------------------------------------------------------------
  ;; (defun advice-show-paren-function (fn)
  ;;   (cond ((looking-at-p "\\s(") (funcall fn))
  ;;         (t (save-excursion
  ;;              (ignore-errors (backward-up-list))
  ;;              (funcall fn)))))
  ;; (advice-add 'show-paren-function :around 'advice-show-paren-function)
  ;;-------------------------------------------------------------
  (defadvice show-paren-function (around advice-show-paren-function activate)
    (cond ((looking-at-p "\\s(") ad-do-it)
          (t (save-excursion
               (ignore-errors (backward-up-list))
               ad-do-it))))

  (defun test-make ()
    (interactive)
    (if (looking-at-p "\\s(")
        (message "found")
      (message "not found")))

  (defun test-back ()
    (interactive)
    (backward-up-list))

  (defun test-remove-advice ()
    (interactive)
    (advice-remove 'show-paren-function 'advice-show-paren-function))


  (set-cursor-color "red")
  (fset 'yes-or-no-p 'y-or-n-p)
  ;; (setq use-short-answers t)
  (delete-selection-mode 1)

  (add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))
  (add-to-list 'auto-mode-alist '("\\.ice\\'" . c++-mode))

  ;; 针对 CJK 字符，提高 word-wrap 的效果
  (setq word-wrap-by-category t)
  )

;; https://emacs-china.org/t/emacs-builtin-mode/11937
;; auto reload File
(use-package autorevert
  :ensure nil
  :hook (after-init . global-auto-revert-mode))

;; 行号
;; (if (>= emacs-major-version 26)
;;     ;; config built-in "display-line-number-mode" (require Emacs >= 26)
;;     ;; enable line numbering (or "linum-mode")
;;     (let ((hook-list '(sh-mode-hook
;;                        cmake-mode-hook
;;                        emacs-lisp-mode-hook
;;                        matlab-mode-hook
;;                        rust-mode-hook
;;                        go-mode-hook
;;                        clojure-mode-hook
;;                        python-mode-hook
;;                        c-mode-common-hook
;;                        lua-mode-hook
;;                        ;; org-mode-hook
;;                        package-menu-mode-hook
;;                        makefile-gmake-mode-hook
;;                        ;;  Gnome
;;                        makefile-bsdmake-mode-hook ; OS X
;;                        ess-mode-hook)))
;;       (setq-default display-line-numbers-width 2)
;;       (setq-default display-line-numbers-width-start t)  ;; 行数右对齐
;;       ;; (setq-default display-line-numbers-type 'relative)
;;       (setq display-line-numbers-current-absolute t)
;;       (dolist (hook-element hook-list)
;;         (add-hook hook-element 'display-line-numbers-mode)))
;;   ) 
(global-display-line-numbers-mode)

;; abbrev
;; (abbrev-mode t)
;; (define-abbrev-table 'global-abbrev-table '(("lf" "liang.feng")))

;; When you visit a file, point goes to the last place where it was when you previously visited the same file.
;; remember cursor position. When file is opened, put cursor at last position
(if (version< emacs-version "25.0")
    (progn
      (require 'saveplace)
      (setq-default save-place t))
  (save-place-mode 1))

;;-------------------------------------------------------------
;; code format
;;-------------------------------------------------------------
;; tab settings
(setq-default indent-tabs-mode nil) ; tab 改为插入空格
(setq c-basic-offset 4) ; c c++ 缩进4个空格
;; https://www.emacswiki.org/emacs/IndentingC
;; https://en.wikipedia.org/wiki/Indent_style
(setq c-default-style "linux")
(setq default-tab-width 4)
;; 设置tab为4个宽度
(setq-default tab-width 4)
;; https://www.gnu.org/software/emacs/manual/html_node/ccmode/c_002doffsets_002dalist.html#c_002doffsets_002dalist
;; https://www.gnu.org/software/emacs/manual/html_node/ccmode/Style-Variables.html#Style-Variables
;; https://www.gnu.org/software/emacs/manual/html_node/ccmode/Custom-Line_002dUp.html#Custom-Line_002dUp
(c-set-offset 'innamespace 0)
;; (c-set-offset 'inclass 4)
(c-set-offset 'inline-open 0)
(c-set-offset 'inline-close 0)
;; disable guess python indent warning
(setq python-indent-guess-indent-offset-verbose nil)

;; Underscore "_" is not a word character
;; https://github.com/emacs-evil/evil
(add-hook 'prog-mode-hook (lambda () (modify-syntax-entry ?_ "w")))
(add-hook 'emacs-lisp-mode-hook (lambda () (modify-syntax-entry ?- "w")))
(add-hook 'help-mode-hook (lambda () (modify-syntax-entry ?- "w")))

;; 花括号自动换行的问题
;; http://ergoemacs.org/emacs/emacs_insert_brackets_by_pair.html
(when (fboundp 'electric-pair-mode)
  (electric-pair-mode t)
  ;; (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
  
  ;; (defun electric-pair ()
  ;;   "If at end of line, insert character pair without surrounding spaces.
  ;;   Otherwise, just insert the typed character."
  ;;   (interactive)
  ;;   (if (eolp) (let (parens-require-spaces) (insert-pair)) (self-insert-command 1)))
  )

;; advice for find-file
;; (defun advice-find-file (filename &optional wildcards)
;;   (interactive)
;;   (if (file-exists-p filename)
;;       t
;;     (y-or-n-p (message "%s not exist! create it?" filename))))
;; (advice-add 'find-file :before-while 'advice-find-file)

;; (defadvice find-file (around advice-find-file activate)
;;   (if (file-exists-p filename)
;;       ad-do-it
;;     (if (y-or-n-p (message "%s not exist! create it!" filename))
;;         ad-do-it))
;;   )

;; after execute shell-command goto bottom of output buffer
;; (defadvice shell-command (around adivce-shell-command activate)
;;   ad-do-it
;;   (switch-to-buffer "*Shell Command Output*")
;;   (goto-char (point-max)))

;; advice for evil search
;; (defadvice evil-search-next (after advice-for-evil-search-next activate)
;;   (evil-scroll-line-to-center (line-number-at-pos)))
;; (defadvice evil-search-previous (after advice-for-evil-search-previous activate)
;;   (evil-scroll-line-to-center (line-number-at-pos)))


;; adjust for work server
(when (or (equal system-name "tms2")
          (equal system-name "ceph1")
          (equal system-name "ubuntu"))
  (add-hook
   'emacs-startup-hook
   (lambda ()
     (quit-windows-on
      (get-buffer "*Warnings*")))))

(use-package diff
  :ensure t
  :after evil
  :config
  (evil-define-key 'normal diff-mode-map "q" 'kill-this-buffer)
  (evil-define-key 'normal help-mode-map "q" 'kill-buffer-and-window)
  (evil-define-key 'motion apropos-mode-map "q" 'kill-buffer-and-window)
  ;; create a thread to auto focus on *apropos* window
  (if (fboundp 'make-thread)
      (add-hook 'apropos-mode-hook (lambda ()
                                     (make-thread (lambda ()
                                                    (while (not (get-buffer-window "*Apropos*"))
                                                      (sleep-for 0 100))
                                                    (select-window (get-buffer-window "*Apropos*")))))))
  )

;; https://emacs-china.org/t/scratch-lexical-binding/9378
(add-hook 'emacs-startup-hook
          (lambda ()
            (let ((buffer (get-buffer "*scratch*")))
              (when buffer
                (with-current-buffer buffer
                  (setq lexical-binding t))))))

;; https://emacs-china.org/t/advice/7566/7
(add-hook 'help-mode-hook 'cursor-sensor-mode)
(defun function-advices (function)
  "Return FUNCTION's advices."
  (let ((function-def (advice--symbol-function function))
        (ad-functions '()))
    (while (advice--p function-def)
      (setq ad-functions (append `(,(advice--car function-def)) ad-functions))
      (setq function-def (advice--cdr function-def)))
    ad-functions))

(define-advice describe-function-1 (:after (function) advice-remove-button)
  "Add a button to remove advice."
  (when (get-buffer "*Help*")
    (with-current-buffer "*Help*"
      (save-excursion
        (goto-char (point-min))
        (let ((ad-index 0)
              (ad-list (reverse (function-advices function))))
          (while (re-search-forward "^:[-a-z]+ advice: \\(.+\\)$" nil t)
            (let* ((name (string-trim (match-string 1) "'" "'"))
                   (advice (or (intern-soft name) (nth ad-index ad-list))))
              (when (and advice (functionp advice))
                (let ((inhibit-read-only t))
                  (insert " » ")
                  (insert-text-button
                   "Remove"
                   'cursor-sensor-functions `((lambda (&rest _) (message "%s" ',advice)))
                   'help-echo (format "%s" advice)
                   'action
                   ;; In case lexical-binding is off
                   `(lambda (_)
                      (when (yes-or-no-p (format "Remove %s ? " ',advice))
                        (message "Removing %s of advice from %s" ',function ',advice)
                        (advice-remove ',function ',advice)
                        (revert-buffer nil t)))
                   'follow-link t))))
                        (setq ad-index (1+ ad-index))))))))

(defun helm-advice-remove (function)
  "Remove advice from FUNCTION."
  (interactive (let* ((fn (function-called-at-point))
                      (enable-recursive-minibuffers t)
                      (val (completing-read
                            (if fn
                                (format "Function (default %s): " fn)
                              "Function: ")
                            'help--symbol-completion-table
                            (lambda (f) (or (fboundp f) (get f 'function-documentation)))
                            t nil nil
                            (and fn (symbol-name fn)))))
                 (unless (equal val "")
                   (setq fn (intern val)))
                 (unless (and fn (symbolp fn))
                   (user-error "You didn't specify a function symbol"))
                 (unless (or (fboundp fn) (get fn 'function-documentation))
                   (user-error "Symbol's function definition is void: %s" fn))
                 (list fn)))
  (let* ((ad-alist (mapcar (lambda (ad) (cons (format "%S" ad) ad)) (function-advices function)))
         (default-candidates (mapcar (lambda (ad) (car ad)) ad-alist)))
    (helm :sources
          (helm-build-sync-source "Advices"
                                  :candidates default-candidates
                                  :action
                                  `(("Remove" . (lambda (_)
                                                  (let ((items (helm-marked-candidates)))
                                                    (when (yes-or-no-p (format "Remove %s ? " (if (cdr items) items (car items))))
                                                      (mapc (lambda (item)
                                                              (let ((ad (alist-get item ',ad-alist nil nil 'string=)))
                                                                (message "Removing %s of advice from %s" ',function ad)
                                                                (advice-remove ',function ad)))
                                                            items))))))))))

;; *Message* buffer should be writable in 24.4+
(defadvice switch-to-buffer (after switch-to-buffer-after-hack activate)
  (if (string= "*Messages*" (buffer-name))
      (read-only-mode -1)))

;; after key sequence SPC r p y(run-python) switch to python window
(if (fboundp 'make-thread)
      (add-hook 'inferior-python-mode-hook
                (lambda ()
                  (make-thread (lambda ()
                                 (while (not (get-buffer-window "*Python*"))
                                   (sleep-for 0 100))
                                 (select-window (get-buffer-window "*Python*")))))))

;; enable mouse in terminal
;; (when (not (display-graphic-p))
;;   (xterm-mouse-mode)
;;   )

(provide 'init-better-default)
