;; -*- coding: utf-8; lexical-binding: t; -*-

;; ;; https://blog.csdn.net/lidonghat/article/details/52891343 
;; ;; 打开文件出现乱码时，可以尝试修改字符的编码：
;; ;; M-x revert-buffer-with-coding-system
;; ;; 然后输入对应编码，如：utf-8 或者 chinese-gbk。
;; ;; 在保存的时候还可以指定文件的保存编码：
;; ;; M-x set-buffer-file-coding-system

;; ;; encoding
;; (set-language-environment "UTF-8")
;; ;; (set-default-coding-systems 'utf-8)
;; (set-default-coding-systems 'utf-8-unix)
;; ;; (set-default buffer-file-coding-system 'utf-8)
;; ;; 文件默认保存为 utf-8
;; (set-buffer-file-coding-system 'utf-8-unix)
;; (set-default buffer-file-coding-system 'utf-8-unix)
;; (set-default-coding-systems 'utf-8-unix)
;; ;; (setq-default pathname-coding-system 'euc-cn)
;; ;; (setq file-name-coding-system 'euc-cn)
;; ;;  另外建议按下面的先后顺序来设置中文编码识别方式。
;; ;;  重要提示:写在最后一行的，实际上最优先使用; 最前面一行，反而放到最后才识别。
;; ;;  utf-16le-with-signature 相当于 Windows 下的 Unicode 编码，这里也可写成
;; ;;  utf-16 (utf-16 实际上还细分为 utf-16le, utf-16be, utf-16le-with-signature等多种)
;; ;; (prefer-coding-system 'cp950)
;; ;; (prefer-coding-system 'gb2312)
;; ;; (prefer-coding-system 'cp936)
;; ;; (prefer-coding-system 'gb18030)
;; ;; (prefer-coding-system 'gb18030-dos)
;; (prefer-coding-system 'gb18030-unix)
;; ;; (prefer-coding-system 'utf-16le-with-signature)
;; ;; (prefer-coding-system 'utf-16)
;; ;; (prefer-coding-system 'utf-8)
;; ;; (prefer-coding-system 'utf-8-dos)
;; (prefer-coding-system 'utf-8-unix)
;; ;;  编码设置 end

;; -----------------------------------------------------------------
;; https://blog.csdn.net/yo746862873/article/details/52291780
;;;; 设置编辑环境
;; 设置为中文简体语言环境
;; (set-language-environment 'Chinese-GB)
(set-language-environment 'UTF-8)
(set-default-coding-systems 'utf-8-unix)
;; 设置emacs 使用 utf-8-unix
;; (setq locale-coding-system 'utf-8-unix)
;; 设置键盘输入时的字符编码
(set-keyboard-coding-system 'utf-8-unix)
(set-selection-coding-system 'utf-8-unix)
;; 文件默认保存为 utf-8-unix
(set-buffer-file-coding-system 'utf-8-unix)
(set-default buffer-file-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
;; 解决粘贴中文出现乱码的问题
(set-clipboard-coding-system 'utf-8-unix)
;; 终端中文乱码
(set-terminal-coding-system 'utf-8-unix)
(modify-coding-system-alist 'process "*" 'utf-8-unix)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))
;; 解决文件目录的中文名乱码
(setq-default pathname-coding-system 'utf-8-unix)
(set-file-name-coding-system 'utf-8-unix)
(prefer-coding-system 'gb18030-unix)
(prefer-coding-system 'utf-8-unix)

;; 解决 Shell Mode(cmd) 下中文乱码问题
;; (defun change-shell-mode-coding ()
;;   (progn
;;     (set-terminal-coding-system 'gbk)
;;     (set-keyboard-coding-system 'gbk)
;;     (set-selection-coding-system 'gbk)
;;     (set-buffer-file-coding-system 'gbk)
;;     (set-file-name-coding-system 'gbk)
;;     (modify-coding-system-alist 'process "*" 'gbk)
;;     (set-buffer-process-coding-system 'gbk 'gbk)
;;     (set-file-name-coding-system 'gbk)))
;; -----------------------------------------------------------------

;; 设置垃圾回收，在Windows下，emacs25版本会频繁出发垃圾回收，所以需要设置
;; (when (eq system-type 'windows-nt)
;;   (setq gc-cons-threshold (* 512 1024 1024))
;;   (setq gc-cons-percentage 0.5)
;;   (run-with-idle-timer 5 t #'garbage-collect)
;;   ;; 显示垃圾回收信息，这个可以作为调试用
;;   ;; (setq garbage-collection-messages t)
;;   )

(use-package recentf
  :after evil
  ;; :defer 2
  :config
  (recentf-mode 1)
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
  ;; auto reload file
  (global-auto-revert-mode t)
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
  ;; (run-with-idle-timer 2 nil #'custom-face)
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
  ;; (advice-add #'show-paren-function :around #'advice-show-paren-function)
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
    (advice-remove #'show-paren-function #'advice-show-paren-function))


  (set-cursor-color "red")
  (fset 'yes-or-no-p 'y-or-n-p)
  (delete-selection-mode 1)

  (add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))
  (add-to-list 'auto-mode-alist '("\\.ice\\'" . c++-mode))
  )

;; 行号
(if (>= emacs-major-version 26)
    ;; config built-in "display-line-number-mode" (require Emacs >= 26)
    ;; enable line numbering (or "linum-mode")
    (let ((hook-list '(sh-mode-hook
                       cmake-mode-hook
                       emacs-lisp-mode-hook
                       matlab-mode-hook
                       rust-mode-hook
                       go-mode-hook
                       clojure-mode-hook
                       python-mode-hook
                       c-mode-common-hook
                       ;; org-mode-hook
                       package-menu-mode-hook
                       makefile-gmake-mode-hook
                       ;;  Gnome
                       makefile-bsdmake-mode-hook ; OS X
                       ess-mode-hook)))  
      (setq-default display-line-numbers-width 2)
      (setq-default display-line-numbers-width-start t)  ;; 行数右对齐
      ;; (setq-default display-line-numbers-type 'relative)
      (setq display-line-numbers-current-absolute t)
      (dolist (hook-element hook-list)
        (add-hook hook-element 'display-line-numbers-mode)))
  (global-linum-mode 1)
  (when (not (display-graphic-p))
    (setq linum-format "%d "))) ;; 注意%d后面有空格，即用空格将行号和代码隔


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

;; 花括号自动换行的问题
;; http://tieba.baidu.com/p/3572057629
(when (fboundp 'electric-pair-mode)
  (electric-pair-mode t)
  (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit))

;; advice for find-file
;; (defun advice-find-file (filename &optional wildcards)
;;   (interactive)
;;   (if (file-exists-p filename)
;;       t
;;     (y-or-n-p (message "%s not exist! create it?" filename))))
;; (advice-add #'find-file :before-while #'advice-find-file)

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
  (evil-define-key 'normal diff-mode-map "q" #'kill-this-buffer)
  (evil-define-key 'normal help-mode-map "q" #'kill-buffer-and-window)
  (evil-define-key 'motion apropos-mode-map "q" #'kill-buffer-and-window)
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
                            #'help--symbol-completion-table
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


(provide 'init-better-default)
