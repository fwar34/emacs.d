
;; -*- coding: utf-8; lexical-binding: t; -*-
;; 快速打开配置文件
(define-namespace fwar34-)
(defun open-init-file()
  (interactive)
  (find-file "~/.emacs.d/lisp/init-packages.el"))

;; 隐藏windows换行符
(defun hidden-dos-eol()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

;; 移除windows换行符
(defun remove-dos-eol()
  "Replace DOS eolns CR LR with Unix eolns CR."
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\r" nil t) (replace-match "")))

;; 增强occur, 抓取选中或者光标的词
(defun occur-dwim ()
  "Call `occur' with a sane default."
  (interactive)
  (push (if (region-active-p)
            (buffer-substring-no-properties
             (region-beginning)
             (region-end))
          (let ((sym (thing-at-point 'symbol)))
            (when (stringp sym)
              (regexp-quote sym))))
        regexp-history)
  (call-interactively 'occur))

;; org mode
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/.emacs.d/gtd.org" "工作安排")
         "* TODO [#B] %?\n  %i\n"
         :empty-lines 1)))

;; gnu-global
(defun gtags-update-single(filename)
  "Update Gtags database for changes in a single file"
  (interactive)
  (start-process "update-gtags" "update-gtags" "bash" "-c" (concat "cd " (gtags-root-dir) " ; gtags --single-update " filename )))

(defun gtags-update-current-file()
  (interactive)
  (defvar filename)
  (setq filename (replace-regexp-in-string (gtags-root-dir) "." (buffer-file-name (current-buffer))))
  (gtags-update-single filename)
  (message "Gtags updated for %s" filename))

(defun gtags-update-hook()
  "Update GTAGS file incrementally upon saving a file"
  (when gtags-mode
    (when (gtags-root-dir)
      (gtags-update-current-file))))

(defun scroll-other-window-up ()
  (interactive)
  (scroll-other-window '-))

;; smex or counsel-M-x?
(defvar my-use-smex nil
  "Use `smex' instead of `counsel-M-x' when press M-x.")
(defun my-M-x ()
  (interactive)
  (cond
   (my-use-smex
    (smex))
   ((fboundp 'counsel-M-x)
    ;; `counsel-M-x' will use `smex' to remember history
    (counsel-M-x))
   ((fboundp 'smex)
    (smex))
   (t
    (execute-extended-command))))

(defun my-append-string-marker (str)
  "Append a string to end of current line, then move cursion to origion position."
  (let* ((cursion-position (point-marker)))
    (end-of-line)
    (insert str)
    (goto-char (marker-position cursion-position)))
  )
(defun my-append-semicolon-marker ()
  "Append a ';' to end of current line, then move cursion to origion position."
  (interactive)
  (my-append-string-marker ";")
  )

(defun my-append-string-excursion (str)
  "Append a string to end of a line, then move cursion to origion position"
  (save-excursion
    (end-of-line)
    (insert str)
    )
  )
(defun my-append-semicolon-excursion ()
  "Append a ';' to end of current line, then move cursion to origion position"
  (interactive)
  (my-append-string-excursion ";")
  )

(defun my-append-string (str)
  "Append a string to end of a line"
  (end-of-line)
  ;;(insert-char str)
  (insert str)
  )
(defun my-append-semicolon ()
  "Append a ';' to end of current line."
  (interactive)
  ;; (my-append-string 59)
  (my-append-string ";")
  )

(defun my-display-full-path-of-current-buffer ()
  "Display the full path of current file"
  (interactive)
  (message (buffer-file-name))
  )

;; Shorter modeline
(defvar mode-line-cleaner-alist
  '((auto-complete-mode . "α")
    ;; Major modes
    (lisp-interaction-mode . "Λ")
    )
  "Alist for `clean-mode-line'.
When you add a new element to the alist, keep in mind that you
must pass the correct minor/major mode symbol and a string you
want to use in the modeline *in lieu of* the original.")

(defun clean-mode-line ()
  (interactive)
  (cl-loop for cleaner in mode-line-cleaner-alist
           do (let* ((mode (car cleaner))
                     (mode-str (cdr cleaner))
                     (old-mode-str (cdr (assq mode minor-mode-alist))))
                (when old-mode-str
                  (setcar old-mode-str mode-str))
                ;; major mode
                (when (eq mode major-mode)
                  (setq mode-name mode-str)))))

(add-hook 'after-change-major-mode-hook 'clean-mode-line)

(defun fwar34/recent-file()
  "open recent file, then set state normal"
  (interactive)
  (recentf-open-files)
  (evil-normal-state)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http://blog.binchen.org/index-21.html, Use ivy to open recent directories
(require 'ivy) ; swiper 7.0+ should be installed
(defun fwar34/counsel-goto-recent-directory ()
  "Open recent directory with dired"
  (interactive)
  (unless recentf-mode (recentf-mode 1))
  (let ((collection
         (delete-dups
          (append (mapcar 'file-name-directory recentf-list)
                  ;; fasd history
                  (if (executable-find "fasd")
                      (split-string (shell-command-to-string "fasd -ld") "\n" t))))))
    (ivy-read "directories:" collection :action 'dired)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://blog.csdn.net/loushuai/article/details/51648924
;; (defun list-funcs (arg)
;;   "List functions in buffer."
;;   (interactive "p")
;;   (message "functions")
;;   ;;;  (list-matching-lines "^\\bstatic\\b*\\binline\\b*[ ]*[A-Za-z_<>]+[ ]+[A-Za-z0-9_:]+[\(]"))
;;     (list-matching-lines "^[A-Za-z0-9_]+[ ]+[A-Za-z0-9_<>: ]*[\(]"))

;; #!/usr/bin/env python3
;; #-*- coding: utf-8 -*-

;; # File Name: file_test1.py
;; # Author: Feng
;; # Created Time: Fri 24 Mar 2017 02:27:39 PM CST
;; # Content: 使用pickle模块将数据对象保存到文件
(defun fwar34/insert-python()
  "Insert file describe for python file"
  (interactive)
  (unless (equal system-type 'windows-nt)
    (insert "#!/usr/bin/env python3\n"))
  (insert "#-*- coding: utf-8 -*-\n\n")
  (insert "# File Name: ")
  (insert (buffer-name))
  (insert "\n# Author: Feng\n")
  (insert "# Created Time: ")
  (insert (current-time-string))
  (insert "\n# Content: ")
  )

(defun fwar34/insert-lisp-commit ()
  "Insert lisp commit"
  (interactive)
  (insert ";;-------------------------------------------------------------\n")
  (insert ";; \n")
  (insert ";;-------------------------------------------------------------")
  )

;; reference from http://ergoemacs.org/emacs/elisp_run_current_file.html
(defvar fwar34-run-current-file-before-hook nil "Hook for `fwar34/run-current-file'. Before the file is run.")
(defvar fwar34-run-current-file-after-hook nil "Hook for `fwar34/run-current-file'. After the file is run.")
(defun fwar34/run-current-go-file ()
  "Run or build current golang file.
To build, call `universal-argument' first."
  (interactive)
  (unless (buffer-file-name) (save-buffer))
  (when (buffer-modified-p) (save-buffer))
  (let ((resize-mini-window-internal nil)
        ($filename (buffer-file-name))
        ($prog-name "go")
        $cmd-str)
    (setq $cmd-str (concat $prog-name " \"" $filename "\""))
    (if current-prefix-arg
        (setq $cmd-str (format "%s build \"%s\" " $prog-name $filename))
      (setq $cmd-str (format "%s run \"%s\"" $prog-name $filename)))
    (message "running %s" $filename)
    (message "%s" $cmd-str)
    (shell-command $cmd-str))
  )
(defun fwar34/run-current-file ()
  "Execute the current file.

For example, if the current buffer is x.py, then it'll call 「python x.py」 in a shell.
Output is printed to buffer “*Shell Command Output*”.

The file can be Emacs Lisp, Python, golang, Bash.
File suffix is used to determine what program to run.

If the file is modified or not saved, save it automatically before run.

URL `http://ergoemacs.org/emacs/elisp_run_current_file.html'"
  (interactive)
  (let ((resize-mini-window-internal nil)
        ($suffix-map
         ;; (‹extension› . ‹shell program name›)
         `(("sh" . "bash")
           ("go" . "go run")
           ("py" . ,(if (string-equal system-type "windows-nt") "python" "python3"))
           ("rs" . "cargo run")
           ("cpp" . "g++ -pthread -std=c++11")
           ("java" . "javac")
           ("c". "gcc")))
        $filename
        $file-suffix
        $prog-name
        $cmd-str)
    (unless (buffer-file-name) (save-buffer))
    (when (buffer-modified-p) (save-buffer))
    (setq $filename (buffer-file-name))
    (setq $file-suffix (file-name-extension $filename))
    (setq $prog-name (cdr (assoc $file-suffix $suffix-map)))
    (setq $cmd-str (concat $prog-name " \"" $filename "\""))
    (run-hooks 'fwar34-run-current-file-before-hook)
    (cond
     ((string-equal $file-suffix "el")
      (load $filename))
     ((string-equal $file-suffix "go")
      (fwar34/run-current-go-file))
     ((string-equal $file-suffix "java")
      (shell-command (format "java %s" (file-name-sans-extension (file-name-nondirectory $filename)))))
     (t (if $prog-name
            (progn
              (message "Running")
              (shell-command $cmd-str))
          (error "No recognized program file suffix for this file.")))
     )
    (run-hooks 'fwar34-run-current-file-after-hook)))

;; inspired by http://ergoemacs.org/emacs/elisp_copy-paste_register_1.html
(defun fwar34/highlight-yank-content ()
  "Highlight yank content in red color"
  (interactive)
  (let ((yank-content (substring-no-properties (get-register ?0)))
        start
        end)
    (message yank-content)))

;; evil-yank (beg end type register yank-handler)
(defun test-evil-set-register (&rest _)
  (message (get-register ?0))
  (highlight-regexp (substring-no-properties (get-register ?0)) (facep 'hl-yellow))
  )

;; (advice-add #'evil-set-register :after #'test-evil-set-register)

;; http://ergoemacs.org/emacs/elisp_text_properties.html
(defun fwar34/color (beg end &optional color)
  (with-silent-modifications
    (put-text-property beg end 'font-lock-face `(:background ,color :foreground ,color))))

;; 重新载入emacs配置
(defun mage-reload-config()
  (interactive)
  (load user-init-file nil t))

;; define function to shutdown emacs server instance
(defun server-shutdown ()
  "Save buffers, Quit, and Shutdown (kill) server"
  (interactive)
  (save-some-buffers)
  (kill-emacs))

;; https://emacs.stackexchange.com/questions/10967/how-to-examine-the-new-line-characters-in-emacs
;; also save in Youdao Note
(defun convert-file-to-utf8-unix ()
  "convert current file to utf-8 and end of line is unix"
  (interactive)
  (set-buffer-file-coding-system 'utf-8-unix)
  )

;; ----------------------------------------------------------------------------------------------------
;; https://www.emacswiki.org/emacs/FullScreen#toc21
(defun toggle-fullscreen ()
  "Toggle full screen on X11"
  (interactive)
  (when (eq window-system 'x)
    (set-frame-parameter
     nil 'fullscreen
     (when (not (frame-parameter nil 'fullscreen)) 'fullboth))))
;; ----------------------------------------------------------------------------------------------------
(defvar my-fullscreen-p t "Check if fullscreen is on or off")

(defun my-non-fullscreen ()
  (interactive)
  (if (fboundp 'w32-send-sys-command)
	  ;; WM_SYSCOMMAND restore #xf120
	  (w32-send-sys-command 61728)
	(progn (set-frame-parameter nil 'width 82)
		   (set-frame-parameter nil 'fullscreen 'fullheight))))

(defun my-fullscreen ()
  (interactive)
  (if (fboundp 'w32-send-sys-command)
	  ;; WM_SYSCOMMAND maximaze #xf030
	  (w32-send-sys-command 61488)
	(set-frame-parameter nil 'fullscreen 'fullboth)))

(defun my-toggle-fullscreen ()
  (interactive)
  (setq my-fullscreen-p (not my-fullscreen-p))
  (if my-fullscreen-p
	  (my-non-fullscreen)
	(my-fullscreen)))
;; ----------------------------------------------------------------------------------------------------
(defun my-display-function ()
  "display function declared"
  (interactive)
  (save-excursion

    (beginning-of-defun)
    (setq begin (point))
    (evil-end-of-line)
    (setq end (point))
    (setq end-char (buffer-substring-no-properties end (+ 1 end)))
    ;; 去除每行末尾的空字符
    (while (string= end-char " ")
      (setq end (- end 1))
      (setq end-char (buffer-substring-no-properties end (+ 1 end))))

    (if (string= end-char "{")
        ;; (display-message-or-buffer (buffer-substring begin end)) ;; 最后一个字符是{的话去除最后个{后输出
        (message (buffer-substring begin end)) ;; 最后一个字符是{的话去除最后个{后输出
      (progn
        (while (not (string= end-char ")"))
          (evil-next-line)
          (evil-end-of-line)
          (setq end (point))
          (setq end-char (buffer-substring-no-properties end (+ 1 end))))
        ;; (message (buffer-substring-no-properties begin end)))
        ;; (display-message-or-buffer (buffer-substring begin (+ 1 end)))
        (message (buffer-substring begin (+ 1 end))))))
  )

(defun my-test-point ()
  (interactive)
  (print (point)))

(defun my-test-point-eol ()
  (interactive)
  (print (point-at-eol)))

(defun my-commands (program &rest program-args)
  (interactive)
  (switch-to-buffer (get-buffer-create "*command-output*"))
  (erase-buffer)
  (with-current-buffer "*command-output*"
    (evil-local-set-key 'normal (kbd "q") #'evil-buffer))
      ;; (start-process "my-make" "*make-output*" "make")
      (apply #'start-process "mycommands" "*command-output*" program program-args)
  (goto-char (point-max)))

(defun my-commands-shell (command)
  (interactive)
  (switch-to-buffer (get-buffer-create "*command-output*"))
  (erase-buffer)
  (with-current-buffer "*command-output*"
    (evil-local-set-key 'normal (kbd "q") #'evil-buffer))
      ;; (start-process "my-make" "*make-output*" "make")
      (start-process-shell-command "mycommands" "*command-output*" command)
  (goto-char (point-max)))

(defun my-commands-shell-change-default-directory (command)
  (let ((default-directory (string-join (butlast (split-string buffer-file-name "/") 1) "/")))
    (start-process-shell-command "mycommands" "*command-output*" command)
    (switch-to-buffer (get-buffer-create "*command-output*"))
    (erase-buffer)
    (with-current-buffer "*command-output*"
      (evil-local-set-key 'normal (kbd "q") #'evil-buffer))
    ;; (start-process "my-make" "*make-output*" "make")
    (goto-char (point-max))))

(defun my-change-default-directory ()
  (interactive)
  (setq default-directory (string-join (butlast (split-string buffer-file-name "/") 1) "/"))
  (message "change default-directory to %s" (string-join (butlast (split-string buffer-file-name "/") 1) "/")))

(defun interrupt-my-commands ()
  (interactive)
  (interrupt-process "*command-output*"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-is-char (c)
  (or ;; (= c ?.)
      (= c ?-)
      (= c ?_)
      (and (>= c ?0) (<= c ?9))
      (and (>= c ?A) (<= c ?Z))
      (and (>= c ?a) (<= c ?z))))
(defun my-word-at-point (&optional search-back)
  "my search word at point function"
  (let* ((ret (char-to-string (following-char)))
         (b (line-beginning-position))
         (e (line-end-position)))

    ;; backward
    (when search-back
      (save-excursion
        (backward-char)
        (while (and (>= (point) b) (my-is-char (following-char)))
          (setq ret (concat (char-to-string (following-char)) ret))
          (backward-char))))

    ;; forward
    (save-excursion
      (forward-char)
      (while (and (< (point) e) (my-is-char (following-char)))
        (setq ret (concat ret (char-to-string (following-char))))
        (forward-char)))
    ret))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; {{{
(defun my-search-whole-word (fn)
  (interactive)
  (funcall fn (my-word-at-point t)))

(defun my-search-forward-word (fn)
  (interactive)
  (funcall fn (my-word-at-point)))

(defun my-swiper-forward-word ()
  (interactive)
  (swiper (my-word-at-point)))
;; }}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; {{{
;; https://emacs-china.org/t/emacs-builtin-mode/11937/63
;; emacs builtin transient
(defun transient-winner-undo ()
  "Transient version of `winner-undo'."
  (interactive)
  (winner-mode)
  (message "Winner: [u]ndo [r]edo")
  (set-transient-map
   (let ((map (make-sparse-keymap)))
     (define-key map [?u] #'winner-undo)
     (define-key map [?r] #'winner-redo)
     map)
   t))
;; }}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; {{{
(defun my-transient-yank ()
  (interactive)
  (message "Yank: [t]ing at point [r]egister")
  (set-transient-map
   (let ((map (make-sparse-keymap)))
     (define-key map [?t] #'isearch-yank-kill)
     (define-key map [?r] #'ivy-yank-word)
     map)
   t))
;; }}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; {{{
;; (defun my-vc-operator ()
;;   (interactive)
;;   (if (string-match "Git" vc-mode)
;;       (let ((branch (substring-no-properties vc-mode 5)))
;;         (message "branch %s" branch)
;;         ;; (vc-pull (concat "origin " branch))
;;         ;; (start-process "git pull" (concat "git pull origin " branch))
;;         )
;;     (vc-pull)
;;     ))
;; }}}

(provide 'init-minefunc)
