;; 禁用响铃
(setq ring-bell-function 'ignore)

;; auto reload file
(global-auto-revert-mode t)

;; 禁用备份文件
(setq make-backup-files nil)
(setq auto-save-default nil)

;; 高亮光标增强
(define-advice show-paren-function(:around (fn) fix-show-paren-function)
  "Highlight enclosing parens."
  (cond ((looking-at-p "\\s(") (funcall fn))
	(t (save-excursion
	     (ignore-errors (backward-up-list))
	     (funcall fn)))))

;; 括号匹配高亮
(add-hook 'emacs-lisp-mode-hook 'show-paren-mode)

;; 行号
(global-linum-mode 1)

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

;;
(delete-selection-mode 1)

;; abbrev
(abbrev-mode t)
(define-abbrev-table 'global-abbrev-table '(("lf" "liang.feng")
					    ))


;; 打开recent files
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-item 10)

(fset 'yes-or-no-p 'y-or-n-p)

;; dired递归copy delete
(setq dired-recursive-copies 'always)
(setq dired-recursive-deletes 'always)

(put 'dired-find-alternate-file 'disabled nil)

(require 'dired-x)

(setq dired-dwim-target t)

(provide 'init-better-default)
