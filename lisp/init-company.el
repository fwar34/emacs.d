;;; init-company.el --- Useful preset transient commands  -*- coding:utf-8; lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(use-package company
  :demand t
  ;; :custom
  ;; (company-minimum-prefix-length 1)
  :bind
  (:map company-active-map
        ("C-w" . backward-kill-word)
        ("C-u" . company-previous-page)
        ("C-d" . company-next-page)
        ("C-j" . company-filter-candidates)
        ("C-s" . counsel-company)
        ;; ("C-m" . company-complete-selection) ;; RET选中当前补全选项(RET和C-m关系@see http://zhangley.com/article/emacs-ret/)
        ("C-m" . (lambda () (interactive)
                   (if company-selection
                       (company-complete-selection)
                     (if (equal major-mode 'eshell-mode)
                         (eshell-send-input)
                       (newline-and-indent))))) ;; RET判断当前是否有选中的补全，如果有则直接补全，如果没有就换行(排除eshell-mode)
        ("C-l" . yas-expand)) ;; C-l 在终端下与 tmux 的 prefix 键冲突，所以又用 general-define-key 设置了下面的 mapping
  :config
  (global-company-mode)
  (setq company-text-face-extra-attributes '(:weight bold :slant italic))
  
  (general-define-key
   :keymaps 'company-active-map
   :prefix "`"
   "`" 'self-insert-command
   "l" 'yas-expand)

  ;; (setq company-auto-commit t)
  ;; 32 空格, 41 右圆括号, 46 是 dot 字符
  ;; 这里我们移除空格，添加逗号(44), 分号(59)
  ;; 注意： C-x = 用来检测光标下字符的数字，(insert 数字) 用来测试数字对应的字符。
  ;; (setq company-auto-commit-chars '(41 46 44 59 13))

  ;; Number the candidates (use M-1, M-2 etc to select completions).
  ;; (setq company-show-numbers t)
  (setq company-show-quick-access 'left)
  ;; make previous/next selection in the popup cycles
  (setq company-selection-wrap-around t)
  ;; Some languages use camel case naming convention,
  ;; so company should be case sensitive.
  ;; (setq company-dabbrev-ignore-case nil)
  (setq company-dabbrev-ignore-case t)
  ;; Trigger completion immediately.
  ;; (setq company-idle-delay 0.3)
  ;; I don't like the downcase word in company-dabbrev!
  (setq company-dabbrev-downcase nil
        company-clang-insert-arguments nil
        company-require-match nil
        company-etags-ignore-case t)

  ;; {{{
  ;; 使用1-9来选择company补全选项, 0来使用company-filter-candidates(使用my-company-zero-key-for-filter来开关)
  ;; https://emacs-china.org/t/tab-company-yasnippet/15590/9
  ;; https://github.com/abo-abo/oremacs/blob/9c1dd95f52bd6f65313c50c1a85c8bacdde74581/modes/ora-company.el
  (defvar my-company-zero-key-for-filter nil
    "If t, pressing 0 calls `company-filter-candidates' per company's status.")

  (defun ora-company-number ()
    "Forward to `company-complete-number'.
Unless the number is potentially part of the candidate.
In that case, insert the number."
    (interactive)
    (let* ((k (this-command-keys))
           (re (concat "^" company-prefix k))
           (n (if (equal k "0") 10 (string-to-number k)))
           ;; 数字、小数直接输入
           (digits "^[[:digit:].]+"))
      (cond
       ((or (cl-find-if (lambda (s) (or (string-match re s) (string-match digits s))) company-candidates)
            (> n (length company-candidates))
            (looking-back "[0-9]+\\.[0-9]*" (line-beginning-position)))
        (self-insert-command 1))

       ((and (eq n 10) my-company-zero-key-for-filter)
        (company-filter-candidates))

       (t
        (company-complete-number n)))))

  ;;   (defun ora-company-number ()
  ;;     "Forward to `company-complete-number'.
  ;; Unless the number is potentially part of the candidate.
  ;; In that case, insert the number."
  ;;     (interactive)
  ;;     (let* ((k (this-command-keys))
  ;;            (re (concat "^" company-prefix k)))
  ;;       (if (or (cl-find-if (lambda (s) (string-match re s))
  ;;                           company-candidates)
  ;;               (> (string-to-number k)
  ;;                  (length company-candidates))
  ;;               (looking-back "[0-9]+\\.[0-9]*" (line-beginning-position)))
  ;;           (self-insert-command 1)
  ;;         (company-complete-number
  ;;          (if (equal k "0")
  ;;              10
  ;;            (string-to-number k))))))

  ;; 0-9 来选择补全
  (let ((map company-active-map))
    (mapc (lambda (x) (define-key map (format "%d" x) 'ora-company-number))
          (number-sequence 0 9))
    ;; (define-key map " " (lambda ()
    ;;                       (interactive)
    ;;                       (company-abort)
    ;;                       (self-insert-command 1)))
    ;; (define-key map (kbd "<return>") nil)
    )

  ;; 自定义前面的补全数字提示
  ;; (defun ora--company-good-prefix-p (orig-fn prefix)
  ;;   (unless (and (stringp prefix) (string-match-p "\\`[0-9]+\\'" prefix))
  ;;     (funcall orig-fn prefix)))
  ;; (advice-add 'company--good-prefix-p :around 'ora--company-good-prefix-p)
  ;; }}}

  ;; {{{
  ;; https://emacs-china.org/t/company/17187/2
  ;; company 候选项去重，将去重后的所有候选项 newseq 返回， company-transformers 就是来修改候选项的变量
  (defun eye/company-remove-dups (candidates)
    (let ((newseq))
      (mapcar #'(lambda (c) (if (not (member c newseq)) (add-to-list 'newseq c))) candidates)
      newseq))
  ;; (add-to-list 'company-transformers 'eye/company-remove-dups)
  ;; }}}
  )

  (with-eval-after-load 'company
    ;; (if (fboundp 'evil-declare-change-repeat)
    ;;     (mapc 'evil-declare-change-repeat
    ;;           '(company-complete-common
    ;;             company-select-next
    ;;             company-select-previous
    ;;             company-complete-selection
    ;;             company-complete-number)))
    ;; 使用 c-n/c-p 来选择 company 的候选补全项
    (define-key company-active-map (kbd "M-n") nil)
    (define-key company-active-map (kbd "M-p") nil)
    (define-key company-active-map (kbd "TAB") 'company-select-next-if-tooltip-visible-or-complete-selection)
    (define-key company-active-map (kbd "C-n") 'company-select-next)
    (define-key company-active-map (kbd "C-p") 'company-select-previous)
    ;; (add-to-list 'company-backends 'company-cmake)
    ;; (add-to-list 'company-backends 'company-c-headers)
    ;; can't work with TRAMP
    (setq company-backends (delete 'company-ropemacs company-backends))
    (setq company-backends (delete 'company-capf company-backends))
    (setq company-backends (delete 'company-clang company-backends))


    ;; @see https://github.com/redguardtoo/emacs.d/commit/2ff305c1ddd7faff6dc9fa0869e39f1e9ed1182d
    (defadvice company-in-string-or-comment (around company-in-string-or-comment-hack activate)
      ;; you can use (ad-get-arg 0) and (ad-set-arg 0) to tweak the arguments
      (if (memq major-mode '(php-mode html-mode web-mode nxml-mode))
          (setq ad-return-value nil)
        ad-do-it))

    ;; press SPACE will accept the highlighted candidate and insert a space
    ;; `M-x describe-variable company-auto-complete-chars` for details
    ;; That's BAD idea.
    (setq company-auto-complete nil)

    ;; NOT to load company-mode for certain major modes.
    ;; Ironic that I suggested this feature but I totally forgot it
    ;; until two years later.
    ;; https://github.com/company-mode/company-mode/issues/29
    (setq company-global-modes
          '(not
            eshell-mode comint-mode erc-mode gud-mode rcirc-mode
            minibuffer-inactive-mode)))

  (use-package company-tabnine
    :disabled
    :ensure t
    :config
    (add-to-list 'company-backends 'company-tabnine))

  ;; https://github.com/redguardtoo/company-ctags
  (use-package company-ctags
    :ensure t
    :after company
    :config
    (setq company-ctags-extra-tags-files '("$HOME/TAGS" "/usr/include/c++/TAGS"))
    (company-ctags-auto-setup)

    ;; Use rusty-tags to generate tags file for Rust programming language.
    ;; Add below code into ~/.emacs,
    (setq company-ctags-tags-file-name "rusty-tags.emacs")

    ;; company-ctags-ignore-case
    (setq company-ctags-ignore-case t))

  ;; @see https://github.com/company-mode/company-mode/issues/348
  (use-package company-statistics
    :ensure t
    :after company
    :config
    (company-statistics-mode))

  (use-package company-c-headers
    :ensure t
    :config
    (add-to-list 'company-backends 'company-c-headers))

(provide 'init-company)
;;; init-company.el ends here
