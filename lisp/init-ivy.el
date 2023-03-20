;;; init-ivy.el --- Ivy settings -*- coding:utf-8; lexical-binding: t; -*-
;;; Commentary:

;;; Code:

;; {{{
;; C-n下一个选项
;; C-p上一行选项
;; M-<第一个选项
;; M->最后一个选项
;; C-v向下翻页，页数由ivy-height确定
;; M-v向上翻页，同样页数由ivy-height确定
;; M-p上一条历史记录
;; M-n下一条历史记录
;; }}}
(use-package counsel
  ;; counsel repository contains:
  ;; Ivy, a generic completion mechanism for Emacs.
  ;; Counsel, a collection of Ivy-enhanced versions of common Emacs commands.
  ;; Swiper, an Ivy-enhanced alternative to isearch.
  :bind
  (([remap switch-to-buffer] . ivy-switch-buffer)
   ([remap isearch-forward] . swiper)
   ([remap describe-function] . counsel-describe-function)
   ([remap describe-variable] . counsel-describe-variable)
   ([remap describe-symbol] . counsel-describe-symbol)
   ([remap hungry-delete-backward] . ivy-backward-delete-char)
   ([remap find-file] . counsel-find-file)
   ("M-x" . counsel-M-x)
   :map ivy-minibuffer-map
   ("M-l" . ivy-restrict-to-matches)
   ;; ("C-w" . backward-kill-word) ;; 在ivy中已经将backward-kill-word remap成了ivy-backward-kill-word
   ("C-w" . ivy-backward-kill-word))
  :config
  ;; https://melpa.org/#/ivy-hydra
  (use-package ivy-hydra)

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
        ivy-format-function 'ivy-format-function-line
        ivy-use-virtual-buffers t
        ivy-count-format "%d/%d ")
  (ivy-mode 1) ;; M-j ivy-yank-word，将光标的word读入minibuffer，很像vim中的功能
  ;; C-y yank，可以在minibuffer中粘贴
  ;; 默认就是fancy
  ;; (setq ivy-display-style 'fancy)
  ;; (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)
  (setq enable-recursive-minibuffers t)
  (setq ivy-use-selectable-prompt t)
  ;; (define-key ivy-minibuffer-map (kbd "C-i") 'counsel-evil-registers)
  ;; (define-key isearch-mode-map (kbd "C-i") 'counsel-evil-registers)
  ;; (define-key isearch-mode-map (kbd "C-n") 'ivy-next-line)
  ;; (define-key isearch-mode-map (kbd "C-p") 'ivy-previous-line)
  ;; (define-key ivy-minibuffer-map (kbd "C-r") 'counsel-minibuffer-history)
  ;; (global-set-key (kbd "C-h f") 'counsel-describe-function)
  ;; (global-set-key (kbd "C-h v") 'counsel-describe-variable)

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
                                  "--iglob" "!*.html"
                                  "."))

  ;; https://emacs-china.org/t/emacs-helm-ag/6764
  ;; 支持中文搜索，但是只有两个汉字以上才能搜索到结果，还不清楚原因
  ;; (when (equal system-type 'windows-nt)
  ;; win10如果默认改成了utf8编码则不需要底下这个配置
  ;;   (modify-coding-system-alist 'process "ag" '(utf-8 . chinese-gbk-dos))
  ;;   (modify-coding-system-alist 'process "rg" '(utf-8 . chinese-gbk-dos)))
  (transient-define-prefix my-ivy-minibuffer-transient ()
    "my ivy minibuffer commands"
    [[" <ivy commands>"
      ("RET" "ivy-done" ivy-done)
      ("," "self insert \",\"" self-insert-command)
      ("s" "ivy-restrict-to-matches" ivy-restrict-to-matches)
      ("d" "swiper-avy" swiper-avy)
      ("c" "ivy-occur" ivy-occur)
      ("f" "ivy-call" ivy-call :transient t) ;; M-return
      ("a" "ivy-beginning-of-buffer" ivy-beginning-of-buffer :transient t)
      ("e" "ivy-end-of-buffer" ivy-end-of-buffer :transient t)
      ("j" "ivy-next-line-and-call" ivy-next-line-and-call :transient t)
      ("k" "ivy-previous-line-and-call" ivy-previous-line-and-call :transient t)
      ("J" "ivy-immediate-done" ivy-immediate-done)
      ("p" "clipboard-yank" clipboard-yank)
      ("P" "yank-from-kill-ring" yank-from-kill-ring)]])
  (bind-key "," 'my-ivy-minibuffer-transient ivy-minibuffer-map)

  (use-package amx
    :config
    (amx-mode))

  (use-package ivy-xref
    :init
    ;; xref initialization is different in Emacs 27 - there are two different
    ;; variables which can be set rather than just one
    (when (>= emacs-major-version 27)
      (setq xref-show-definitions-function 'ivy-xref-show-defs))
    ;; Necessary in Emacs <27. In Emacs 27 it will affect all xref-based
    ;; commands other than xref-find-definitions (e.g. project-find-regexp)
    ;; as well
    (setq xref-show-xrefs-function 'ivy-xref-show-xrefs))

  ;; {{{
  ;; https://emacs-china.org/t/ivy-occur/12083
  (defvar ivy-occur-filter-prefix ">>> ")

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

  (defun ivy-occur/undo ()
    (interactive)
    (let ((inhibit-read-only t))
      (if (save-excursion
            (goto-char (point-min))
            (looking-at ivy-occur-filter-prefix))
          (undo)
        (user-error "Filter stack is empty"))))

  (defun ivy|occur-mode-setup ()
    (local-set-key "/" 'ivy-occur/filter-lines)
    (local-set-key (kbd "M-/") 'ivy-occur/undo))

  (add-hook 'ivy-occur-mode-hook 'ivy|occur-mode-setup)
  (add-hook 'ivy-occur-grep-mode-hook 'ivy|occur-mode-setup)
  ;; }}}
  )

(use-package ivy-rich
  :hook
  (ivy-mode . ivy-rich-mode)
  :config
  (setq ivy-format-function 'ivy-format-function-line)
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
  ;; To abbreviate paths using abbreviate-file-name (e.g. replace “/home/username” with “~”)
  (setq ivy-rich-path-style 'abbrev)
  (ivy-rich-project-root-cache-mode))

(provide 'init-ivy)
;;; init-ivy.el ends here
