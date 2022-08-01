;;; init-c.el --- C language settings -c.el -*- coding:utf-8; lexical-binding: t; -*-
;;; Commentary:

;;; Code:

;; highlight call function exculde (if,else,while,for)
;; https://www.emacswiki.org/emacs/AddKeywords
;; https://www.emacswiki.org/emacs/FontLockKeywords
;; (font-lock-add-keywords
;;  'c-mode
;;  ;; '(("\\<\\(\\sw+\\) ?(" 1 'company-echo-common)))
;;  '(("\\<\\(\\sw+\\) *(" 1 'font-lock-function-name-face)))

;; (font-lock-add-keywords
;;  'c-mode
;;  '(("\\<\\(if\\|for\\|switch\\|while\\)\\>" . 'font-lock-keyword-face)))

;; (font-lock-add-keywords
;;  'c++-mode
;;  ;; '(("\\<\\(\\sw+\\) ?(" 1 'company-echo-common)))
;;  '(("\\<\\(\\sw+\\) *(" 1 'font-lock-function-name-face)))

;; (font-lock-add-keywords
;;  'c++-mode
;;  '(("\\<\\(if\\|for\\|switch\\|while\\)\\>" . 'font-lock-keyword-face)))

;; A highlighting printf format specifier like vim
;; https://www.emacswiki.org/emacs/AddKeywords
;; (defvar font-lock-format-specifier-face
;;   'font-lock-format-specifier-face
;;   "Face name to use for format specifiers.")

;; (defface font-lock-format-specifier-face
;;   '((t (:foreground "OrangeRed1")))
;;   "Font Lock mode face used to highlight format specifiers."
;;   :group 'font-lock-faces)

;; https://emacs-china.org/t/topic/9320/2
(add-hook
 'c-mode-common-hook
 (lambda ()
   (font-lock-add-keywords
    nil
    '(("[^%]\\(%\\([[:digit:]]+\\$\\)?[-+' #0*]*\\([[:digit:]]*\\|\\*\\|\\*[[:digit:]]+\\$\\)\\(\\.\\([[:digit:]]*\\|\\*\\|\\*[[:digit:]]+\\$\\)\\)?\\([hlLjzt]\\|ll\\|hh\\)?\\([aAbdiuoxXDOUfFeEgGcCsSpn]\\|\\[\\^?.[^]]*\\]\\)\\)"
       1
       font-lock-builtin-face
       t)
      ("\\(%%\\)"
       1
       font-lock-type-face
       t))
    t)))

;; http://maskray.me/blog/2017-12-03-c++-language-server-cquery
;; C/C++ mode hook在项目根目录有compile_commands.json时自动启用`lsp-cquery-enable
;; (defun my//enable-cquery-if-compile-commands-json ()
;;   (when
;;       (and (not (and (boundp 'lsp-mode) lsp-mode))
;;            (or
;;             (cl-some (lambda (x) (string-match-p x buffer-file-name)) my-cquery-whitelist)
;;             (cl-notany (lambda (x) (string-match-p x buffer-file-name)) my-cquery-blacklist))
;;            (or (locate-dominating-file default-directory "compile_commands.json")
;;                (locate-dominating-file default-directory ".cquery")))
;;     (setq eldoc-idle-delay 0.2)
;;     (lsp-cquery-enable)
;;     (lsp-enable-imenu)
;;     (when (>= emacs-major-version 26)
;;       (lsp-ui-doc-mode 1))))

(provide 'init-c)
;;; init-c.el ends here
