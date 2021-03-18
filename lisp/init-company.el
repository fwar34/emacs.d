;; -*- coding: utf-8; lexical-binding: t; -*-

;; 开启全局company
(use-package company
  :ensure t
  :config
  (global-company-mode)
  ;; (add-hook 'after-init-hook 'global-company-mode)
  ;; Number the candidates (use M-1, M-2 etc to select completions).
  (setq company-show-numbers t)
  ;; make previous/next selection in the popup cycles
  (setq company-selection-wrap-around t) 
  ;; Some languages use camel case naming convention,
  ;; so company should be case sensitive.
  (setq company-dabbrev-ignore-case nil)
  ;; Trigger completion immediately.
  (setq company-idle-delay 0)
  ;; I don't like the downcase word in company-dabbrev!
  (setq company-dabbrev-downcase nil
        company-clang-insert-arguments nil
        company-require-match nil
        company-etags-ignore-case t)
  )

(if (fboundp 'evil-declare-change-repeat)
    (mapc #'evil-declare-change-repeat
          '(company-complete-common
            company-select-next
            company-select-previous
            company-complete-selection
            company-complete-number
            )))

(with-eval-after-load 'company
  ;; (add-to-list 'company-backends 'company-cmake)
  (add-to-list 'company-backends 'company-c-headers)
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
          minibuffer-inactive-mode))
  ;; reference from https://github.com/zilongshanren/spacemacs-private/commit/58d2f890af1ba65ce81d7a0b814a147dba90227c
  ;; company-mode，可以使用数字键来选取 condicates
  ;; (defun ora-company-number ()
  ;;   "Forward to `company-complete-number'.
  ;;    Unless the number is potentially part of the candidate.
  ;;    In that case, insert the number."
  ;;   (interactive)
  ;;   (let* ((k (this-command-keys))
  ;;          (re (concat "^" company-prefix k)))
  ;;     (if (cl-find-if (lambda (s) (string-match re s))
  ;;                     company-candidates)
  ;;         (self-insert-command 1)
  ;;       (company-complete-number (string-to-number k)))))

  ;; (let ((map company-active-map))
  ;;   (mapc
  ;;    (lambda (x)
  ;;      (define-key map (format "%d" x) 'ora-company-number))
  ;;    (number-sequence 0 9))
  ;;   (define-key map " " (lambda ()
  ;;                         (interactive)
  ;;                         (company-abort)
  ;;                         (self-insert-command 1)))
  ;;   (define-key map (kbd "<return>") nil))
  )

;; {{ setup company-ispell
(defun toggle-company-ispell ()
  (interactive)
  (cond
   ((memq 'company-ispell company-backends)
    (setq company-backends (delete 'company-ispell company-backends))
    (message "company-ispell disabled"))
   (t
    (add-to-list 'company-backends 'company-ispell)
    (message "company-ispell enabled!"))))

(defun company-ispell-setup ()
  ;; @see https://github.com/company-mode/company-mode/issues/50
  (when (boundp 'company-backends)
    (make-local-variable 'company-backends)
    (add-to-list 'company-backends 'company-ispell)
    ;; https://github.com/redguardtoo/emacs.d/issues/473
    (if (and (boundp 'ispell-alternate-dictionary)
             ispell-alternate-dictionary)
        (setq company-ispell-dictionary ispell-alternate-dictionary))))

;; message-mode use company-bbdb.
;; So we should NOT turn on company-ispell
;; (add-hook 'org-mode-hook 'company-ispell-setup)
;; }}

;; (eval-after-load 'company-etags
;;   '(progn
;;      ;; insert major-mode not inherited from prog-mode
;;      ;; to make company-etags work
;;      (add-to-list 'company-etags-modes 'web-mode)
;;      (add-to-list 'company-etags-modes 'c-mode)
;;      (add-to-list 'company-etags-modes 'c++-mode)
;;      (add-to-list 'company-etags-modes 'lua-mode)))

(when (and (equal system-type 'gnu/linux) nil)
  (use-package lsp-mode :commands lsp)
  (use-package lsp-ui :commands lsp-ui-mode)
  (use-package company-lsp :commands company-lsp)
  ;; (use-package ccls
  ;;   :hook
  ;;   ((c-mode c++-mode objc-mode cuda-mode) . (lambda () (require 'ccls) (lsp))))

  (use-package emacs-ccls
    :disabled
    :defer t
    :config
    ;; (setq ccls-executable "/path/to/ccls/Release/ccls")
    ;; (setq ccls-args '("--log-file=/tmp/ccls.log"))
    )
  )

(use-package company-tabnine
  :ensure t
  :config
  (add-to-list 'company-backends #'company-tabnine))

(use-package irony
  :disabled
  :ensure t
  :defer t
  :config
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'objc-mode-hook 'irony-mode)

  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  (message "irony")
  )

(use-package company-irony
  :disabled
  :ensure t
  :defer t
  :config
  (add-to-list 'company-backends 'company-irony)
  (message "company-irony")
  )

(use-package ycmd
  :disabled
  :ensure t
  :config
  ;; (add-hook 'after-init-hook #'global-ycmd-mode)
  (add-hook 'c++-mode-hook 'ycmd-mode)
  ;; (set-variable 'ycmd-server-command `("python3" ,(file-truename "~/Downloads/YouCompleteMe/third_party/ycmd/ycmd")))
  (set-variable 'ycmd-server-command `("python3" ,(file-truename "/usr/share/vim/vimfiles/third_party/ycmd/ycmd")))
  )

(use-package company-ycmd
  :disabled
  :ensure t
  :config
  ;; (require 'company-ycmd)
  (company-ycmd-setup)
  )

;; https://github.com/redguardtoo/company-ctags
(use-package company-ctags
  :ensure t
  :after company
  :config
  (setq company-ctags-extra-tags-files '("$HOME/TAGS" "/usr/include/c++/TAGS"))
  (company-ctags-auto-setup)

  ;; Set company-ctags-fuzzy-match-p to fuzzy match the candidates.
  ;; The input could match any part of the candidate instead of the beginning of the candidate.
  ;; Here is example how to combine counsel with company-ctags,
  (require 'counsel)
  (defun my-counsel-company ()
    "Input code from company backend using fuzzy matching."
    (interactive)
    (company-abort)
    (let* ((company-backends '(company-ctags))
           (company-ctags-fuzzy-match-p t))
      (counsel-company)))

  ;; Use rusty-tags to generate tags file for Rust programming language.
  ;; Add below code into ~/.emacs,
  (setq company-ctags-tags-file-name "rusty-tags.emacs")
  )

(provide 'init-company)
