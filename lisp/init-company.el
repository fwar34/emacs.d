;; -*- coding: utf-8; lexical-binding: t; -*-

(use-package company
  :ensure t
  :custom
  (company-minimum-prefix-length 1)
  :bind
  (:map company-active-map
        ("C-w" . evil-delete-backward-word)
        ("C-u" . company-previous-page)
        ("C-d" . company-next-page)
        ;; ("C-s" . company-filter-candidates)
        ("C-s" . counsel-company))

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
  )

(use-package company-tabnine
  :disabled
  :ensure t
  :config
  (add-to-list 'company-backends #'company-tabnine))

;; https://github.com/redguardtoo/company-ctags
(use-package company-ctags
  :ensure t
  :after company
  :config
  (setq company-ctags-extra-tags-files '("$HOME/TAGS" "/usr/include/c++/TAGS"))
  (company-ctags-auto-setup)

  ;; Use rusty-tags to generate tags file for Rust programming language.
  ;; Add below code into ~/.emacs,
  (setq company-ctags-tags-file-name "rusty-tags.emacs"))

;; @see https://github.com/company-mode/company-mode/issues/348
(use-package company-statistics
  :ensure t
  :config
  (company-statistics-mode)
  )

(use-package company-c-headers
  :ensure t
  :config
  (add-to-list 'company-backends 'company-c-headers)
  )

(use-package company-jedi
  :ensure t
  :after python
  :config
  (defun my/python-mode-hook ()
    (add-to-list 'company-backends 'company-jedi))
  (add-hook 'python-mode-hook 'my/python-mode-hook)
  )

(use-package company-english-helper
  ;; write by lazycat
  :straight
  (:host github :repo "manateelazycat/company-english-helper")
  :config
  ;; toggle-company-english-helper
  )

(provide 'init-company)
