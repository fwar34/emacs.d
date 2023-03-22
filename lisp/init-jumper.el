;;; init-packages.el --- Packages -*- coding:utf-8; lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(use-package better-jumper
  :straight
  (:host github :repo "gilbertw1/better-jumper")
  :bind
  ("C-o" . 'better-jumper-jump-backward)
  ("C-i" . 'better-jumper-jump-forward)
  :init
  (defun my-jumper-advice-for-function (&rest _)
    (better-jumper-set-jump))
  :config
  (dolist (con '(ivy-switch-buffer
                 persp-ivy-switch-buffer
                 counsel-gtags-dwim
                 counsel-gtags-find-definition
                 counsel-gtags-find-reference
                 counsel-gtags-find-symbol
                 counsel-gtags-find-file
                 counsel-gtags-go-forward
                 counsel-gtags-go-backward
                 counsel-find-file
                 counsel-imenu
                 beginning-of-defun
                 dired-single-buffer
                 beginning-of-buffer
                 end-of-buffer
                 dired-jump
                 ;; ivy-done
                 end-of-defun
                 mark-whole-buffer
                 mark-defun
                 counsel-rg
                 counsel-ag
                 my-search-forward-word
                 my-search-whole-word
                 lispyville-beginning-of-defun
                 lispyville-end-of-defun
                 lispyville--maybe-enter-special
                 swiper
                 swiper-all
                 my-swiper-forward-word
                 swiper-thing-at-point
                 swiper-all-thing-at-point
                 find-function
                 find-variable
                 find-function-on-key
                 counsel-describe-function
                 counsel-describe-variable
                 counsel-describe-symbol
                 counsel-recentf
                 meow-visit
                 meow-reverse
                 meow-last-buffer
                 meow-query-replace
                 meow-query-replace-regexp
                 meow-goto-line
                 meow-search
                 meow-inner-of-thing
                 meow-bounds-of-thing
                 meow-beginning-of-thing
                 meow-end-of-thing
                 counsel-etags-recent-tag
                 counsel-etags-find-tag
                 counsel-etags-grep
                 counsel-etags-find-tag-at-point
                 counsel-etags-list-tag
                 counsel-etags-list-tag-in-current-file
                 xref-find-definitions
                 xref-find-references
                 avy-goto-word-0
                 avy-goto-char-2
                 scroll-up-command
                 scroll-down-command
                 git-gutter:previous-hunk
                 git-gutter:next-hunk
                 diff-hl-previous-hunk
                 diff-hl-next-hunk
                 rg-menu
                 
                 ))
    (advice-add con :before #'my-jumper-advice-for-function)))

(provide 'init-jumper)
;;; init-packages.el ends here
