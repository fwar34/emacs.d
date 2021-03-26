;; -*- coding: utf-8; lexical-binding: t; -*-
;; define my prefix C-y
;; (define-prefix-command 'ctl-y-map)
;; (global-set-key (kbd "C-y") 'ctl-y-map)
;; define my prefix M-m
(define-prefix-command 'M-m-map)
(global-set-key (kbd "M-m") 'M-m-map)
;; swiper setting
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "M-m p") 'clipboard-yank)
(global-set-key (kbd "M-m u") 'eval-last-sexp)
;; (global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "M-m M-m") 'counsel-find-file)

;; iedit bind
;; (global-set-key (kbd "M-m oe") 'iedit-mode)
;; org bind
(global-set-key (kbd "M-m oa") 'org-agenda)
;; minefunc org template
(global-set-key (kbd "M-m oc") 'org-capture)

;; http://ergoemacs.org/emacs/emacs_dired_tips.html
;; (require 'dired )
;; (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file) ; was dired-advertised-find-file
;; (define-key dired-mode-map (kbd "^") (lambda () (interactive) (find-alternate-file "..")))  ; was dired-up-directory

;; dired重用buffer
;; (put 'dired-find-alternate-file 'disabled nil)
;; (with-eval-after-load 'dired
;;   (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
;;   (define-key dired-mode-map (kbd "^") (lambda () (interactive) (find-alternate-file ".."))))  ; was dired-up-directory


;; 使用 c-n/c-p 来选择 company 的候选补全项
(with-eval-after-load 'company
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "TAB") #'company-select-next-if-tooltip-visible-or-complete-selection)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous)
  )

;; auto-yasnippet bind
(global-set-key (kbd "H-w") #'aya-create)
(global-set-key (kbd "H-y") #'aya-expand)

;; set C-w delte a word backward
;; (global-set-key (kbd "C-w") 'backward-kill-word)
;; browse-kill-ring
(global-set-key (kbd "M-m y") 'browse-kill-ring)

;; fix-word
(global-set-key (kbd "M-u") 'fix-word-upcase)
(global-set-key (kbd "M-l") 'fix-word-downcase)
(global-set-key (kbd "M-c") 'fix-word-capitalize)

;; function-args
;; (global-set-key (kbd "M-g") 'fa-abort)

(global-set-key (kbd "M-m .") #'imenu-anywhere)
;; (global-set-key (kbd "M-o c") #'(lambda() (interactive) (set-input-method "pyim")))

;; font sizes
;; 使用C-+和C--增加和减小字体大小
;; (global-set-key [C-kp-add] 'text-scale-increase)
;; (global-set-key [C-kp-subtract] 'text-scale-decrease)

(provide 'init-keybindings)
