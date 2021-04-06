;; -*- coding: utf-8; lexical-binding: t; -*-

(require 'package)
;; load autoload files and populate load-path’s
(package-initialize)
;; store load-path
(setq fwar34-dumped-load-path load-path
      fwar34-dumped t)
;; (package-initialize) doens’t require each package, we need to load
;; those we want manually
(dolist (package '(use-package company ivy counsel org helpful
                    general helpful use-package which-key
                    recentf-ext swiper ivy-prescient find-char
                    aggressive-indent windman winner elec-pair 
                    rainbow-delimiters highlight-parentheses hl-todo buffer-move
                    savehist minions ws-butler
                    expand-region isolate outshine magit eglot))
  (require package))
;; pre-load themes
(load-theme 'zenburn t t)
;; dump image
(dump-emacs-portable "~/.emacs.d/emacs.pdmp")
