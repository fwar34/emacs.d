;; -*- coding: utf-8; lexical-binding: t; -*-

(require 'package)
;; load autoload files and populate load-path’s
(package-initialize)
;; store load-path
(setq fwar34-dumped-load-path load-path
      fwar34-dumped t)
;; (package-initialize) doens’t require each package, we need to load
;; those we want manually
(dolist (package '(
                   company
                   ivy
                   counsel
                   ;; org
                   general
                   ;; use-package
                   which-key 
                   swiper
                   winner
                   elec-pair 
                   rainbow-delimiters
                   highlight-parentheses 
                   savehist 
                   expand-region
                   magit
                   ))
  (require package))
;; pre-load themes
(load-theme 'zenburn t t)
;; dump image
(dump-emacs-portable "~/.emacs.d/emacs.pdmp")
