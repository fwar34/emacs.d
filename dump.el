;; -*- coding: utf-8; lexical-binding: t; -*-

(require 'package)
;; load autoload files and populate load-path’s
(package-initialize)
;; store load-path
(setq fwar34-dumped-load-path load-path
      fwar34-dumped t)
;; (package-initialize) doens’t require each package, we need to load

(setq evil-want-keybinding nil)
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
                   ;;;
                   undo-tree
                   ;; evil
                   ;; evil-collection
                   hungry-delete
                   transient
                   ivy-xref
                   ivy-rich
                   js2-mode
                   web-mode
                   goto-chg
                   ;;
                   popwin
                   winum
                   ;; youdao-dictionary
                   neotree
                   ;; lispyville
                   beacon
                   symbol-overlay
                   ;;;;
                   rainbow-mode
                   fix-word
                   ;; browse-kill-ring
                   markdown-mode
                   highlight-numbers
                   highlight-quoted
                   highlight-defined
                   highlight-parentheses
                   imenu-list
                   git-gutter
                   diff-hl
                   ;; taglist
                   ;;;
                   ace-popup-menu
                   volatile-highlights
                   ))
  (require package))

;; pre-load themes
(load-theme 'zenburn t t)
;; dump image
(dump-emacs-portable "~/.emacs.d/emacs.pdmp")
