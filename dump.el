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
(setq dump-packages '(
                      zenburn-theme
                      company
                      company-ctags
                      company-statistics
                      company-c-headers
                      ivy
                      counsel
                      general
                      use-package
                      which-key 
                      swiper
                      winner
                      elec-pair 
                      rainbow-delimiters
                      highlight-parentheses 
                      savehist 
                      expand-region
                      ;; magit
                      ;;
                      undo-tree
                      hungry-delete
                      transient
                      ivy-xref
                      ivy-rich
                      ;; js2-mode
                      ;; web-mode
                      goto-chg
                      ;;
                      popwin
                      winum
                      ;; neotree
                      beacon
                      symbol-overlay
                      ;;
                      rainbow-mode
                      fix-word
                      ;; markdown-mode
                      highlight-numbers
                      highlight-quoted
                      highlight-defined
                      highlight-parentheses
                      imenu-list
                      git-gutter
                      ;;
                      ace-popup-menu
                      volatile-highlights
                      ;; dired
                      ;; dired-k
                      ;; dired-single
                      lispy
                      highlight-symbol
                      ;; multi-term
                      ;; thrift
                      ;; lua-mode
                      ;; cider
                      god-mode
                      ;;
                      indent-guide
                      ;; caps-lock
                      perspective
                      ;; info-colors
                      form-feed
                      ;; cc-mode
                      ;;
                      ;; go-mode
                      ;; rust-mode
                      unicad
                      recentf
                      hideshow
                      isearch
                      ;; ediff
                      hydra
                      wgrep
                      projectile
                      avy
                      ))

(dolist (package dump-packages)
  (print package)
  (require package))

(message "dump require complete.")

;; pre-load themes
;; (load-theme 'zenburn t t)
;; dump image
(dump-emacs-portable "~/.emacs.d/emacs.pdmp")
