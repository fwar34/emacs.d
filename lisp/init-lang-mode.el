;;; init-lang-mode.el --- Useful preset transient commands  -*- coding:utf-8; lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(use-package vimrc-mode
  :mode "\\.vim\\(rc\\)?\\'")

(use-package go-mode
  :mode "\\.go\\'")

(use-package rust-mode
  :mode "\\.rs\\'")

(use-package markdown-mode
  :mode
  (("README\\.md\\'" . gfm-mode)
   ("\\.md\\'" . markdown-mode)
   ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "multimarkdown"))

(use-package thrift
  :mode
  ("\\.thrift\\'" . thrift-mode))

;; The mode breaks with newer emacs: Unknown rx form ‘symbol’
;; https://github.com/immerrr/lua-mode/issues/155#issuecomment-723492284
(use-package lua-mode
  :mode "\\.lua\\'"
  :interpreter "lua"
  :config
  ;; (add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
  ;; (add-to-list 'interpreter-mode-alist '("lua" . lua-mode))
  (setq lua-indent-level 4)
  (setq lua-indent-string-contents t)
  (setq lua-prefix-key nil))

(use-package yaml-mode
  :mode "\\.yml\\'")

(use-package protobuf-mode
  :mode "\\.proto\\'")

(provide 'init-lang-mode)
;;; init-lang-mode.el ends here
