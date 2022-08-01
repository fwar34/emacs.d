;;; init-web.el --- Useful preset transient commands  -*- coding: utf-8; lexical-binding: t; no-byte-compile: t -*-
;;; Commentary:

;;; Code:

;; js2-mode setting
(use-package js2-mode
  :mode "\\.js\\'")

(use-package web-mode
  :mode
  (("\\.phtml\\'" . web-mode)
   ("\\.tpl\\.php\\'" . web-mode)
   ("\\.[agj]sp\\'" . web-mode)
   ("\\.as[cp]x\\'" . web-mode)
   ("\\.erb\\'" . web-mode)
   ("\\.mustache\\'" . web-mode)
   ("\\.djhtml\\'" . web-mode)
   ("\\.html?\\'" . web-mode))
  :config
  (define-key web-mode-map (kbd "C-n") 'web-mode-tag-match))

(use-package emmet-mode
  :straight
  (:host github :repo "smihica/emmet-mode")
  :hook
  ((sgml-mdoe . emmet-mode)    ;; Auto-start on any markup modes
   (html-mode . emmet-mode)    ;; enable Emmet's css abbreviation.
   (web-mode . emmet-mode)
   (css-mode . emmet-mode)))

(provide 'init-web)
;;; init-web.el ends here
