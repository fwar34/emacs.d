;; -*- coding: utf-8; lexical-binding: t; -*-

;;; Code:
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :hook
  (after-init . global-flycheck-mode)
  )

(provide 'init-flycheck)
