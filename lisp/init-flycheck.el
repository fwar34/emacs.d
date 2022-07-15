;;; init-flycheck.el --- Useful preset transient commands  -*- coding:utf-8; lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(eval-when-compile (require 'use-package))

(use-package flycheck
  :ensure t
  ;; https://stackoverflow.com/questions/53697743/use-package-with-config-function-might-not-be-available-at-runtime
  ;; https://github.com/jwiegley/use-package/issues/838#issuecomment-629865020
  :functions flycheck-add-mode
  :init
  (global-flycheck-mode)
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  :hook
  (after-init . global-flycheck-mode)
  :config
  (require 'flycheck)
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  )

(provide 'init-flycheck)
;;; init-flycheck.el ends here
