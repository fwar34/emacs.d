;;; init-flycheck.el --- Useful preset transient commands  -*- coding:utf-8; lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(eval-when-compile (require 'use-package))

(use-package flycheck
  :disabled
  :ensure t
  ;; https://github.com/jwiegley/use-package/issues/838#issuecomment-629865020
  ;; :functions flycheck-add-mode
  :preface
  ;; https://stackoverflow.com/questions/53697743/use-package-with-config-function-might-not-be-available-at-runtime
  (declare-function flycheck-add-mode "flycheck")
  :init
  (global-flycheck-mode)
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  :hook
  (after-init . global-flycheck-mode)
  :config
  (require 'flycheck)
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  )

(defun my-flycheck-evil-insert-state-entry-hook-setup2 ()
  "Enable flycheck mode in evil-insert-state-entry-hook."
  (unless (featurep 'flycheck)
    (remove-hook 'pre-command-hook #'my-flycheck-evil-insert-state-entry-hook-setup2)
    (use-package flycheck
      :hook
      (prog-mode . flycheck-mode)
      :custom
      (flycheck-temp-prefix ".flycheck")
      (flycheck-check-syntax-automatically '(save mode-enabled))
      (flycheck-emacs-lisp-load-path 'inherit)
      (flycheck-indication-mode 'right-fringe))
    (flycheck-mode)))
; (add-hook 'pre-command-hook #'my-flycheck-evil-insert-state-entry-hook-setup2)

(provide 'init-flycheck)
;;; init-flycheck.el ends here
