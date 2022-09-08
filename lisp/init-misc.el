;;; init-misc.el --- Useful preset transient commands  -*- coding:utf-8; lexical-binding: t; -*-
;;; Commentary:

;;; Code:

;; {{ auto-save.el
;; reference from https://github.com/redguardtoo/emacs.d
;; (with-eval-after-load 'evil
;;   (defun local-require (pkg)
;;     (unless (featurep pkg)
;;       (load (expand-file-name
;;              (cond
;;               ((eq pkg 'go-mode-load)
;;                (format "~/.emacs.d/site-lisp/go-mode/%s" pkg))
;;               (t
;;                (format "~/.emacs.d/site-lisp/%s/%s" pkg pkg))))
;;             t t)))
;;     (local-require 'auto-save)
;;     ;; (add-to-list 'auto-save-exclude 'file-too-big-p t)
;;     (add-to-list 'auto-save-exclude "COMMIT_EDITMSG")
;;     (add-to-list 'auto-save-exclude "TAGS")
;;     (add-to-list 'auto-save-exclude "tags")
;;     (add-to-list 'auto-save-exclude "\\.cpp")
;;     (setq auto-save-idle 2) ; 2 seconds
;;     (setq auto-save-slient t)
;;     (auto-save-enable))

(use-package super-save
  :disabled
  :config
  (super-save-mode +1)
  ;; (setq super-save-auto-save-when-idle t)
  )
;; }}

(provide 'init-misc)
;;; init-misc.el ends here
