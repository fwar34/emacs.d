;; -*- coding: utf-8; lexical-binding: t; -*-

;; {{ auto-save.el
;; reference from https://github.com/redguardtoo/emacs.d
(eval-after-load 'evil
  (progn
    (local-require 'auto-save)
    ;; (add-to-list 'auto-save-exclude 'file-too-big-p t)
    (add-to-list 'auto-save-exclude "COMMIT_EDITMSG")
    (add-to-list 'auto-save-exclude "TAGS")
    (add-to-list 'auto-save-exclude "tags")
    (add-to-list 'auto-save-exclude "\\.cpp")
    (setq auto-save-idle 2) ; 2 seconds
    (setq auto-save-slient t)
    (auto-save-enable)
    ))
;; }}

(provide 'init-misc)
