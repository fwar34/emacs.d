;; -*- coding: utf-8; lexical-binding: t; -*-

(use-package one-key
  :load-path "lisp/one-key2.el"
  :bind
  (("C-c C-o" . one-key-menu-test))
  :config
  (setq one-key-menu-test-alist
        '(
          (("n" . "evil normal state") . evil-normal-state)
          (("i" . "evil insert state") . evil-insert-state)
          ))
  (defun one-key-menu-test ()
    "The `one-key' menu for TEST."
    (interactive)
    (one-key-menu "TEST" one-key-menu-test-alist t))
  )

(provide 'init-one-key)
