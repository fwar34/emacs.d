;; -*- coding: utf-8; lexical-binding: t; -*-
(use-package winner-mode
  :ensure nil ;; emacs自带
  :hook
  (after-init . winner-mode)
  :config
  )

(use-package ediff
  :ensure nil
  :hook
  ;; 可以应用在ediff上，恢复由ediff导致的窗体变动。
  (ediff-quit . winner-undo)
  )

(provide 'init-builtin)
