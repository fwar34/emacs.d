;; -*- coding: utf-8; lexical-binding: t; -*-
;; init shell
;; (add-hook 'shell-mode-hook (lambda ()
;;                              (evil-local-set-key 'emacs (kbd "C-w") 'evil-delete-backward-word)))

(use-package shell
  :ensure t
  :commands shell
  :config
  (add-hook 'shell-mode-hook (lambda ()
                               (maximize-window)))
  )

(provide 'init-shell)
