;; -*- coding: utf-8; lexical-binding: t; -*-

(use-package eglot
  :ensure t
  :config
  (add-hook 'go-mode-hook 'eglot-ensure)
  (add-hook 'python-mode-hook 'eglot-ensure)
  (add-hook 'rust-mode-hook 'eglot-ensure)
  )

(use-package lsp-mode
  :disabled
  :ensure t
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (go-mode . lsp-deferred)
         (python-mode . lsp-deferred)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp-deferred
  :config
  ;; optionally
  (use-package lsp-ui
    :disabled
    :ensure t
    :after lsp-mode
    :commands lsp-ui-mode)
  ;; if you are ivy user
  (use-package lsp-ivy
    :ensure t
    :after lsp-mode
    :commands lsp-ivy-workspace-symbol)
  ;; optionally if you want to use debugger
  ;; (use-package dap-mode)
  ;; (use-package dap-LANGUAGE) to load the dap adapter for your language

  (use-package lsp-jedi
    :ensure t
    :after lsp-mode
    :config
    (with-eval-after-load "lsp-mode"
      (add-to-list 'lsp-disabled-clients 'pyls)
      (add-to-list 'lsp-enabled-clients 'jedi)))
  )



(provide 'init-lsp)
