;; -*- coding: utf-8; lexical-binding: t; -*-

(use-package eglot
  :disabled
  :ensure t
  :config
  (add-hook 'go-mode-hook 'eglot-ensure)
  (add-hook 'python-mode-hook 'eglot-ensure)
  (add-hook 'rust-mode-hook 'eglot-ensure)
  )

;; python: sudo pip install python-lsp-server
;; go: GO111MODULE=on go get golang.org/x/tools/gopls@latest
;; rust: rustup update && rustup component add rls rust-analysis rust-src
;; lua: luarocks install --server=https://luarocks.org/dev lua-lsp --local 现在（2021/5/12）还不支持 lua5.4
(use-package lsp-mode
  :ensure t
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (go-mode . lsp-deferred)
         (python-mode . lsp-deferred)
         (rust-mode . lsp-deferred)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp-deferred
  :config
  ;; optionally
  (use-package lsp-ui
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

  ;; 现在（2021/5/12）还不支持 lua5.4
  (let ((lua-version (shell-command-to-string "lua -v | awk '{print $2}'")))
    (unless (> (string-to-number lua-version) 5.3)
      (add-hook 'lua-mode-hook 'lsp-deferred)))
  )

(provide 'init-lsp)
