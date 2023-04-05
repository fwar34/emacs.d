;;; init-lsp.el --- Useful preset transient commands  -*- coding:utf-8; lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(use-package eglot
  :disabled
  :ensure t
  :config
  (add-hook 'go-mode-hook 'eglot-ensure)
  (add-hook 'python-mode-hook 'eglot-ensure)
  (add-hook 'rust-mode-hook 'eglot-ensure))

(use-package nox
  :disabled
  :ensure t
  :straight
  (:host github :repo "manateelazycat/nox")
  :config
  (dolist (hook (list
                 ;; 'js-mode-hook
                 'rust-mode-hook
                 'python-mode-hook
                 ;; 'ruby-mode-hook
                 ;; 'java-mode-hook
                 ;; 'sh-mode-hook
                 ;; 'php-mode-hook
                 'c-mode-common-hook
                 'c-mode-hook
                 ;; 'csharp-mode-hook
                 'c++-mode-hook
                 ;; 'haskell-mode-hook
                 ))
    (add-hook hook '(lambda () (nox-ensure)))))

(use-package lsp-mode
  ;; lsp 的安装可以去 nvim 中使用 :LspInstall 安装
  ;; python: sudo pip install python-lsp-server
  ;; go: GO111MODULE=on go get golang.org/x/tools/gopls@latest
  ;; rust: rustup update && rustup component add rls rust-analysis rust-src
  ;; lua: luarocks install --server=https://luarocks.org/dev lua-lsp --local 现在（2021/5/12）还不支持 lua5.4
  ;; lua: lua-language-server 直接使用 M-x lsp 就会自动安装
  ;; clojure: This Server supports automatic install. Install this language server with M-x lsp-install-server RET clojure-lsp RET. 或者 yay -S clojure-lsp-bin
  ;; c++: yay -S ccls 或者 yay -S clang
  :defer t
  :after company
  :if (equal system-type 'gnu/linux)
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  ;; (setq lsp-keymap-prefix "C-l")
  :hook
  ((go-mode . lsp-deferred)
   (python-mode . lsp-deferred)
   (rust-mode . lsp-deferred)
   (clojure-mode . lsp-deferred)
   (lua-mode . lsp-deferred)
   (js2-mode . lsp-deferred)
   (web-mode . lsp-deferred)
   (css-mode . lsp-deferred)
   ;; (c-mode . lsp-deferred)
   ;; (c++-mode . lsp-deferred)
   ;; if you want which-key integration
   (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp-deferred
  :config
  (message (format "lsp-deferred @ %s" major-mode))
  (setq ;;lsp-completion-provider :none
   lsp-enable-indentation nil
   lsp-enable-on-type-formatting nil
   lsp-rust-server 'rls
   lsp-pyls-plugins-flake8-max-line-length 1000
   lsp-pylsp-plugins-flake8-max-line-length 1000
   lsp-pylsp-plugins-pycodestyle-max-line-length 1000
   lsp-pyls-plugins-pycodestyle-max-line-length 1000
   ;; lsp-completion-filter-on-incomplete nil
   ;; lsp-completion-enable-additional-text-edit nil
   ;; lsp-completion-sort-initial-results nil
   )

  ;; {{{
  ;; fix: gopls requires a module at the root of your workspace. You can work with multiple modules by opening each one as a workspace folder.
  ;; https://go.googlesource.com/tools/+/refs/heads/master/gopls/doc/emacs.md
  ;; https://go.googlesource.com/tools/+/refs/heads/master/gopls/doc/workspace.md
  ;; (lsp-register-custom-settings
  ;;  '(("gopls.experimentalWorkspaceModule" t t)))
  ;; }}}
  
  (use-package lsp-diagnostics
    :ensure nil
    :after lsp-mode
    :config
    (add-to-list 'lsp-diagnostics-disabled-modes 'c++-mode))
  (setq lsp-headerline-breadcrumb-enable-diagnostics nil)
  ;; (setq lsp-modeline-diagnostics-enable nil)

  ;; (lsp-diagnostics-mode -1)
  (lsp-modeline-diagnostics-mode -1)

  ;; optionally
  (use-package lsp-ui
    :after lsp-mode
    :commands lsp-ui-mode
    :general
    (:states 'normal
             :jump t
             :prefix ";"
             ;; :keymaps 'lsp-mode-map
             "K" #'lsp-ui-doc-show
             "J" #'lsp-ui-doc-focus-frame)
    (:states 'normal
             :jump t
             :prefix "SPC"
             "xx" #'major-mode-hydras/lsp-ui-mode/body)
    :mode-hydra
    (lsp-ui-mode
     ;; (:title "<perspective commands>")
     ("Buffer"
      (("f" lsp-format-buffer "format buffer")
       ("m" lsp-ui-iment "imenu")
       ("x" lsp-execute-code-action "execute action"))
      "Server"
      (("R" lsp-restart-workspace "restart")
       ("S" lsp-shutdown-workspace "shutdown")
       ("e" lsp-describe-session "describe session"))
      "Find"
      (("d" lsp-ui-peek-find-definitions "definition")
       ("c" lsp-find-declaration "declaration")
       ("r" lsp-ui-peek-find-references "references")
       ("i" lsp-ui-peek-find-implementation "implementation")
       ("t" lsp-find-type-definition "type")
       ("s" lsp-signature-help "signature")
       ("o" lsp-describe-thing-at-point "documentation")
       ("n" lsp-rename "rename"))))
    :custom
    (lsp-ui-sideline-show-hover t)
    (lsp-ui-doc-position 'at-point)
    ;; (lsp-ui-doc-show-with-cursor t)
    (lsp-ui-sideline-show-diagnostics t)
    :config

    ; (evil-define-key 'normal go-mode-map "K" 'lsp-ui-doc-glance)
    ;; (evil-define-key 'normal go-mode-map "K" 'lsp-ui-doc-show)
    ; (evil-define-key 'normal lsp-ui-doc-frame-mode-map "H" 'lsp-ui-doc-unfocus-frame)
    ; (evil-define-key 'normal lsp-ui-doc-frame-mode-map "q" 'lsp-ui-doc-hide)
    )

  ;; if you are ivy user
  (use-package lsp-ivy
    :after lsp-mode
    :commands lsp-ivy-workspace-symbol)
  ;; optionally if you want to use debugger
  ;; (use-package dap-mode)
  ;; (use-package dap-LANGUAGE) to load the dap adapter for your language

  ;; lua-lsp 现在（2021/5/12）还不支持 lua5.4
  (let ((lua-version (shell-command-to-string "lua -v | awk '{print $2}'")))
    (unless (> (string-to-number lua-version) 5.3)
      (add-hook 'lua-mode-hook 'lsp-deferred)))

  (with-eval-after-load 'hydra
    (defhydra hydra-lsp (:exit t :hint nil)
      "
 Buffer^^               Server^^                   Symbol
-----------------------------------------------------------------------------------------------------
 [_f_] format           [_M-r_] restart            [_d_] declaration  [_i_] implementation  [_o_] documentation
 [_m_] imenu            [_S_]   shutdown           [_D_] definition   [_t_] type            [_r_] rename
 [_x_] execute action   [_M-s_] describe session   [_R_] references   [_s_] signature"
      ("d" lsp-find-declaration)
      ("D" lsp-ui-peek-find-definitions)
      ("R" lsp-ui-peek-find-references)
      ("i" lsp-ui-peek-find-implementation)
      ("t" lsp-find-type-definition)
      ("s" lsp-signature-help)
      ("o" lsp-describe-thing-at-point)
      ("r" lsp-rename)

      ("f" lsp-format-buffer)
      ("m" lsp-ui-imenu)
      ("x" lsp-execute-code-action)

      ("M-s" lsp-describe-session)
      ("M-r" lsp-restart-workspace)
      ("S" lsp-shutdown-workspace)))
  )

(provide 'init-lsp)
;;; init-lsp.el ends here
