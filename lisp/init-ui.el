;;; init-ui.el --- Ui settings -*- coding:utf-8; lexical-binding: t; -*-
;;; Commentary:

;;; Code:

;; {{{ Themes
(use-package monokai-theme
  :disabled
  :if (and (display-graphic-p) (not (equal system-name "ubuntu-openbox")))
  :custom-face
  ;; 可以使用counsel-describe-face来查找face
  (org-block ((t (:extend t))))
  (org-block-begin-line ((t (:extend t))))
  (org-block-end-line ((t (:extend t))))
  :ensure t
  :config
  (if (and (equal system-type 'windows-nt) (> emacs-major-version 24))
      (add-hook 'window-setup-hook '(lambda () (load-theme 'monokai t)))
    (add-hook 'after-init-hook '(lambda () (load-theme 'monokai t))))
  (setq monokai-height-minus-1 0.8
        monokai-height-plus-1 1.0
        monokai-height-plus-2 1.0
        monokai-height-plus-3 1.0
        monokai-height-plus-4 1.0)
  ;; If you would like to use variable-pitch-mode you can enable it with:
  (setq monokai-user-variable-pitch t)
  ;; (custom-set-faces '(hl-line ((t (:extend t)))))
  )

;; (use-package monokai-pro-theme
;;   :disabled
;;   :ensure t
;;   :config
;;   (load-theme 'monokai-pro t)
;;   )

;; (use-package darkokai-theme
;;   :disabled
;;   :ensure t
;;   :config
;;   (load-theme 'darkokai t)
;;   )

(use-package zenburn-theme
  :disabled
  :if (or (not (display-graphic-p)) (and (display-graphic-p) (equal system-name "ubuntu-openbox")))
  :config
  (load-theme 'zenburn t))

(use-package atom-one-dark-theme
  :disabled
  :ensure t
  :config
  ;; (load-theme 'atom-one-dark t)
  )

(use-package doom-themes
  :disabled
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold nil    ; if nil, bold is universally disabled
        doom-themes-enable-italic nil) ; if nil, italics is universally disabled
  ;; (load-theme 'doom-one t)
  ;; (load-theme 'doom-vibrant t)
  ;; (load-theme 'doom-dracula t)
  ;; (load-theme 'doom-gruvbox t)
  ;; (load-theme 'doom-meltbus t)
  ;; (load-theme 'doom-miramare t)
  ;; (load-theme 'doom-monokai-pro t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package gruvbox-theme
  :disabled
  :config
  ;; (enable-theme 'gruvbox)
  )

(use-package material-theme
  :disabled
  :ensure t
  :config
  ;; (enable-theme 'material)
  )

(use-package zerodark-theme
  :disabled
  :ensure t
  :config
  ;; (enable-theme 'zerodark)
  ;; (zerodark-setup-modeline-format)
  )

(use-package color-theme-sanityinc-tomorrow
  :disabled
  :config
  ;; (color-theme-sanityinc-tomorrow-bright)
  ;; (color-theme-sanityinc-tomorrow-eighties)
  ;; (load-theme 'sanityinc-tomorrow-eighties t)
  )

;; (use-package monokai-alt-theme
;;   :disabled
;;   :ensure t
;;   :config
;;   )

;; (use-package color-theme-almost-monokai
;;   :disabled
;;   :load-path "lisp"
;;   :config
;;   (color-theme-almost-monokai)
;;   )

;; (use-package color-theme-molokai
;;   :straight
;;   (:host github :repo "alloy-d/color-theme-molokai")
;;   :config
;;   (color-theme-molokai)
;;   )
;; }}}

;; {{{ highlight -----------------------------------------------------------------------------
;; 上色括号之类的符号
(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

;; rainbow-mode
(use-package rainbow-mode
  :disabled
  :after evil
  :config
  (rainbow-mode 1))

;; 上色变量
(use-package rainbow-identifiers
  :hook
  (prog-mode . rainbow-identifiers-mode))

;; highlight-numbers
(use-package highlight-numbers
  :unless window-system
  :hook
  (prog-mode . highlight-numbers-mode))

;; highlight-quoted
(use-package highlight-quoted
  :hook
  (emacs-lisp-mode . highlight-quoted-mode))

;; highlight-defined
(use-package highlight-defined
  :disabled
  :hook
  (emacs-lisp-mode . highlight-defined-mode))

(use-package highlight-parentheses
  :config
  ;; (add-hook 'prog-mode-hook 'highlight-parentheses-mode)
  ;; (define-globalized-minor-mode global-highlight-parentheses-mode
  ;;   highlight-parentheses-mode
  ;;   (lambda () (highlight-parentheses-mode t)))
  (global-highlight-parentheses-mode t))
;; }}} -----------------------------------------------------------------------------

(use-package tree-sitter
  :disabled
  :hook
  (emacs-startup . global-tree-sitter-mode)
  :config
  (use-package tree-sitter-langs)
  ;; (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook 'tree-sitter-hl-mode))

;; beacon
(use-package beacon
  :hook
  (emacs-startup . beacon-mode))

;; https://github.com/wolray/symbol-overlay/
;; https://github.com/wolray/symbol-overlay/issues/59
(use-package symbol-overlay
  ;;默认n,p,i,q在高亮的地方点击为下一个，上一个，取消所有的高亮，替换
  :commands symbol-overlay
  :config
  (transient-define-prefix symbol-overlay-transient ()
    "Symbol Overlay transient"
    ["Symbol Overlay"
     ["Overlays"
      ("." "Add/Remove at point" symbol-overlay-put)
      ("k" "Remove All" symbol-overlay-remove-all)]
     ["Move to Symbol"
      ("n" "Next" symbol-overlay-switch-forward)
      ("p" "Previous" symbol-overlay-switch-backward)]
     ["Other"
      ("m" "Hightlight symbol-at-point" symbol-overlay-mode)]])
  ;; Or you may prefer to overwrite the keymap
  ;; (let ((map (make-sparse-keymap)))
  ;;   (define-key map (kbd "r") 'symbol-overlay-query-replace)
  ;;   (define-key map (kbd "x") 'symbol-overlay-remove-all)
  ;;   (define-key map (kbd "n") 'symbol-overlay-jump-next)
  ;;   (define-key map (kbd "p") 'symbol-overlay-jump-prev)
  ;;   (setq symbol-overlay-map map))
  )


;; 缩进显示竖线, 在 cpp 大文件中性能有问题，现在只在需要的地方开启
(use-package indent-guide
  :hook
  (emacs-lisp-mode . indent-guide-mode))

(use-package info-colors
  :hook
  (Info-selection . info-colors-fontify-node))

;; Display ^L glyphs as horizontal lines
;; https://depp.brause.cc/form-feed/
(use-package form-feed
  :hook
  (prog-mode . form-feed-mode))

(use-package hl-todo
  :config
  (global-hl-todo-mode))

(use-package awesome-tab
  :straight
  (:host github :repo "manateelazycat/awesome-tab")
  :config
  (when (not (display-graphic-p))
    (setq awesome-tab-display-icon nil))
  (setq awesome-tab-height 180)
  (awesome-tab-mode t)
  (with-eval-after-load 'evil
    (bind-key (kbd "H") 'awesome-tab-backward-tab evil-normal-state-map)
    (bind-key (kbd "L") 'awesome-tab-forward-tab evil-normal-state-map))

  (setq awesome-tab-terminal-dark-select-background-color "color-237")
  (setq awesome-tab-terminal-dark-select-foreground-color "brightred")

  (defun awesome-tab-hide-tab (x)
    (let ((name (format "%s" x)))
      (or
       (string-prefix-p "*epc" name)
       (string-prefix-p "*helm" name)
       (string-prefix-p "*Compile-Log*" name)
       (string-prefix-p "*lsp" name)
       (string-prefix-p "*Help*" name)
       (string-prefix-p "*fzf*" name)
       (string-prefix-p "*vterm*" name)
       (string-prefix-p "*NeoTree*" name)
       (string-prefix-p "*ivy-occur" name)
       (string-prefix-p "*Calendar*" name)
       (string-prefix-p "*Org todo*" name)
       (string-prefix-p "*Org tags*" name)
       (string-prefix-p "magit" name)
       (string-prefix-p "COMMIT_EDITMSG" name)
       (equal major-mode 'dired-mode)
       (equal major-mode 'eshell-mode)))))

(use-package centaur-tabs
  :disabled
  :demand t
  :defines
  x-underline-at-dewscent-line
  :hook
  ((dired-mode . centaur-tabs-local-mode)
   (vterm-mode . centaur-tabs-local-mode)
   (minibuffer-mode . centaur-tabs-local-mode))
  :config
  (centaur-tabs-mode t)
  (setq centaur-tabs-plain-icons t)
  (setq centaur-tabs-set-close-button nil)
  (setq centaur-tabs-set-modified-marker t)
  (setq centaur-tabs-cycle-scope 'tabs)
  (setq centaur-tabs-show-count t)
  (setq centaur-tabs-style "bar")
  ;; (setq centaur-tabs-style "slant")
  (setq centaur-tabs-set-bar 'left)
  ;; (setq centaur-tabs-set-bar 'under)
  ;; Note: If you're not using Spacmeacs, in order for the underline to display
  ;; correctly you must add the following line:
  (setq x-underline-at-dewscent-line t)

  (with-eval-after-load 'evil
    (bind-key (kbd "H") 'centaur-tabs-backward evil-normal-state-map)
    (bind-key (kbd "L") 'centaur-tabs-forward evil-normal-state-map)))

(load-theme 'sanityinc-tomorrow-eighties-mine t)

;;
(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

(use-package emacs
  :ensure nil
  :config
  (when (display-graphic-p)
    (menu-bar-mode -1)
    (tool-bar-mode -1)
    (scroll-bar-mode -1)))

(provide 'init-ui)
;;; init-ui.el ends here
