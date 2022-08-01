;;; init-evil.el --- Useful preset transient commands  -*- coding:utf-8; lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(use-package evil
  :preface
  (transient-define-prefix my-evil-search-transient ()
    "my evil search commands"
    [[" <edit commands>"
      ("c" "isearch occur" isearch-occur)
      ("," "isearch quote insert" isearch-quote-char) ;; C-q
      ("p" "paste last from kill ring" isearch-yank-pop-only)
      ("y" "paste from kill ring" isearch-yank-pop)]])
  :hook
  (emacs-startup . evil-mode)
  :init
  ;; (setq evil-want-keybinding nil) must put before load evil
  ;; See https://github.com/emacs-evil/evil-collection/issues/60 for more details.
  (setq evil-want-keybinding nil)
  ;; disable status in echo area
  (setq evil-echo-state nil)
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-C-i-jump t)
  (setq evil-want-fine-undo "yes")
  (setq evil-want-Y-yank-to-eol t)
  ;; (setq evil-no-display t)              ;; not display evil state in echo area
  :bind
  (:map isearch-mode-map
        ("<up>" . #'isearch-ring-retreat)
        ("<down>" . #'isearch-ring-advance)
        ("," . #'my-evil-search-transient))
  (:map evil-normal-state-map
        ("C-a" . #'evil-first-non-blank)
        ("C-e" . #'evil-end-of-line))
  :config

  (defun my-evil-undo/redo-advice (&rest _)
    (unless (featurep 'undo-tree)
      (advice-remove 'evil-redo #'my-evil-undo/redo-advice)
      (advice-remove 'evil-undo #'my-evil-undo/redo-advice)
      (global-undo-tree-mode)))
  (advice-add 'evil-undo :before #'my-evil-undo/redo-advice)
  (advice-add 'evil-redo :before #'my-evil-undo/redo-advice)

  ;; 设置光标样式
  (setq evil-motion-state-cursor '(box "red"))
  (setq evil-visual-state-cursor '(box "orange"))
  (setq evil-emacs-state-cursor  '((hbar . 5) "indianred"))
  (setq evil-insert-state-cursor '((hbar . 5) "yellow")
        evil-normal-state-cursor '(box "purple"))

  (define-key evil-ex-search-keymap (kbd ";g") 'keyboard-quit)
  (evil-ex-define-cmd "q[uit]" 'kill-this-buffer)

  ;; https://emacs.stackexchange.com/questions/31334/history-of-search-terms-for-evil-mode
  ;; (custom-set-variables '(evil-search-module 'evil-search))

  ;; (if (featurep 'undo-tree)
  ;;     (evil-set-undo-system 'undo-tree)
  ;;   (evil-set-undo-system 'undo-redo))
  (evil-set-undo-system 'undo-tree)
  (use-package avy)

  :custom
  ;; https://emacs-china.org/t/evil-insert-state-or-evil-emacs-state/16710/6?u=fwar34
  (evil-ex-interactive-search-highlight 'selected-window)
  (evil-want-C-g-bindings t))

(defun my-evil-pre-command-hook-setup ()
  "Enable evil packages in `pre-command-hook'."
  (with-eval-after-load 'evil
    (remove-hook 'pre-command-hook #'my-evil-pre-command-hook-setup)

    (use-package evil-collection
      :after evil
      :config
      (evil-collection-init))

    (use-package evil-surround
      :after evil
      :config
      (global-evil-surround-mode))

    (use-package evil-visualstar
      :after evil
      :config
      (global-evil-visualstar-mode))))
(add-hook 'pre-command-hook #'my-evil-pre-command-hook-setup)

(use-package evil-escape
  :defer t
  :preface
  (defun my-evil-escape-evil-insert-state-entry-hook-setup ()
    "Enable evil-escape in `evil-insert-state-entry-hook'."
    (unless (featurep 'evil-escape)
      (remove-hook 'evil-insert-state-entry-hook #'my-evil-escape-evil-insert-state-entry-hook-setup)
      (evil-escape-mode)))
  :hook
  (evil-insert-state-entry . my-evil-escape-evil-insert-state-entry-hook-setup)
  :config
  (setq-default evil-escape-delay 0.3)
  (setq-default evil-escape-key-sequence ";g"))

(use-package evil-nerd-commenter
  :commands
  (evilnc-comment-or-uncomment-lines
   evilnc-copy-and-comment-lines)
  :config
  ;; must put before (evilnc-default-hotkeys t t)
  (setq evilnc-use-comment-object-setup nil)
  (evilnc-default-hotkeys t t))

(use-package evil-easymotion
  :disabled
  :after evil
  :config
  (evilem-default-keybindings (kbd "-")))

;; Press “%” to jump between matched tags in Emacs. For example, in HTML “<div>” and “</div>” are a pair of tags
(use-package evil-matchit
  :commands
  evilmi-jump-items
  :general
  (:states 'normal
           :jump t
           "%" #'evilmi-jump-items)
  :config
  (global-evil-matchit-mode 1))

;; Easy text exchange operator for Evil. This is the port of vim-exchange by Tom McDonald.
;; gx (and gX) can also be used from visual mode, which is sometimes easier than coming up with the right {motion}
;; If you're using the same motion again (e.g. exchanging two words using gxiw), you can use . (evil-repeat) the second time.
;; gxx works as you expect.
(use-package evil-exchange
  :commands
  (evil-exchange
   evil-exchange-cancel)
  :general
  (:states '(normal visual)
           :jump t
           "gx" 'evil-exchange
           "gX" 'evil-exchange-cancel)
  :config
  ;; change default key bindings (if you want) HERE
  ;; (setq evil-exchange-key (kbd "zx"))
  ;; (evil-exchange-install)
  )

;; {{ @see https://github.com/syl20bnr/spacemacs/blob/master/doc/DOCUMENTATION.org#replacing-text-with-iedit
;; same keybindgs as spacemacs:
;;  - "SPC s e" to start `iedit-mode'
;;  - "TAB" to toggle current occurrence
;;  - "n" next, "N" previous (obviously we use "p" for yank)
;;  - "gg" the first occurence, "G" the last occurence
;;  - Please note ";;" or `avy-goto-char-timer' is also useful
(use-package evil-iedit-state
  ;; 2021-12-23 报错，等修复了再打开
  ;; https://github.com/syl20bnr/evil-iedit-state/issues/36
  :disabled
  :after evil)

(use-package evil-snipe
  :disabled
  :after evil
  ;; :hook
  ;; (magit-mode . turn-off-evil-snipe-override-mode)
  :config
  (evil-snipe-mode +1)
  ;; and disable in specific modes
  (push 'dired-mode evil-snipe-disabled-modes)
  (push 'package-menu-mode evil-snipe-disabled-modes)
  (push 'global-mode evil-snipe-disabled-modes)
  ;; To map : to a python function (but only in python-mode):
  (add-hook 'python-mode-hook
	    (lambda ()
	      (make-variable-buffer-local 'evil-snipe-aliases)
	      (push '(?: "def .+:") evil-snipe-aliases)))

  ;; Integration into avy/evil-easymotion
  ;; This will allow you to quickly hop into avy/evil-easymotion right after a snipe.
  (define-key evil-snipe-parent-transient-map (kbd "C-;")
    (evilem-create 'evil-snipe-repeat
		   :bind ((evil-snipe-scope 'buffer)
			  (evil-snipe-enable-highlight)
			  (evil-snipe-enable-incremental-highlight))))

  ;; Evil-snipe can override evil-mode's native motions with 1-char sniping:
  ;; https://github.com/hlissner/evil-snipe
  (evil-snipe-override-mode +1)

  ;; https://github.com/hlissner/evil-snipe#conflicts-with-other-plugins
  ;; It seems evil-snipe-override-mode causes problems in Magit buffers, to fix this:
  (add-hook 'magit-mode-hook 'turn-off-evil-snipe-override-mode))

;; evil-smartparens
(use-package evil-smartparens
  :disabled
  :after evil
  :hook
  (smartparens-enabled . evil-smartparens-mode))

(provide 'init-evil)
;;; init-evil.el ends here
