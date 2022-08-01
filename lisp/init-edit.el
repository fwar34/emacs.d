;;; init-packages.el --- Useful preset transient commands  -*- coding:utf-8; lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(use-package undo-tree
  :commands
  undo-tree-visualize
  :custom
  (undo-tree-history-directory-alist '(("." .  "~/.emacs.d/undo")))
  :config
  (global-undo-tree-mode))

(use-package smart-hungry-delete
  :bind (([remap backward-delete-char-untabify] . smart-hungry-delete-backward-char)
         ([remap delete-backward-char] . smart-hungry-delete-backward-char)
         ([remap delete-char] . smart-hungry-delete-forward-char))
  :init (smart-hungry-delete-add-default-hooks))

(use-package expand-region
  :commands er/expand-region)

(use-package wgrep
  :commands
  wgrep-change-to-wgrep-mode
  :pretty-hydra
  (my-hydra-wgrep
   (:foreign-keys warn :color teal :quit-key "q")
   ("<wgrep-mode commands>"
    (("x" wgrep-abort-changes "Discard all changes and return to original mode.")
     ("f" wgrep-finish-edit "Apply changes to file buffers.")
     ("w" wgrep-change-to-wgrep-mode "Change to wgrep mode."))))
  :general
  (:states 'normal
           :jump t
           :keymaps 'wgrep-mode-map
           "," #'my-hydra-wgrep/body)
  :preface
  (defun my-ivy-occur-mode-hook-setup ()
    (evil-local-set-key 'normal (kbd ",") #'my-hydra-wgrep/body))
  :config
  (add-hook 'ivy-occur-grep-mode-hook #'my-ivy-occur-mode-hook-setup))

(defun my-evil-insert-state-entry-hook-setup2 ()
  "Enable some packages in evil-state-entry-hook."
  (when (or (not (featurep 'yasnippet)) (and (boundp 'yas/global-mode) (not yas/global-mode)) (not (boundp 'yas/global-mode)))
    (remove-hook 'evil-insert-state-entry-hook #'my-evil-insert-state-entry-hook-setup2)
    (use-package yasnippet
      :config
      (yas-global-mode)
      (setq yas-snippet-dirs '("~/.emacs.d/mysnippets"))

      (use-package ivy-yasnippet
        :after yasnippet
        :config
        (setq ivy-yasnippet-expand-keys nil)))))
(add-hook 'evil-insert-state-entry-hook #'my-evil-insert-state-entry-hook-setup2)

(use-package fix-word
  :commands
  (fix-word-upcase
   fix-word-downcase
   fix-word-capitalize))

(use-package browse-kill-ring
  :commands
  browse-kill-ring
  :config
  (with-eval-after-load 'evil
    (evil-define-key 'normal browse-kill-ring-mode-map "q" 'browse-kill-ring-quit)
    (evil-define-key 'normal browse-kill-ring-mode-map (kbd "RET") 'browse-kill-ring-insert-and-quit)))

;; Emacs minor mode for Eclipse-like moving and duplications of lines or selections with convenient key bindings.
(use-package move-dup
  :bind (("M-p"   . move-dup-move-lines-up)
         ("C-M-p" . move-dup-duplicate-up)
         ("M-n"   . move-dup-move-lines-down)
         ("C-M-n" . move-dup-duplicate-down)))

(provide 'init-edit)
;;; init-edit.el ends here
