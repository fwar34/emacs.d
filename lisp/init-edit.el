;;; init-packages.el --- Useful preset transient commands  -*- coding:utf-8; lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(use-package undo-tree
  :commands
  undo-tree-visualize
  :custom
  (undo-tree-history-directory-alist '(("." .  "~/.emacs.d/undo")))
  :init
  (add-hook 'undo-tree-mode-hook #'(lambda ()
                                     (local-set-key (kbd "q") 'quit-window)))
  :config
  (global-undo-tree-mode))

(use-package smart-hungry-delete
  :bind (([remap backward-delete-char-untabify] . smart-hungry-delete-backward-char)
         ([remap delete-backward-char] . smart-hungry-delete-backward-char)
         ([remap delete-char] . smart-hungry-delete-forward-char)
         ([remap c-electric-backspace] . smart-hungry-delete-backward-char))
  :init (smart-hungry-delete-add-default-hooks))

(use-package expand-region
  :commands er/expand-region
  ;; :bind ("M-=" . er/expand-region)
  )

(use-package embrace
  :ensure t
  :pretty-hydra
  (my-hydra-embrace
   (:foreign-keys warn :color teal :quit-key "q" :title "<embrace commands>")
   ("Column"
    (("a" embrace-add "embrace-add")
    ("c" embrace-change "embrace-change")
    ("d" embrace-delete "embrace-change"))))
  :bind
  ("M-=" . my-hydra-embrace/body))

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
  :init
  (with-eval-after-load "wgrep"
    (define-key wgrep-mode-map (kbd "C-c C-c") #'wgrep-finish-edit))
  :preface
  (defun my-ivy-occur-mode-hook-setup ()
    (local-set-key (kbd ",") #'my-hydra-wgrep/body))
  :config
  (define-key wgrep-mode-map (kbd ",") 'my-hydra-wgrep/body)
  (add-hook 'ivy-occur-grep-mode-hook #'my-ivy-occur-mode-hook-setup))

(defun my-meow-insert-state-entry-hook-setup2 ()
  "Enable some packages in meow-state-entry-hook."
  (when (or (not (featurep 'yasnippet)) (and (boundp 'yas/global-mode) (not yas/global-mode)) (not (boundp 'yas/global-mode)))
    (remove-hook 'meow-insert-mode-hook #'my-meow-insert-state-entry-hook-setup2)
    (use-package yasnippet
      :config
      (yas-global-mode)
      (setq yas-snippet-dirs '("~/.emacs.d/mysnippets"))

      (use-package ivy-yasnippet
        :after yasnippet
        :config
        (setq ivy-yasnippet-expand-keys nil)))))
(add-hook 'meow-insert-mode-hook #'my-meow-insert-state-entry-hook-setup2)

(use-package fix-word
  :commands
  (fix-word-upcase
   fix-word-downcase
   fix-word-capitalize))

(use-package browse-kill-ring
  :commands
  browse-kill-ring
  :config
  (define-key browse-kill-ring-mode-map "q" 'browse-kill-ring-quit)
  (define-key browse-kill-ring-mode-map (kbd "RET") 'browse-kill-ring-insert-and-quit)
  )

;; Emacs minor mode for Eclipse-like moving and duplications of lines or selections with convenient key bindings.
(use-package move-dup
  :bind (("M-p"   . move-dup-move-lines-up)
         ;; ("C-M-p" . move-dup-duplicate-up)
         ("M-n"   . move-dup-move-lines-down)
         ;; ("C-M-n" . move-dup-duplicate-down)
         ))

(use-package evil-nerd-commenter
  :commands
  (evilnc-comment-or-uncomment-lines
   evilnc-copy-and-comment-lines)
  :config
  ;; must put before (evilnc-default-hotkeys t t)
  (setq evilnc-use-comment-object-setup nil)
  (evilnc-default-hotkeys t t))

(provide 'init-edit)
;;; init-edit.el ends here
