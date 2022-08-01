;;; init-filebrowser.el --- Useful preset transient commands  -*- coding:utf-8; lexical-binding: t; -*-
;;; Commentary:

;;; Code:

;; https://www.emacswiki.org/emacs/NeoTree
(use-package neotree
  :commands
  neotree-toggle
  :config
  ;; Note: For users who want to use the icons theme. Pls make sure you have
  ;; installed the all-the-icons package and its fonts.
  ;; (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  ;; (setq neo-theme (when (display-graphic-p) 'icons))
  ;; (setq neo-theme 'arrow)
  ;; Every time when the neotree window is opened, let it find current file and jump to node.
  (setq neo-smart-open t)
  )

(use-package dired-sidebar
  :bind (("C-x C-n" . dired-sidebar-toggle-sidebar))
  :commands (dired-sidebar-toggle-sidebar)
  :init
  (add-hook 'dired-sidebar-mode-hook
	    (lambda ()
	      (unless (file-remote-p default-directory)
		(auto-revert-mode))))
  :config
  (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
  (push 'rotate-windows dired-sidebar-toggle-hidden-commands)

  (setq dired-sidebar-subtree-line-prefix "__")
  (setq dired-sidebar-theme 'vscode)
  (setq dired-sidebar-use-term-integration t)
  (setq dired-sidebar-use-custom-font t))

(use-package ibuffer-sidebar
  :commands (ibuffer-sidebar-toggle-sidebar)
  :config
  (setq ibuffer-sidebar-use-custom-font t)
  (setq ibuffer-sidebar-face `(:family "Helvetica" :height 140)))

(defun sidebar-toggle ()
  "Toggle both `dired-sidebar' and `ibuffer-sidebar'."
  (interactive)
  (dired-sidebar-toggle-sidebar)
  (ibuffer-sidebar-toggle-sidebar))

(provide 'init-filebrowser)
;;; init-filebrowser.el ends here
