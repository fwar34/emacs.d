;;; init-packages.el --- Packages -*- coding:utf-8; lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(use-package benchmark-init
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'emacs-startup-hook 'benchmark-init/deactivate))

(use-package esup
  :commands esup
  ;; To use MELPA Stable use ":pin melpa-stable"
  :pin melpa)

(use-package winum
  ;; Navigate windows and frames using numbers.
  :preface
  (defun my-winum-pre-command-hook-setup ()
    "Enable winum-mode in pre-command-hook."
    (unless (featurep 'winum)
      (remove-hook 'pre-command-hook #'my-winum-pre-command-hook-setup)
      (winum-mode)))
  :hook
  (pre-command . my-winum-pre-command-hook-setup)
  :config
  (setq winum-auto-setup-mode-line nil))

(use-package which-key
  :preface
  (defun my-which-key-pre-command-hook-setup (&rest _)
    "Enable which-key in pre-command-hook."
    (unless (featurep 'which-key)
      (remove-hook 'pre-command-hook #'my-which-key-pre-command-hook-setup)
      (which-key-mode)))
  :hook
  (pre-command . my-which-key-pre-command-hook-setup)
  :config
  (setq which-key-allow-imprecise-window-fit t) ; performance
  (setq which-key-separator ":")
  (setq which-key-show-docstrings t)
  (which-key-setup-minibuffer))

(use-package projectile
  :hook
  (after-init . projectile-mode)
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c C-p") 'projectile-command-map)
  (setq projectile-completion-system 'ivy))

;; imenu-list
(use-package imenu-list
  :commands
  imenu-list-smart-toggle
  :bind
  (:map imenu-list-major-mode-map
	("j" . next-line)
	("k" . previous-line)
	("q" . kill-buffer-and-window)))

;; Replace GUI popup menu in Emacs with something more efficient
(use-package ace-popup-menu
  :preface
  (defun my-ace-popup-menu-pre-command-hook-setup (&rest _)
    "Enable which-key in pre-command-hook."
    (unless (featurep 'ace-popup-menu)
      (remove-hook 'pre-command-hook #'my-ace-popup-menu-pre-command-hook-setup)
      (ace-popup-menu-mode)))
  :hook
  (pre-command . my-ace-popup-menu-pre-command-hook-setup)
  :config
  (ace-popup-menu-mode))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :config
  (exec-path-from-shell-initialize))

(use-package elpa-mirror
  :commands
  elpamr-create-mirror-for-installed
  :config
  (setq elpamr-default-output-directory "~/.myelpa"))

(defun my-flycheck-evil-insert-state-entry-hook-setup2 ()
  "Enable flycheck mode in evil-insert-state-entry-hook."
  (unless (featurep 'flycheck)
    (remove-hook 'pre-command-hook #'my-flycheck-evil-insert-state-entry-hook-setup2)
    (use-package flycheck
      :hook
      (prog-mode . flycheck-mode)
      :custom
      (flycheck-temp-prefix ".flycheck")
      (flycheck-check-syntax-automatically '(save mode-enabled))
      (flycheck-emacs-lisp-load-path 'inherit)
      (flycheck-indication-mode 'right-fringe))
    (flycheck-mode)))
(add-hook 'pre-command-hook #'my-flycheck-evil-insert-state-entry-hook-setup2)

(use-package god-mode
  :functions
  which-key-enable-god-mode-support
  evil-make-intercept-map
  :commands
  god-execute-with-current-bindings
  :init
  (with-no-warnings
    (with-eval-after-load 'evil
      (bind-key "," 'god-execute-with-current-bindings evil-normal-state-map)))
  :config
  ;; which-key support god-mode
  (which-key-enable-god-mode-support)
  ;; You can change the entire modeline's foreground and background to indicate whether God mode is active as follows:
  (defun my-god-mode-update-modeline ()
    (let ((limited-colors-p (> 257 (length (defined-colors)))))
      (cond (god-local-mode (progn
			      ;; (set-face-background 'mode-line (if limited-colors-p "red" "#e9e2cb"))
			      (set-face-background 'mode-line (if limited-colors-p "red" "orange red"))
			      ;; (set-face-background 'mode-line-inactive (if limited-colors-p "red" "orange red"))
			      ))
	    (t (progn
		 ;; (set-face-background 'mode-line (if limited-colors-p "black" "#2B2B2B"))
		 ;; (set-face-background 'mode-line-inactive (if limited-colors-p "black" "#2B2B2B"))))
		 ;; 使用counsel-color-emacs可以查看
		 (set-face-background 'mode-line (if limited-colors-p "black" "gray26"))
		 ;; (set-face-background 'mode-line-inactive (if limited-colors-p "black" "gray26"))
		 )))))

  (add-hook 'god-mode-enabled-hook 'my-god-mode-update-modeline)
  (add-hook 'god-mode-disabled-hook 'my-god-mode-update-modeline)

  (bind-key (kbd ".") 'repeat god-local-mode-map)
  (bind-key (kbd ",") 'keyboard-quit god-local-mode-map))

(provide 'init-packages)
;;; init-packages.el ends here
