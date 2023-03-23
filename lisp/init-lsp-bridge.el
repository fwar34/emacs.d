;;; init-lsp-bridge.el --- Useful preset transient commands  -*- coding:utf-8; lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(unless (package-installed-p 'yasnippet)
  (package-install 'yasnippet))

(straight-use-package
 '(lsp-bridge :host github
              :repo "manateelazycat/lsp-bridge"
              :files ("*.el" "*.py" "acm" "core" "langserver"
                      "multiserver" "resources")))

(unless (display-graphic-p)
  (straight-use-package
   '(popon :host nil :repo "https://codeberg.org/akib/emacs-popon.git"))
  (straight-use-package
   '(acm-terminal :host github :repo "twlz0ne/acm-terminal")))

(add-hook 'emacs-startup-hook
          (lambda ()
            (require 'yasnippet)
            (yas-global-mode 1)

            (require 'lsp-bridge)
            (setq acm-enable-quick-access 1)
            ;; (global-lsp-bridge-mode)
            (add-hook 'emacs-lisp-mode-hook #'lsp-bridge-mode)

            (unless (display-graphic-p)
              (with-eval-after-load 'acm
                (require 'acm-terminal)))))

;; (straight-use-package
;;  '(popon :type git :repo "https://codeberg.org/akib/emacs-popon.git"))

;; (use-package acm-terminal
;;   :straight
;;   (:host github :repo "twlz0ne/acm-terminal")
;;   :if (not (display-graphic-p))
;;   :config
;;   (with-eval-after-load 'acm
;;     (require 'acm-terminal))
;;   )

;; (straight-use-package
;;   '(lsp-bridge :type git :repo "https://github.com/manateelazycat/lsp-bridge.git"))
;; (use-package lsp-bridge
;;   :ensure nil
;;   ;; :straight
;;   ;; (:host github :repo "manateelazycat/lsp-bridge")
;;   :config
;;   (unless (package-installed-p 'yasnippet)
;;     (package-install 'yasnippet))

;;   (add-hook 'emacs-startup-hook
;;             #'(lambda ()
;;                 (require 'yasnippet)
;;                 (yas-global-mode 1)

;;                 (require 'lsp-bridge)
;;                 (global-lsp-bridge-mode)

;;                 ))
;; )

(provide 'init-lsp-bridge)
;;; init-lsp-bridge.el ends here
