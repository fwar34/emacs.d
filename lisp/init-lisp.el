;;; init-lisp.el --- Useful preset transient commands  -*- coding:utf-8; lexical-binding: t; -*-
;;; Commentary:

;;; Code:

;; (use-package lispy
;;   :ensure t
;;   :disabled
;;   :hook
;;   (emacs-lisp-mode . lispy-mode)
;;   :config
;;   ;; (define-key lispy-mode-map (kbd “<delete>”) 'lispy-delete)
;;   ;; (define-key lispy-mode-map (kbd “C-d”) 'lispy-delete-backward)
;;   ;; (define-key lispy-mode-map (kbd “C-k”) 'lispy-kill)
;;   ;; (define-key lispy-mode-map (kbd “C-y”) 'lispy-yank)
;;   ;; (define-key lispy-mode-map (kbd “C-e”) 'lispy-move-end-of-line)
;;   )

;; https://github.com/noctuid/lispyville
(use-package lispyville
  :defer t
  ;; :disabled
  ;; :hook
  ;; lispyvill used with lispy
  ;; (lispy-mode . lispyville-mode)

  ;; Lispyville can also be used without lispy:
  ;; :hook
  ;; (emacs-lisp-mode . lispyville-mode)
  ;; (lisp-mode . lispyville-mode)
  :config
  (lispyville-set-key-theme
   ;; his is probably the simplest method of improving things. By default,
   ;; pressing escape after using something like lispy-mark from special will
   ;; enter normal state but won’t cancel the region. Lispyville provides lispyville-normal-state
   ;; to deactivate the region and enter normal state in one step. You can map it manually or
   ;; use the escape key theme (e.g. (lispyville-set-key-theme '(... (escape insert emacs)))).
   ;; '((escape insert emacs)
   ;;   additional-movement prettify atom-motions slurp/barf-cp additional additional-wrap))
   '((escape insert emacs)
     additional-movement slurp/barf-cp additional commentary text-objects wrap)))

(use-package cider
  :disabled
  :defer t
  :hook
  (clojure-mode . cider-mode)
  ;; :config
  ;; (add-hook 'clojure-mode-hook 'cider-mode)
  )

;; (use-package inf-clojure
;;   :disabled
;;   :ensure t
;;   ;; :defer t
;;   :config
;;   (add-hook 'clojure-mode-hook 'inf-clojure-minor-mode)
;;   )

(provide 'init-lisp)
;;; init-lisp.el ends here
