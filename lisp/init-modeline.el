;; -*- coding: utf-8; lexical-binding: t; -*-
;;-------------------------------------------------------------
;; init-modeline
;; https://blog.csdn.net/xh_acmagic/article/details/78939246
;;-------------------------------------------------------------

(use-package telephone-line
  :disabled
  :unless window-system
  :ensure t
  :delight
  :config
  (telephone-line-mode t)
  )


;; (use-package init-my-modeline
;;   :load-path "lisp"
;;   ;; :unless window-system
;;   )

;; (add-hook 'after-init-hook (lambda () (require 'init-my-modeline)))
(use-package nano-modeline
  :disabled
  :ensure t
  :config
  (nano-modeline-mode)
  )

(use-package simple-modeline
  :disabled
  :ensure t
  :hook (after-init . simple-modeline-mode)
  )

;; (use-package mood-line
;;   :ensure t
;;   :config
;;   (mood-line-mode)
;;   )
(use-package mood-line
  :ensure t
  :straight (mood-line :type git
                       :host gitlab
                       :repo "fwar34/mood-line")
  :config
  (mood-line-mode)
  )

(use-package vs-modeline
  :disabled
  :ensure t
  :straight (vs-modeline :type git
                         :host github
                         :repo "VojtechStep/vs-modeline.el")
  :demand t
  :config
  (vs-modeline-mode))

;; spaceline
(use-package spaceline
  :disabled
  :ensure t
  :if window-system
  :config
  ;; When nil, winum-mode will not display window numbers in the mode-line.
  ;; You might want this to be nil if you use a package that already manages window numbers in the mode-line.
  (setq winum-auto-setup-mode-line nil)
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
  ;; (spaceline-spacemacs-theme))
  (spaceline-emacs-theme))

(use-package maple-modeline
  :disabled
  :load-path "lisp"
  :hook (after-init . maple-modeline-init)
  :config
  ;; standard or minimal
  (setq maple-modeline-style 'standard)
  ;; standard or reset or some number
  (setq maple-modeline-width 'standard))

(use-package doom-modeline
  :disabled
  :ensure t
  :hook (after-init . doom-modeline-mode))

(use-package mood-line
  :disabled
  :ensure t
  :hook
  (after-init . mood-line-mode))

(use-package smart-mode-line
  :disabled
  :ensure t
  :init
  (setq sml/theme 'light)
  :hook
  (after-init . (lambda () (sml/setup)))
  )

(use-package powerline
  :disabled
  :ensure t
  :config
  (powerline-default-theme)
  )

(use-package airline-themes
  :disabled
  :ensure t
  :config
  (load-theme 'airline-light)
  )

(use-package powerline-evil
  :disabled
  :ensure t
  :config
  ;; (setq powerline-evil-tag-style 'standard)
  )

(provide 'init-modeline)
