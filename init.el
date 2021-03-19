;; -*- coding: utf-8; lexical-binding: t; -*-
;; (require 'org-install)
;; (require 'ob-tangle)
;; (org-babel-load-file (expand-file-name "liang.org" user-emacs-directory))
;; (setq gc-cons-threshold (* 2 1000 1000))

;; this is master write
;; master write 2

(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)

;; http://www.sohu.com/a/301863132_100034897
;; -q ignores personal Emacs files but loads the site files.
;; emacs -q --eval='(message "%s" (emacs-init-time))'
;; For macOS users:
;; open -n /Applications/Emacs.app --args -q --eval='(message "%s" (emacs-init-time))'

;; Use a hook so the message doesn't get clobbered by other messages.
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(defun local-require (pkg)
  (unless (featurep pkg)
    (load (expand-file-name
           (cond
            ((eq pkg 'go-mode-load)
             (format "~/.emacs.d/site-lisp/go-mode/%s" pkg))
            (t
             (format "~/.emacs.d/site-lisp/%s/%s" pkg pkg))))
          t t)))

(package-initialize)
(add-to-list 'load-path "~/.emacs.d/lisp")
(require 'init-packages)
(require 'init-ui)
(require 'init-hydra)
(require 'init-modeline)

(with-eval-after-load 'evil (require 'init-evil))
(add-hook 'after-init-hook (lambda () (require 'init-company)))
(add-hook 'after-init-hook (lambda () (require 'init-tags)))
(add-hook 'after-init-hook (lambda () (require 'init-eshell)))
(add-hook 'after-init-hook (lambda () (require 'init-shell)))
(add-hook 'after-init-hook (lambda () (require 'init-dired)))
(add-hook 'after-init-hook (lambda () (require 'init-org)))
;; (add-hook 'after-init-hook (lambda () (require 'auto-save)
;;                              (auto-save-enable))) 
(add-hook 'after-init-hook (lambda () (require 'init-better-default)))
(add-hook 'after-init-hook (lambda ()
                             (require 'init-minefunc)
                             (require 'init-c)))
(add-hook 'after-init-hook (lambda () (require 'init-misc)))
(add-hook 'after-init-hook (lambda () (require 'init-keybindings)))
(add-hook 'after-init-hook (lambda () (require 'init-custom)))

(require 'init-calendar)
;; (require 'unicad)
(add-hook 'after-init-hook
          (lambda ()
            (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
            (if (file-exists-p (expand-file-name "custom.el"))
                (load-file custom-file))))

(global-set-key [f9] #'lispy-mode)
(add-hook 'emacs-startup-hook (lambda ()
                                (setq gc-cons-threshold 16777216
                                      gc-cons-percentage 0.1)))
