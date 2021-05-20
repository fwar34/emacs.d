;; -*- coding: utf-8; lexical-binding: t; -*-

;; https://archive.casouri.cat/note/2020/painless-transition-to-portable-dumper/index.html
(defvar fwar34-dumped nil
  "non-nil when a dump file is loaded.(Because dump.el sets this variable).")

(defvar fwar34-dumped-load-path nil
  "restore load path")

(if fwar34-dumped                 
    (message "emacs start with dump")   
  (message "emacs start without dump"))

(defmacro fwar34-if-dump (then &rest else)
  "Evaluate IF if running with a dump file, else evaluate ELSE."
  (declare (indent 1))
  `(if fwar34-dumped
     ,then
     ,@else))

(fwar34-if-dump
  (progn
    (setq load-path fwar34-dumped-load-path)
    (global-font-lock-mode)
    (transient-mark-mode)
    (add-hook 'after-init-hook
	      (lambda ()
		(save-excursion
		  (switch-to-buffer "*scratch*")
		  (lisp-interaction-mode)))))
  ;;
  (package-initialize))

;; 设置垃圾回收，在windows下，emacs25版本会频繁发出垃圾回收
(when (equal system-type 'windows-nt)
  (setq gc-cons-threshold (* 512 1024 1024))
  (setq gc-cons-percentage 0.5)
  (run-with-idle-timer 5 t #'garbage-collect)
  ;; 显示垃圾回收信息，可以作为调试用
  (setq garbage-collect-message t))

;; (setq gc-cons-threshold 402653184
;;       gc-cons-percentage 0.6)
;; (add-hook 'emacs-startup-hook (lambda ()
;;                                 (setq gc-cons-threshold 16777216
;;                                       gc-cons-percentage 0.1)))

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

(add-to-list 'load-path "~/.emacs.d/lisp")
(require 'init-packages)
(require 'init-ui)
(require 'init-fonts)
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
(add-hook 'after-init-hook (lambda () (require 'init-custom)))
(add-hook 'after-init-hook (lambda () (require 'init-builtin)))
(require 'init-transient)
(require 'init-lsp)

(require 'init-calendar)
;; (require 'unicad)
(add-hook 'after-init-hook
          (lambda ()
            (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
            (if (file-exists-p (expand-file-name "custom.el"))
                (load-file custom-file))))
