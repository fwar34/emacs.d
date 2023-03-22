;;; init.el --- Useful preset transient commands  -*- coding: utf-8; lexical-binding: t; no-byte-compile: t -*-
;;; Commentary:

;;; Code:

(add-hook 'emacs-startup-hook
          (lambda ()
            "Recover GC values after startup."
            (setq gc-cons-threshold 800000)))

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

(require 'package)
(setq package-archives
      '(
        ;; option0
        ;; ("gnu" . "https://mirrors.sjtug.sjtu.edu.cn/emacs-elpa/gnu/")
        ;; ("melpa" . "https://mirrors.sjtug.sjtu.edu.cn/emacs-elpa/melpa/")
        ;; option1
        ;; ("melpa" . "https://melpa.org/packages/")
        ;; ("melpa-stable" . "https://stable.melpa.org/packages/")
        ;; option2
        ("gnu" . "https://mirrors.163.com/elpa/gnu/")
        ("melpa" . "https://mirrors.163.com/elpa/melpa/")
        ("melpa-stable" . "https://mirrors.163.com/elpa/stable-melpa/")
        ))

;; (setq load-prefer-newer t)
(package-initialize)
;; (require 'auto-compile)
;; (auto-compile-on-load-mode)
;; (auto-compile-on-save-mode)

;; 防止反复调用 package-refresh-contents 会影响加载速度
(when (not package-archive-contents)
  (package-refresh-contents))

(add-to-list 'load-path "~/.emacs.d/lisp")
(require 'init-base)
;; (require 'init-evil)

(provide 'init)
;;; init.el ends here
