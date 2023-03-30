;;; init-base.el --- Useful preset transient commands  -*- coding:utf-8; lexical-binding: t; -*-
;;; Commentary:

;;; Code:

;; Setup `use-package'
;; https://phenix3443.github.io/notebook/emacs/modes/use-package-manual.html
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'bind-key)
(setq use-package-always-ensure t)

(define-prefix-command 'M-u-map)
(global-set-key (kbd "M-u") 'M-u-map)
;; (define-prefix-command 'C-j-map)
;; (global-set-key (kbd "C-j") 'C-j-map)

(use-package auto-compile
  :no-require t)

;; straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(use-package transient
  :straight
  (:host github :repo "magit/transient"))

(use-package hydra)

(use-package major-mode-hydra
  :demand t
  :bind
  ("M-SPC" . major-mode-hydra)
  :custom
  (major-mode-hydra-invisible-quit-key "q")
  (major-mode-hydra-title-generator
        '(lambda (mode)
           (s-concat "\n<"
                     ;; (s-repeat 20 " ")
                     ;; (all-the-icons-icon-for-mode mode :v-adjust 0.05)
                     (symbol-name mode)
                     " commands>")))
  )

(use-package general
  :config
  ;; (general-evil-setup t)
  )

;; install manually
;; C-x C-f ~/gnu-elpa-keyring-update-2019.3.tar
;; M-x package-install-from-buffer
;; http://elpa.gnu.org/packages/gnu-elpa-keyring-update.html
(use-package gnu-elpa-keyring-update)

(use-package init-minefunc
  :load-path "lisp"
  :commands
  (highlight-remove-all
   my-async-task
   my-search-whole-word
   my-search-forward-word
   my-swiper-forward-word
   my-M-x
   my-append-semicolon-excursion
   my-display-full-path-of-current-buffer
   fwar34/counsel-goto-recent-directory
   fwar34/insert-python
   fwar34/insert-lisp-commit
   fwar34/run-current-file
   convert-file-to-utf8-unix
   my-display-function
   my-commands-shell
   my-change-default-directory
   interrupt-my-commands
   transient-winner-undo
   my-transient-yank
   my-find-other-file
   my-convert-radix
   my-convert-radix-hex
   my-convert-radix-deci
   my-test-face
   my-test-syntax-ppss
   my-kill-line
   open-init-file
   start-my-timer
   stop-my-timer
   my-switch-message-buffer
   my-remove-0x00
   my-task-file-open
   my-set-frame
   my-project-root))

(use-package popup)

(provide 'init-base)
;;; init-base.el ends here
