;;; init-base.el --- Useful preset transient commands  -*- coding:utf-8; lexical-binding: t; -*-
;;; Commentary:

;;; Code:

;; 文字生成拼写的大字
(use-package figlet
  :commands figlet
  :if (executable-find "figlet"))

(use-package quickrun
  :commands quickrun
  :unless (string-equal "windows-nt" system-type)
  :config
  (setq quickrun-timeout-seconds nil)
  ;; Use this parameter as C++ default
  (quickrun-add-command "c++/c1z"
    '((:command . "g++")
      (:exec    . ("%c -std=c++1z %o -o %e %s -lpthread"
                   "%e %a"))
      (:remove  . ("%e")))
    :default "c++")

  ;; Use this parameter in pod-mode
  (quickrun-add-command "pod"
    '((:command . "perldoc")
      (:exec    . "%c -T -F %s"))
    :mode 'pod-mode)

  ;; You can override existing command
  (quickrun-add-command "c/gcc"
    '((:exec . ("%c -std=c++1z %o -o %e %s -lpthread"
                "%e %a")))
    :override t))

(use-package rg
  :bind
  (("C-c rg" . rg-menu)
   :map rg-mode-map
   ("w" . wgrep-change-to-wgrep-mode)
   ("j" . compilation-next-error)
   ("k" . compilation-previous-error))
  :config
  ;; (rg-enable-menu)
  ; (add-hook 'rg-mode-hook (lambda () (evil-set-initial-state 'rg-mode 'emacs)))
  
(transient-define-prefix my-rg-transient ()
    "my ivy minibuffer commands"
    [[" <ivy commands>"
      ("RET" "ivy-done" ivy-done)
      ("," "self insert \",\"" self-insert-command)]])
  (bind-key "," 'my-rg-transient rg-mode-map)

  ;; https://github.com/DogLooksGood/meomacs/blob/master/editor.org#rgel
  (autoload 'rg-project "wgrep" nil t)
  (autoload 'rg-project "rg" nil t)
  (with-eval-after-load "wgrep"
    (define-key wgrep-mode-map (kbd "C-c C-c") #'wgrep-finish-edit))
  (define-key project-prefix-map "r" 'rg-project)
  )

(use-package xclip
  :disabled
  :ensure t
  :config
  (xclip-mode 1)
  ;; (setq select-enable-clipboard t)
  ;; (setq select-enable-primary t)
  ;; (xclip-set-selection 'primary )
  )

;; https://github.com/purcell/disable-mouse
(use-package disable-mouse
  :preface
  (declare-function global-disable-mouse-mode "disable-mouse-autoloads")
  :if (and (display-graphic-p) (string-equal "FL-NOTEBOOK" (upcase (system-name))))
  :config
  (global-disable-mouse-mode)
  ; (mapc 'disable-mouse-in-keymap
  ;   (list evil-motion-state-map
  ;         evil-normal-state-map
  ;         evil-visual-state-map
  ;         evil-insert-state-map))
  )

(use-package fzf
  :unless (equal system-type 'windows-nt)
  :commands
  (fzf
   fzf-directory
   fzf-switch-buffer
   fzf-find-file
   fzf-find-file-in-dir
   fzf-git
   fzf-git-files
   fzf-hg
   fzf-projectile
   fzf-git-grep
   fzf-recentf
   fzf-grep
   fzf-grep-dwim)
  :config
  (setq fzf/args "-x --color bw --print-query --margin=1,0 --no-hscroll"
        fzf/executable "fzf"
        fzf/git-grep-args "-i --line-number %s"
        ;; command used for `fzf-grep-*` functions
        ;; example usage for ripgrep:
        fzf/grep-command "rg --no-heading -nH"
        ;; fzf/grep-command "grep -nrH"
        ;; If nil, the fzf buffer will appear at the top of the window
        fzf/position-bottom t
        fzf/window-height 15))

(use-package devdocs
  :if (string= (getenv "MYHOSTNAME") "desktop-archlinux")
  :commands
  (devdocs-peruse devdocs-lookup devdocs-install devdocs-delete devdocs-update-all)
  :config
  (add-hook 'python-mode-hook
            (lambda () (setq-local devdocs-current-docs '("python~3.10")))))

(use-package color-rg
  :disabled
  :straight
  (:type github :repo "manateelazycat/color-rg")
  :config
  )

(provide 'init-tools)
;;; init-tools.el ends here
