;;; init-help.el --- Useful preset transient commands  -*- coding:utf-8; lexical-binding: t; -*-
;;; Commentary:

;;; Code:

;; https://emacstalk.github.io/post/002/
;; 在 Emacs 28 中新增了 shortdoc-display-group 命令 ，对常用函数进行了归类展示
(use-package elisp-demos
  :config
  (advice-add 'describe-function-1 :after #'elisp-demos-advice-describe-function-1)
  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update))

(use-package helpful
  :preface
  (require 'transient)
  (transient-define-prefix my-helpful-transient ()
    "my helpful commands"
    [[" <helpful commands>"
      ("c" "helpful callable" helpful-callable)
      ("f" "helpful function" helpful-function)
      ("v" "helpful variable" helpful-variable)
      ("k" "helpful key" helpful-key)
      ("d" "helpful at point" helpful-key)
      ("C" "helpful command" helpful-command)]])
  (global-set-key (kbd "M-u hp") #'my-helpful-transient)
  :pretty-hydra
  (my-hydra-helpful
   (:foreign-keys warn :color teal :quit-key "q" :title "<helpful commands>")
   ("Column"
    (("c" helpful-callable "helpful callable")
     ("f" helpful-function "helpful function")
     ("v" helpful-variable "helpful variable")
     ("k" helpful-key "helpful key")
     ("d" helpful-at-point "helpful at point")
     ("C" helpful-command "helpful command"))))
  :commands
  (helpful-callable
   helpful-function
   helpful-variable
   helpful-key
   helpful-at-point
   helpful-command)
  :config
  ;; Note that the built-in `describe-function' includes both functions
  ;; and macros. `helpful-function' is functions only, so we provide
  ;; `helpful-callable' as a drop-in replacement.
  ;; (global-set-key (kbd "C-h f") #'helpful-callable)

  ;; (global-set-key (kbd "C-h v") #'helpful-variable)
  ;; (global-set-key (kbd "C-h k") #'helpful-key)

  ;; Lookup the current symbol at point. C-c C-d is a common keybinding
  ;; for this in lisp modes.
  ;; (global-set-key (kbd "C-c C-d") #'helpful-at-point)

  ;; Look up *F*unctions (excludes macros).
  ;;
  ;; By default, C-h F is bound to `Info-goto-emacs-command-node'. Helpful
  ;; already links to the manual, if a function is referenced there.
  ;; (global-set-key (kbd "C-h F") #'helpful-function)

  ;; Look up *C*ommands.
  ;;
  ;; By default, C-h C is bound to describe `describe-coding-system'. I
  ;; don't find this very useful, but it's frequently useful to only
  ;; look at interactive functions.
  ;; (global-set-key (kbd "C-h C") #'helpful-command)
  )

;; (defun my-help-setup ()
;;   (modify-syntax-entry ?- "w")
;;   (let ((help-buffer (get-buffer "*Help*")))
;;     (when (and (buffer-live-p help-buffer) (not (get-buffer-window "*Help*")))
;;       (switch-to-buffer-other-window help-buffer))))
;; (add-hook 'help-mode-hook #'my-help-setup)

(define-key diff-mode-map "q" 'kill-this-buffer)
(define-key help-mode-map "q" 'kill-buffer-and-window)
(define-key help-mode-map (kbd "C-i") #'help-go-forward)
(define-key help-mode-map (kbd "C-o") #'help-go-back)
(transient-define-prefix my-help-transient ()
  "Help transient"
  :transient-non-suffix 'transient--do-stay
  [["Help actions"
   ("b" "help-go-back" help-go-back :transient t)
   ("f" "help-go-forward" help-go-forward :transient t)
   ("s" "help-view-source" help-view-source)
   ("i" "help-goto-info" help-goto-info)
   ("c" "help-customize" help-customize)
   ("w" "Show the docs for the symbol at point" help-follow-symbol)]
  ["Help buttons"
   ("p" "Move to the Previous Button in the help buffer" backward-button :transient t)
   ("n" "Move to the Next Button in the help buffer" forward-button :transient t)]])
(define-key help-mode-map (kbd "M-u mm") #'my-help-transient)
(add-hook 'apropos-mode-hook #'(lambda ()
                                 (local-set-key (kbd "q") #'kill-buffer-and-window)))

(provide 'init-help)
;;; init-help.el ends here
