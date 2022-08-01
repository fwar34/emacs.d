;;; init-workspace.el --- Useful preset transient commands  -*- coding:utf-8; lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(use-package perspective
  :commands
  (persp-switch-by-number
   persp-switch
   persp-kill-buffer*
   persp-switch-to-buffer*
   persp-switch-to-buffer
   persp-switch-last
   persp-rename
   persp-add-buffer
   persp-remove-buffer
   persp-kill
   persp-next
   persp-prev
   persp-state-save
   persp-state-load)
  :bind
  (("C-x C-b" . persp-ivy-switch-buffer) ; or use a nicer switcher, see below
   ("C-c C-b" . persp-list-buffers)         ; or use a nicer switcher, see below
   ("C-x b" . persp-switch-to-buffer*)
   ("C-x k" . persp-kill-buffer*)
   ([remap switch-to-buffer] . persp-ivy-switch-buffer)
   ([remap kill-buffer] . persp-kill-buffer*))
  :custom
  (persp-mode-prefix-key (kbd "C-c M-p"))  ; pick your own prefix key here
  ;; https://emacs.stackexchange.com/questions/14802/never-keep-current-list-of-tags-tables-also
  ;; Don't ask user whether add tags to list, default add.
  (tags-add-tables t)

  (persp-interactive-completion-function 'ivy-completing-read)
  (persp-sort 'created)
  ;; (persp-mode-prefix-key (kbd "C-x C-i"))
  (persp-state-default-file "~/.emacs.d/perspective.save")
  ;; (persp-show-modestring 'header)
  (persp-modestring-short t)
  :mode-hydra
  (persp-mode
   ;; (:title "<perspective commands>")
   ("Buffer"
    (("c" persp-kill-buffer* "persp-kill-buffer*")
     ("a" persp-add-buffer "Query an open buffer to add to current perspective")
     ("d" persp-remove-buffer "Query a buffer to remove from current perspective"))
    "Switch"
    (("n" persp-next "Switch to next perspective" :color red)
     ("p" persp-prev "Switch to previous perspective" :color red)
     ("w" persp-switch "persp-switch")
     ("b" persp-switch-last "persp-switch-last"))
    "Edit"
    (("r" persp-rename "persp-rename")
     ("k" persp-kill "Query a perspective to kill"))
    "Load"
    (("s" persp-state-save "Save all perspectives in all frames to a file")
     ("l" persp-state-load "Load all perspectives from a file"))))
  :config
  (unless (featurep 'counsel)
    (counsel-mode 1))
  (persp-mode)
  (add-hook 'kill-emacs-hook 'persp-state-save))

(use-package workgroups2
  :disabled
  :hook
  (after-init . workgroups-mode)
  :init
  ;; Change prefix key (before activating WG)
  ;; ;; (setq wg-prefix-key (kbd "M-p"))
  (setq wg-prefix-key (kbd "C-c C-c"))
  :config
  ;; Mode Line changes
  ;; Display workgroups in Mode Line?
  (setq wg-mode-line-display-on t)          ; Default: (not (featurep 'powerline))
  (setq wg-flag-modified t)                 ; Display modified flags as well
  (setq wg-mode-line-decor-left-brace "<"
	wg-mode-line-decor-right-brace ">"  ; how to surround it
	wg-mode-line-decor-divider ":")

  ;; ;; What to do on Emacs exit / workgroups-mode exit?
  ;; ;; (setq wg-emacs-exit-save-behavior           'save)      ; Options: 'save 'ask nil
  ;; ;; (setq wg-workgroups-mode-exit-save-behavior 'save)      ; Options: 'save 'ask nil

  ;; ;; Change workgroups session file
  (setq wg-session-file "~/.emacs.d/.emacs_workgroups"))

(provide 'init-workspace)
;;; init-workspace.el ends here
