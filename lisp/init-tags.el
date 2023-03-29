;;; init-tags.el --- Useful preset transient commands  -*- coding:utf-8; lexical-binding: t; -*-
;;; Commentary:

;;; Code:
;; (defun my-gtags-init()
;;   (when (executable-find "pygmentize")
;;     (setenv "GTAGSLABEL" "pygments")
;;     (if (eq system-type 'windows-nt)
;;         ;; (setenv "GTAGSCONF" (expand-file-name "~/global/share/gtags/gtags.conf"))
;;         (setenv "GTAGSCONF"
;;                 (let ((str (executable-find "gtags")))
;;                   (string-match "global.*" str)
;;                   (replace-match "global/share/gtags/gtags.conf" nil nil str 0)))
;;       (setenv "GTAGSCONF" "/usr/local/share/gtags/gtags.conf")))
;;   )
;; (my-gtags-init)

;; ggtags
(use-package ggtags
  :disabled
  :ensure t
  :hook
  (prog-mode . ggtags-mode)
  :config
  (setq ggtags-highlight-tag nil)
  ;; (add-hook 'c-mode-common-hook
  ;;           (lambda()
  ;;             (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
  ;;               (ggtags-mode 1))))
  ;; (add-hook 'python-hook
  ;;           (lambda()
  ;;             (when (derived-mode-p 'python-mode)
  ;;               (ggtags-mode 1))))
  ;; (ggtags-mode 1)
  (setq-local imenu-create-index-function 'ggtags-build-imenu-index))

;; https://github.com/redguardtoo/counsel-etags#ctags-setup
(use-package counsel-etags
  :disabled
  :commands
  (counsel-etags-find-tag-at-point)
  :init
  ;; Setup auto update now
  (add-hook 'prog-mode-hook
            (lambda ()
              (add-hook 'after-save-hook
                        'counsel-etags-virtual-update-tags 'append 'local)))
  :config
  (with-eval-after-load 'counsel-etags
    ;; counsel-etags-ignore-directories does NOT support wildcast
    (add-to-list 'counsel-etags-ignore-directories "build_clang")
    (add-to-list 'counsel-etags-ignore-directories "debian")
    (add-to-list 'counsel-etags-ignore-directories ".ccls-cache")
    (add-to-list 'counsel-etags-ignore-directories "doxygen-doc")
    ;; counsel-etags-ignore-filenames supports wildcast
    (add-to-list 'counsel-etags-ignore-filenames "*.log")
    (add-to-list 'counsel-etags-ignore-filenames "*.html")
    (add-to-list 'counsel-etags-ignore-filenames "*.tag")
    (add-to-list 'counsel-etags-ignore-filenames "TAGS")
    (add-to-list 'counsel-etags-ignore-filenames "*.xml")
    (add-to-list 'counsel-etags-ignore-filenames "*.json"))

  (setq counsel-etags-ctags-options-base "~/.ctags")
  (setq counsel-etags-update-interval 60)

  ;; auto update tags
  ;; Don't ask before rereading the TAGS files if they have changed
  (setq tags-revert-without-query t)
  ;; Don't warn when TAGS files are large
  (setq large-file-warning-threshold nil))

(use-package citre
  ;; :disabled
  :defer t
  :init
  ;; This is needed in `:init' block for lazy load to work.
  (require 'citre-config)
  ;; Bind your frequently used commands.
  (global-set-key (kbd "C-x c j") 'citre-jump)
  (global-set-key (kbd "C-x c J") 'citre-jump-back)
  (global-set-key (kbd "C-x c p") 'citre-ace-peek)
  (global-set-key (kbd "C-x c u") 'citre-update-this-tags-file)
  :pretty-hydra
  (my-hydra-citre
   (:foreign-keys warn :red teal :quit-key "q" :title "<citre commands>")
   ("Jump"
    (("m" citre-mode "citre mode" :toggle t)
     ("j" citre-jump "Jump to the definition of the symbol at point.")
     ("J" citre-jump-back "Go back to the position before last ‘citre-jump’."))
    "Peek"
    (("p" citre-ace-peek "Peek the definition of a symbol on screen using ace jump."))
    "Update"
    (("u" citre-update-this-tags-file "Update the currently used tags file.")
     ("c" citre-create-tags-file "Create a new tags file."))))
  :config
  (setq
   ;; Set these if readtags/ctags is not in your path.
   ;; citre-readtags-program "/path/to/readtags"
   ;; citre-ctags-program "/path/to/ctags"
   ;; Set this if you use project management plugin like projectile.  It's
   ;; used for things like displaying paths relatively, see its docstring.
   citre-project-root-function 'projectile-project-root
   ;; Set this if you want to always use one location to create a tags file.
   ;; citre-default-create-tags-file-location 'global-cache
   ;; See the "Create tags file" section above to know these options
   citre-use-project-root-when-creating-tags t
   citre-auto-enable-citre-mode-modes '(prog-mode)
   citre-peek-auto-restore-after-jump nil
   citre-prompt-language-for-ctags-command t))

(use-package imenu-anywhere
  :commands imenu-anywhere)

(provide 'init-tags)
;;; init-tags.el ends here
