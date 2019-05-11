;;-------------------------------------------------------------
;; init-tags.el
;;-------------------------------------------------------------
(defun my-gtags-init()
  (when (executable-find "pygmentize")
    (setenv "GTAGSLABEL" "pygments")
    (if (eq system-type 'windows-nt)
        ;; (setenv "GTAGSCONF" (expand-file-name "~/global/share/gtags/gtags.conf"))
        (setenv "GTAGSCONF"
                (let ((str (executable-find "gtags")))
                  (string-match "global.*" str)
                  (replace-match "global/share/gtags/gtags.conf" nil nil str 0)))
      (setenv "GTAGSCONF" "/usr/local/share/gtags/gtags.conf")))
  )
(my-gtags-init)

;; ggtags
(use-package ggtags
  :disabled
  :ensure t
  :hook
  (prog-mode . ggtags-mode)
  :delight ggtags-mode
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
  (setq-local imenu-create-index-function #'ggtags-build-imenu-index)
  )

(use-package counsel-gtags
  :ensure t
  :defer t
  :config
  (setq counsel-gtags-auto-update t)
  )

(use-package counsel-etags
  :disabled
  :ensure t
  :defer
  :config
  (eval-after-load 'counsel-etags
    '(progn
       ;; counsel-etags-ignore-directories does NOT support wildcast
       (add-to-list 'counsel-etags-ignore-directories "build_clang")
       (add-to-list 'counsel-etags-ignore-directories "debian")
       ;; counsel-etags-ignore-filenames supports wildcast
       (add-to-list 'counsel-etags-ignore-filenames "*.log")
       (add-to-list 'counsel-etags-ignore-filenames "*.html")
       (add-to-list 'counsel-etags-ignore-filenames "*.tag")
       (add-to-list 'counsel-etags-ignore-filenames "*.json")))

  ;; auto update tags
  ;; Don't ask before rereading the TAGS files if they have changed
  (setq tags-revert-without-query t)
  ;; Don't warn when TAGS files are large
  (setq large-file-warning-threshold nil)
  ;; Setup auto update now
  (add-hook 'prog-mode-hook
            (lambda ()
              (add-hook 'after-save-hook
                        'counsel-etags-virtual-update-tags 'append 'local)))
  )

(provide 'init-tags)
