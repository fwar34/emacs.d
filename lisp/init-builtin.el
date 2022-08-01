;;; init-builtin.el --- Useful preset transient commands  -*- coding:utf-8; lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(use-package winner
  :ensure nil
  :config
  (winner-mode))

(use-package ediff
  :ensure nil
  :hook
  ;; 可以应用在ediff上，恢复由ediff导致的窗体变动。
  (ediff-quit . winner-undo))

(use-package isearch
  :ensure nil
  :bind
  (:map isearch-mode-map
        ([remap isearch-delete-char] . isearch-del-char))
  :custom
  (isearch-lazy-count t)
  (lazy-count-prefix-format "%s/%s ")
  (lazy-highlight-cleanup nil))

;; 功能             原生        evil-mode
;; hs-hide-block	C-c @ C-h	zc
;; hs-show-block	C-c @ C-s	zo
;; hs-hide-all	    C-c @ C-M-h	zm
;; hs-show-all      C-c @ C-M-s	zr
;; hs-hide-level	C-c @ C-l	无
;; hs-toggle-hiding	C-c @ C-c	za
(use-package hideshow
  :ensure nil
  :hook (prog-mode . hs-minor-mode)
  :config
  (defconst hideshow-folded-face '((t (:inherit 'font-lock-comment-face :box t))))

  (defun hideshow-folded-overlay-fn (ov)
    (when (eq 'code (overlay-get ov 'hs))
      (let* ((nlines (count-lines (overlay-start ov) (overlay-end ov)))
             (info (format " ... #%d " nlines)))
        (overlay-put ov 'display (propertize info 'face hideshow-folded-face)))))

  (setq hs-set-up-overlay 'hideshow-folded-overlay-fn)
  )

;; proced 类似 top, 全平台支持
(setq-default proced-auto-update-flag t ; 自动刷新
			  proced-auto-update-interval 1) ; 默认为5秒一次

(use-package xref
  :ensure nil
  :init
  ;; On Emacs 28, `xref-search-program' can be set to `ripgrep'.
  ;; `project-find-regexp' benefits from that.
  (when (>= emacs-major-version 28)
    (when (executable-find "rg")
    (setq xref-search-program 'ripgrep))
    ;; (setq xref-show-xrefs-function #'xref-show-definitions-completing-read)
    ;; (setq xref-show-definitions-function #'xref-show-definitions-completing-read)
    )
  :hook
  ((xref-after-return xref-after-jump) . recenter))

(provide 'init-builtin)
;;; init-builtin.el ends here
