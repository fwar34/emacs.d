;; -*- coding: utf-8; lexical-binding: t; -*-
;; https://emacs-china.org/t/emacs-builtin-mode/11937/63

(use-package winner
  :ensure nil ;; emacs自带
  :config
  (winner-mode)
  )

(use-package ediff
  :ensure nil
  :hook
  ;; 可以应用在ediff上，恢复由ediff导致的窗体变动。
  (ediff-quit . winner-undo)
  )

(use-package isearch
  :config
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
  :if (executable-find "rg")
  :config
  (setq xref-search-program 'ripgrep))

(provide 'init-builtin)
