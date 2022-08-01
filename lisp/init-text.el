;;; init-text.el --- Packages -*- coding:utf-8; lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(use-package easy-hugo
  :init
  ;; (setq easy-hugo-postdir "content/posts")
  (setq easy-hugo-basedir "~/mine/fwar34.github.io/")
  (setq easy-hugo-url "https://fwar34.github.io")
  (setq easy-hugo-root "~/mine/fwar34.github.io/")
  (setq easy-hugo-previewtime "300")
  (setq easy-hugo-default-ext ".org")
  (setq easy-hugo-org-header t)
  :bind ("C-c C-h" . easy-hugo))

;; ox-hugo and org2jekyll recursion require XXXXX
;; ox-hugo is an Org exporter backend that exports Org to Hugo-compatible Markdown (Blackfriday) and also generates the front-matter (in TOML or YAML format).
(use-package ox-hugo
  :disabled
  :ensure t
  :after ox)

;; markdown and org preview
(use-package grip-mode
  :disabled
  :ensure t
  :bind (:map markdown-mode-command-map
              ("g" . grip-mode))
  :config
  ;; This package adds an Org mode export backend for GitHub Flavored Markdown.
  (use-package ox-gfm
    :after org))

;; Markdown，Org，HTML预览也可以使用这个，依赖simple-httpd 和 websocket，不会生成其它外部文件
(use-package maple-preview
  :init
  (use-package websocket)
  (use-package simple-httpd)
  :load-path "site-lisp/emacs-maple-preview"
  :commands maple-preview-mode)

(provide 'init-text)
;;; init-text.el ends here
