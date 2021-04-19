;; -*- coding: utf-8; lexical-binding: t; -*-
;;-------------------------------------------------------------
;; init-org
;;-------------------------------------------------------------
(with-eval-after-load 'org
  ;; reference https://raw.githubusercontent.com/Cheukyin/.emacs.d/master/init-org-jekyll.el
  ;; http://cheukyin.github.io/jekyll/emacs/2014-08/org2jekyll.html
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (shell . t)
     (emacs-lisp . t)
     (C . t)))
  (setq org-confirm-babel-evaluate nil)
  (setq org-src-fontify-natively t)
  ;; indent codes in org mode
  ;; https://stackoverflow.com/questions/15773354/indent-code-in-org-babel-src-blocks
  (setq org-src-tab-acts-natively t)
  ;; https://emacs-china.org/t/topic/440/10
  (cl-case system-type
    ('gnu/linux
     (custom-set-faces
      ;; custom-set-faces was added by Custom.
      ;; If you edit it by hand, you could mess it up, so be careful.
      ;; Your init file should contain only one such instance.
      ;; If there is more than one, they won't work right.
      '(org-table ((t (:foreground "#6c71c4" :family "Ubuntu Mono")))))))
  )

(use-package htmlize
  :after org
  :ensure t
  )

(use-package ob-go
  :ensure t
  :after org
  )

(use-package ob-rust
  :ensure t
  :after org
  ) 

(use-package evil-org
  ;; https://github.com/Somelauw/evil-org-mode/blob/master/doc/keythemes.org
  :ensure t
  :hook
  (org-mode . evil-org-mode)
  :config
  (evil-org-set-key-theme '(textobjects insert navigation additional shift todo heading return))
  ;; If you want < and > to promote / demote headings and items on a single press, add the following to your org setup:
  (evil-define-key 'normal evil-org-mode-map
    (kbd ">") 'org-meta-right
    (kbd "<") 'org-meta-left)
  )

;; org-pomodoro setting
(use-package org-pomodoro
  :ensure t
  :after org
  )

(use-package org-bullets
  :ensure t
  :hook
  (org-mode . (lambda () (org-bullets-mode 1)))
  )

(use-package org2jekyll
  :ensure t
  :after org
  :config
  (custom-set-variables '(org2jekyll-blog-author "feng")
                        ;; '(org2jekyll-source-directory (expand-file-name "~/test/org"))
                        ;; '(org2jekyll-jekyll-directory (expand-file-name "~/test/public_html"))
                        '(org2jekyll-source-directory  "")
                        '(org2jekyll-jekyll-directory  "")
                        '(org2jekyll-jekyll-drafts-dir "")
                        ;; '(org2jekyll-jekyll-posts-dir  "_posts/")
                        '(org2jekyll-jekyll-posts-dir "")
                        '(org-publish-project-alist
                          `(("default"
                             :base-directory ,(org2jekyll-input-directory)
                             :base-extension "org"
                             ;; :publishing-directory "/ssh:user@host:~/html/notebook/"
                             :publishing-directory ,(org2jekyll-output-directory)
                             :publishing-function org-html-publish-to-html
                             :headline-levels 4
                             :section-numbers nil
                             :with-toc nil
                             :html-head "<link rel=\"stylesheet\" href=\"./css/style.css\" type=\"text/css\"/>"
                             :html-preamble t
                             :recursive t
                             :make-index t
                             :html-extension "html"
                             :body-only t)

                            ("post"
                             :base-directory ,(org2jekyll-input-directory)
                             :base-extension "org"
                             :publishing-directory ,(org2jekyll-output-directory org2jekyll-jekyll-posts-dir)
                             :publishing-function org-html-publish-to-html
                             :headline-levels 4
                             :section-numbers nil
                             :with-toc nil
                             :html-head "<link rel=\"stylesheet\" href=\"./css/style.css\" type=\"text/css\"/>"
                             :html-preamble t
                             :recursive t
                             :make-index t
                             :html-extension "html"
                             :body-only t)

                            ("images"
                             :base-directory ,(org2jekyll-input-directory "img")
                             :base-extension "jpg\\|gif\\|png"
                             :publishing-directory ,(org2jekyll-output-directory "img")
                             :publishing-function org-publish-attachment
                             :recursive t)

                            ("js"
                             :base-directory ,(org2jekyll-input-directory "js")
                             :base-extension "js"
                             :publishing-directory ,(org2jekyll-output-directory "js")
                             :publishing-function org-publish-attachment
                             :recursive t)

                            ("css"
                             :base-directory ,(org2jekyll-input-directory "css")
                             :base-extension "css\\|el"
                             :publishing-directory ,(org2jekyll-output-directory "css")
                             :publishing-function org-publish-attachment
                             :recursive t)

                            ;; ("web" :components ("images" "js" "css"))
                            )))
  )

;; reference from http://ju.outofmemory.cn/entry/348743
;; https://www.zmonster.me/2018/02/28/org-mode-capture.html#orgbfd201a
(with-eval-after-load 'org
  (setq org-agenda-files '("~/.emacs.d"))
  (setq org-default-notes-file "~/.emacs.d/org/inbox.org")
  ;; clear org-capture-templates
  (setq org-capture-templates nil)
  ;; add a group
  (add-to-list 'org-capture-templates '("t" "Tasks"))
  (add-to-list 'org-capture-templates
               '("td" "Todo" entry (file+headline "~/.emacs.d/cap/gtd.org" "工作安排")
                 "* TODO [#B] %?\n  %i\n"
                 :empty-lines 1))
  (add-to-list 'org-capture-templates
               '("tw" "Work Task" entry (file+headline "~/.emacs.d/cap/work.org" "工作内容")
                 "* TODO %^{任务名}\n%U\n%a\n" :clock-in t :clock-resume t))
  (add-to-list 'org-capture-templates
               '("j" "Journal" entry (file+datetree "~/.emacs.d/cap/journal.org" "我的记录")
                 "* 我的记录\n%U"))
  (add-to-list 'org-capture-templates
               '("i" "Inbox" entry (file "~/.emacs.d/cap/inbox.org")
                 "* %U - %^{heading} %^g\n %?\n"))
  (add-to-list 'org-capture-templates
               '("n" "Notes" entry (file "~/.emacs.d/cap/notes.org")
                 "* %^{heading} %t %^g\n  %?\n"))
  (add-to-list 'org-capture-templates
               '("w" "Web collections" entry (file+headline "~/.emacs.d/cap/web.org" "Web")
                 "* %U %:annotation\n\n%:initial\n\n%?"))
  (add-to-list 'org-capture-templates
               `("b" "Blog" plain (file ,(concat "~/.emacs.d/cap/blog/"
                                                 (format-time-string "%Y-%m-%d.org")))
                 ,(concat "#+startup: showall\n"
                          "#+options: toc:nil\n"
                          "#+begin_export html\n"
                          "---\n"
                          "layout     : post\n"
                          "title      : %^{标题}\n"
                          "categories : %^{类别}\n"
                          "tags       : %^{标签}\n"
                          "---\n"
                          "#+end_export\n"
                          "#+TOC: headlines 2\n")))
  
  (setq org-src-fontify-natively t))

;; (add-hook 'org-mode-hook 'evil-org-mode)
;; (evil-org-set-key-theme '(navigation insert textobjects additional calendar))
;; (require 'evil-org-agenda)
;; (evil-org-agenda-set-keys)
;; https://github.com/Somelauw/evil-org-mode#common-issues
;; (setq evil-want-C-i-jump nil)

;; 默认情况下，Org Mode没有打开Markdown文档的转换功能，需要将下面的小代码放到Emacs 的启动配置文件中：
;; (setq org-export-backends (quote (ascii html icalendar latex md)))
(with-eval-after-load "org" '(require 'ox-md nil t))

;; 在配置文件中（我使用的是模块化的配置，所以我的配置在 init-org.el 文件中）增加如下程序，就可实现 org-mode 中的自动换行。
(add-hook 'org-mode-hook (lambda () (setq truncate-lines nil)))

(provide 'init-org)
