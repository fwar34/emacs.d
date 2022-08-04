;;; init-org.el --- Useful preset transient commands  -*- coding:utf-8; lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(use-package org
  :ensure nil
  :defer t
  :mode-hydra
  (org-mode
   (:foreign-keys run :color pink :title "<org-mode commands>")
   ("outline"
    (("h" outline-previous-visible-heading "previous visible heading line")
     ("l" outline-next-visible-heading "next visible heading line")
     ("j" outline-forward-same-level "same level forward")
     ("k" outline-backward-same-level "same level backward")
     ("s" org-shifttab "org-shifttab")
     ("t" org-cycle "org-cycle"))
    "org"
    (("d" org-schedule "org-schedule" :exit t)
     ("y" org-deadline "org-deadline" :exit t)
     ("x" org-agenda "org-agenda" :exit t)
     ("w" org-capture "org-capture" :exit t)
     ("p" org-priority "org-priority" :exit t)
     ("m" org-time-stamp "org-time-stamp" :exit t)
     ("i" org-insert-structure-template "org-insert-structure-template" :color blue)
     ("<" hydra-org-template/body "insert template" :exit t)
     ("e" org-show-todo-tree "org-show-todo-tree")
     ("a" org-show-all "org-show-all"))))
  :config
  ;; https://emacs-china.org/t/org-org-indent-mode/16057
  ;; turn on 'org-indent-mode' by default
  (setq org-startup-indented t)
  ;; reference https://raw.githubusercontent.com/Cheukyin/.emacs.d/master/init-org-jekyll.el
  ;; http://cheukyin.github.io/jekyll/emacs/2014-08/org2jekyll.html
  (org-babel-do-load-languages 'org-babel-load-languages '((python . t)
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

  ;; 默认情况下，Org Mode没有打开Markdown文档的转换功能，需要将下面的小代码放到Emacs 的启动配置文件中：
  ;; (setq org-export-backends (quote (ascii html icalendar latex md)))
  (require 'ox-md)

  ;; https://www.zmonster.me/2015/07/15/org-mode-planning.html
  (setq org-todo-keywords '((sequence "TODO(t)" "DOING(i)" "|" "DONE(d)" "ABORT(a)")))
  (setq org-todo-keyword-faces '(("DOING" . "palevioletred")
                                 ("ABORT" . "orange")))
  (setq org-tag-faces '(("trunk" . "brightmagenta")
                        ("dev" . "red")
                        ("develop@trunk" . "brightmagenta")
                        ("develop@dev" . "red")))

  (cl-pushnew '("not" . "note") org-structure-template-alist))

(use-package htmlize :after org)
(use-package ob-go :after org)
(use-package ob-rust :after org)
(use-package org-bullets :after org
  :config
  (org-bullets-mode 1))

(use-package evil-org
  ;; https://github.com/Somelauw/evil-org-mode/blob/master/doc/keythemes.org
  :hook
  (org-mode . evil-org-mode)
  :config
  (evil-org-set-key-theme '(textobjects insert navigation additional shift todo heading return))
  ;; If you want < and > to promote / demote headings and items on a single press, add the following to your org setup:
  (evil-define-key 'normal evil-org-mode-map
    (kbd ">") 'org-meta-right
    (kbd "<") 'org-meta-left))

;; 在 wslg 下面不停的弹出新的 emacs 进程
;; org-pomodoro setting
(use-package org-pomodoro
  :disabled
  :ensure t
  :after org)

;; ox-hugo and org2jekyll recursion require XXXXX
(use-package org2jekyll
  :disabled
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
                            ))))

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

;; 在配置文件中（我使用的是模块化的配置，所以我的配置在 init-org.el 文件中）增加如下程序，就可实现 org-mode 中的自动换行。
(add-hook 'org-mode-hook (lambda () (setq truncate-lines nil)))

(with-eval-after-load 'org
  (require 'org-tempo) ; Required from org 9 onwards for old template expansion
  ;; Reset the org-template expnsion system, this is need after upgrading to org 9 for some reason
  (setq org-structure-template-alist (eval (car (get 'org-structure-template-alist 'standard-value))))
  (defun hot-expand (str &optional mod header)
    "Expand org template.

STR is a structure template string recognised by org like <s. MOD is a
string with additional parameters to add the begin line of the
structure element. HEADER string includes more parameters that are
prepended to the element after the #+HEADER: tag."
    (let (text)
      (when (region-active-p)
        (setq text (buffer-substring (region-beginning) (region-end)))
        (delete-region (region-beginning) (region-end))
        (deactivate-mark))
      (when header (insert "#+HEADER: " header) (forward-line))
      (insert str)
      (org-tempo-complete-tag)
      (when mod (insert mod) (forward-line))
      (when text (insert text))))
  (defhydra hydra-org-template (:color blue :hint nil)
    "
 _c_enter  _q_uote     _e_macs-lisp    _L_aTeX:
 _l_atex   _E_xample   _p_erl          _i_ndex:
 _a_scii   _v_erse     _P_erl tangled  _I_NCLUDE:
 _s_rc     _n_ote      plant_u_ml      _H_TML:
 _h_tml    ^ ^         ^ ^             _A_SCII:
"
    ("s" (hot-expand "<s"))
    ("E" (hot-expand "<e"))
    ("q" (hot-expand "<q"))
    ("v" (hot-expand "<v"))
    ("n" (hot-expand "<not"))
    ("c" (hot-expand "<c"))
    ("l" (hot-expand "<l"))
    ("h" (hot-expand "<h"))
    ("a" (hot-expand "<a"))
    ("L" (hot-expand "<L"))
    ("i" (hot-expand "<i"))
    ("e" (hot-expand "<s" "emacs-lisp"))
    ("p" (hot-expand "<s" "perl"))
    ("u" (hot-expand "<s" "plantuml :file CHANGE.png"))
    ("P" (hot-expand "<s" "perl" ":results output :exports both :shebang \"#!/usr/bin/env perl\"\n"))
    ("I" (hot-expand "<I"))
    ("H" (hot-expand "<H"))
    ("A" (hot-expand "<A"))
    ("<" self-insert-command "ins")
    ("o" nil "quit"))

  (define-key org-mode-map "<"
    (lambda () (interactive)
      (if (or (region-active-p) (looking-back "^"))
          (hydra-org-template/body)
        (self-insert-command 1)))))

(provide 'init-org)
;;; init-org.el ends here
