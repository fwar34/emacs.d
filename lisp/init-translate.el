;;; init-translate.el --- Useful preset transient commands  -*- coding:utf-8; lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(use-package youdao-dictionary
  :straight
  (:host github :repo "xuchunyang/youdao-dictionary.el")
  ;; :after evil
  ;; :functions
  ;; evil-define-key
  :commands
  (youdao-dictionary-search-at-point
   youdao-dictionary-search-at-point+
   youdao-dictionary-search-from-input)
  :config
  ;; (use-package popup)
  ;; popwin setting
  ;; (use-package popwin
  ;;   :config
  ;;   (popwin-mode 1))

  ;; Enable Cache
  (setq url-automatic-caching t)
  ;; Integrate with popwin-el (https://github.com/m2ym/popwin-el)
  ;; (push "*Youdao Dictionary*" popwin:special-display-config)
  ;; Set file path for saving search history
  (setq youdao-dictionary-search-history-file "~/.emacs.d/.youdao")
  ;; Enable Chinese word segmentation support (支持中文分词)
  (setq youdao-dictionary-use-chinese-word-segmentation t)

  ;; press 'q' to quit youdao output buffer
  ;; (defun my-quit-window (&rest _)
  ;;   (with-current-buffer "*Youdao Dictionary*"
  ;;     (evil-local-set-key 'normal (kbd "q") 'quit-window)))
  ;; (advice-add 'youdao-dictionary-search-at-point :after 'my-quit-window)
  ;; (defadvice youdao-dictionary-search-at-point (after advice-youdao-point activate)
  ;;   (with-current-buffer "*Youdao Dictionary*"
  ;;     (evil-local-set-key 'normal (kbd "q") 'quit-window)))
  ;; (with-eval-after-load 'evil
  ;;   (evil-define-key 'normal youdao-dictionary-mode-map "q" 'kill-buffer-and-window))
  ;; (add-hook 'youdao-dictionary-mode-hook (lambda () (interactive) (evil-local-set-key 'normal "q" #'kill-buffer-and-window)))
  ;; (add-hook 'youdao-dictionary-mode-hook
  ;;           (lambda ()
  ;;             (define-key evil-normal-state-local-map (kbd "q") 'quit-window)))
  )

(use-package go-translate
  :disabled
  :ensure t
  :custom
  (go-translate-base-url "https://translate.google.cn")
  (go-translate-local-language "zh-CN")
  (go-translate-buffer-follow-p t) ;;翻译完成后总是将光标切换到翻译窗口
  :config
  ;; https://emacs-china.org/t/google/14407/48?u=fwar34
  (setq go-translate-token-current (cons 430675 2721866130))
  )

(use-package english-teacher
  :straight
  (:host github :repo "loyalpartner/english-teacher.el")
  :hook ((Info-mode
          elfeed-show-mode
          eww-mode
          Man-mode
          ;; help-mode
          Woman-Mode) . english-teacher-follow-mode)
  :config
  (defun english-teacher-eldoc-show-result-function (origin translation)
    (eldoc-message (format "%s:%s\norigin:%s"
                           (symbol-name english-teacher-backend)
                           translation
                           origin)))
  (setq english-teacher-show-result-function 'english-teacher-eldoc-show-result-function))

(provide 'init-translate)
;;; init-translate.el ends here
