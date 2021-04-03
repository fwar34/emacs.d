;; -*- coding: utf-8; lexical-binding: t; -*-
;; init dired

;; (use-package dired+
;;   :straight t)

;; https://emacs-china.org/t/emacs-builtin-mode/11937/20
;; 显示或隐藏隐藏文件，按键P
(define-advice dired-do-print (:override (&optional _))
    "Show/hide dotfiles."
    (interactive)
    (if (or (not (boundp 'dired-dotfiles-show-p)) dired-dotfiles-show-p)
        (progn
          (setq-local dired-dotfiles-show-p nil)
          (dired-mark-files-regexp "^\\.")
          (dired-do-kill-lines))
      (revert-buffer)
      (setq-local dired-dotfiles-show-p t)))

;; dired显示选项
(setq dired-listing-switches "-Afhlvt")

(with-eval-after-load 'dired
  ;; dired递归copy delete
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (require 'dired-x)
  (setq dired-dwim-target t))

;; dired-imenu
(use-package dired-imenu
  :disabled
  :after dired
  :ensure t
  )

(use-package dired-rainbow
  :disabled
  :ensure t
  :after dired
  :commands dired-rainbow-define dired-rainbow-define-chmod
  :init
  (dired-rainbow-define dotfiles "gray" "\\..*")
  ;; (dired-rainbow-define web "#4e9a06" ("htm" "html" "xhtml" "xml" "xaml" "css" "js"
  ;;                                      "json" "asp" "aspx" "haml" "php" "jsp" "ts"
  ;;                                      "coffee" "scss" "less" "phtml"))
  ;; (dired-rainbow-define prog "green yellow3" (".*\\.el" "l" "ml" "py" "rb" "pl" "pm" "c"
  ;;                                       "cpp" "cxx" "c++" "h" "hpp" "hxx" "h++"
  ;;                                       "m" "cs" "mk" "make" "swift" "go" "java"
  ;;                                       "asm" "robot" "yml" "yaml" "rake" "lua"))
  ;; (dired-rainbow-define sh "yellow" ("sh" "bash" "zsh" "fish" "csh" "ksh"
  ;;                                          "awk" "ps1" "psm1" "psd1" "bat" "cmd"))
  ;; (dired-rainbow-define text "yellow green" ("txt" "md" "org" "ini" "conf" "rc"
  ;;                                            "vim" "vimrc" "exrc"))
  ;; (dired-rainbow-define doc "spring green" ("doc" "docx" "ppt" "pptx" "xls" "xlsx"
  ;;                                           "csv" "rtf" "wps" "pdf" "texi" "tex"
  ;;                                           "odt" "ott" "odp" "otp" "ods" "ots"
  ;;                                           "odg" "otg"))
  ;; (dired-rainbow-define misc "gray50" ("DS_Store" "projectile" "cache" "elc"
  ;;                                      "dat" "meta"))
  ;; (dired-rainbow-define media "#ce5c00" ("mp3" "mp4" "MP3" "MP4" "wav" "wma"
  ;;                                        "wmv" "mov" "3gp" "avi" "mpg" "mkv"
  ;;                                        "flv" "ogg" "rm" "rmvb"))
  ;; (dired-rainbow-define picture "purple3" ("bmp" "jpg" "jpeg" "gif" "png" "tiff"
  ;;                                          "ico" "svg" "psd" "pcd" "raw" "exif"
  ;;                                          "BMP" "JPG" "PNG"))
  ;; (dired-rainbow-define archive "saddle brown" ("zip" "tar" "gz" "tgz" "7z" "rar"
  ;;                                               "gzip" "xz" "001" "ace" "bz2" "lz"
  ;;                                               "lzma" "bzip2" "cab" "jar" "iso"))
  ;; boring regexp due to lack of imagination
  (dired-rainbow-define log (:inherit default :italic t) ".*\\.log")
  ;; highlight executable files, but not directories
  (dired-rainbow-define-chmod executable-unix "green" "-[rw-]+x.*")
  )

(use-package dired-k
  :after dired
  :ensure t
  :config
  ;; always execute dired-k when dired buffer is opened
  (add-hook 'dired-initial-position-hook 'dired-k)
  (add-hook 'dired-after-readin-hook #'dired-k-no-revert)
  ;; (setq dired-k-style 'git)
  (setq dired-k-style 'k.zsh)
  (setq dired-k-padding 1)
  ;; (define-key dired-mode-map (kbd "K") 'dired-k)
  ;; ;; You can use dired-k alternative to revert-buffer
  ;; (define-key dired-mode-map (kbd "g") 'dired-k)
  )

(use-package dired-single
  ;; https://github.com/crocket/dired-single
  ;; dired-single
  :ensure t
  :after dired
  :init
  (setq dired-single-use-magic-buffer t)
  (setq dired-single-magic-buffer-name "*dired-buffer*")
  (add-hook 'dired-mode-hook (lambda () (rename-buffer "*dired-buffer*")))

  ;; key map
  (with-eval-after-load 'evil
    (defun my-dired-init ()
      "Bunch of stuff to run for dired, either immediately or when it's loaded."
      (evil-define-key 'normal dired-mode-map "q" #'kill-this-buffer)
      (evil-define-key 'emacs dired-mode-map "q" #'kill-this-buffer)
      (evil-define-key 'normal dired-mode-map "f" #'swiper)
      (evil-define-key 'normal dired-mode-map "h" (lambda () (interactive) (dired-single-buffer "..")))
      (evil-define-key 'normal dired-mode-map "l" 'dired-single-buffer)
      (evil-define-key 'normal dired-mode-map "^" (lambda () (interactive) (dired-single-buffer "..")))
      (if (display-graphic-p)
          (progn
            (evil-define-key 'normal dired-mode-map [return] 'dired-single-buffer)
            (evil-define-key 'normal dired-mode-map [mouse-1] 'dired-single-buffer-mouse))
        (evil-define-key 'normal dired-mode-map (kbd "RET") 'dired-single-buffer)))
    ;; if dired's already loaded, then the keymap will be bound
    (if (boundp 'dired-mode-map)
        ;; we're good to go; just add our bindings
        (my-dired-init)
      ;; it's not loaded yet, so add our bindings to the load-hook
      (add-hook 'dired-load-hook 'my-dired-init)))
  :config
  ;;-------------------------------------------------------------
  ;; advice-add 
  ;;-------------------------------------------------------------
  (defun advice-dired-single-buffer (fn &optional DEFAULT-DIRNAME)
    (save-excursion
      (end-of-line)
      (let* ((eol (point))
             (need-del (if (string= DEFAULT-DIRNAME "..")
                           nil
                         (beginning-of-line)
                         (not (re-search-forward "^  d" eol t)))))
        (funcall fn DEFAULT-DIRNAME)
        (if need-del
            (kill-buffer "*dired-buffer*")))))
  (advice-add #'dired-single-buffer :around #'advice-dired-single-buffer)
  ;;-------------------------------------------------------------
  ;; define-advice
  ;;-------------------------------------------------------------
  ;; (define-advice dired-single-buffer (:around (fn &optional DEFAULT-DIRNAME) advice-dired-single-buffer)
  ;;   "kill dired buffer when file open"
  ;;   (end-of-line)
  ;;   (let* ((eol (point))
  ;;          (need-del (if (string= DEFAULT-DIRNAME "..")
  ;;                        nil
  ;;                      (beginning-of-line)
  ;;                      (not (re-search-forward "^  d" eol t))
  ;;                      )))
  ;;     (funcall fn DEFAULT-DIRNAME)
  ;;     (if need-del
  ;;         (kill-buffer "*dired-buffer*")))
  ;;     )
  ;;-------------------------------------------------------------
  ;; defadvice
  ;;-------------------------------------------------------------
  ;; (defadvice dired-single-buffer (around advice-dired-single-buffer (&optional DEFAULT-DIRNAME) activate)
  ;;   (save-excursion
  ;;     (end-of-line)
  ;;     (let* ((eol (point))
  ;;            (need-del (if (string= DEFAULT-DIRNAME "..")
  ;;                          nil
  ;;                        (beginning-of-line)
  ;;                        (not (re-search-forward "^  d" eol t)))))
  ;;       ad-do-it
  ;;       (if need-del
  ;;           (kill-buffer "*dired-buffer*")))))
  )

(provide 'init-dired)
