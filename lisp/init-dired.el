;; -*- coding: utf-8; lexical-binding: t; -*-
;; init dired

(use-package dired
  :config
  (general-define-key
   :states 'normal
   :keymaps 'dired-mode-map
   "w" 'dired-toggle-read-only
   "," 'my-dired-hydra/body
   "." 'hydra-dired/body)

  (general-define-key
   :states 'normal
   :keymaps 'wdired-mode-map
   :prefix ","
   "qq" 'wdired-abort-changes
   "zz" 'wdired-finish-edit)


  (defhydra my-dired-hydra (:hint nil :color pink)
    "
_+_ mkdir          _v_iew           _m_ark             _(_ details        _i_nsert-subdir    wdired
_C_opy             _O_ view other   _U_nmark all       _)_ omit-mode      _$_ hide-subdir    C-x C-q : edit
_D_elete           _o_pen other     _u_nmark           _l_ redisplay      _w_ kill-subdir    C-c C-c : commit
_R_ename           _M_ chmod        _t_oggle           _g_ revert buf     _e_ ediff          C-c ESC : abort
_Y_ rel symlink    _G_ chgrp        _E_xtension mark   _s_ort             _=_ pdiff
_S_ymlink          ^ ^              _F_ind marked      _,_ toggle hydra   \\ flyspell
_r_sync            ^ ^              ^ ^                ^ ^                _?_ summary
_z_ compress-file  _A_ find regexp
_Z_ compress       _Q_ repl regexp

T - tag prefix
"
    ("\\" dired-do-ispell)
    ("(" dired-hide-details-mode)
    (")" dired-omit-mode)
    ("+" dired-create-directory)
    ("=" diredp-ediff)         ;; smart diff
    ("?" dired-summary)
    ("$" diredp-hide-subdir-nomove)
    ("A" dired-do-find-regexp)
    ("C" dired-do-copy)        ;; Copy all marked files
    ("D" dired-do-delete)
    ("E" dired-mark-extension)
    ("e" dired-ediff-files)
    ("F" dired-do-find-marked-files)
    ("G" dired-do-chgrp)
    ("g" revert-buffer)        ;; read all directories again (refresh)
    ("i" dired-maybe-insert-subdir)
    ("l" dired-do-redisplay)   ;; relist the marked or singel directory
    ("M" dired-do-chmod)
    ("m" dired-mark)
    ("O" dired-display-file)
    ("o" dired-find-file-other-window)
    ("Q" dired-do-find-regexp-and-replace)
    ("R" dired-do-rename)
    ("r" dired-do-rsynch)
    ("S" dired-do-symlink)
    ("s" dired-sort-toggle-or-edit)
    ("t" dired-toggle-marks)
    ("U" dired-unmark-all-marks)
    ("u" dired-unmark)
    ("v" dired-view-file)      ;; q to exit, s to search, = gets line #
    ("w" dired-kill-subdir)
    ("Y" dired-do-relsymlink)
    ("z" diredp-compress-this-file)
    ("Z" dired-do-compress)
    ("q" nil)
    ("," nil :color blue)
    )
  )

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
(setq dired-listing-switches "-Aflvt")

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
  ;; :disabled
  :ensure t
  :after dired
  :config
  (dired-rainbow-define-chmod directory "#6cb2eb" "d.*")
  (dired-rainbow-define html "#eb5286" ("css" "less" "sass" "scss" "htm" "html" "jhtm" "mht" "eml" "mustache" "xhtml"))
  (dired-rainbow-define xml "#f2d024" ("xml" "xsd" "xsl" "xslt" "wsdl" "bib" "json" "msg" "pgn" "rss" "yaml" "yml" "rdata"))
  (dired-rainbow-define document "#9561e2" ("docm" "doc" "docx" "odb" "odt" "pdb" "pdf" "ps" "rtf" "djvu" "epub" "odp" "ppt" "pptx"))
  (dired-rainbow-define markdown "#ffed4a" ("org" "etx" "info" "markdown" "md" "mkd" "nfo" "pod" "rst" "tex" "textfile" "txt"))
  (dired-rainbow-define database "#6574cd" ("xlsx" "xls" "csv" "accdb" "db" "mdb" "sqlite" "nc"))
  (dired-rainbow-define media "#de751f" ("mp3" "mp4" "MP3" "MP4" "avi" "mpeg" "mpg" "flv" "ogg" "mov" "mid" "midi" "wav" "aiff" "flac"))
  (dired-rainbow-define image "#f66d9b" ("tiff" "tif" "cdr" "gif" "ico" "jpeg" "jpg" "png" "psd" "eps" "svg"))
  (dired-rainbow-define log "#c17d11" ("log"))
  (dired-rainbow-define shell "#f6993f" ("awk" "bash" "bat" "sed" "sh" "zsh" "vim"))
  (dired-rainbow-define interpreted "#38c172" ("py" "ipynb" "rb" "pl" "t" "msql" "mysql" "pgsql" "sql" "r" "clj" "cljs" "scala" "js"))
  (dired-rainbow-define compiled "#4dc0b5" ("asm" "cl" "lisp" "el" "c" "h" "c++" "h++" "hpp" "hxx" "m" "cc" "cs" "cp" "cpp" "go" "f" "for" "ftn" "f90" "f95" "f03" "f08" "s" "rs" "hi" "hs" "pyc" ".java"))
  (dired-rainbow-define executable "#8cc4ff" ("exe" "msi"))
  (dired-rainbow-define compressed "#51d88a" ("7z" "zip" "bz2" "tgz" "txz" "gz" "xz" "z" "Z" "jar" "war" "ear" "rar" "sar" "xpi" "apk" "xz" "tar"))
  (dired-rainbow-define packaged "#faad63" ("deb" "rpm" "apk" "jad" "jar" "cab" "pak" "pk3" "vdf" "vpk" "bsp"))
  (dired-rainbow-define encrypted "#ffed4a" ("gpg" "pgp" "asc" "bfe" "enc" "signature" "sig" "p12" "pem"))
  (dired-rainbow-define fonts "#6cb2eb" ("afm" "fon" "fnt" "pfb" "pfm" "ttf" "otf"))
  (dired-rainbow-define partition "#e3342f" ("dmg" "iso" "bin" "nrg" "qcow" "toast" "vcd" "vmdk" "bak"))
  (dired-rainbow-define vc "#0074d9" ("git" "gitignore" "gitattributes" "gitmodules"))
  (dired-rainbow-define-chmod executable-unix "#38c172" "-.*x.*")
  )

(use-package diredfl
  :disabled
  :ensure t
  :after dired
  :config
  (diredfl-global-mode)
  )

(use-package dired-k
  :ensure t
  :after dired
  :config
  ;; always execute dired-k when dired buffer is opened
  (add-hook 'dired-initial-position-hook 'dired-k)
  (add-hook 'dired-after-readin-hook 'dired-k-no-revert)
  ;; (setq dired-k-style 'git)
  (setq dired-k-style 'k.zsh)
  (setq dired-k-padding 1)
  ;; (define-key dired-mode-map (kbd "K") 'dired-k)
  ;; ;; You can use dired-k alternative to revert-buffer
  ;; (define-key dired-mode-map (kbd "g") 'dired-k)
  (defun my-wdired-advice ()
    (when (or (equal major-mode 'wdired-mode) (equal major-mode 'dired-mode))
      (dired-k)))
  (advice-add 'wdired-abort-changes :after 'my-wdired-advice)
  ;; (advice-remove 'wdired-abort-changes 'my-wdired-advice)
  (advice-add 'wdired-finish-edit :after 'my-wdired-advice)
  ;; (advice-remove 'wdired-abort-changes 'my-wdired-advice)
  )

(use-package dired-single
    ;; https://github.com/crocket/dired-single
    ;; dired-single
    :ensure t
    :after dired
    :init
    ;; (setq dired-single-use-magic-buffer t)
    ;; (setq dired-single-magic-buffer-name "*dired-buffer*")
    ;; (add-hook 'dired-mode-hook (lambda () (rename-buffer "*dired-buffer*")))

    ;; key map
    (with-eval-after-load 'evil
      (defun my-dired-init ()
        "Bunch of stuff to run for dired, either immediately or when it's loaded."
        (define-key dired-mode-map [remap dired-find-file] 'dired-single-buffer)
        (define-key dired-mode-map [remap dired-mouse-find-file-other-window] 'dired-single-buffer-mouse)
        (define-key dired-mode-map [remap dired-up-directory] 'dired-single-up-directory)
        (evil-define-key 'normal dired-mode-map "q" 'kill-this-buffer)
        (evil-define-key 'emacs dired-mode-map "q" 'kill-this-buffer)
        (evil-define-key 'normal dired-mode-map "f" 'swiper)
        (evil-define-key 'normal dired-mode-map "h" 'dired-single-up-directory)
        (evil-define-key 'normal dired-mode-map "l" 'dired-single-buffer)
        (evil-define-key 'normal dired-mode-map "^" 'dired-single-up-directory)
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
    ;; (advice-add 'dired-single-buffer :around 'advice-dired-single-buffer)
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
