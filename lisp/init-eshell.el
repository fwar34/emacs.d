;;; init-eshell.el --- Useful preset transient commands  -*- coding:utf-8; lexical-binding: t; -*-
;;; Commentary:

;;; Code:


(defun my/eshell-init-keymap ()
  ;; https://emacs-china.org/t/topic/4579
  (defun fwar34/esh-history ()
    "Interactive search eshell history."
    (interactive)
    (require 'em-hist)
    (save-excursion
      (let* ((start-pos (eshell-bol))
             (end-pos (point-at-eol))
             (input (buffer-substring-no-properties start-pos end-pos)))
        (let* ((command (ivy-read "Command: "
                                  (delete-dups
                                   (when (> (ring-size eshell-history-ring) 0)
                                     (ring-elements eshell-history-ring)))
                                  :preselect input
                                  :action 'ivy-completion-in-region-action))
               (cursor-move (length command)))
          (kill-region (+ start-pos cursor-move) (+ end-pos cursor-move))
          )))
    ;; move cursor to eol
    (end-of-line))

  (defun fwar34/ivy-eshell-history ()
    "Interactive search eshell history."
    (interactive)
    (require 'em-hist)
    (let* ((start-pos (save-excursion (eshell-bol) (point)))
           (end-pos (point))
           (input (buffer-substring-no-properties start-pos end-pos))
           (command (ivy-read "Command: "
                              (delete-dups
                               (when (> (ring-size eshell-history-ring) 0)
                                 (ring-elements eshell-history-ring)))
                              :initial-input input)))
      (setf (buffer-substring-no-properties start-pos end-pos) command)
      (end-of-line)))

  (evil-define-key 'insert eshell-mode-map (kbd "C-r") 'fwar34/ivy-eshell-history)
  (evil-define-key 'insert eshell-mode-map (kbd "M-j") 'pyim-convert-string-at-point)
  (evil-define-key 'insert eshell-mode-map ";tm" 'aweshell-toggle)
  (evil-define-key 'insert eshell-mode-map ";sh" 'aweshell-toggle)
  (evil-define-key 'insert eshell-mode-map ";g" 'evil-normal-state)
  (evil-define-key 'insert eshell-mode-map ";;" 'self-insert-command)
  (evil-define-key 'insert eshell-mode-map (kbd "TAB") 'completion-at-point))
; (add-hook 'eshell-first-time-mode-hook 'my/eshell-init-keymap)

(use-package eshell
  :ensure nil
  :commands eshell
  :init
  (setq eshell-aliases-file (concat user-emacs-directory "eshell/alias"))
  ;; :hook
  ;; (eshell-mode . company-mode)
  :config
  ;; (progn
  ;;   (when (not (functionp 'eshell/rgrep))
  ;;     (defun eshell/rgrep (&rest args)
  ;;       "Use Emacs grep facility instead of calling external grep."
  ;;       (eshell-grep "rgrep" args t)))
  ;;   (add-hook 'eshell-mode-hook
  ;;             (lambda ()(eshell-cmpl-initialize)))
  (add-hook 'eshell-mode-hook (lambda () (setq-local global-hl-line-mode nil)))

  ;; 如果win10没有改默认编码为utf8则打开下面的注释
  ;; windows中eshell设置中文
  ;; (when (string-equal system-type "windows-nt")
  ;;   (with-eval-after-load 'eshell
  ;;     (set-language-environment "chinese-GB")))

  ;; (custom-set-variables
  ;;  '(eshell-visual-options (quote (("git" "log" "diff" "show")))))
  )

;; (use-package esh-autosuggest
;;   :disabled
;;   :ensure t
;;   :after eshell
;;   :hook
;;   ;; eshell-banner-message "What would you like to do?\n\n"
;;   (eshell-mode . esh-autosuggest-mode)
;;   ;; If you have use-package-hook-name-suffix set to nil, uncomment and use the
;;   ;; line below instead:
;;   ;; :hook (eshell-mode-hook . esh-autosuggest-mode)
;;   :config
;;   (setq evil-collection-company-use-tng nil)

;;   (defun setup-eshell-ivy-completion ()
;;     (define-key eshell-mode-map [remap eshell-pcomplete] 'completion-at-point)
;;     ;; only if you want to use the minibuffer for completions instead of the
;;     ;; in-buffer interface
;;     ;; (setq-local ivy-display-functions-alist
;;     ;;             (remq (assoc 'ivy-completion-in-region ivy-display-functions-alist)
;;     ;;                   ivy-display-functions-alist))
;;     )

;;   (add-hook 'eshell-first-time-mode-hook 'setup-eshell-ivy-completion)
;;   )

(use-package eshell-prompt-extras
  :after eshell
  :init
  (progn
    (setq eshell-highlight-prompt nil
          eshell-prompt-function 'epe-theme-lambda)))

(use-package eshell-autojump
  :after eshell)
;;-------------------------------------------------------------
;; samrayleung
;;-------------------------------------------------------------
;; must install fasd

;; Replace shell-pop package with customized function
(defun samray/split-window-right-and-move ()
  "Split window vertically and move to the other window."
  (interactive)
  (split-window-right)
  (other-window 1)
  )
(defun samray/split-window-below-and-move ()
  "Split window horizontally and move to the other window!"
  (interactive)
  (split-window-below)
  (other-window 1)
  )
(defun fwar34/eshell-pop ()
  "Pop and hide eshell with this function."
  (interactive)
  (let* ((eshell-buffer-name "*eshell*")
         (eshell-window (get-buffer-window eshell-buffer-name 'visible))
         (cwd default-directory)
         (change-cwd (lambda ()
                       (progn
                         (goto-char (point-max))
                         (evil-insert-state)
                         (eshell-kill-input)
                         ;; There is somethings wrong with eshell/cd
                         ;; So replace with `insert`
                         (insert " cd " cwd)
                         (eshell-send-input)
                         ))))
    ;; Eshell buffer exists?
    (if (get-buffer eshell-buffer-name)
        ;; Eshell buffer is visible?
        (if eshell-window
            ;; Buffer in current window is eshell buffer?
            (if (string= (buffer-name (window-buffer)) eshell-buffer-name)
                (if (not (one-window-p))
                    (progn (bury-buffer)
                           (delete-window)))
              ;; If not, select window which points to eshell bufffer.
              (select-window eshell-window)
              (funcall change-cwd)
              )
          ;; If eshell buffer is not visible, split a window and switch to it.
          (progn
            ;; Use `split-window-sensibly` to split window with policy
            ;; If window cannot be split, force to split split window horizontally
            (when (not (split-window-sensibly))
              (samray/split-window-below-and-move))
            (switch-to-buffer eshell-buffer-name)
            (funcall change-cwd)))
      ;; If eshell buffer doesn't exist, create one
      (progn
        (when (not (split-window-sensibly))
          (samray/split-window-below-and-move))
        (eshell)
        (funcall change-cwd))))
  )

;;-------------------------------------------------------------
;; https://blog.csdn.net/argansos/article/details/6867575
;;-------------------------------------------------------------
(defun pcmpl-package-cache(name)
  "return a list of packages in cache"
  (unless (equal name "")
    (split-string (shell-command-to-string (concat "apt-cache pkgnames " name " 2> /dev/null")))))
(defun pcomplete/sai()
  "completion for `sai'"
  (while
      (pcomplete-here (pcmpl-package-cache (pcomplete-arg 'last)))))

;;-------------------------------------------------------------
;; https://blog.csdn.net/argansos/article/details/6867575
;;-------------------------------------------------------------
(defvar eshell-path-alist
  `(("e" . ,user-emacs-directory)
    ("t" . "/tmp")
    ("down" . "~/downloads/")
    ("rust" . "~/mine/Rust/")
    ("py" . "~/mine/Python/")
    ("nvim" . "~/.config/nvim/")
    ))
(defun shell/d (arg)
  (let ((path (cdr (assoc arg eshell-path-alist))))
    (eshell/cd path)))
(defun pcomplete/d ()
  (pcomplete-here (mapcar 'car eshell-path-alist)))


;;-------------------------------------------------------------
;; https://www.emacswiki.org/emacs/EshellCompletion
;;-------------------------------------------------------------
;; sudo
(defun pcomplete/sudo ()
  "Completion rules for the `sudo' command."
  (let ((pcomplete-ignore-case t))
    (pcomplete-here (funcall pcomplete-command-completion-function))
    (while (pcomplete-here (pcomplete-entries)))))

;; (defun fwar34/foxy-command (first &rest other)
;;   (let ((command first))
;;     (if (listp other)
;;         (setq command (concat first " " (mapconcat 'identity other " "))))
;;     (shell-command (concat "foxy.sh " command))))
;; (defalias 'foxy 'fwar34/foxy-command)

(defalias 'q 'aweshell-toggle)

;;-------------------------------------------------------------
;; impliment
;;-------------------------------------------------------------
(defun fwar34/port1080-exist-p ()
  "Judge port 1080 is already in use"
  (or (and (executable-find "netstat")
           (string-match "\\b:1080\\b" (shell-command-to-string "netstat -nap")))
      (and (executable-find "ss")
           (string-match
            ".*1080\\b.*v2ray"
            (shell-command-to-string "ss -lp")))))

(defun fwar34/proxy-command-use-lisp (first &rest other)
  (if (fwar34/port1080-exist-p)
      (error "port 1080 already in use!!!!!")
    (let ((process (start-process
                    "my-v2ray"
                    (get-buffer "*Warnings*")
                    "/usr/bin/v2ray/v2ray"
                    "-config"
                    (expand-file-name
                     "~/mine/Other/v2ray/client.config.json.nocdn"))))
      (while (not (fwar34/port1080-exist-p)))
      (let ((command first))
        (if (listp other)
            (setq command (concat first " " (mapconcat 'identity other " "))))
        (message "Start execute %s" command)
        (shell-command (concat "proxychains4 " command)))
      (if (eq (process-status process) 'run)
          (delete-process process))))
  )
(defalias 'licmd 'fwar34/proxy-command-use-lisp)

;; https://emacs-china.org/t/emacs-builtin-mode/11937/83?u=fwar34
(use-package em-term
  :ensure nil
  ;; :no-require t
  :after eshell
  :custom
  (eshell-visual-commands '("top" "htop" "less" "more" "bat"))
  (eshell-visual-subcommands '(("git" "help" "lg" "log" "diff" "show")))
  (eshell-visual-options '(("git" "--help" "--paginate"))))

(use-package aweshell
  :straight
  (:host github :repo "manateelazycat/aweshell")
  :commands aweshell-toggle)

(provide 'init-eshell)
;;; init-eshell.el ends here
