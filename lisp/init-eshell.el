;; -*- coding: utf-8; lexical-binding: t; -*-
;; init eshell

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
                                :action #'ivy-completion-in-region-action))
             (cursor-move (length command)))
        (kill-region (+ start-pos cursor-move) (+ end-pos cursor-move))
        )))
  ;; move cursor to eol
  (end-of-line)
  )

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
    (setf (buffer-substring start-pos end-pos) command)
    (end-of-line))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://github.com/samrayleung/emacs.d/blob/master/lisp/init-eshell.el
;;; Inspire by http://blog.binchen.org/posts/use-ivy-mode-to-search-bash-history.html
(defun samray/parse-bash-history ()
  "Parse the bash history."
  (interactive)
  (let (collection bash_history)
    (shell-command "history -r") ; reload history
    (setq collection
          (nreverse
           (split-string (with-temp-buffer (insert-file-contents (file-truename "~/.bash_history"))
                                           (buffer-string))
                         "\n"
                         t)))
    (when (and collection (> (length collection) 0)
               (setq bash_history collection))
      bash_history)))

(defun samray/parse-zsh-history ()
  "Parse the bash history."
  (interactive)
  (let (collection zsh_history)
    (shell-command "history -r") ; reload history
    (setq collection
          (nreverse
           (split-string (with-temp-buffer (insert-file-contents (file-truename "~/.zsh_history"))
                                           (replace-regexp-in-string "^:[^;]*;" "" (buffer-string)))
                         "\n"
                         t)))
    (when (and collection (> (length collection) 0)
               (setq zsh_history collection))
      zsh_history)))

(defun samray/esh-history ()
  "Interactive search eshell history."
  (interactive)
  (require 'em-hist)
  (save-excursion
    (let* ((start-pos (eshell-beginning-of-input))
           (input (eshell-get-old-input))
           (esh-history (when (> (ring-size eshell-history-ring) 0)
                          (ring-elements eshell-history-ring)))
           (all-shell-history (append esh-history (samray/parse-zsh-history) (samray/parse-bash-history)))
           )
      (let* ((command (ivy-read "Command: "
                                (delete-dups all-shell-history)
                                :initial-input input
                                :require-match t
                                :action #'ivy-completion-in-region-action))
             )
        (eshell-kill-input)
        (insert command)
        )))
  ;; move cursor to eol
  (end-of-line))

(defun fwar34/eshell-clear-buffer ()
  "Clear terminal."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))

(use-package eshell
  :ensure t
  :commands eshell
  :init
  (setq eshell-aliases-file (concat user-emacs-directory "eshell/alias"))
  :hook
  (eshell-mode . company-mode)
  :config
  (progn
    (when (not (functionp 'eshell/rgrep))
      (defun eshell/rgrep (&rest args)
        "Use Emacs grep facility instead of calling external grep."
        (eshell-grep "rgrep" args t)))
    (add-hook 'eshell-mode-hook
              (lambda ()(eshell-cmpl-initialize)))
    (add-hook 'eshell-mode-hook (lambda ()
                                  (setq-local global-hl-line-mode nil))))
  (evil-set-initial-state 'eshell-mode 'emacs)
  (add-hook 'eshell-mode-hook (lambda ()
                                (evil-local-set-key 'emacs (kbd "C-w") #'evil-delete-backward-word)))

  ;; windows中eshell设置中文
  (when (string-equal system-type "windows-nt")
    (with-eval-after-load 'eshell
      (set-language-environment "chinese-GB")))

  ;; (custom-set-variables
  ;;  '(eshell-visual-options (quote (("git" "log" "diff" "show")))))
  
  ;; (add-hook 'eshell-mode-hook 'company-mode)
  :bind
  (;; ([remap samray/smarter-move-beginning-of-line] . eshell-bol)
   ;; ("C-a" . eshell-bol)
   ;; ([remap kill-region] . samray/kill-word-backward)
   ;; ("C-w" . samray/kill-word-backward)
   ;; ([remap evil-insert-digraph] . paredit-kill)
   ;; ("C-k" . paredit-kill)
   ([remap evil-paste-from-register] . samray/esh-history)
   ;; ("C-r" . samray/esh-history)
   ;; ("C-l" . fwar34/eshell-clear-buffer))
   ))

(use-package esh-autosuggest
  :ensure t
  :after eshell
  :hook
  ;; eshell-banner-message "What would you like to do?\n\n"
  (eshell-mode . esh-autosuggest-mode)
  ;; If you have use-package-hook-name-suffix set to nil, uncomment and use the
  ;; line below instead:
  ;; :hook (eshell-mode-hook . esh-autosuggest-mode)
  )

(use-package eshell-prompt-extras
  :after eshell
  :ensure t
  :init
  (progn
    (setq eshell-highlight-prompt nil
          eshell-prompt-function 'epe-theme-lambda))
  )

(use-package eshell-autojump
  :ensure t
  :after eshell
  :config
  )
;;-------------------------------------------------------------
;; samrayleung
;;-------------------------------------------------------------
;; must install fasd
(defun fwar34/eshell-fasd-z (&rest args)
  "Use fasd to change direct more effectively by passing ARGS."
  (setq args (eshell-flatten-list args))
  (let* ((fasd (concat "fasd " (car args)))
         (fasd-result (shell-command-to-string fasd))
         (path (replace-regexp-in-string "\n$" "" fasd-result))
         )
    (eshell/cd path)
    (eshell/echo path)
    ))

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

(require 'company)
(require 'cl-lib)
(defun samray/company-eshell-autosuggest-candidates (prefix)
  "Select the first eshell history candidate with prefix PREFIX."
  (let* ((esh-history (when (> (ring-size eshell-history-ring) 0)
                        (ring-elements eshell-history-ring)))
         (all-shell-history (append esh-history (samray/parse-zsh-history) (samray/parse-bash-history)))
         (history
          (delete-dups ;; 列表去重
           (mapcar (lambda (str) ;; 去掉原始文本的前后空格
                     (string-trim (substring-no-properties str)))
                   all-shell-history)))
         (most-similar (cl-find-if
                        (lambda (str)
                          (string-prefix-p prefix str))
                        history)))
    (when most-similar
      `(,most-similar)))
  )

(defun samray/company-eshell-autosuggest--prefix ()
  "Get current eshell input"
  (let ((prefix
         (string-trim-left
          (buffer-substring-no-properties
           (save-excursion
             (eshell-bol))
           (save-excursion (end-of-line) (point))))))
    (if (not (string-empty-p prefix))
        prefix
      'stop))
  )

(defun samray/company-eshell-autosuggest (command &optional arg &rest ignored)
  "`company-mode' backend to provide eshell history suggestion."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-eshell))
    (prefix (and (eq major-mode 'eshell-mode)
                 (samray/company-eshell-autosuggest--prefix)))
    (candidates (samray/company-eshell-autosuggest-candidates arg))))

(defun samray/setup-company-eshell-autosuggest ()
  "Set up company completion for Eshell."
  (with-eval-after-load 'company
    (setq-local company-backends '(samray/company-eshell-autosuggest))
    (setq-local company-frontends '(company-preview-frontend))))
;; (add-hook 'eshell-mode-hook 'samray/setup-company-eshell-autosuggest)

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
  (pcomplete-here (mapcar #'car eshell-path-alist)))


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
;; (defalias 'foxy #'fwar34/foxy-command)

(defalias 'q #'kill-this-buffer)
(defalias 'exit #'kill-this-buffer)

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
(defalias 'licmd #'fwar34/proxy-command-use-lisp)

;; https://emacs-china.org/t/emacs-builtin-mode/11937/83?u=fwar34
(use-package em-term
  :ensure nil
  :custom
  (eshell-visual-commands '("top" "htop" "less" "more" "bat"))
  (eshell-visual-subcommands '(("git" "help" "lg" "log" "diff" "show")))
  (eshell-visual-options '(("git" "--help" "--paginate"))))

(provide 'init-eshell)
