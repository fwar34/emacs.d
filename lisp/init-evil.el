;; -*- coding: utf-8; lexical-binding: t; -*-


;; highlight persistent
;; https://stackoverflow.com/questions/25768036/emacs-evil-non-incremental-search-and-persistent-highlighting/34252236#34252236
;; This will highlight all searches done with isearch or Evil search. 
;; The highlight will remain until you make another one or 
;; call highlight-remove-all. I've mapped it to leader SPC with:
(defun highlight-remove-all ()
  (interactive)
  (hi-lock-mode -1)
  (hi-lock-mode 1)
  ;; (highlight-symbol-remove-all)
  (symbol-overlay-remove-all)
  (lazy-highlight-cleanup t)
  )
(defun search-highlight-persist ()
  ;; (highlight-regexp (car-safe (if isearch-regexp
  ;;                                 regexp-search-ring
  ;;                               search-ring))
  ;;                   (facep 'hi-yellow))
  (highlight-regexp (car-safe regexp-search-ring)))
(defadvice isearch-exit (after isearch-hl-persist activate)
  (highlight-remove-all)
  (search-highlight-persist))
(defadvice evil-search-incrementally (after evil-search-hl-persist activate)
  (highlight-remove-all)
  (search-highlight-persist))

;; highlight "* #" search
(defadvice evil-search-word-forward (after advice-evil-search-word-forward activate)
  (highlight-remove-all)
  (search-highlight-persist))
(defadvice evil-search-word-backward (after advice-evil-search-word-forward activate)
  (highlight-remove-all)
  (search-highlight-persist))

;; equivalent of 'nnoremap n nzz' in vim
;; https://github.com/noctuid/evil-guide
(defun my-center-line (&rest _)
  (highlight-remove-all)
  (search-highlight-persist)
  (evil-scroll-line-to-center nil))
(advice-add 'evil-search-next :after 'my-center-line)
(advice-add 'evil-search-previous :after 'my-center-line)

;;-------------------------------------------------------------
;; highlight yank region
;; https://emacs-china.org/t/emacs-vim/9203/13
;;-------------------------------------------------------------
(defface fwar34-hi-yellow
  '((((min-colors 88) (background dark))
     (:background "yellow1" :foreground "black"))
    (((background dark)) (:background "yellow" :foreground "black"))
    (((min-colors 88)) (:background "yellow1"))
    (t (:background "yellow")))
  "Default face for hi-lock mode.")

(defun fwar34/highlight-yank (beg end &rest _)
  (let ((overlay (make-overlay beg end)))
    (overlay-put overlay 'face 'fwar34-hi-yellow)
    (overlay-put overlay 'fwar34-flag t) ;; set my overlay flag
    (make-thread (lambda ()
                   (sleep-for 0 500)
                   ;; (remove-overlays beg end 'face 'fwar34-hi-yellow)))
                   (delete-overlay overlay)))
    ))
;; (advice-add 'evil-yank :after 'fwar34/highlight-yank) ;; evil-delete also use evil-yank
(advice-add 'evil-yank-rectangle :after 'fwar34/highlight-yank)
(advice-add 'evil-yank-lines :after 'fwar34/highlight-yank)
(advice-add 'evil-yank-characters :after 'fwar34/highlight-yank)

(advice-add 'lispyville-yank :after 'fwar34/highlight-yank)

;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Finding-Overlays.html#Finding-Overlays
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Managing-Overlays.html#Managing-Overlays
(defun fwar34/remove-fwar34-overlay-before-paste (&rest _)
  (interactive)
  (let ((overlays (overlays-at (point))))
    (while overlays
      (let ((overlay (car overlays)))
        (when (overlay-get overlay 'fwar34-flag)
          (delete-overlay overlay)))
      (setq overlays (cdr overlays)))))
(advice-add 'evil-paste-before :after 'fwar34/remove-fwar34-overlay-before-paste)
(advice-add 'evil-paste-after :after 'fwar34/remove-fwar34-overlay-before-paste)

;; esc quit
;; http://wikemacs.org/index.php/Evil
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

(evil-define-key 'normal global-map "gs" 'evil-avy-goto-char)
;; initial apropos-mode to evil-normal-state
;; (dolist (p '((apropos-mode . normal)
;;              (browse-kill-ring-mode . normal)))
;;   (evil-set-initial-state (car p) (cdr p)))
(loop for (mode . state) in '((apropos-mode . normal)
                              (browse-kill-ring-mode . normal)
                              ;; (ivy-occur-grep-mode . emacs)
                              ;; (proced-mode . emacs)
                              (imenu-list-major-mode . emacs)
                              (calc-mode . emacs)
                              (easy-hugo-mode . emacs)
                              (calculator-mode . emacs))
      do (evil-set-initial-state mode state))

;; TAB and C-i is the same
;; (define-key evil-normal-state-map (kbd "TAB") 'other-window)
;; (define-key evil-normal-state-map (kbd "C-i") 'evil-jump-forward)
(define-key evil-normal-state-map (kbd "ge") 'evil-goto-line)
(define-key evil-visual-state-map (kbd "gg") 'evil-change-to-previous-state)
;; (define-key evil-insert-state-map "///" 'eval-last-sexp)
(evil-define-key 'normal package-menu-mode-map "f" 'swiper)

;; (define-key evil-normal-state-map (kbd "M-j") 'pyim-toggle-input-ascii)
(define-key evil-normal-state-map (kbd "C-]") 'counsel-etags-find-tag-at-point)
(define-key evil-insert-state-map (kbd "C-j") 'evil-next-line)
(define-key evil-insert-state-map (kbd "C-k") 'evil-previous-line)
;; (define-key evil-insert-state-map (kbd "C-u") 'evil-delete-whole-line)
(define-key evil-insert-state-map (kbd "C-u") 'my-kill-line)
(define-key evil-insert-state-map (kbd "C-d") 'evil-delete-line)
(define-key evil-insert-state-map (kbd "C-l") 'hungry-delete-forward)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;copy from chenbin.emacs.d;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; As a general RULE, mode specific evil leader keys started
;; with uppercased character or 'g' or special character except "=" and "-"
(evil-declare-key 'normal org-mode-map
  "gh" 'outline-up-heading
  "gl" 'outline-next-visible-heading
  "gj" 'outline-forward-same-level
  "gk" 'outline-backward-same-level
  "$" 'org-end-of-line ; smarter behaviour on headlines etc.
  "^" 'org-beginning-of-line ; ditto
  "<" (lambda () (interactive) (org-demote-or-promote 1)) ; out-dent
  ">" 'org-demote-or-promote ; indent
  (kbd "TAB") 'org-cycle)

(evil-declare-key 'normal markdown-mode-map
  "gh" 'outline-up-heading
  "gl" 'outline-next-visible-heading
  "gj" 'outline-forward-same-level
  "gk" 'outline-backward-same-level
  (kbd "TAB") 'org-cycle)
;; 
;; I prefer Emacs way after pressing ":" in evil-mode
(define-key evil-ex-completion-map (kbd "C-a") 'evil-first-non-blank)
(define-key evil-ex-completion-map (kbd "C-b") 'backward-char)
(define-key evil-ex-completion-map (kbd "M-p") 'previous-complete-history-element)
(define-key evil-ex-completion-map (kbd "M-n") 'next-complete-history-element)

;; (define-key evil-normal-state-map "Y" (kbd "y$"))
;; (define-key evil-normal-state-map (kbd "RET") 'ivy-switch-buffer-by-pinyin) ; RET key is preserved for occur buffer
(define-key evil-normal-state-map "go" 'goto-char)
(define-key evil-normal-state-map "g1" 'evil-avy-goto-word-or-subword-1)
(define-key evil-normal-state-map "g2" 'evil-avy-goto-char-2)
;; (define-key evil-normal-state-map (kbd "M-y") 'counsel-browse-kill-ring)
;; (define-key evil-normal-state-map (kbd "C-]") 'counsel-etags-find-tag-at-point)
;; (define-key evil-insert-state-map (kbd "C-x C-n") 'evil-complete-next-line)
;; (define-key evil-insert-state-map (kbd "C-x C-p") 'evil-complete-previous-line)

;; I learn this trick from ReneFroger, need latest expand-region
;; @see https://github.com/redguardtoo/evil-matchit/issues/38
(define-key evil-visual-state-map (kbd "v") 'er/expand-region)
(define-key evil-insert-state-map (kbd "C-a") 'evil-first-non-blank)
(define-key evil-insert-state-map (kbd "C-e") 'move-end-of-line)
;; (define-key evil-insert-state-map (kbd "C-u") 'kill-whole-line)
;; (define-key evil-insert-state-map (kbd "C-k") 'kill-line)
;; (define-key evil-insert-state-map (kbd "M-j") 'yas-expand)
;; (define-key evil-emacs-state-map (kbd "M-j") 'yas-expand)
;; (global-set-key (kbd "C-r") 'undo-tree-redo)

;; https://github.com/noctuid/evil-guide#default-keybindings-and-getting-help
;; Command Properties :jump
(dolist (con '(ivy-switch-buffer 
               evil-indent
               counsel-gtags-dwim
               counsel-gtags-find-definition
               counsel-gtags-find-reference
               counsel-gtags-find-symbol
               counsel-gtags-find-file
               counsel-gtags-go-forward
               counsel-gtags-go-backward
               counsel-find-file
               counsel-imenu
               beginning-of-defun
               dired-single-buffer
               dired-jump
               ivy-done
               ;; ivy--done
               ;; ivy-immediate-done
               end-of-defun
               counsel-etags-find-tag-at-point
               counsel-etags-list-tag
               counsel-etags-grep
               counsel-etags-find-tag
               evil-first-non-blank
               evil-end-of-line
               evil-search-next
               evil-search-previous
               evil-backward-section-begin
               evil-backward-section-end
               counsel-ag
               counsel-rg
               my-search-forward-word
               my-search-whole-word
               lispyville-beginning-of-defun
               lispyville-end-of-defun
               lispyville--maybe-enter-special
               swiper-all
               my-swiper-forward-word
               swiper-thing-at-point
               swiper-all-thing-at-point
               evil-avy-goto-char
               evil-avy-goto-char-2
               evil-avy-goto-word-or-subword-1
               evilem-motion-find-char
               evilem-motion-forward-word-begin
               evilem-motion-backward-word-begin
               evilem-motion-next-line
               evilem-motion-previous-line
               find-function
               find-variable
               find-function-on-key
               ;; push-button
               ;; persp-ivy-switch-buffer
               counsel-describe-function
               counsel-describe-variable
               counsel-describe-symbol
               counsel-recentf
               ))
  (evil-add-command-properties con :jump t))

;; {{ Use `;` as one leader key
;; }}
(general-define-key :keymaps '(normal visual)
                    :jump t
                    ;; :states '(normal motion insert emacs)
                    :prefix ";"
                    ;; :non-normal-prefix "M-;"
                    ;; switch window
                    ;; "1" 'select-window-1
                    ;; "2" 'select-window-2
                    ;; "3" 'select-window-3
                    ;; "4" 'select-window-4
                    ;; "5" 'select-window-5
                    ;; "6" 'select-window-6
                    ;; "7" 'select-window-7
                    ";" 'evil-repeat-find-char
                    "1" 'winum-select-window-1
                    "2" 'winum-select-window-2
                    "3" 'winum-select-window-3
                    "4" 'winum-select-window-4
                    "5" 'winum-select-window-5
                    "6" 'winum-select-window-6
                    "7" 'winum-select-window-7
                    "dw" 'delete-window
                    "xx" 'highlight-remove-all
                    ;; "x1" 'delete-other-windows
                    "x2" 'split-window-below
                    "x3" 'split-window-right
                    ;;
                    "a" 'evil-first-non-blank
                    "e" 'evil-end-of-line
                    "SPC" 'counsel-M-x
                    "TAB" 'other-window
                    ;; "qr" 'query-replace
                    "qr" 'quickrun
                    "qs" 'query-replace-regexp
                    ;; highlight-symbol
                    ;; "xx" 'highlight-symbol-remove-all ;; map to ";xx"
                    "hs" 'highlight-symbol
                    "hn" 'highlight-symbol-next
                    "hp" 'highlight-symbol-prev
                    "hr" 'highlight-symbol-query-replace
                    "ln" 'highlight-symbol-nav-mode ; use M-n/M-p to navigation between symbols
                    ;;symbol-overlay默认n,p,i,q在高亮的地方点击为下一个，上一个，取消所有的高亮，替换
                    "st" 'symbol-overlay-transient
                    "so" 'symbol-overlay-put
                    "sr" 'symbol-overlay-remove-all
                    ;;
                    "ia" 'my-append-semicolon-excursion
                    "ic" 'fwar34/insert-python
                    ;; "ia" 'my-append-semicolon-marker
                    "do" 'delete-other-windows
                    "ff" 'counsel-find-file
                    "fd" 'my-display-full-path-of-current-buffer
                    "fb" 'beginning-of-defun
                    "fz" 'counsel-fzf
                    ;; "wf" 'popup-which-function
                    ;; "ww" 'narrow-or-widen-dwim
                    "cd" 'my-change-default-directory
                    "ii" 'counsel-imenu
                    "il" 'imenu-list-smart-toggle
                    ;; "tb" 'taglist-list-tags
                    "jj" 'taglist-list-tags
                    "sw" 'transpose-words ;;swap two words next to each other
                    "xm" 'my-M-x
                    ;; "bk" 'buf-move-up
                    ;; "bj" 'buf-move-down
                    ;; "bh" 'buf-move-left
                    ;; "bl" 'buf-move-right
                    "li" 'swiper
                    "lb" 'swiper-all
                    "ls" 'my-swiper-forward-word
                    "lw" 'swiper-thing-at-point
                    "la" 'swiper-all-thing-at-point
                    "oc" 'occur-dwim
                    "oi" 'isearch-occur
                    "qq" 'quit-window
                    ;; "hv" 'describe-variable
                    "ge" 'goto-line
                    ;; "gg" 'counsel-gtags-dwim ; jump from reference to definition or vice versa
                    ;; "gs" 'counsel-gtags-find-symbol
                    ;; "gr" 'counsel-gtags-find-reference
                    ;; "gu" 'counsel-gtags-update-tags
                    "ud" 'undo-tree-visualize
                    "ua" 'universal-argument
                    ;; "qg" 'counsel-etags-grep
                    ;; "dd" 'counsel-etags-grep-symbol-at-point
                    "fa" 'counsel-rg
                    "fw" (lambda () (interactive) (my-search-whole-word 'counsel-rg))
                    "fs" (lambda () (interactive) (my-search-forward-word 'counsel-rg))
                    "gg" 'counsel-ag
                    "gw" (lambda () (interactive) (my-search-whole-word 'counsel-ag))
                    "gs" (lambda () (interactive) (my-search-forward-word 'counsel-ag))
                    ;; "ha" 'helm-ag
                    "fe" 'end-of-defun
                    "fm" 'mark-defun
                    ;; "sc" 'scratch
                    ;; "jd" 'dumb-jump-go
                    ;; "xd" 'xref-find-definitions
                    ;; "xr" 'xref-find-references
                    "xd" (lambda (identifier)
                           (interactive (list (xref--read-identifier "Find references of: ")))
                           (unless (featurep 'ivy-xref)
                             (require 'ivy-xref))
                           (xref-find-definitions identifier))

                    "xr" (lambda (identifier)
                           (interactive (list (xref--read-identifier "Find references of: ")))
                           (unless (featurep 'ivy-xref)
                             (require 'ivy-xref))
                           (xref-find-references identifier))
                    ;; "jb" 'dumb-jump-back
                    "dj" 'dired-jump ;; open the dired from current file
                    ;; "dj" 'counsel-dired
                    "ht" 'counsel-etags-find-tag-at-point ; better than find-tag C-]
                    "hr" 'counsel-etags-recent-tag
                    "hf" 'counsel-etags-find-tag
                    "hg" 'counsel-etags-grep
                    "hl" 'counsel-etags-list-tag
                    "hc" 'counsel-etags-list-tag-in-current-file
                    "hh" 'ivy-imenu-anywhere
                    ;; "bm" 'counsel-bookmark-goto
                    ;; "br" 'counsel-browse-kill-ring
                    "kr" 'browse-kill-ring
                    "cf" 'counsel-grep  ; grep current buffer
                    "cg" 'counsel-git   ; find file
                    ;; "cs" 'counsel-git-grep-by-selected ; quickest grep should be easy to press
                    ;; "cm" 'counsel-git-find-my-file
                    ;; buffer ;;;;;
                    ;; "bk" 'kill-buffer
                    ;; "wk" 'kill-buffer-and-window
                    ;; "kb" 'kill-buffer
                    "kb" 'persp-kill-buffer*
                    "kw" 'kill-buffer-and-window
                    ;; "bs" 'switch-to-buffer
                    ;; "bs" 'counsel-switch-buffer
                    ;; "bs" 'ivy-switch-buffer
                    ;; "bs" 'persp-ivy-switch-buffer
                    "bs" 'persp-switch-to-buffer*
                    "bp" 'switch-to-prev-buffer
                    "bb" 'evil-buffer
                    "zz" 'save-buffer
                    ;; ;;;;;;;;;;;;;;;;;;;;;;;
                    ;; "fo" 'ff-find-other-file
                    "fo" 'my-find-other-file
                    "mm" 'evilmi-jump-items
                    "tt" 'neotree-toggle
                    "yw" 'youdao-dictionary-search-at-point
                    "yy" 'youdao-dictionary-search-at-point+
                    "yd" 'youdao-dictionary-search-from-input
                    ;; "er" 'er/expand-region
                    ;; "rf" 'recentf-open-files
                    ;; "rm" 'my-recent-file
                    "rm" 'counsel-recentf
                    "sf" 'isearch-forward-regexp
                    "sb" 'isearch-backward-regexp
                    ;; "sr" 'replace-regexp
                    "ci" 'evilnc-comment-or-uncomment-lines
                    "cl" 'evilnc-quick-comment-or-uncomment-to-the-line
                    ;; "ll" 'evilnc-quick-comment-or-uncomment-to-the-line
                    "cc" 'evilnc-copy-and-comment-lines
                    "cp" 'evilnc-comment-or-uncomment-paragraphs
                    "cr" 'comment-or-uncomment-region
                    "cv" 'evilnc-toggle-invert-comment-line-by-line
                    "."  'evilnc-copy-and-comment-operator
                    "\\" 'evilnc-comment-operator ; if you prefer backslash key
                    ;; Search character(s) at the beginning of word
                    ;; See https://github.com/abo-abo/avy/issues/70
                    ;; You can change the avy font-face in ~/.custom.el:
                    ;;  (eval-after-load 'avy
                    ;;   '(progn
                    ;;      (set-face-attribute 'avy-lead-face-0 nil :foreground "black")
                    ;;      (set-face-attribute 'avy-lead-face-0 nil :background "#f86bf3")))
                    ;; ";" 'avy-goto-char-2
                    ;; "w" 'avy-goto-word-or-subword-1
                    ;; "a" 'avy-goto-char-timer
                    ;; "db" 'sdcv-search-pointer ; in buffer
                    ;; "dt" 'sdcv-search-input+ ; in tip
                    ;; "dd" 'my-lookup-dict-org
                    ;; "lm" 'lookup-doc-in-man
                    ;; "lf" 'list-funcs
                    ;; "gw" 'evil-avy-goto-word-or-subword-1
                    "gc" 'evil-avy-goto-char-2
                    ;; "gg" 'evil-ace-jump-char-mode
                    ;; "gs" 'w3m-google-search
                    ;; "gf" 'w3m-google-by-filetype
                    ;; "gd" 'w3m-search-financial-dictionary
                    ;; "gj" 'w3m-search-js-api-mdn
                    ;; "ga" 'w3m-java-search
                    ;; "gh" 'w3mext-hacker-search ; code search in all engines with firefox
                    ;; "pf" 'project-find-file
                    "pf" (lambda () (interactive)
                           (unless (featurep 'counsel)
                             (require 'counsel))
                           (project-find-file))
                    "pg" 'project-find-regexp
                    "sm" 'smex
                    "sj" 'smex-major-mode-commands
                    "se" 'open-init-file
                    "ce" 'evil-emacs-state
                    ;; "gq" 'w3m-stackoverflow-search
                    ;; "mws" 'mpc-which-song
                    ;; "ms" 'mpc-next-prev-song
                    ;; "tm" 'eshell
                    ;; "tm" 'my-multi-term
                    "tm" 'vterm-toggle
                    ;; "sh" (lambda ()
                    ;;        (interactive)
                    ;;        (if (equal system-type 'windows-nt)
                    ;;            (message "shell did not work on windows")
                    ;;          (ansi-term "/usr/bin/zsh")
                    ;;          ))
                    ;; "sh" 'multi-term
                    "sh" 'aweshell-toggle
                    "rr" 'fwar34/counsel-goto-recent-directory
                    "rc" 'fwar34/run-current-file)

 ;; {{ Use `SPC` as one leader key
(general-define-key :keymaps '(normal visual)
                    :jump t
                    ;; :states '(normal insert emacs)
                    :prefix "SPC"
                    ;; :non-normal-prefix "M-SPC"
                    "1" (lambda () (interactive) (persp-switch-by-number 1))
                    "2" (lambda () (interactive) (persp-switch-by-number 2))
                    "3" (lambda () (interactive) (persp-switch-by-number 3))
                    "4" (lambda () (interactive) (persp-switch-by-number 4))
                    "5" (lambda () (interactive) (persp-switch-by-number 5))
                    "6" (lambda () (interactive) (persp-switch-by-number 6))
                    "7" (lambda () (interactive) (persp-switch-by-number 7))
                    "SPC" 'evil-ex
                    "cc" 'convert-file-to-utf8-unix
                    ;; "cn" 'my-convert-radix-word
                    "cv" 'my-convert-radix
                    "ch" 'my-convert-radix-hex
                    "cd" 'my-convert-radix-deci
                    ;; "ss" 'wg-create-workgroup ; save windows layout
                    "se" 'evil-iedit-state/iedit-mode ; start iedit in emacs
                    "sc" 'shell-command
                    "ac" 'async-shell-command
                    "st" 'org-shifttab
                    "cp" 'caps-lock-mode
                    "TAB" 'outline-toggle-children
                    "eh" 'toggle-company-english-helper
                    ;; "ss" 'sr-speedbar-toggle
                    ;; "ll" 'my-wg-switch-workgroup ; load windows layout
                    ;; "yy" 'hydra-launcher/body
                    ;; "hh" 'multiple-cursors-hydra/body
                    ;; "gi" 'gist-region ; only workable on my computer
                    ;; "tt" 'my-toggle-indentation
                    ;; "gs" 'git-gutter:set-start-revision
                    ;; "gh" 'git-gutter-reset-to-head-parent
                    ;; "gr" 'git-gutter-reset-to-default
                    ;; "ps" 'profiler-start
                    ;; "pr" 'profiler-report
                    ;; "ud" 'my-gud-gdb
                    ;; "uk" 'gud-kill-yes
                    ;; "ur" 'gud-remove
                    ;; "ub" 'gud-break
                    ;; "uu" 'gud-run
                    ;; "up" 'gud-print
                    ;; "ue" 'gud-cls
                    ;; "un" 'gud-next
                    ;; "us" 'gud-step
                    ;; "ui" 'gud-stepi
                    ;; "uc" 'gud-cont
                    ;; "uf" 'gud-finish
                    "ma" 'magit
                    "mf" 'with-editor-finish
                    "mc" 'with-editor-cancel
                    ;; "ma" 'mc/mark-all-like-this-dwim
                    ;; "md" 'mc/mark-all-like-this-in-defun
                    ;; "am" 'ace-mc-add-multiple-cursors
                    ;; "aw" 'ace-swap-window
                    ;; "af" 'ace-maximize-window
                    ;; "mn" 'mc/mark-next-like-this
                    ;; "ms" 'mc/skip-to-next-like-this
                    ;; "xc" 'save-buffers-kill-terminal
                    "ic" 'interrupt-my-commands
                    ;; "ma" (lambda () (interactive) (my-commands "make"))
                    ;; ;; "mr" (lambda () (interactive) (my-commands "make" "rebuild"))
                    ;; "mr" (lambda () (interactive) (my-commands "make" "clean" (my-commands "make")))
                    ;; "mm" (lambda () (interactive) (my-commands "make" "flash"))
                    ;; ;; "mc" (lambda () (interactive) (shell-command "make stcflash"))
                    ;; "mc" (lambda () (interactive) (my-commands "make" "clean"))
                    ;; "ms" (lambda () (interactive) (my-commands "make" "space"))
                    ;; "mj" (lambda () (interactive) (my-commands "make" "-j" "5"))
                    ;; "md" (lambda () (interactive) (my-commands "cd" "..") (my-commands "fakeroot" "debian/rules" "binary"))
                    "bj" (lambda () (interactive) (my-commands-shell "make -j 5"))
                    ;; "mr" (lambda () (interactive) (my-commands "make" "rebuild"))
                    ;; "mr" (lambda () (interactive) (my-commands-shell "make rebuild"))
                    ;; "mc" (lambda () (interactive) (shell-command "make stcflash"))
                    "bc" (lambda () (interactive) (my-commands-shell "make clean"))
                    ;; "bs" (lambda () (interactive) (my-commands-shell "make space"))
                    "bs" 'persp-switch-to-buffer
                    ;; 
                    ;; "mj" (lambda () (interactive) (my-commands-shell "make -j 5"))
                    "br" (lambda () (interactive) (my-commands-shell "make clean; make -j 5"))
                    ;; "md" (lambda () (interactive)
                    ;;        (let ((default-directory (string-join (butlast (split-string default-directory "/") 2) "/")))
                    ;;          (my-commands-shell "cd ..; fakeroot debian/rules binary")))
                    "bd" (lambda () (interactive)
                             (my-commands-shell "cd ..; fakeroot debian/rules binary"))
                    "bf" (lambda () (interactive) (my-commands-shell "make -j 5 && cd ..; fakeroot debian/rules binary"))
                    "qq" 'save-buffers-kill-terminal
                    "xz" 'suspend-frame
                    "kk" 'scroll-other-window
                    "jj" 'scroll-other-window-up
                    "lm" 'display-line-numbers-mode
                    ;; "me" 'mc/edit-lines
                    ;; "=" 'increase-default-font-height ; GUI emacs onl
                    ;; "-" 'decrease-default-font-height ; GUI emacs only
                    ;; liang.feng
                    ;; "bu" 'backward-up-list
                    ;; liang.feng
                    ;; "bb" 'back-to-previous-buffer
                    ;; liang.feng
                    ;; "em" 'erase-message-buffer
                    "fn" 'my-display-function
                    "fa" 'ack
                    "ff" '(lambda () (interactive) (fzf-find-file-in-dir "~"))
                    "fz" 'fzf
                    "ft" 'fzf-git
                    "fl" 'fzf-git-files
                    ;; "fg" 'fzf-git-grep
                    "fd" 'fzf-directory
                    ;; liang.feng
                    ;; "eb" 'eval-buffer
                    ;; "sd" 'sudo-edit
                    ;; liang.feng
                    ;; "ee" 'eval-expression
                    ;; "aa" 'copy-to-x-clipboard ; used frequently
                    ;; "ac" 'aya-create
                    ;; "ae" 'aya-expand
                    ;; "zz" 'paste-from-x-clipboard ; used frequently
                    ;; "cy" 'strip-convert-lines-into-one-big-string
                    "cy" 'clipboard-yank
                    ;; (define-key evil-normal-state-map (kbd "[ cp") 'git-gutter+-previous-hunk)
                    ;; (define-key evil-normal-state-map (kbd "[ cn") 'git-gutter+-next-hunk)
                    ;; (define-key evil-normal-state-map (kbd "[ st") 'git-gutter+-stage-hunk)
                    ;; "cn" (lambda ()
                    ;;        (interactive)
                    ;;        (if (display-graphic-p)
                    ;;            (git-gutter:next-hunk (line-number-at-pos)) 
                    ;;          (diff-hl-next-hunk)))
                    "ds" 'git-gutter:statistic
                    ;; "du" '(lambda ()
                    ;;        (interactive)
                    ;;        (define-advice git-gutter:popup-hunk (:around (orig-fun &rest args) my-git-gutter:popup-hunk)
                    ;;          (let ((res (apply orig-fun args)))
                    ;;            (when res
                    ;;              (switch-to-buffer-other-window res)
                    ;;              (evil-local-set-key 'normal (kbd "q") 'kill-buffer-and-window))))
                    ;;        (git-gutter:popup-hunk))
                    "du" '(lambda ()
                            (interactive)
                            (if (featurep 'diff-hl)
                                (diff-hl-diff-goto-hunk)
                              (progn
                                (define-advice git-gutter:popup-hunk (:around (orig-fun &rest args) my-git-gutter:popup-hunk)
                                  (let ((res (apply orig-fun args)))
                                    (when res
                                      (switch-to-buffer-other-window res)
                                      (evil-local-set-key 'normal (kbd "q") 'kill-buffer-and-window))))
                                (git-gutter:popup-hunk))))
                    ;; "dr" '(lambda ()
                    ;;         (interactive)
                    ;;         (advice-remove 'git-gutter:popup-hunk 'git-gutter:popup-hunk@my-git-gutter:popup-hunk)
                    ;;         (git-gutter:revert-hunk))
                    "dr" '(lambda ()
                            (interactive)
                            (if (featurep 'diff-hl)
                                (diff-hl-revert-hunk)
                              (progn
                                (advice-remove 'git-gutter:popup-hunk 'git-gutter:popup-hunk@my-git-gutter:popup-hunk)
                                (git-gutter:revert-hunk))))
                    ;; "dn" 'git-gutter:next-hunk
                    ;; "dp" 'git-gutter:previous-hunk
                    "dn" '(lambda ()
                            (interactive)
                            (if (featurep 'diff-hl)
                                (diff-hl-next-hunk)
                              (git-gutter:next-hunk (line-number-at-pos))))
                    "dp" '(lambda ()
                            (interactive)
                            (if (featurep 'diff-hl)
                                (diff-hl-previous-hunk)
                              (git-gutter:previous-hunk (line-number-at-pos))))
                    ;; "cp" (lambda ()
                    ;;        (interactive)
                    ;;        (if (display-graphic-p)
                    ;;            (git-gutter:previous-hunk (line-number-at-pos))
                    ;;          (diff-hl-previous-hunk)))
                    "c=" 'vc-diff
                    "cl" 'vc-pull
                    ;; "cl" 'my-vc-operator
                    "cu" 'vc-push
                    ;; liang.feng
                    ;; "bs" '(lambda () (interactive) (goto-edge-by-comparing-font-face -1))
                    ;; liang.feng
                    ;; "es" 'goto-edge-by-comparing-font-face
                    ;; "vj" 'my-validate-json-or-js-expression
                    ;; liang.feng
                    ;; "kc" 'kill-ring-to-clipboard
                    ;; "mcr" 'my-create-regex-from-kill-ring
                    ;; "ntt" 'neotree-toggle
                    ;; "ntf" 'neotree-find ; open file in current buffer in neotree
                    ;; "ntd" 'neotree-project-dir
                    ;; "nth" 'neotree-hide

                    ;; "fn" 'cp-filename-of-current-buffer
                    ;; "fp" 'cp-fullpath-of-current-buffer
                    ;; "ff" 'toggle-full-window ;; I use WIN+F in i3
                    ;; "hd" 'describe-function
                    "hf" 'find-function
                    "hv" 'find-variable
                    "hk" 'find-function-on-key
                    "df" 'counsel-describe-function
                    "dv" 'counsel-describe-variable
                    "dk" 'describe-key
                    "dd" (lambda ()
                           (interactive)
                           (lispy-describe-inline)
                           (if (window-live-p (get-buffer-window "*lispy-help*"))
                               (select-window (get-buffer-window "*lispy-help*"))))
                    "da" 'lispy-arglist-inline
                    ;; "ss" 'fa-show
                    ;; "gg" 'ggtags-find-tag-dwim
                    ;; "gs" 'ggtags-find-other-symbol
                    ;; "gr" 'ggtags-find-reference
                    ;; "ip" 'find-file-in-project
                    ;; liang.feng
                    ;; "kk" 'find-filem-in-project-by-selected
                    ;; liang.feng
                    ;; "kn" 'find-file-with-similar-name ; ffip v5.3.1
                    ;; "fd" 'find-directory-in-project-by-selected
                    ;; "trm" 'get-term
                    "wf" 'toggle-frame-fullscreen
                    "wm" 'toggle-frame-maximized
                    "le" 'elisp-index-search
                    "ls" 'emacs-index-search
                    ;; "ti" 'fastdef-insert
                    ;; "th" 'fastdef-insert-from-history
                    ;; liang.feng
                    ;; "epy" 'emmet-expand-yas
                    ;; liang.feng
                    ;; "epl" 'emmet-expand-line
                    ;; "rd" 'evilmr-replace-in-defun
                    ;; "rb" 'evilmr-replace-in-buffer
                    ;; "ts" 'evilmr-tag-selected-region ;; recommended
                    ;; "tua" 'artbollocks-mode
                    ;; "cby" 'cb-switch-between-controller-and-view
                    ;; "cbu" 'cb-get-url-from-controller
                    ;; "bm" 'bookmark-set
                    ;; "bj" 'bookmark-jump
                    "bm" 'counsel-bookmark
                    "be" 'edit-bookmarks
                    ;; "gs" (lambda ()
                           ;; (interactive)
                           ;; (let* ((ffip-diff-backends
                                   ;; '(("Show git commit" . (let* ((git-cmd "git --no-pager log --date=short --pretty=format:'%h|%ad|%s|%an'")
                                                                 ;; (collection (split-string (shell-command-to-string git-cmd) "\n" t))
                                                                 ;; (item (ffip-completing-read "git log:" collection)))
                                                            ;; (when item
                                                              ;; (shell-command-to-string (format "git show %s" (car (split-string item "|" t))))))))))
                             ;; (ffip-show-diff 0)))
                    ;; "gd" 'ffip-show-diff-by-description ;find-file-in-project 5.3.0+
                    ;; "sf" 'counsel-git-show-file
                    ;; "sh" 'my-select-from-search-text-history
                    ;; "df" 'counsel-git-diff-file
                    ;; "rjs" 'run-js
                    ;; liang.feng
                    ;; "jsr" 'js-send-region
                    ;; liang.feng
                    ;; "jsb" 'js-clear-send-buffer
                    ;; "rmz" 'run-mozilla
                    "rpy" 'run-python
                    "rlu" 'run-lua
                    "rru" 'rust-run
                    "rfm" 'rust-format-buffer
                    "tci" 'toggle-company-ispell
                    ;; liang.feng
                    ;; "kb" 'kill-buffer-and-window ;; "k" is preserved to replace "C-g"
                    ;; "it" 'issue-tracker-increment-issue-id-under-cursor
                    ;; "ls" 'highlight-symbol
                    ;; "lq" 'highlight-symbol-query-replace
                    ;;liang.feng
                    ;; "bm" 'pomodoro-start ;; beat myself
                    ;; "ij" 'rimenu-jump
                    ;; @see https://github.com/pidu/git-timemachine
                    ;; p: previous; n: next; w:hash; W:complete hash; g:nth version; q:quit
                    ;; "tm" 'my-git-timemachine
                    ;; "tdb" 'tidy-buffer
                    ;; "tdl" 'tidy-current-line
                    ;; toggle overview,  @see http://emacs.wordpress.com/2007/01/16/quick-and-dirty-code-folding/
                    ;; "ov" 'my-overview-of-current-buffer
                    ;; "or" 'open-readme-in-git-root-directory
                    "oo" 'compile
                    "c$" 'org-archive-subtree ; `C-c $'
                    ;; org-do-demote/org-do-premote support selected region
                    "c<" 'org-do-promote ; `C-c C-<'
                    "c>" 'org-do-demote ; `C-c C->'
                    "cam" 'org-tags-view ; `C-c a m': search items in org-file-apps by tag
                    "cxi" 'org-clock-in ; `C-c C-x C-i'
                    "cxo" 'org-clock-out ; `C-c C-x C-o'
                    "cxr" 'org-clock-report ; `C-c C-x C-r'
                    ;; "rr" 'my-counsel-recentf
                    ;; "rh" 'counsel-yank-bash-history ; bash history command => yank-ring
                    ;; "da" 'diff-region-tag-selected-as-a
                    ;; "db" 'diff-region-compare-with-b
                    "di" 'evilmi-delete-items
                    "si" 'evilmi-select-items
                    ;; liang.feng
                    ;; "jb" 'js-beautify
                    ;; liang.feng
                    ;; "jp" 'my-print-json-path
                    ;; "sep" 'string-edit-at-point
                    ;; "sec" 'string-edit-conclude
                    ;; "sea" 'string-edit-abort
                    "xe" 'eval-last-sexp
                    ;; "x0" 'delete-window
                    ;; "x1" 'delete-other-windows
                    ;; "x2" 'my-split-window-vertically
                    ;; "x3" 'my-split-window-horizontally
                    ;; "s2" 'ffip-split-window-vertically
                    ;; "s3" 'ffip-split-window-horizontally
                    ;; "rw" 'rotate-windows
                    "ru" 'undo-tree-save-state-to-register ; C-x r u
                    "rU" 'undo-tree-restore-state-from-register ; C-x r U
                    ;; "xt" 'toggle-window-split
                    ;; "uu" 'winner-undo
                    ;; "UU" 'winner-redo
                    ;; "to" 'toggle-web-js-offset
                    ;; liang.feng
                    ;; "sl" 'sort-lines
                    ;; "ulr" 'uniquify-all-lines-region
                    ;; "ulb" 'uniquify-all-lines-buffer
                    ;; "fc" 'cp-ffip-ivy-last
                    ;; "ss" 'swiper-the-thing ; http://oremacs.com/2015/03/25/swiper-0.2.0/ for guide
                    ;; liang.feng
                    ;; "bc" '(lambda () (interactive) (wxhelp-browse-class-or-api (thing-at-point 'symbol)))

                    "og" 'org-agenda
                    "otl" 'org-toggle-link-display
                    "oa" '(lambda ()
                            (interactive)
                            (unless (featurep 'org) (require 'org))
                            (counsel-org-agenda-headlines))
                    "oc" (lambda ()
                           (interactive)
                           (if (> emacs-major-version 25)
                               (counsel-org-capture)
                             (org-capture)))
                    ;; "oc" 'org-capture
                    ;; "om" 'toggle-org-or-message-mode
                    "pf" 'projectile-find-file-dwim
                    "pg" 'projectile-grep
                    "ps" 'projectile-speedbar-toggle
                    "py" (lambda ()
                           (interactive)
                           (pyim-export-personal-words "~/.emacs.d/pyim-mine.pyim")
                           (pyim-export "~/.emacs.d/pyim-mine.cipin"))
                    "pi" (lambda ()
                           (interactive)
                           (pyim-import "~/.emacs.d/pyim-mine.cipin"))
                    "ar" 'align-regexp
                    ;; "ww" 'ace-jump-mode
                    ;; "ec" 'ace-jump-char-mode
                    ;; "el" 'ace-jump-line-mode
                    "ww" 'persp-switch ;; Query a perspective to switch to, or create
                    "bb" 'persp-switch-last
                    "wr" 'persp-rename
                    "wa" 'persp-add-buffer ;; Query an open buffer to add to current perspective
                    "wd" 'persp-remove-buffer ;; Query a buffer to remove from current perspective
                    "wk" 'persp-kill ;; Query a perspective to kill
                    "wn" 'persp-next ;; Switch to next perspective
                    "wp" 'persp-prev ;; Switch to previous perspective
                    "ws" 'persp-state-save ;; Save all perspectives in all frames to a file
                    ;; "wl" '(lambda () (interactive) (persp-state-load "~/.emacs.d/perspective.save")) ;; Load all perspectives from a file
                    "wl" 'persp-state-load ;; Load all perspectives from a file
                    ;; "xx" 'er/expand-region
                    ;; "xf" 'ido-find-file
                    ;; "xb" 'ivy-switch-buffer-by-pinyin
                    "xh" 'mark-whole-buffer
                    ;; "xk" 'ido-kill-buffer
                    ;; "xs" 'save-buffer
                    ;; "vm" 'vc-rename-file-and-buffer
                    ;; "vc" 'vc-copy-file-and-rename-buffer
                    ;; "xvv" 'vc-next-action ; 'C-x v v' in original
                    ;; "vg" 'vc-anntate ; 'C-x v g' in original
                    ;; "vl" 'vc-print-log
                    ;; "vv" 'vc-msg-show
                    ;; "hh" 'cliphist-paste-item
                    ;; "yu" 'cliphist-select-item
                    ;; "ih" 'my-goto-git-gutter ; use ivy-mode
                    "ir" 'ivy-resume
                    "wi" 'widen
                    "xnd" 'narrow-to-defun
                    ;; "ycr" 'my-yas-reload-all
                    "xnr" 'narrow-to-region)
;; }}


;; must put after "nvmap :prefix \"SPC\""
;; evil-easymotion
;; (evilem-default-keybindings "SPC")

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;end copy from chenbin.emacs.d;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'init-evil)
