;;; init-keybindings.el --- Useful preset transient commands  -*- coding:utf-8; lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(with-eval-after-load 'evil
  ;; highlight persistent
  ;; https://stackoverflow.com/questions/25768036/emacs-evil-non-incremental-search-and-persistent-highlighting/34252236#34252236
  ;; This will highlight all searches done with isearch or Evil search.
  ;; The highlight will remain until you make another one or
  ;; call highlight-remove-all. I've mapped it to leader SPC with:
  (defun highlight-remove-all ()
    "Remove all highlight overlay."
    (interactive)
    (hi-lock-mode -1)
    (hi-lock-mode 1)
    ;; (highlight-symbol-remove-all)
    (symbol-overlay-remove-all)
    (lazy-highlight-cleanup t))

  (defun search-highlight-persist ()
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

  (cl-loop for (mode . state) in '((apropos-mode . normal)
                                   (browse-kill-ring-mode . normal)
                                   ;; (ivy-occur-grep-mode . emacs)
                                   ;; (proced-mode . emacs)
                                   (imenu-list-major-mode . emacs)
                                   (calc-mode . emacs)
                                   (easy-hugo-mode . emacs)
                                   (calculator-mode . emacs))
           do (evil-set-initial-state mode state))

  ;; TAB and C-i is the same
  (define-key evil-normal-state-map (kbd "ge") 'evil-goto-line)
  (define-key evil-visual-state-map (kbd "gg") 'evil-change-to-previous-state)

  (evil-define-key 'normal package-menu-mode-map "f" 'swiper)

  (define-key evil-normal-state-map (kbd "C-]") 'counsel-etags-find-tag-at-point)
  (define-key evil-insert-state-map (kbd "C-j") 'evil-next-line)
  (define-key evil-insert-state-map (kbd "C-k") 'evil-previous-line)
  (define-key evil-insert-state-map (kbd "C-u") 'my-kill-line)
  (define-key evil-insert-state-map (kbd "C-d") 'evil-delete-line)
  (define-key evil-insert-state-map (kbd "C-l") 'hungry-delete-forward)

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

  (define-key evil-ex-completion-map (kbd "C-a") 'evil-first-non-blank)
  (define-key evil-ex-completion-map (kbd "C-b") 'backward-char)
  (define-key evil-ex-completion-map (kbd "M-p") 'previous-complete-history-element)
  (define-key evil-ex-completion-map (kbd "M-n") 'next-complete-history-element)

  (define-key evil-normal-state-map "go" 'goto-char)
  (define-key evil-normal-state-map "g1" 'evil-avy-goto-word-or-subword-1)
  (define-key evil-normal-state-map "g2" 'evil-avy-goto-char-2)

  ;; I learn this trick from ReneFroger, need latest expand-region
  ;; @see https://github.com/redguardtoo/evil-matchit/issues/38
  (define-key evil-visual-state-map (kbd "v") 'er/expand-region)
  (define-key evil-insert-state-map (kbd "C-a") 'evil-first-non-blank)
  (define-key evil-insert-state-map (kbd "C-e") 'move-end-of-line)

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
                 counsel-recentf))
    (evil-add-command-properties con :jump t))

  (general-define-key :keymaps '(normal visual)
                      :jump t
                      ;; :states '(normal motion insert emacs)
                      :prefix ";"
                      ;; :non-normal-prefix "M-;"
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
                      "x2" 'split-window-below
                      "x3" 'split-window-right
                      "a" 'evil-first-non-blank
                      "e" 'evil-end-of-line
                      "SPC" 'counsel-M-x
                      "TAB" 'other-window
                      "qr" 'quickrun
                      "qs" 'query-replace-regexp
                      ;;symbol-overlay默认n,p,i,q在高亮的地方点击为下一个，上一个，取消所有的高亮，替换，h help
                      "st" 'symbol-overlay-transient
                      "so" 'symbol-overlay-put
                      "sr" 'symbol-overlay-remove-all
                      "ia" 'my-append-semicolon-excursion
                      "ic" 'fwar34/insert-python
                      "do" 'delete-other-windows
                      "ff" 'counsel-find-file
                      "fd" 'my-display-full-path-of-current-buffer
                      "fb" 'beginning-of-defun
                      "fz" '(lambda () (interactive) (counsel-fzf "" "~"))
                      "cd" 'my-change-default-directory
                      "ii" 'counsel-imenu
                      "il" 'imenu-list-smart-toggle
                      "jj" 'taglist-list-tags
                      "sw" 'transpose-words ;;swap two words next to each other
                      "xm" 'my-M-x
                      "li" 'swiper
                      "lb" 'swiper-all
                      "ls" 'my-swiper-forward-word
                      "lw" 'swiper-thing-at-point
                      "la" 'swiper-all-thing-at-point
                      "oc" 'occur-dwim
                      "oi" 'isearch-occur
                      "qq" 'quit-window
                      "ge" 'goto-line
                      "ud" 'undo-tree-visualize
                      "ua" 'universal-argument
                      "fa" 'counsel-rg
                      "fw" (lambda () (interactive) (my-search-whole-word 'counsel-rg))
                      "fs" (lambda () (interactive) (my-search-forward-word 'counsel-rg))
                      "gg" 'counsel-ag
                      "gw" (lambda () (interactive) (my-search-whole-word 'counsel-ag))
                      "gs" (lambda () (interactive) (my-search-forward-word 'counsel-ag))
                      "fe" 'end-of-defun
                      "fm" 'mark-defun
                      "xd" (lambda (identifier)
                             (interactive (list (xref--read-identifier "Find definitions of: ")))
                             (unless (featurep 'ivy-xref)
                               (require 'ivy-xref))
                             (xref-find-definitions identifier))

                      "xr" (lambda (identifier)
                             (interactive (list (xref--read-identifier "Find references of: ")))
                             (unless (featurep 'ivy-xref)
                               (require 'ivy-xref))
                             (xref-find-references identifier))
                      "dj" 'dired-jump ;; open the dired from current file
                      "ht" 'counsel-etags-find-tag-at-point ; better than find-tag C-]
                      "hr" 'counsel-etags-recent-tag
                      "hf" 'counsel-etags-find-tag
                      "hg" 'counsel-etags-grep
                      "tl" 'counsel-etags-list-tag
                      "tc" 'counsel-etags-list-tag-in-current-file
                      "hh" 'ivy-imenu-anywhere
                      "kr" 'browse-kill-ring
                      "cf" 'counsel-grep  ; grep current buffer
                      "cg" 'counsel-git   ; find file
                      "kw" 'kill-buffer-and-window
                      "bs" 'persp-ivy-switch-buffer
                      "kb" 'persp-kill-buffer*
                      "bp" 'switch-to-prev-buffer
                      "bb" 'evil-buffer
                      "zz" 'save-buffer
                      "fo" 'my-find-other-file
                      "mm" 'evilmi-jump-items
                      "tt" 'neotree-toggle
                      "yw" 'youdao-dictionary-search-at-point
                      "yy" 'youdao-dictionary-search-at-point+
                      "yd" 'youdao-dictionary-search-from-input
                      "rm" 'counsel-recentf
                      "sf" 'isearch-forward-regexp
                      "sb" 'isearch-backward-regexp
                      "ci" 'evilnc-comment-or-uncomment-lines
                      "cl" 'evilnc-quick-comment-or-uncomment-to-the-line
                      "cc" 'evilnc-copy-and-comment-lines
                      "cp" 'evilnc-comment-or-uncomment-paragraphs
                      "cr" 'comment-or-uncomment-region
                      "cv" 'evilnc-toggle-invert-comment-line-by-line
                      "."  'evilnc-copy-and-comment-operator
                      "\\" 'evilnc-comment-operator ; if you prefer backslash key
                      "gc" 'evil-avy-goto-char-2
                      "pf" (lambda () (interactive)
                             (unless (featurep 'counsel)
                               (require 'counsel))
                             (project-find-file))
                      "pg" 'project-find-regexp
                      "sm" 'smex
                      "sj" 'smex-major-mode-commands
                      "se" 'open-init-file
                      "ce" 'evil-emacs-state
                      "tm" 'vterm-toggle
                      "sh" 'aweshell-toggle
                      "rr" 'fwar34/counsel-goto-recent-directory
                      "rc" 'fwar34/run-current-file
                      "vv" 'my-vterm-transient)

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
                      "cv" 'my-convert-radix
                      "ch" 'my-convert-radix-hex
                      "cd" 'my-convert-radix-deci
                      "se" 'evil-iedit-state/iedit-mode ; start iedit in emacs
                      "sc" 'shell-command
                      "ac" 'async-shell-command
                      "st" 'org-shifttab
                      "cp" 'caps-lock-mode
                      "TAB" 'outline-toggle-children
                      "eh" 'toggle-company-english-helper
                      "ma" 'magit
                      "mf" 'with-editor-finish
                      "mc" 'with-editor-cancel
                      "ic" 'interrupt-my-commands
                      "hp" 'my-helpful-transient
                      "hh" 'my-hydra-helpful/body
                      "bj" (lambda () (interactive) (my-commands-shell "make -j 5"))
                      "bc" (lambda () (interactive) (my-commands-shell "make clean"))
                      "bs" 'persp-switch-to-buffer
                      "br" (lambda () (interactive) (my-commands-shell "make clean; make -j 5"))
                      "bd" (lambda () (interactive)
                             (my-commands-shell "cd ..; fakeroot debian/rules binary"))
                      "bf" (lambda () (interactive) (my-commands-shell "make -j 5 && cd ..; fakeroot debian/rules binary"))
                      "qq" 'save-buffers-kill-terminal
                      "xz" 'suspend-frame
                      "kk" 'scroll-other-window
                      "jj" 'scroll-other-window-up
                      "lm" 'display-line-numbers-mode
                      "fn" 'my-display-function
                      "fa" 'ack
                      "ff" '(lambda () (interactive) (fzf-find-file-in-dir "~"))
                      "fz" 'fzf
                      "ft" 'fzf-git
                      "fl" 'fzf-git-files
                      "fd" 'fzf-directory
                      "cy" 'clipboard-yank
                      "ds" 'git-gutter:statistic
                      "du" '(lambda ()
                              (interactive)
                              (if (featurep 'diff-hl)
                                  (diff-hl-diff-goto-hunk)
                                (define-advice git-gutter:popup-hunk (:around (orig-fun &rest args) my-git-gutter:popup-hunk)
                                  (let ((res (apply orig-fun args)))
                                    (when res
                                      (switch-to-buffer-other-window res)
                                      (evil-local-set-key 'normal (kbd "q") 'kill-buffer-and-window))))
                                (git-gutter:popup-hunk)))
                      "dr" '(lambda ()
                              (interactive)
                              (if (featurep 'diff-hl)
                                  (diff-hl-revert-hunk)
                                (progn
                                  (advice-remove 'git-gutter:popup-hunk 'git-gutter:popup-hunk@my-git-gutter:popup-hunk)
                                  (git-gutter:revert-hunk))))
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
                      "c=" 'vc-diff
                      "cl" 'vc-pull
                      "cu" 'vc-push
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
                      "wf" 'toggle-frame-fullscreen
                      "wm" 'toggle-frame-maximized
                      "le" 'elisp-index-search
                      "ls" 'emacs-index-search
                      "bm" 'counsel-bookmark
                      "be" 'edit-bookmarks
                      "rpy" 'run-python
                      "rlu" 'run-lua
                      "rru" 'rust-run
                      "rfm" 'rust-format-buffer
                      "tci" 'toggle-company-ispell
                      "oo" 'compile
                      "c$" 'org-archive-subtree ; `C-c $'
                      "c<" 'org-do-promote ; `C-c C-<'
                      "c>" 'org-do-demote ; `C-c C->'
                      "cam" 'org-tags-view ; `C-c a m': search items in org-file-apps by tag
                      "cxi" 'org-clock-in ; `C-c C-x C-i'
                      "cxo" 'org-clock-out ; `C-c C-x C-o'
                      "cxr" 'org-clock-report ; `C-c C-x C-r'
                      "di" 'evilmi-delete-items
                      "si" 'evilmi-select-items
                      "xe" 'eval-last-sexp
                      "mm" 'my-misc-transinet
                      "ru" 'undo-tree-save-state-to-register ; C-x r u
                      "rU" 'undo-tree-restore-state-from-register ; C-x r U
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
                      "ww" 'persp-switch ;; Query a perspective to switch to, or create
                      "bb" 'persp-switch-last
                      "wl" 'major-mode-hydras/persp-mode/body
                      "xh" 'mark-whole-buffer
                      "ir" 'ivy-resume
                      "wi" 'widen
                      "xnd" 'narrow-to-defun
                      "xnr" 'narrow-to-region)

  ;; (with-eval-after-load 'evil
  ;;     (with-eval-after-load 'elisp-mode
  ;;       (dolist (keymap (list emacs-lisp-mode-map lisp-interaction-mode-map))
  ;; 	(define-leader-key 'normal keymap :localleader
  ;; 			   "in" 'info-lookup-symbol
  ;; 			   ;; eval
  ;; 			   "eb" 'eval-buffer
  ;; 			   "ed" 'eval-defun
  ;; 			   "ee" 'eval-last-sexp
  ;; 			   "el" 'load-library
  ;; 			   ;; goto
  ;; 			   "gl" 'find-library))))
  )

(provide 'init-keybindings)
;;; init-keybindings.el ends here
