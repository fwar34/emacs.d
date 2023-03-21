;;; init-meow.el --- Useful preset transient commands  -*- coding:utf-8; lexical-binding: t; -*-
;;; Commentary:

(defun key-mappings-setup ()
  (global-set-key (kbd "C-c du") #'(lambda ()
                                     (interactive)
                                     (if (featurep 'diff-hl)
                                         (diff-hl-diff-goto-hunk)
                                       (define-advice git-gutter:popup-hunk (:around (orig-fun &rest args) my-git-gutter:popup-hunk)
                                         (let ((res (apply orig-fun args)))
                                           (when res
                                             (switch-to-buffer-other-window res)
                                             (evil-local-set-key 'normal (kbd "q") 'kill-buffer-and-window))))
                                       (git-gutter:popup-hunk))))
  (global-set-key (kbd "C-c dr") #'(lambda ()
                                     (interactive)
                                     (if (featurep 'diff-hl)
                                         (diff-hl-revert-hunk)
                                       (progn
                                         (advice-remove 'git-gutter:popup-hunk 'git-gutter:popup-hunk@my-git-gutter:popup-hunk)
                                         (git-gutter:revert-hunk)))))
  (global-set-key (kbd "C-c dn") #'(lambda ()
                                     (interactive)
                                     (if (featurep 'diff-hl)
                                         (diff-hl-next-hunk)
                                       (git-gutter:next-hunk (line-number-at-pos)))))
  (global-set-key (kbd "C-c dp") #'(lambda ()
                                     (interactive)
                                     (if (featurep 'diff-hl)
                                         (diff-hl-previous-hunk)
                                       (git-gutter:previous-hunk (line-number-at-pos)))))
  (global-set-key (kbd "C-c d=") 'diff-hl-diff-goto-hunk)
  (global-set-key (kbd "C-]") 'counsel-etags-find-tag-at-point)
  (global-set-key (kbd "C-r") 'undo-redo)
  )

(defun meow-insert-setup ()
  ;; (bind-key "C-w" 'backward-kill-word meow-insert-state-keymap)
  (define-key meow-insert-state-keymap (kbd "C-w") #'backward-kill-word)
  ;; (define-key meow-insert-state-keymap (kbd ";tm") 'vterm-toggle)
  )

;;; Code:
(use-package meow
  :init
  (defun meow-setup ()
    (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
    (meow-motion-overwrite-define-key
     '("j" . meow-next)
     '("k" . meow-prev)
     '("<escape>" . ignore))
    (meow-leader-define-key
     ;; SPC j/k will run the original command in MOTION state.
     '("j" . "H-j")
     '("k" . "H-k")
     ;; Use SPC (0-9) for digit arguments.
     '("1" . meow-digit-argument)
     '("2" . meow-digit-argument)
     '("3" . meow-digit-argument)
     '("4" . meow-digit-argument)
     '("5" . meow-digit-argument)
     '("6" . meow-digit-argument)
     '("7" . meow-digit-argument)
     '("8" . meow-digit-argument)
     '("9" . meow-digit-argument)
     '("0" . meow-digit-argument)
     '("/" . meow-keypad-describe-key)
     '("?" . meow-cheatsheet))
    (meow-leader-define-key
     ;; '("fn" . c-display-defun-name)
     '("qq" . save-buffers-kill-terminal)
     '("fn" . (lambda () (message (c-defun-name))))
     '("ww" . major-mode-hydras/persp-mode/body)
     '("bb" . persp-switch-last)
     '("ir" . ivy-resume)
     )
    (meow-normal-define-key
     '("0" . meow-expand-0)
     '("9" . meow-expand-9)
     '("8" . meow-expand-8)
     '("7" . meow-expand-7)
     '("6" . meow-expand-6)
     '("5" . meow-expand-5)
     '("4" . meow-expand-4)
     '("3" . meow-expand-3)
     '("2" . meow-expand-2)
     '("1" . meow-expand-1)
     '("-" . negative-argument)
     '("/" . meow-visit)
     '("," . meow-inner-of-thing)
     '("." . meow-bounds-of-thing)
     '("[" . meow-beginning-of-thing)
     '("]" . meow-end-of-thing)
     '("a" . meow-join)
     '("A" . mark-whole-buffer)
     '("b" . meow-back-word)
     '("B" . meow-back-symbol)
     '("c" . meow-change)
     '("d" . meow-cancel-selection)
     '("D" . meow-grab)
     '("e" . meow-line)
     '("E" . meow-line-expand)
     '("f" . meow-find)
     '("F" . meow-find-expand)
     '("g" . beginning-of-buffer)
     '("G" . end-of-buffer)
     '("h" . meow-left)
     '("H" . meow-left-expand)
     '("i" . meow-append)
     '("I" . meow-insert)
     '("j" . meow-next)
     '("J" . meow-next-expand)
     '("k" . meow-prev)
     '("K" . meow-prev-expand)
     '("l" . meow-right)
     '("L" . meow-right-expand)
     '("m" . meow-mark-word)
     '("M" . meow-mark-symbol)
     '("n" . meow-search)
     '("N" . meow-pop-search)
     '("o" . meow-block)
     '("O" . meow-to-block)
     '("p" . meow-yank)
     '("q" . meow-quit)
     '("Q" . meow-goto-line)
     '("r" . meow-replace)
     '("R" . meow-swap-grab)
     '("s" . meow-kill)
     '("S" . meow-kill-whole-line)
     '("t" . meow-till)
     '("T" . meow-till-expand)
     '("u" . meow-undo)
     '("U" . meow-undo-in-selection)
     '("v" . meow-reverse)
     '("w" . meow-next-word)
     '("W" . meow-next-symbol)
     '("x" . meow-delete)
     '("X" . meow-backward-delete)
     '("y" . meow-save)
     '("Y" . meow-sync-grab)
     '("z" . meow-pop-selection)
     '("<" . meow-open-above)
     '(">" . meow-open-below)
     '("'" . repeat)
     '("`" . meow-last-buffer)
     '("\\" . meow-comment)
     '("&" . meow-query-replace-regexp)
     '("%" . meow-query-replace)
     '("<escape>" . ignore))
    (meow-normal-define-key
     '("; 1" . winum-select-window-1)
     '("; 2" . winum-select-window-2)
     '("; 3" . winum-select-window-3)
     '("; 4" . winum-select-window-4)
     '("; 5" . winum-select-window-5)
     '("; 6" . winum-select-window-6)
     '("; 7" . winum-select-window-7)
     '("; 8" . winum-select-window-8)
     '("; 9" . winum-select-window-9)
     '("; x2" . split-window-below)
     '("; x3" . split-window-right)
     '("; bb" . meow-last-buffer)
     '("; so" . symbol-overlay-put)
     '("; st" . symbol-overlay-transient)
     '("; ma" . magit)
     '("; xx" . highlight-remove-all)
     '("; ci" . evilnc-comment-or-uncomment-lines)
     '("; cl" . evilnc-quick-comment-or-uncomment-to-the-line)
     '("; cc" . evilnc-copy-and-comment-lines)
     '("; cp" . evilnc-comment-or-uncomment-paragraphs)
     '("; cr" . comment-or-uncomment-region)
     '("; cv" . evilnc-toggle-invert-comment-line-by-line)
     '("; ." . evilnc-copy-and-comment-operator)
     '("; \\" . evilnc-comment-operator) ; if you prefer backslash key
     '("; ii" . counsel-imenu)
     '("; fa" . counsel-rg)
     '("; tl" . counsel-etags-list-tag)
     '("; tr" . counsel-etags-recent-tag)
     '("; tf" . counsel-etags-find-tag)
     '("; tg" . counsel-etags-grep)
     '("; tl" . counsel-etags-list-tag)
     '("; tc" . counsel-etags-list-tag-in-current-file)
     '("; li" . swiper)
     '("; kr" . browse-kill-ring)
     '("; rm" . counsel-recentf)
     '("; ff" . counsel-find-file)
     '("; fd" . my-display-full-path-of-current-buffer)
     '("; fb" . beginning-of-defun)
     '("; fe" . end-of-defun)
     '("; fz" . (lambda () (interactive) (counsel-fzf "" "~")))
     '("; sw" . transpose-words) ;;swap two words next to each other
     '("; kw" . kill-buffer-and-window)
     '("; bs" . persp-ivy-switch-buffer)
     '("; kb" . persp-kill-buffer*)
     '("; tm" . vterm-toggle)
     '("; fo" . my-find-other-file)
     '("; yw" . youdao-dictionary-search-at-point)
     '("; yy" . youdao-dictionary-search-at-point+)
     '("; yd" . youdao-dictionary-search-from-input)
     '("; fw" . (lambda () (interactive) (my-search-whole-word 'counsel-rg)))
     '("; fs" . (lambda () (interactive) (my-search-forward-word 'counsel-rg)))
     '("; ls" . my-swiper-forward-word)
     '("; lw" . swiper-thing-at-point)
     '("; zz" . save-buffer)
     '("; md" . mark-defun)
     '("; xd" . (lambda (identifier)
                  (interactive (list (xref--read-identifier "Find definitions of: ")))
                  (unless (featurep 'ivy-xref)
                    (require 'ivy-xref))
                  (xref-find-definitions identifier)))

     '("; xr" . (lambda (identifier)
                  (interactive (list (xref--read-identifier "Find references of: ")))
                  (unless (featurep 'ivy-xref)
                    (require 'ivy-xref))
                  (xref-find-references identifier)))
     '("; dj" . dired-jump) ;; open the dired from current file
     '("==" . indent-region)
     '("; TAB" . other-window)
     '("; ;w" . avy-goto-word-0)
     '("; gs" . avy-goto-char-2)
     '("; do" . delete-other-windows)
     '("; pf" . (lambda () (interactive)
                  (unless (featurep 'counsel)
                    (require 'counsel))
                  (project-find-file)))
     '("; pg" . project-find-regexp)
     '("; se" . open-init-file)
     '("; rr" . fwar34/counsel-goto-recent-directory)
     '("; rc" . fwar34/run-current-file)
     '("; SPC" . counsel-M-x)

     ))
  :config
  (meow-setup)
  (meow-global-mode 1)
  (key-mappings-setup)
  (meow-insert-setup)

  ;; Use jk to escape from insert state to normal state
  (defvar meow-two-char-escape-sequence ";g")
  (defvar meow-two-char-escape-delay 0.5)
  (defvar meow-two-char-exclude-modes '(vterm-mode))
  (defun meow--two-char-exit-insert-state (s)
    "Exit meow insert state when pressing consecutive two keys.

S is string of the two-key sequence."
    (when (and (meow-insert-mode-p) (not (member major-mode meow-two-char-exclude-modes)))
      (let ((modified (buffer-modified-p))
            (undo-list buffer-undo-list))
        (insert (elt s 0))
        (let* ((second-char (elt s 1))
               (event
                (if defining-kbd-macro
                    (read-event nil nil)
                  (read-event nil nil meow-two-char-escape-delay))))
          (when event
            (if (and (characterp event) (= event second-char))
                (progn
                  (backward-delete-char 1)
                  (set-buffer-modified-p modified)
                  (setq buffer-undo-list undo-list)
                  (meow-insert-exit))
              (push event unread-command-events)))))))
  (defun meow-two-char-exit-insert-state ()
    "Exit meow insert state when pressing consecutive two keys."
    (interactive)
    (meow--two-char-exit-insert-state meow-two-char-escape-sequence))
  ;; (define-key meow-insert-state-keymap (substring meow-two-char-escape-sequence 0 1) #'meow-two-char-exit-insert-state)
  (bind-key (substring meow-two-char-escape-sequence 0 1) #'meow-two-char-exit-insert-state meow-insert-state-keymap)
  (add-hook 'meow-insert-enter-hook (lambda ()
                                      (when (member major-mode meow-two-char-exclude-modes)
                                        (message "meow-insert")
                                        ;; (local-set-key (kbd "; tm") 'vterm-toggle)
                                        ;; (define-key term-mode-map (kbd ";tm") 'vterm-toggle)
                                        (general-define-key
                                         :keymaps 'term-raw-map
                                         :prefix ";"
                                         "tm" 'vterm-toggle
                                         ";" #'(lambda () (term-send-raw-string ";")))
                                        ;; (bind-key (kbd ";tm") 'vterm-toggle term-raw-map)
                                        ;; (define-key term-raw-map (kbd ";tm") 'vterm-toggle)
                                        ;; (define-key term-raw-map (kbd ";;") (lambda () (term-send-raw-string ";")))

                                        ;; (unbind-key (substring meow-two-char-escape-sequence 0 1) meow-insert-state-keymap)
                                        ;; (bind-key (substring meow-two-char-escape-sequence 0 1) #'meow-two-char-exit-insert-state meow-insert-state-keymap)
                                        )
                                      
                                      ))
  )

(provide 'init-meow)
;;; init-meow.el ends here
