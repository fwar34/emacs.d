;;; init-git.el --- Useful preset transient commands  -*- coding:utf-8; lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(use-package magit
  :commands magit
  :config
  ;; https://www.helplib.com/GitHub/article_131559
  ;; (evil-define-key evil-magit-state magit-mode-map "?"'evil-search-backward)
  (setq magit-diff-refine-hunk t))

(use-package git-gutter
  :disabled
  ;; :bind
  ;; (("SPC c n" . git-gutter:next-hunk)
  ;;  ("SPC c p" . git-gutter:previous-hunk))
  ;; :if (not (display-graphic-p))
  :ensure t
  :after evil
  ;; :if (display-graphic-p)
  :config
  ;; If you enable global minor mode
  (global-git-gutter-mode t)
  ;; If you would like to use git-gutter.el and linum-mode
  ;; (unless (display-graphic-p) (git-gutter:linum-setup))
  ;; (git-gutter:linum-setup)
  ;; Use for 'Git'(`git`), 'Mercurial'(`hg`), 'Bazaar'(`bzr`), and 'Subversion'(`svn`) projects
  ;; (custom-set-variables '(git-gutter:handled-backends '(git hg bzr svn)))
  (custom-set-variables '(git-gutter:handled-backends '(git svn)))
  ;; inactivate git-gutter-mode in asm-mode and image-mode
  (custom-set-variables '(git-gutter:disabled-modes '(asm-mode image-mode c++-mode)))
  ;; Hide gutter when there are no changes if git-gutter:hide-gutter is non-nil. (Default is nil)
  (custom-set-variables '(git-gutter:hide-gutter t))
  ;; If you set git-gutter :update-interval seconds larger than 0,
  ;; git-gutter updates diff information in real-time by idle timer.
  (custom-set-variables '(git-gutter:update-interval 0))
  (custom-set-variables '(git-gutter:visual-line t))

  ;; console not display, because git-gutter has bug in emacs26 no window
  ;; (unless window-system (custom-set-variables '(git-gutter:display-p nil)))
  ;; diff information is updated at hooks in git-gutter:update-hooks.
  (add-to-list 'git-gutter:update-hooks 'focus-in-hook)
  ;; diff information is updated after command in git-gutter:update-commands executed.
  (add-to-list 'git-gutter:update-commands 'other-window)
  ;; (custom-set-variables
  ;;  '(git-gutter:modified-sign "~") ;; two space
  ;;  '(git-gutter:added-sign "++")    ;; multiple character is OK
  ;;  '(git-gutter:deleted-sign "--"))
  ;; (set-face-background 'git-gutter:modified "purple") ;; background color
  ;; (set-face-foreground 'git-gutter:added "green")
  ;; (set-face-foreground 'git-gutter:deleted "red")
  ;; (set-face-background 'git-gutter:modified "purple") ;; background color
  ;; (set-face-background 'git-gutter:added "green")
  ;; (set-face-background 'git-gutter:deleted "red")
  ;; Jump to next/previous hunk
  ;; (define-key evil-normal-state-map (kbd "[ c") 'git-gutter:previous-hunk)
  ;; (define-key evil-normal-state-map (kbd "] c") 'git-gutter:next-hunk)
  ;; (define-key evil-normal-state-map (kbd "] s") 'git-gutter:stage-hunk)

  ;; https://github.com/noctuid/evil-guide
  ;; you could use this to have git-gutter’s commands for navigating hunks save the current location before jumping:
  (evil-add-command-properties 'git-gutter:next-hunk :jump t)
  (evil-add-command-properties 'git-gutter:previous-hunk :jump t)

  (defhydra hydra-git-gutter (:body-pre (git-gutter-mode 1)
                                        :hint nil)
    "
Git gutter:
  _j_: next hunk        _s_tage hunk     _q_uit
  _k_: previous hunk    _r_evert hunk    _Q_uit and deactivate git-gutter
  ^ ^                   _p_opup hunk
  _h_: first hunk
  _l_: last hunk        set start _R_evision
"
    ("j" git-gutter:next-hunk)
    ("k" git-gutter:previous-hunk)
    ("h" (progn (goto-char (point-min))
                (git-gutter:next-hunk 1)))
    ("l" (progn (goto-char (point-min))
                (git-gutter:previous-hunk 1)))
    ("s" git-gutter:stage-hunk)
    ("r" git-gutter:revert-hunk)
    ("p" git-gutter:popup-hunk)
    ("R" git-gutter:set-start-revision)
    ("q" nil :color blue)
    ("Q" (progn (git-gutter-mode -1)
                ;; git-gutter-fringe doesn't seem to
                ;; clear the markup right away
                (sit-for 0.1)
                (git-gutter:clear))
     :color blue)))

(use-package git-gutter-fringe
  :disabled
  :ensure t
  :after git-gutter
  :if (display-graphic-p)
  :config
  (set-face-foreground 'git-gutter-fr:modified "purple")
  (set-face-foreground 'git-gutter-fr:added "green")
  (set-face-foreground 'git-gutter-fr:deleted "red"))

(use-package diff-hl
  ;; windows 下 svn 命令行可以使用 https://www.visualsvn.com/downloads/ 中的 “Apache Subversion command line tools”，或者直接 scoop install sliksvn
  ;; scoop install unxutils sliksvn
  ;; mine/vimfiles/windows.ps1，scoop安装脚本
  :after evil
  :config
  (global-diff-hl-mode)
  (unless (display-graphic-p)
    (diff-hl-margin-mode))
  (advice-add 'svn-status-update-modeline :after 'diff-hl-update)
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(provide 'init-git)
;;; init-git.el ends here
