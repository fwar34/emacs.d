;; -*- coding: utf-8; lexical-binding: t; -*-
;;-------------------------------------------------------------
;; init-hydra
;; https://github.com/abo-abo/hydra/blob/05871dd6c8af7b2268bd1a10eb9f8a3e423209cd/hydra-examples.el#L190
;;-------------------------------------------------------------
(use-package hydra
  :after evil
  :ensure t
  :init
  (define-prefix-command 'M-u-map)
  (global-set-key (kbd "M-u") 'M-u-map)
  :config

  ;;-------------------------------------------------------------
  ;; vi
  (defun hydra-vi/pre ()
    (set-cursor-color "#e52b50"))
  (defun hydra-vi/post ()
    (set-cursor-color "#ffffff"))
  (global-set-key (kbd "M-u vi")
                  (defhydra hydra-vi (:pre hydra-vi/pre :post hydra-vi/post :color amaranth)
                    "vi"
                    ("l" forward-char)
                    ("h" backward-char)
                    ("j" next-line)
                    ("k" previous-line)
                    ("m" set-mark-command "mark")
                    ;; ("a" move-beginning-of-line "beg")
                    ("a" evil-first-non-blank "beg")
                    ("e" move-end-of-line "end")
                    ("d" delete-region "del" :color blue)
                    ("y" kill-ring-save "yank" :color blue)
                    ("q" nil "quit")))
  (hydra-set-property 'hydra-vi :verbosity 1)

  ;;-------------------------------------------------------------
  ;; window
  (defhydra hydra-window
    (:color red :hint nil)
    "
                               -- WINDOW MENU --
    "

    ("z" ace-window "ace" :color blue :column "1-Switch")
    ("h" windmove-left "← window")
    ("j" windmove-down "↓ window")
    ("k" windmove-up "↑ window")
    ("l" windmove-right "→ window")
    ("s" split-window-below "split window" :color blue :column "2-Split Management")
    ("v" split-window-right "split window vertically" :color blue)
    ("d" delete-window "delete current window")
    ("f" follow-mode "toogle follow mode")
    ("u" winner-undo "undo window conf" :column "3-Undo/Redo")
    ("r" winner-redo "redo window conf")
    ("b" balance-windows "balance window height" :column "4-Sizing")
    ("m" maximize-window "maximize current window")
    ("M" minimize-window "minimize current window")
    ("q" nil "quit menu" :color blue :column nil))
  (global-set-key (kbd "M-u w") #'hydra-window/body)

  ;;-------------------------------------------------------------
  ;; org
  (defhydra hydra-org (:color red :hint nil)
    "
    ^outline^                              ^org^
    ^^^^^^^^--------------------------------------------------------
    _h_: previous visible heading line     _oa_: org-agenda
    _l_: next visible heading line         _oc_: org-capture
    _j_: same level forward        
    _k_: same level backward
    "
    ("h" outline-previous-visible-heading)
    ("l" outline-next-visible-heading)
    ("j" outline-forward-same-level)
    ("k" outline-backward-same-level)
    ("tb" org-cycle)
    ("sb" org-shifttab)
    ("oa" org-agenda :exit t)
    ("oc" org-capture :exit t)
    ("q" nil "cancel" :color bule))
  (evil-define-key 'normal 'global (kbd "M-u og") #'hydra-org/body)
  ;; (with-eval-after-load 'org
  ;;   (define-key org-mode-map (kbd "M-u og") 'hydra-org/body))

  ;;-------------------------------------------------------------
  ;; fwar34
  (defhydra hydra-fwar34 (:columns 3 :exit t)
    "
            -- MY COMMANDS --
    "
    ("li" fwar34/insert-lisp-commit "lisp commit")
    ("py" fwar34/insert-python "python commit")
    ("rc" fwar34/run-current-file "run current file")
    ("q" nil "cancale" :color blue))
  ;; (define-key evil-normal-state-map (kbd "M-u f") #'hydra-fwar34/body)
  (evil-define-key '(normal insert) 'global (kbd "M-u fw") #'hydra-fwar34/body)

  ;;-------------------------------------------------------------
  ;; M-um
  (defhydra hydra-M-um (:color pink :hint nil)
    "
    ^kill-ring^                      ^iedit-mode^        ^fix-word^
    ^^-----------------------------------------------------------------------------
    _p_: paste from clipboard        _me_: iedit mode    _u_: fix-word-upcase
    _y_: grab the symbol at point    ^  ^                _d_: fix-word-downcase
    ^ ^                              ^  ^                _c_: fix-word-capitalize
    "
    ("p" clipboard-yank :exit t)
    ("y" ack-yank-symbol-at-point :exit t)
    ("me" iedit-mode)
    ("u" fix-word-upcase :exit t)
    ("d" fix-word-downcase :exit t)
    ("c" fix-word-capitalize :exit t)
    ("q" nil "cancale" :color blue))
  (global-set-key (kbd "M-u m") #'hydra-M-um/body)
  ;; (define-key evil-ex-completion-map (kbd "M-u m") #'hydra-M-um/body)
  ;; (define-key evil-ex-search-keymap (kbd "M-u m") #'hydra-M-um/body)

  ;;-------------------------------------------------------------
  ;; apropos
  (defhydra hydra-apropos (:color blue :hint nil)
    ;; "
    ;; _a_propos        _c_ommand
    ;; _d_ocumentation  _l_ibrary
    ;; _v_ariable       _u_ser-option
    ;; ^ ^          valu_e_"
    ("a" apropos "apropos" :column "Apropos")
    ("d" apropos-documentation "documentation")
    ("v" apropos-variable "variable")
    ("c" apropos-command "command")
    ("l" apropos-library "library")
    ("u" apropos-user-option "user-option")
    ("e" apropos-value "value")
    ("q" nil "cancel" :exit t :column nil))
  (global-set-key (kbd "M-u ap") 'hydra-apropos/body)

  ;;-------------------------------------------------------------
  ;; agenda
  (defhydra hydra-org-agenda-view (:hint none)
    "
    _d_: ?d? day        _g_: time grid=?g?  _a_: arch-trees
    _w_: ?w? week       _[_: inactive       _A_: arch-files
    _t_: ?t? fortnight  _f_: follow=?f?     _r_: clock report=?r?
    _m_: ?m? month      _e_: entry text=?e? _D_: include diary=?D?
    _y_: ?y? year       _q_: quit           _L__l__c_: log = ?l?"
    ("SPC" org-agenda-reset-view)
    ("d" org-agenda-day-view (if (eq 'day (org-agenda-cts)) "[x]" "[ ]"))
    ("w" org-agenda-week-view (if (eq 'week (org-agenda-cts)) "[x]" "[ ]"))
    ("t" org-agenda-fortnight-view (if (eq 'fortnight (org-agenda-cts)) "[x]" "[ ]"))
    ("m" org-agenda-month-view (if (eq 'month (org-agenda-cts)) "[x]" "[ ]"))
    ("y" org-agenda-year-view (if (eq 'year (org-agenda-cts)) "[x]" "[ ]"))
    ("l" org-agenda-log-mode (format "% -3S" org-agenda-show-log))
    ("L" (org-agenda-log-mode '(4)))
    ("c" (org-agenda-log-mode 'clockcheck))
    ("f" org-agenda-follow-mode (format "% -3S" org-agenda-follow-mode))
    ("a" org-agenda-archives-mode)
    ("A" (org-agenda-archives-mode 'files))
    ("r" org-agenda-clockreport-mode (format "% -3S" org-agenda-clockreport-mode))
    ("e" org-agenda-entry-text-mode (format "% -3S" org-agenda-entry-text-mode))
    ("g" org-agenda-toggle-time-grid (format "% -3S" org-agenda-use-time-grid))
    ("D" org-agenda-toggle-diary (format "% -3S" org-agenda-include-diary))
    ("!" org-agenda-toggle-deadlines)
    ("[" (let ((org-agenda-include-inactive-timestamps t))
           (org-agenda-check-type t 'timeline 'agenda)
           (org-agenda-redo)
           (message "Display now includes inactive timestamps as well")))
    ("q" (message "Abort") :exit t)
    ("v" nil))
  (with-eval-after-load 'org-agenda
    (define-key org-agenda-mode-map (kbd "M-u ag") 'hydra-org-agenda-view/body))

  ;;-------------------------------------------------------------
  ;; pyim
  (defhydra hydra-pyim (:color blue :hint nil)
    "
    ^pyim^                  
    ^^^^^^^^-----------------
    _si_: set input pyim
    _co_: convert string at point
    "
    ("si" (lambda () (set-input-method "pyim")))
    ("co" pyim-convert-string-at-point )
    ("q" nil "cancale" :color blue))
  (global-set-key (kbd "M-u py") #'hydra-pyim/body)

  ;;-------------------------------------------------------------
  (require 'windmove)
  (defun hydra-move-splitter-left (arg)
    "Move window splitter left."
    (interactive "p")
    (if (let ((windmove-wrap-around))
          (windmove-find-other-window 'right))
        (shrink-window-horizontally arg)
      (enlarge-window-horizontally arg)))

  (defun hydra-move-splitter-right (arg)
    "Move window splitter right."
    (interactive "p")
    (if (let ((windmove-wrap-around))
          (windmove-find-other-window 'right))
        (enlarge-window-horizontally arg)
      (shrink-window-horizontally arg)))

  (defun hydra-move-splitter-up (arg)
    "Move window splitter up."
    (interactive "p")
    (if (let ((windmove-wrap-around))
          (windmove-find-other-window 'up))
        (enlarge-window arg)
      (shrink-window arg)))

  (defun hydra-move-splitter-down (arg)
    "Move window splitter down."
    (interactive "p")
    (if (let ((windmove-wrap-around))
          (windmove-find-other-window 'up))
        (shrink-window arg)
      (enlarge-window arg)))
  ;; move window splitter
  (defhydra hydra-splitter ()
    "splitter"
    ("h" hydra-move-splitter-left)
    ("j" hydra-move-splitter-down)
    ("k" hydra-move-splitter-up)
    ("l" hydra-move-splitter-right))
  (global-set-key (kbd "M-u sp") #'hydra-splitter/body)

  ;;-------------------------------------------------------------
  ;; jump to error
  (defhydra hydra-error ()
    "goto-error"
    ("h" first-error "first")
    ("j" next-error "next")
    ("k" previous-error "prev")
    ("v" recenter-top-bottom "recenter")
    ("q" nil "quit"))
  (global-set-key (kbd "M-u er") #'hydra-error/body)

  ;;-------------------------------------------------------------
  ;; lispyville
  (defhydra hydra-lispyville (:color blue :hint nil)
    ("(" lispyville-wrap-round "wrap round with (" :column "lispyville-wrap")
    ("[" lispyville-wrap-brackets "wrap round with [")
    ("{" lispyville-wrap-braces "wrap round with {")
    ("q" nil "cancel" :exit t :column nil))
  (global-set-key (kbd "M-u li") #'hydra-lispyville/body)

  ;;-------------------------------------------------------------
  ;; dired
  (defhydra hydra-dired (:color blue :hint nil)
    ("ud" dired-undo "Undo in a Dired buffer." :column "dired commands")
    ("ha" dired-hide-all "Hide all subdirectories, leaving only their header lines.")
    ("q" nil "cancel" :exit t :column nil))
  (define-key dired-mode-map (kbd "M-u dd") 'hydra-dired/body)

  ;;-------------------------------------------------------------
  ;; magit
  (defhydra hydra-magit (:color blue :hint nil)
    ("rv" magit-revert "Revert existing commits, with or without creating new commits." :column "magit commands")
    ("q" nil "cancel" :exit t :column nil))
  (define-key magit-mode-map (kbd "M-u gi") 'hydra-magit/body)

  ;;-------------------------------------------------------------
  ;; shell
  (defhydra hydra-shell (:color blue :hint nil)
    ("0" (delete-window) "delete window" :column "shell commands")
    ("q" nil "cancel" :exit t :column nil))
  (define-key shell-mode-map (kbd "M-u sh") 'hydra-shell/body)

  ;;-------------------------------------------------------------
  ;; info
  (defhydra hydra-info (:color blue :hint nil)
    ("ls" elisp-index-search "Look up TOPIC in the indices of the Emacs Lisp Reference Manual." :column "info commands")
    ("es" emacs-index-search "Look up TOPIC in the indices of the Emacs User Manual.")
    ("c" Info-copy-current-node-name "Put the name of the current Info node into the kill ring.")
    ("h" Info-help "Enter the Info tutorial.")
    ("n" Info-next-reference "Move cursor to the next cross-reference or menu item in the node." :color red)
    ("p" Info-prev-reference "Move cursor to the previous cross-reference or menu item in the node." :color red)
    ("l" Info-final-node "Go to the final node in this file.")
    ("q" nil "cancel" :exit t :column nil))
  ;; (global-set-key (kbd "M-u if") #'hydra-info/body)
  (define-key Info-mode-map (kbd "M-u if") #'hydra-info/body)

  ;;-------------------------------------------------------------
  ;; font settings
  (defhydra hydra-font (:color blue :hint nil)
    ;; 增加字体大小
    ("+" (lambda ()
           (interactive)
           (let ((old-face-attribute (face-attribute 'default :height)))
             (set-face-attribute 'default nil :height (+ old-face-attribute 10)))) "increase font 10" :column "fonts commands")
    ;; 减小字体大小
    ("-" (lambda ()
           (interactive)
           (let ((old-face-attribute (face-attribute 'default :height)))
             (set-face-attribute 'default nil :height (- old-face-attribute 10)))) "decrease font 10" :column "fonts commands")
    ("q" nil "cancel" :exit t :column nil))
  ;; (define-key magit-mode-map (kbd "M-u ft") 'hydra-font/body)
  ;; (global-set-key (kbd "M-u ft") #'hydra-M-um/body)
  (evil-define-key 'normal 'global (kbd "M-u ft") #'hydra-font/body)
  )

(provide 'init-hydra)
