;;; init-transient.el --- Useful preset transient commands  -*- coding:utf-8; lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(define-prefix-command 'M-u-map)
(global-set-key (kbd "M-u") 'M-u-map)

(use-package cargo-transient
  :custom
  (cargo-transient-buffer-name-function 'project-prefixed-buffer-name))

(use-package transient-dwim
  :custom
  (transient-detect-key-conflicts t)
  :bind ("M-=" . transient-dwim-dispatch))

;; {{{
;;----------------------------------------------------------------
;; https://www.reddit.com/r/emacs/comments/m518xh/transient_api_example_alternative_bindings_part_1/
;;----------------------------------------------------------------
(require 'cl-lib)

(use-package transient
  :straight
  (:host github :repo "magit/transient")
  :config
  (message "transient config")
  (transient-bind-q-to-quit)
  (global-set-key (kbd "C-c tt") 'pmx-transient-toy)
  (global-set-key (kbd "C-c ty") 'my-transient-yank)
  ;; (setq transient-display-buffer-action '(display-buffer-below-selected))

  (transient-define-suffix pmx-show-prefix ()
    "Show the prefix that invoked this suffix"
    :description "prefix"
    (interactive)
    (message "Current prefix key: %s" transient-current-prefix))

  (transient-define-suffix pmx-show-command ()
    "Show this command"
    :description "current command"
    (interactive)
    (message "Current command: %s" transient-current-command))

  (transient-define-suffix pmx-show-suffixes ()
    "Show the current suffixes"
    :description "suffixes"
    (interactive)
    (message "Current suffixes: %s" (cl-mapcar
                                     (lambda (obj)
                                       (oref obj description))
                                     transient-current-suffixes)))

  (transient-define-suffix pmx-show-args ()
    "Show current infix args"
    :description "infix args"
    (interactive)
    (message "Current infix args: %s" (transient-args transient-current-command)))

  (transient-define-suffix pmx-send-message ()
    "Send message to minibuffer"
    :description "send message"
    :transient t
    (interactive)
    (message "Message sent at %s. Happy?" (shell-command-to-string "echo -n $(date)")))

  (transient-define-argument pmx-affirmative ()
    "Are we affirmative?"
    :description "affirmative"
    :argument "affirmative")

  (transient-define-argument pmx-yep-nope ()
    "Is it yep or is it nope?"
    :description "yep or nope"
    :class 'transient-option
    :shortarg "-y"
    :argument "--yepnope="
    :choices '("yep" "nope"))

  (transient-define-argument pmx-abc ()
    "Which letters do you like?"
    :description "abc"
    :class 'transient-option
    :shortarg "-a"
    :argument "--abc="
    :choices '("A" "B" "C"))

  (defvar pmx--variable "A string" "A variable brought to you by pmx")

  (transient-define-argument pmx-set-lisp-variable ()
    "Set a lisp variable, pmx--variable.  Won't show up in infix arguments."
    :description "set pmx--variable"
    :class 'transient-lisp-variable
    :shortarg "-l"
    :variable 'pmx--variable
    :argument "--letters=")

  (transient-define-suffix pmx-show-lisp-variable ()
    "Access pmx--variable"
    :description "show pmx--variable"
    (interactive)
    (message "Current value of pmx--variable: %s" pmx--variable))

  (transient-define-suffix pmx-dynamic-suffix ()
    "Description depends on pmx--variable"
    :if-not '(lambda () (string-equal pmx--variable "abc"))
    :description '(lambda () (format "pmx %s" pmx--variable))
    (interactive)
    (message "Current value of pmx--variable: %s" pmx--variable))

  (transient-define-prefix pmx-nested-transient ()
    "Some subcommands, like tree menus from the land of mice"
    ["Switches"
     ("-s" "another switch" ("-x" "--conflicting"))]
    ["Sub Command Introspection"
     ("i" pmx-show-args)
     ("p" pmx-show-prefix)
     ("s" pmx-show-suffixes)
     ("c" pmx-show-command)]
    ["Dynamic Commands"
     ("d" pmx-dynamic-suffix)])

  (transient-define-prefix pmx-transient-toy ()
    "Figure out how to use transient's API properly"
    [:class transient-columns
            ["Things"
             ("-w" "switch"  ("-w" "--switch"))]
            ["Others"
             ("i" pmx-show-args)
             ("p" pmx-show-prefix)
             ("s" pmx-show-suffixes)
             ("c" pmx-show-command)
             ("m" pmx-send-message)]
            ["More"
             ("f" pmx-affirmative)
             ("y" pmx-yep-nope)
             ("a" pmx-abc)
             ("l" pmx-set-lisp-variable)
             ("w" pmx-show-lisp-variable)]
            ["Drilldown"
             ("d" "drilldown" pmx-nested-transient)]
            ["Quit"
             ("q" "quit" keyboard-quit)]])


  (use-package transient-posframe
    :disabled
    :ensure t
    :if (display-graphic-p)
    :config
    (transient-posframe-mode)))
;; }}}

;; {{{
;;----------------------------------------------------------------
;; https://magit.vc/manual/transient.html
;; https://gist.github.com/abrochard/dd610fc4673593b7cbce7a0176d897de
;; ‘define-transient-command’ is an obsolete alias (as of Transient 0.3.0); use ‘transient-define-prefix’ instead.
;;----------------------------------------------------------------
;; A simple transient
(defun test-function-1 ()
  (interactive)
  (message "Test function"))
(define-transient-command test-transient-1 ()
  "Test Transient Title"
  ["Actions"
   ("a" "Action a" test-function-1)
   ("s" "Action s" test-function-1)
   ("d" "Action d" test-function-1)])
;; (test-transient-1)
;;----------------------------------------------------------------
;; Transient with switches
;; We can easily define command line switches in our transient interface.
(defun test-function-2 (&optional args)
  (interactive
   (list (transient-args 'test-transient-2)))
  (message "args: %s" args))
(define-transient-command test-transient-2 ()
  "Test Transient Title"
  ["Arguments"
   ("-s" "Switch" "--switch")
   ("-a" "Another switch" "--another")]
  ["Actions"
   ("d" "Action d" test-function-2)])
;; (test-transient-2)
;;----------------------------------------------------------------
;; Transient with params
;; A bit more complex than simple switches, params let users enter a value.
(defun test-function-3 (&optional args)
  "Test function, ARGS."
  (interactive
   (list (transient-args 'test-transient-3)))
  (message "args %s" args))

(define-infix-argument test-transient-3:--message ()
  :description "Message"
  :class 'transient-option
  :shortarg "-m"
  :argument "--message=")

(define-transient-command test-transient-3 ()
  "Test Transient Title"
  ["Arguments"
   ("-s" "Switch" "--switch")
   ("-a" "Another switch" "--another")
   (test-transient-3:--message)]
  ["Actions"
   ("d" "Action d" test-function-3)])
;; (test-transient-3)
;; After some feedback, I wanted to share that it is simpler and better
;; here to not define the infix argument separately. Instead, the transient
;; could be defined this way and have the same effect
(defun test-function-4 (&optional args)
  (interactive
   (list (transient-args 'test-transient-4)))
  (message "args %s" args))

(define-transient-command test-transient-4 ()
  "Test Transient Title"
  ["Arguments"
   ("-s" "Switch" "--switch")
   ("-a" "Another switch" "--another")
   ("-m" "Message" "--message=")] ;;simpler
  ["Actions"
   ("d" "Action d" test-function-4)])
;; (test-transient-4)
;; }}}

;; {{{ my transient commands
(global-set-key (kbd "M-u ll") 'my-misc-transinet)

(transient-define-prefix my-hl-todo ()
  "my hl-todo commands"
  [[" <helpful commands>"
    ("p" "hl-todo-previous" hl-todo-previous)
    ("n" "hl-todo-next" hl-todo-next)
    ("o" "hl-todo-occur" hl-todo-occur)
    ("i" "hl-todo-insert" hl-todo-insert)]])

(transient-define-prefix my-misc-transinet ()
  "my misc commands"
  :transient-non-suffix 'transient--do-stay
  [[" <my misc commands>"
    ("h" "hl-todo" my-hl-todo)
    ("p" "clipboard-yank" clipboard-yank)
    ("s" "shortdoc-display-group" shortdoc-display-group)
    ("e" "hydra etags hydra" hydra-counsel-etags/body)
    ("t" "straight commands" my-straight-transient)
    ("c" "cargo commands" cargo-transient)
    ("w" "wgrep-change-to-wgrep-mode" wgrep-change-to-wgrep-mode)
    ("i" "change input method" hydra-input-method/body)
    ("k" "open my task.org" my-task-file-open)
    ("l" "electric-pair-mode" electric-pair-mode)]
   [" <page scroll>"
    ("d" "scroll page down" evil-scroll-page-down :transient t)
    ("u" "scroll page up" evil-scroll-page-up :transient t)]])

;; }}}

(defhydra hydra-straight-helper (:hint nil)
  "
_c_heck all       |_f_etch all     |_m_erge all      |_n_ormalize all   |p_u_sh all
_C_heck package   |_F_etch package |_M_erge package  |_N_ormlize package|p_U_sh package
----------------^^+--------------^^+---------------^^+----------------^^+------------||_q_uit||
_r_ebuild all     |_p_ull all      |_v_ersions freeze|_w_atcher start   |_g_et recipe
_R_ebuild package |_P_ull package  |_V_ersions thaw  |^ ^               |prun_e_ build"
  ("c" straight-check-all)
  ("C" straight-check-package)
  ("r" straight-rebuild-all)
  ("R" straight-rebuild-package)
  ("f" straight-fetch-all)
  ("F" straight-fetch-package)
  ("p" straight-pull-all)
  ("P" straight-pull-package)
  ("m" straight-merge-all)
  ("M" straight-merge-package)
  ("n" straight-normalize-all)
  ("N" straight-normalize-package)
  ("u" straight-push-all)
  ("U" straight-push-package)
  ("v" straight-freeze-versions)
  ("V" straight-thaw-versions)
  ("w" straight-watcher-start)
  ("g" straight-get-recipe)
  ("e" straight-prune-build)
  ("q" nil))

(transient-define-prefix my-straight-transient ()
  " <straight commands>"
  ["                                                          <straight commands>"
   [("c" "check all" straight-check-all)
    ("C" "straight-check-package" straight-check-package)
    ("r" "straight-rebuild-all" straight-rebuild-all)
    ("R" "straight-rebuild-package" straight-rebuild-package)]
   [("f" "straight-fetch-all" straight-fetch-all)
    ("F" "straight-fetch-package" straight-fetch-package)
    ("p" "straight-pull-all" straight-pull-all)
    ("P" "straight-pull-package" straight-pull-package)]
   [("m" "straight-merge-all" straight-merge-all)
    ("M" "straight-merge-package" straight-merge-package)
    ("n" "straight-normalize-all" straight-normalize-all)
    ("N" "straight-normalize-package" straight-normalize-package)]
   [("u" "straight-push-all" straight-push-all)
    ("U" "straight-push-package" straight-push-package)
    ("v" "straight-freeze-versions" straight-freeze-versions)
    ("V" "straight-thaw-versions" straight-thaw-versions)]
   [("w" "straight-watcher-start" straight-watcher-start)
    ("W" "straight-watcher-stop" straight-watcher-stop)
    ("g" "straight-get-recipe" straight-get-recipe)
    ("e" "straight-prune-build" straight-prune-build)]])

(provide 'init-transient)
;;; init-transient.el ends here
