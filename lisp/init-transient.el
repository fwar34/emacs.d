;; -*- coding: utf-8; lexical-binding: t; -*-
;; {{{
;;----------------------------------------------------------------
;; https://www.reddit.com/r/emacs/comments/m518xh/transient_api_example_alternative_bindings_part_1/
;;----------------------------------------------------------------
(require 'cl-lib)

(use-package transient
  :ensure t
  :straight
  (:host github :repo "magit/transient")
  :init
  (define-prefix-command 'M-i-map)
  (global-set-key (kbd "M-i") 'M-i-map)
  :config
  (transient-bind-q-to-quit)
  (global-set-key (kbd "M-i mt") 'pmx-transient-toy)
  (global-set-key (kbd "M-i tt") 'my-transient-yank)
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
  [["Commands"
    ("p" "hl-todo-previous" hl-todo-previous)
    ("n" "hl-todo-next" hl-todo-next)
    ("o" "hl-todo-occur" hl-todo-occur)
    ("i" "hl-todo-insert" hl-todo-insert)]
   ])

(transient-define-prefix my-misc-transinet ()
  "my misc commands"
  [[" <my misc commands>"
    ("h" "hl-todo" my-hl-todo)
    ("p" "clipboard-yank" clipboard-yank)
    ("s" "shortdoc-display-group" shortdoc-display-group)
    ("e" "hydra etags hydra" hydra-counsel-etags/body)
    ("t" "straight commands" my-straight-transient)]])
;; }}}

(provide 'init-transient)
