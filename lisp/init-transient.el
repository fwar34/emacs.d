;; -*- coding: utf-8; lexical-binding: t; -*-
;; https://www.reddit.com/r/emacs/comments/m518xh/transient_api_example_alternative_bindings_part_1/
(require 'cl-lib)

(use-package transient
  :ensure t
  :config
  )

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
           ("d" "drilldown" pmx-nested-transient)]])

(global-set-key (kbd "M-o") 'pmx-transient-toy)

(use-package transient-posframe
  :ensure t
  :if (display-graphic-p)
  :config
  (transient-posframe-mode))

(provide 'init-transient)
