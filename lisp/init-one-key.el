;; -*- coding: utf-8; lexical-binding: t; -*-

(use-package one-key
  :ensure t
  :straight
  (:host github :repo "manateelazycat/one-key")
  :bind
  (("C-c C-o" . one-key-menu-magit))
  :config
      (defun one-key-menu-magit ()
    "The `one-key' menu for magit"
    (interactive)
    (one-key-create-menu
   "MAGIT"
   '(
     (("s" . "Magit status") . magit-status)
     (("c" . "Magit checkout") . magit-checkout)
     (("C" . "Magit commit") . magit-commit)
     (("u" . "Magit push to remote") . magit-push-current-to-pushremote)
     (("p" . "Magit delete remote branch") . magit-delete-remote-branch)
     (("i" . "Magit pull") . magit-pull-from-upstream)
     (("r" . "Magit rebase") . magit-rebase)
     (("e" . "Magit merge") . magit-merge)
     (("l" . "Magit log") . magit-log-all)
     (("L" . "Magit blame") . magit-blame+)
     (("b" . "Magit branch") . magit-branch)
     (("B" . "Magit buffer") . magit-process-buffer)
     (("D" . "Magit discarded") . magit-discard)
     (("," . "Magit init") . magit-init)
     (("." . "Magit add remote") . magit-remote-add)
     )
   t))
  )

(provide 'init-one-key)
