;; -*- coding: utf-8; lexical-binding: t; -*-

(defun svn-diff ()
  (interactive)
  (switch-to-buffer (get-buffer-create "*svn-diff*"))
  (erase-buffer)
  (evil-local-set-key 'normal (kbd "q") #'evil-buffer)
  (start-process "my-make" "*svn-diff*" "svn" "diff")
  (goto-char (point-min)))

(provide 'init-svn)
