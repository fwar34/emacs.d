;;; init-terminal.el --- Useful preset transient commands  -*- coding:utf-8; lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(use-package multi-term
  :disabled
  ; :after evil
  :config
  (cond
   ((equal system-type 'windows-nt) (setq multi-term-program "eshell"))
   ((equal system-type 'gnu/linux) (setq multi-term-program "/usr/bin/zsh")))

  ;; (define-key term-mode-map "\e\C-l" 'evil-buffer)
  ;; (define-key term-raw-map ";bb" 'evil-buffer)

  ;; (cl-dolist (element term-bind-key-alist)
  ;;   (setq bind-key (car element))
  ;;   (setq bind-command (cdr element))
  ;;   (cond
  ;;    ((stringp bind-key) (setq bind-key (read-kbd-macro bind-key)))
  ;;    ((vectorp bind-key) nil)
  ;;    (t (signal 'wrong-type-argument (list 'array bind-key))))
  ;;   (define-key term-raw-map bind-key bind-command))

  ;; https://www.jianshu.com/p/2c1ac913d2cb
  ;; 如果想保留自己在其他mode下的快捷键，将快捷键添加到 term-bind-key-alist这个列表中
  ; (add-to-list 'term-bind-key-alist '("M-l" . evil-buffer))
  ;; (add-to-list 'term-bind-key-alist '("M-y" . term-paste))
  (add-to-list 'term-bind-key-alist '("C-x C-x" . (lambda () (interactive) (term-send-raw-string "\C-x"))))
  ;; 修改快捷键的map,如果你发你定义自己的快捷键与该major-mode的冲突，可以直接修改它的key-map
  ;; (define-key term-mode-map (kbd "C-=") 'evil-buffer)
  ;; (define-key term-raw-map (kbd "C-=") 'evil-buffer)
  ; (define-key term-raw-map ";tm" 'vterm-toggle)
  ; (define-key term-raw-map ";;" (lambda () (term-send-raw-string ";")))
  ;; (define-key term-raw-map ";g" 'evil-normal-state)
  
  (defun my-multi-term ()
    (interactive)
    (if (equal 'windows-nt system-type)
        (aweshell-toggle)
      (if (string-match "*terminal<[0-9]\\{1,2\\}>*" (buffer-name))
          ; (evil-buffer nil)
        (let ((index 1)
              (term-buffer))
          (catch 'break
            (while (<= index 10)
              (setq target-buffer (format "*%s<%s>*" multi-term-buffer-name index))
              (when (buffer-live-p (get-buffer target-buffer))
                (setq term-buffer target-buffer)
                (throw 'break nil))
              (setq index (1+ index))))
          (if term-buffer
              (switch-to-buffer term-buffer)
            (multi-term))
          )))
    )

  ;; (with-parsed-tramp-file-name default-directory path
  ;;       (let ((method (cadr (assoc `tramp-login-program (assoc path-method tramp-methods)))))
  ;;         (message (concat method " " (when path-user (concat path-user "@")) path-host "\C-m"))
  ;;         (message (concat "cd " path-localname "\C-m"))))

  ;; (with-parsed-tramp-file-name default-directory path
  ;;   (message path-user)
  ;;   )

  )

(use-package powershell
  :disabled
  :if (equal system-type 'windows-nt))

(use-package vterm
  :unless (equal system-type 'windows-nt)
  :commands vterm
  :init
  
  )

(use-package vterm-toggle
  :preface
  (declare-function vterm-send-string "vterm")
  :unless (equal system-type 'windows-nt)
  :bind*
  ([f5] . vterm-toggle)
  ([f12] . vterm-toggle-cd)
  (:map vterm-mode-map
	("," . my-vterm-mode-transient)
	([f5] . vterm-toggle)
	([f12] . vterm-toggle-cd))
  :config
;; (define-prefix-command 'u-map)
;; (global-set-key (kbd ";t") 'u-map)
  (define-key vterm-mode-map (kbd "C-j") 'vterm-toggle-insert-cd)
  ;; (define-key vterm-mode-map (kbd ";tm") 'vterm-toggle)
  ;; (add-hook 'vterm-mode-hook #'(lambda () (message "enter veterm hook")
  ;;                                (local-set-key (kbd ";tm") #'vterm-toggle)
  ;;                                ))
  ;; (general-define-key
  ;;  :keymaps 'vterm-mode-map
  ;;  :prefix ";"
  ;;  ";" 'self-insert-command
  ;;  "tm" 'vterm-toggle)
  ;; https://github.com/akermu/emacs-libvterm#keybindings
  ;; 使用 C-q 在 vtem 中发送下个字符到 terminal, 比如 ";"
  ; (evil-define-key 'insert vterm-mode-map (kbd "C-q") 'vterm-send-next-key)

  (transient-define-prefix my-vterm-transient ()
    " <vterm mode commans>"
    [" <vterm commands>\n---------------------------------------"
     [" <Toggle>"
      ("f" "vterm-toggle-forward" vterm-toggle-forward)
      ("b" "vterm-toggle-backward" vterm-toggle-backward)]])

  (defun my-vterm-insert ()
    "My vterm insert \",\"."
    (interactive)
    (vterm-send-string ","))

  (transient-define-prefix my-vterm-mode-transient ()
    " <vterm misc commands>"
    [" <vterm commands>\n--------------------------------------------------------------"
     [" <Yank>"
      ("p" "vterm-yank" vterm-yank) ;;
      ("P" "vterm-yank-pop" vterm-yank-pop)] ;; M-y
     [" <Misc>"
      ("n" "vterm-send-next-key" vterm-send-next-key)
      ("," "insert ," my-vterm-insert)
      ("x" "send C-x" (lambda () (interactive) (term-send-raw-string "\C-x")))]
     [" <Toggle>"
      ("f" "vterm-toggle-forward" vterm-toggle-forward)
      ("b" "vterm-toggle-backward" vterm-toggle-backward)]])

  ;; Switch to next vterm buffer
  (define-key vterm-mode-map (kbd "s-n") 'vterm-toggle-forward)
  ;; Switch to previous vterm buffer
  (define-key vterm-mode-map (kbd "s-p") 'vterm-toggle-backward))

(provide 'init-terminal)
;;; init-terminal.el ends here
