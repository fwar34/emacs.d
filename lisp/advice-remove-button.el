;;; advice-remove-button.el --- Useful preset transient commands  -*- coding:utf-8; lexical-binding: t; -*-
;;; Commentary:

;;; Code:
;; https://gist.github.com/twlz0ne/f93debe098e0e39ebce5476b62c8ebbb
(defun function-advices (function)
  "Return FUNCTION's advices.

Last-Updated 2022-06-29 00:01:07 +8000"
  (let ((flist (indirect-function function)) advices)
    (when (and (consp flist)
               (or (eq 'macro (car flist))
                   (and (autoloadp flist) (memq (nth 4 flist) '(macro t)))))
      (setq flist (cdr flist)))
    (while (advice--p flist)
      (setq advices `(,@advices ,(advice--car flist)))
      (setq flist (advice--cdr flist)))
    advices))

(define-advice describe-function-1 (:after (function) advice-remove-button)
  "Add a button to remove advice.

Based on @xuchunyang's work in https://emacs-china.org/t/advice/7566
Last-Updated 2022-06-29 00:01:07 +8000"
  (when (get-buffer "*Help*")
    (with-current-buffer "*Help*"
      (save-excursion
        (goto-char (point-min))
        (let ((ad-list (function-advices function)))
          (while (re-search-forward "^\\(?:This \\(?:function\\|macro\\) has \\)?:[-a-z]+ advice: \\(.+\\)\\.?$" nil t)
            (let* ((name (string-trim (match-string 1) "[‘'`]" "[’']"))
                   (symbol (intern-soft name))
                   (advice (or symbol (car ad-list))))
              (when advice
                (when symbol
                  (cl-assert (eq symbol (car ad-list))))
                (let ((inhibit-read-only t))
                  (insert " » ")
                  (insert-text-button
                   "Remove"
                   'cursor-sensor-functions `((lambda (&rest _) (message "%s" ',advice)))
                   'help-echo (format "%s" advice)
                   'action
                   ;; In case lexical-binding is off
                   `(lambda (_)
                      (when (yes-or-no-p (format "Remove %s ? " ',advice))
                        (message "Removing %s of advice from %s" ',function ',advice)
                        (advice-remove ',function ',advice)
                        (revert-buffer nil t)))
                   'follow-link t))))
            (setq ad-list (cdr ad-list))))))))

(provide 'advice-remove-button)
;;; advice-remove-button.el ends here
