;; -*- coding: utf-8; lexical-binding: t; -*-
;;-------------------------------------------------------------
;; init-modeline
;; https://blog.csdn.net/xh_acmagic/article/details/78939246
;;-------------------------------------------------------------
(require 'modeline-face)

;;-------------------------------------------------------------
;; reference from lispyville : integrating with lispy in modeline
;;-------------------------------------------------------------
(defvar lispyville-insert-states '(insert emacs hybrid iedit-insert)
  "Insertion states that lispy special can be used from.")

;; * Mode Line Integration
(defun lispyville--special-p ()
  "Return whether the point is in special."
  (or (region-active-p)
      (and (not (lispy--in-string-or-comment-p))
           (or (lispy-left-p)
               (lispy-right-p)
               (and (lispy-bolp)
                    (or (looking-at lispy-outline-header)
                        (looking-at lispy-outline)))))))

(defun lispyville--lispy-keybindings-active-p ()
  "Return whether lispy keybindings are active."
  (and lispy-mode
       (memq evil-state lispyville-insert-states)
       (lispyville--special-p)))

(cl-defun lispyville-mode-line-string (&optional (special-text "🍰-special ")
                                              default-text)
  "When added to the mode line, show SPECIAL-TEXT when in special.
When not in special (or not in a state in `lispyville-insert-states'), show
DEFAULT-TEXT."
  `(:eval
    (if (lispyville--lispy-keybindings-active-p)
        (propertize ,special-text 'face 'lispyville-special-face)
      ,default-text)))
;; ------------------------------------------------------------------------

;; (defun fwar34/mode-line-process ()
;;   '(:eval
;;     (when mode-line-process
;;       (propertize (format "(%s)" mode-line-process) 'face 'font-lock-string-face)
;;       )) 
;;   )

(defun fwar34/evil-state ()
  "Display evil state in differente color"
  '(:eval
    (when (bound-and-true-p evil-local-mode)
      (cond
       ;; ((equal 'normal evil-state) (propertize evil-mode-line-tag 'face 'font-lock-evil-normal-face))
       ;; ((equal 'insert evil-state) (propertize evil-mode-line-tag 'face 'font-lock-evil-insert-face))
       ;; ((equal 'replace evil-state) (propertize evil-mode-line-tag 'face 'font-lock-evil-insert-face))
       ;; ((equal 'visual evil-state) (propertize evil-mode-line-tag 'face 'font-lock-evil-visual-face))
       ;; ((equal 'operator evil-state) (propertize evil-mode-line-tag 'face 'font-lock-evil-visual-face))
       ;; ((equal 'emacs evil-state) (propertize evil-mode-line-tag 'face 'font-lock-evil-emacs-face))
       ((equal 'normal evil-state) (propertize evil-mode-line-tag 'face 'spaceline-evil-normal))
       ((equal 'insert evil-state) (propertize evil-mode-line-tag 'face 'spaceline-evil-insert))
       ((equal 'emacs evil-state) (propertize evil-mode-line-tag 'face 'spaceline-evil-emacs))
       ((equal 'replace evil-state) (propertize evil-mode-line-tag 'face 'spaceline-evil-replace))
       ((equal 'visual evil-state) (propertize evil-mode-line-tag 'face 'spaceline-evil-visual))
       ((equal 'motion evil-state) (propertize evil-mode-line-tag 'face 'spaceline-evil-motion))
       ((equal 'operator evil-state) (propertize evil-mode-line-tag 'face 'spaceline-modified))
       (t nil)))))

(defun fwar34/lispy-state ()
  "Display lispy mode in modeline"
  '(:eval
    (let ((enable-mode '(emacs-lisp-mode lisp-interaction-mode)))
      (and (member major-mode enable-mode) (not (bound-and-true-p lispy-mode))
           (propertize " PASTE(lisp)" 'face 'font-lock-evil-emacs-face)))))

;; reference from spaceline
(setq window-number
      ;; "The current window number.
      ;; Requires either `winum-mode' or `window-numbering-mode' to be enabled."
      '(:eval (let* ((num (cond
                           ((bound-and-true-p winum-mode)
                            (winum-get-number))
                           ((bound-and-true-p window-numbering-mode)
                            (window-numbering-get-number))
                           (t nil)))
                     (str (when num (int-to-string num))))
                (when num (propertize str 'face 'font-lock-variable-name-face)))))

(defun spaceline--column-number-at-pos (pos)
  "Column number at POS.  Analog to `line-number-at-pos'."
  (save-excursion (goto-char pos) (current-column)))

(setq my-selection-info 
      '(:eval (when (or mark-active
            (and (bound-and-true-p evil-local-mode)
                 (eq 'visual evil-state)))
    (let* ((lines (count-lines (region-beginning) (min (1+ (region-end)) (point-max))))
           (chars (- (1+ (region-end)) (region-beginning)))
           (cols (1+ (abs (- (spaceline--column-number-at-pos (region-end))
                             (spaceline--column-number-at-pos (region-beginning))))))
           (evil (and (bound-and-true-p evil-state) (eq 'visual evil-state)))
           (rect (or (bound-and-true-p rectangle-mark-mode)
                     (and evil (eq 'block evil-visual-selection))))
           (multi-line (or (> lines 1) (and evil (eq 'line evil-visual-selection)))))
      (cond
       (rect (propertize (format "%d×%d block" lines (if evil cols (1- cols)))
                                  'face 'font-lock-evil-visual-face))
       (multi-line (propertize (format "%d lines" lines) 'face 'font-lock-evil-visual-face))
       (t (propertize (format "%d chars" (if evil chars (1- chars))) 'face 'font-lock-evil-visual-face)))))))

(setq my-flycheck-mode-line
      '(:eval
        (when
            (and (bound-and-true-p flycheck-mode)
                 (or flycheck-current-errors
                     (eq 'running flycheck-last-status-change)))
          (pcase flycheck-last-status-change
            ((\` not-checked) nil)
            ((\` no-checker) (propertize " -" 'face 'warning))
            ((\` running) (propertize " ✷" 'face 'success))
            ((\` errored) (propertize " !" 'face 'error))
            ((\` finished)
             (let* ((error-counts (flycheck-count-errors flycheck-current-errors))
                    (no-errors (cdr (assq 'error error-counts)))
                    (no-warnings (cdr (assq 'warning error-counts)))
                    (face (cond (no-errors 'error)
                                (no-warnings 'warning)
                                (t 'success))))
               (propertize (format "[%s/%s]" (or no-errors 0) (or no-warnings 0))
                           'face face)))
            ((\` interrupted) " -")
            ((\` suspicious) '(propertize " ?" 'face 'warning))))))

(setq my-modeline-time
      '(:eval
        (concat "["
                ;; (propertize (format-time-string "[%H:%M]") 'face 'font-lock-constant-face)
                (propertize (format-time-string "%H:%M") 'face 'font-lock-constant-face) 
                "]")))

(defun zilongshanren/display-mode-indent-width ()
  (let ((mode-indent-level
         (catch 'break
           (dolist (test spacemacs--indent-variable-alist)
             (let ((mode (car test))
                   (val (cdr test)))
               (when (or (and (symbolp mode) (derived-mode-p mode))
                         (and (listp mode) (apply 'derived-mode-p mode))
                         (eq 't mode))
                 (when (not (listp val))
                   (setq val (list val)))
                 (dolist (v val)
                   (cond
                    ((integerp v) (throw 'break v))
                    ((and (symbolp v) (boundp v))
                     (throw 'break (symbol-value v))))))))
           (throw 'break (default-value 'evil-shift-width)))))
    (concat "TS:" (int-to-string (or mode-indent-level 0)))))

(defun zilong/modeline--evil-substitute ()
  "Show number of matches for evil-ex substitutions and highlights in real time."
  '(:eval (when (and (bound-and-true-p evil-local-mode)
             (or (assq 'evil-ex-substitute evil-ex-active-highlights-alist)
                 (assq 'evil-ex-global-match evil-ex-active-highlights-alist)
                 (assq 'evil-ex-buffer-match evil-ex-active-highlights-alist)))
  (propertize
   (let ((range (if evil-ex-range
                    (cons (car evil-ex-range) (cadr evil-ex-range))
                  (cons (line-beginning-position) (line-end-position))))
         (pattern (car-safe (evil-delimited-arguments evil-ex-argument 2))))
     (if pattern
         (format " %s matches " (how-many pattern (car range) (cdr range)))
       " - "))
   'face 'font-lock-preprocessor-face)))
  )

(defun mode-line-fill (face reserve)
  "Return empty space using FACE and leaving RESERVE space on the right."
  (unless reserve (setq reserve 20))
  (when (and window-system (eq 'right (get-scroll-bar-mode)))
    (setq reserve (- reserve 3)))
  (propertize " "
              'display `((space :align-to
                                (- (+ right right-fringe right-margin) ,reserve)))
              'face face))


(setq projectile-mode-line
      (quote (:eval (when (and (bound-and-true-p projectile-mode) (projectile-project-p)) 
                      (propertize (format " P[%s]" (projectile-project-name))
                                  'face 'font-lock-variable-name-face)))))

(setq buffer-name-mode-line
      (quote (:eval (propertize "%b " 'face 'font-lock-string-face))))

;; (setq major-mode-mode-line
;;       (quote (:eval (propertize "%m " 'face 'font-lock-keyword-face))))

(setq my-modeline-major-mode 
  ;; major modes
  (list
    '(:eval (propertize "%m" 'face 'font-lock-string-face
                       'help-echo buffer-file-coding-system))
    '("" mode-line-process)))

(setq file-status-mode-line 
  (quote (:eval (concat "["
            (when (buffer-modified-p)
              (propertize "Mod "
                      'face 'font-lock-warning-face
                      'help-echo "Buffer has been modified"))
            (propertize (if overwrite-mode "Ovr" "Ins")
                    'face 'font-lock-preprocessor-face
                    'help-echo (concat "Buffer is in "
                           (if overwrite-mode
                               "overwrite"
                             "insert") " mode"))
            (when buffer-read-only
              (propertize " RO"
                      'face 'font-lock-type-face
                      'help-echo "Buffer is read-only"))
            "]"))))

(setq my-persp-mode-line
      '(:eval (propertize (mapconcat #'substring-no-properties (persp-mode-line) "") 'face 'font-lock-preprocessor-face)))

(setq flycheck-status-mode-line
  (quote (:eval (pcase flycheck-last-status-change
          (`finished
           (let* ((error-counts (flycheck-count-errors flycheck-current-errors))
              (errors (cdr (assq 'error error-counts)))
              (warnings (cdr (assq 'warning error-counts)))
              (face (cond (errors 'error)
                      (warnings 'warn)
                      (t 'success))))
             (propertize (concat "["
                     (cond
                      (errors (format "✗:%s" errors))
                      (warnings (format "❗:%s" warnings))
                      (t "✔"))
                     "]")
                         'face face)))))))

(setq line-column-mode-line
  (concat "("
   (propertize "%02l" 'face 'font-lock-type-face)
   ":"
   (propertize "%02c" 'face 'font-lock-type-face)
   ")"))

(defun encoding-string ()
  (concat (pcase (coding-system-eol-type buffer-file-coding-system)
            (0 "LF ")
            (1 "CRLF ")
            (2 "CR "))
          (let ((sys (coding-system-plist buffer-file-coding-system)))
            (if (member (plist-get sys :category) '(coding-category-undecided coding-category-utf-8))
                "UTF-8"
              (upcase (symbol-name (plist-get sys :name)))))
          " "))

(setq encoding-mode-line
      '(:eval (propertize 
               (concat (pcase (coding-system-eol-type buffer-file-coding-system)
                         (0 "LF ")
                         (1 "CRLF ")
                         (2 "CR "))
                       (let ((sys (coding-system-plist buffer-file-coding-system)))
                         (if (member (plist-get sys :category) '(coding-category-undecided coding-category-utf-8))
                             "UTF-8"
                           (upcase (symbol-name (plist-get sys :name)))))
                       ))))

(setq time-mode-line (quote (:eval (propertize (format-time-string "%H:%M")))))

(setq-default mode-line-format
      (list
       ;; " %1"
       "["
       window-number
       "]"
       " "
       my-persp-mode-line
       " %1"
       buffer-name-mode-line
       ;; "%1 "
       ;; ;; the buffer name; the file name as a tool tip
       ;; '(:eval (propertize "%b " 'face 'font-lock-keyword-face
       ;;                     'help-echo (buffer-file-name)))
       ;; relative position, size of file
       "["
       (propertize "%p" 'face 'font-lock-constant-face) ;; % above top
       "/"
       (propertize "%I" 'face 'font-lock-constant-face) ;; size
       "]"
       " %1"
       line-column-mode-line
       ;; (fwar34/mode-line-process)
       ;; mode-line-process
       ;; evil state
       " "
       (fwar34/evil-state)
       '(:eval (when (featurep 'lispyville)
                 (lispyville-mode-line-string " @lispy-special@")))
       (fwar34/lispy-state)
       " "
       ;; git info
       '(:eval (when (> (window-width) 90)
                 `(vc-mode vc-mode)
                 ;; reference from spaceline
                 (string-trim (concat vc-mode
                                 (when (buffer-file-name)
                                   (pcase (vc-state (buffer-file-name))
                                     (`up-to-date " ")
                                     (`edited "@Mod")
                                     (`added "@Add")
                                     (`unregistered "@??")
                                     (`removed "@Del")
                                     (`needs-merge "@Con")
                                     (`needs-update "@Upd")
                                     (`ignored "@Ign")
                                     (_ "@Unk")))))))

       ;; " %1"
       " "
       ;major-mode-mode-line
       my-modeline-major-mode 
       ;; '(:eval (when (> (window-width) 90)
       ;;           minor-mode-alist))
       " %1"
       file-status-mode-line
       ;; " "
       ;; '(:eval (zilongshanren/display-mode-indent-width))
       projectile-mode-line
       " "
       my-selection-info
       " "
       (zilong/modeline--evil-substitute)
       ;; " "
       ;; flycheck-status-mode-line
       "%1 "
       my-flycheck-mode-line
       '(:eval (mode-line-fill 'mode-line (+ 7 (string-width (encoding-string)))))
       encoding-mode-line
       " "
       ;; "["
       ;; time-mode-line
       ;; (propertize (format-time-string "[%H:%M]") 'face 'font-lock-constant-face) ;; size
       ;; (propertize (format-time-string "%H:%M") 'face 'font-lock-constant-face) ;; size
       ;; "]"
       my-modeline-time
       ;; mode-line-end-spaces
       ))

;; (set-face-background 'modeline "#4466aa")
;; (set-face-background 'modeline-inactive "#99aaff")
;; (set-face-background 'fringe "#809088")
(unless (display-graphic-p)
  (set-face-background 'mode-line "black")
  (set-face-background 'mode-line-inactive "black"))

;; Here 's how I get a box around the active mode-line :
;; (custom-set-faces '(mode-line ((t (:box (:line-width 2 :color "red"))))))

(provide 'init-my-modeline)
