;;; init-my-modeline.el --- Useful preset transient commands  -*- coding:utf-8; lexical-binding: t; -*-
;;; Commentary:

;;; Code:
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
  " whether lispy keybindings are active."
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

(defun fwar34/wgrep-state ()
  "Change modeline background color"
  '(:eval
    (if (and (not buffer-read-only) (or (equal major-mode 'ivy-occur-grep-mode) (equal major-mode 'rg-mode) (equal major-mode 'wdired-mode)))
        (progn
          ;; (message "IF................")
          (set-face-background 'mode-line "orange")
          ;; (set-face-background 'mode-line-inactive "orange")
          )
      (unless (bound-and-true-p god-local-mode)
        ;; (message "ELSE................")
        (set-face-background 'mode-line (if (display-graphic-p) "gray26" "black"))))))

(defun fwar34/meow-state ()
  "Display evil state in differente color"
  '(:eval
      (cond
       ((string-equal " NORMAL " (meow-indicator)) (propertize (meow-indicator) 'face 'fwar34/spaceline-evil-normal))
       ((string-equal " INSERT " (meow-indicator)) (propertize (meow-indicator) 'face 'fwar34/spaceline-evil-insert))
       ((string-equal " KEYPAD " (meow-indicator)) (propertize (meow-indicator) 'face 'fwar34/spaceline-evil-replace))
       ((string-equal " MOTION " (meow-indicator)) (propertize (meow-indicator) 'face 'fwar34/spaceline-evil-motion))
       ((string-equal " BEACON " (meow-indicator)) (propertize (meow-indicator) 'face 'fwar34/spaceline-modified))
       (t nil))))

(defun fwar34/lispy-state ()
  "Display lispy mode in modeline."
  '(:eval
    (let ((enable-mode '(emacs-lisp-mode lisp-interaction-mode)))
      (and (member major-mode enable-mode) (not (bound-and-true-p lispyville-mode))
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

(setq my-modeline-time
      '(:eval
        (concat "["
                ;; (propertize (format-time-string "[%H:%M]") 'face 'font-lock-constant-face)
                (propertize (format-time-string "%H:%M") 'face 'font-lock-constant-face 'help-echo (emacs-uptime "%D, %z%2h:%.2m")) 
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

;; (defun mode-line-fill (face reserve)
;;   "Return empty space using FACE and leaving RESERVE space on the right."
;;   (unless reserve (setq reserve 20))
;;   (when (and (display-graphic-p) (eq 'right (get-scroll-bar-mode)))
;;     (setq reserve (- reserve 3)))
;;   (if (and (equal system-type 'windows-nt) (string-equal (upcase system-name) "FL-NOTEBOOK"))
;;       (propertize " " 'display `((space :align-to
;;                                         (- (+ right right-fringe right-margin) ,reserve 1.4)))
;;                   'face face)
;;     (propertize " " 'display `((space :align-to
;;                                       (- (+ right right-fringe right-margin) ,reserve)))
;;                 'face face)
;;     )
;;   )

(defun mode-line-fill (reserve)
  "Return empty space using FACE and leaving RESERVE space on the right."
  (unless reserve (setq reserve 20))
  (when (and (display-graphic-p) (eq 'right (get-scroll-bar-mode)))
    (setq reserve (- reserve 3)))
  (if (and (equal system-type 'windows-nt) (string-equal (upcase system-name) "FL-NOTEBOOK"))
      (propertize " " 'display `((space :align-to
                                        (- (+ right right-fringe right-margin) ,reserve 1.4))))
    (propertize " " 'display `((space :align-to
                                      (- (+ right right-fringe right-margin) ,reserve))))))

;; (setq projectile-mode-line
;;       (quote (:eval (when (and (bound-and-true-p projectile-mode) (projectile-project-p)) 
;;                       (propertize (format " P[%s]" (projectile-project-name))
;;                                   'face 'font-lock-variable-name-face)))))

(setq buffer-name-mode-line
      (quote (:eval (propertize "%b " 'face 'font-lock-string-face 'help-echo (buffer-file-name)))))

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

(defun my-persp-mode-line-string ()
  (let ((index 0)
        (names (persp-names))
        (current-name (persp-current-name))
        (max (length (persp-names))))
    (catch 'exit
      (dolist (elt names)
        (when (string-equal elt current-name)
          (throw 'exit nil))
        (setq index (1+ index))
        ))
    (1+ index))
  )

(setq my-persp-mode-line
      ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Face-Attributes.html
      ;; '(:eval (propertize (mapconcat 'substring-no-properties (persp-mode-line) "") 'face '(:family "Monaco" font-lock-preprocessor-face))))
      ;; '(:eval (propertize (mapconcat 'substring-no-properties (persp-mode-line) "") 'face '(:box (:color "orange" :style pressed-button) font-lock-preprocessor-face))))
      '(:eval
        (when persp-mode
          (if (display-graphic-p)
              '(:eval (propertize
                       (format "%s<%d/%d>" (mapconcat 'substring-no-properties (persp-mode-line) "") (my-persp-mode-line-string) (length (persp-names)))
                       'face '(:box (:color "orange") font-lock-preprocessor-face)))
            '(:eval (propertize
                     (format "%s<%d/%d>" (mapconcat 'substring-no-properties (persp-mode-line) "") (my-persp-mode-line-string) (length (persp-names)))
                     'face 'font-lock-preprocessor-face)))
          )
        )
      )

(setq line-column-mode-line
      (concat "("
              ;; '%02' to set to 2 chars at least; prevents flickering
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
       "] "
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
       (fwar34/meow-state)
       ;; '(:eval (meow-indicator))
       '(:eval (when (featurep 'lispyville)
                 (lispyville-mode-line-string " @lispy-special@")))
       ;; (fwar34/wgrep-state)
       (fwar34/lispy-state)
       ;; " "
       ;; git info
       ;; '(:eval (when (> (window-width) 90)
       ;;           `(vc-mode vc-mode)
       ;;           ;; reference from spaceline
       ;;           (string-trim (concat vc-mode
       ;;                           (when (buffer-file-name)
       ;;                             (pcase (vc-state (buffer-file-name))
       ;;                               (`up-to-date " ")
       ;;                               (`edited "@Mod")
       ;;                               (`added "@Add")
       ;;                               (`unregistered "@??")
       ;;                               (`removed "@Del")
       ;;                               (`needs-merge "@Con")
       ;;                               (`needs-update "@Upd")
       ;;                               (`ignored "@Ign")
       ;;                               (_ "@Unk")))))))

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
       ;; projectile-mode-line
       " "
       my-selection-info
       " "
       (zilong/modeline--evil-substitute)
       ;; '(:eval (mode-line-fill 'mode-line (+ 7 (string-width (encoding-string)))))
       ;; '(:eval (mode-line-fill (+ 7 (string-width (encoding-string)))))
       '(:eval (mode-line-fill (string-width (encoding-string))))
       encoding-mode-line
       ;; " "
       ;; "["
       ;; time-mode-line
       ;; (propertize (format-time-string "[%H:%M]") 'face 'font-lock-constant-face) ;; size
       ;; (propertize (format-time-string "%H:%M") 'face 'font-lock-constant-face) ;; size
       ;; "]"
       ;; my-modeline-time
       ;; mode-line-end-spaces
       ))

;; (set-face-background 'modeline "#4466aa")
;; (set-face-background 'modeline-inactive "#99aaff")
;; (set-face-background 'fringe "#809088")
;; (if (display-graphic-p)
;;     (progn
;;       (set-face-background 'mode-line "gray26")
;;       (set-face-background 'mode-line-inactive "gray26"))
;;   (set-face-background 'mode-line "black")
;;   (set-face-background 'mode-line-inactive "black"))
(progn
      (set-face-background 'mode-line "gray26")
      (set-face-background 'mode-line-inactive "gray26"))

;; Here 's how I get a box around the active mode-line :
;; (custom-set-faces '(mode-line ((t (:box (:line-width 2 :color "red"))))))

;; {{ change mode-line color by evil state
(defun my-show-wgrep-or-wdired-state ()
  "Change mode line color to notify user wgrep wdired state or god-mode."
  (if (and (not buffer-read-only) (or (equal major-mode 'ivy-occur-grep-mode) (equal major-mode 'rg-mode) (equal major-mode 'wdired-mode)))
      (progn
        (set-face-background 'mode-line "orange")
        ;; (set-face-background 'mode-line-inactive "gray26")
        )
    (unless (bound-and-true-p god-local-mode)
      (set-face-background 'mode-line (if (display-graphic-p) "gray26" "black"))))
  )
;; TODO 需要重新设置
;; (add-hook 'post-command-hook 'my-show-wgrep-or-wdired-state)
;; }}

(provide 'init-my-modeline)
;;; init-my-modeline.el ends here
