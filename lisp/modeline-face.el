;;-------------------------------------------------------------
;; modeline face
;;-------------------------------------------------------------
(defface lispyville-special-face
  '((t :foreground "#aa4456"))
  "Face for lispyville special mode line indicator."
  :group 'lispyville)

(defface font-lock-builtin-face
  '((((class grayscale) (background light)) :foreground "LightGray" :weight bold)
    (((class grayscale) (background dark))  :foreground "DimGray" :weight bold)
    (((class color) (min-colors 88) (background light)) :foreground "dark slate blue")
    (((class color) (min-colors 88) (background dark))  :foreground "LightSteelBlue")
    (((class color) (min-colors 16) (background light)) :foreground "Orchid")
    (((class color) (min-colors 16) (background dark)) :foreground "LightSteelBlue")
    (((class color) (min-colors 8)) :foreground "blue" :weight bold)
    (t :weight bold))
  "Font Lock mode face used to highlight builtins."
  :group 'font-lock-faces)

(defface font-lock-preprocessor-face
  '((t :inherit font-lock-builtin-face))
  "Font Lock mode face used to highlight preprocessor directives."
  :group 'font-lock-faces)

(defface font-lock-type-face
  '((((class grayscale) (background light)) :foreground "Gray90" :weight bold)
    (((class grayscale) (background dark))  :foreground "DimGray" :weight bold)
    (((class color) (min-colors 88) (background light)) :foreground "ForestGreen")
    (((class color) (min-colors 88) (background dark))  :foreground "PaleGreen")
    (((class color) (min-colors 16) (background light)) :foreground "ForestGreen")
    (((class color) (min-colors 16) (background dark))  :foreground "PaleGreen")
    (((class color) (min-colors 8)) :foreground "green")
    (t :weight bold :underline t))
  "Font Lock mode face used to highlight type and classes."
  :group 'font-lock-faces)

(defface font-lock-keyword-face
  '((((class grayscale) (background light)) :foreground "LightGray" :weight bold)
    (((class grayscale) (background dark))  :foreground "DimGray" :weight bold)
    (((class color) (min-colors 88) (background light)) :foreground "Purple")
    (((class color) (min-colors 88) (background dark))  :foreground "Cyan1")
    (((class color) (min-colors 16) (background light)) :foreground "Purple")
    (((class color) (min-colors 16) (background dark))  :foreground "Cyan")
    (((class color) (min-colors 8)) :foreground "cyan" :weight bold)
    (t :weight bold))
  "Font Lock mode face used to highlight keywords."
  :group 'font-lock-faces)

(defface font-lock-constant-face
  '((((class grayscale) (background light))
     :foreground "LightGray" :weight bold :underline t)
    (((class grayscale) (background dark))
     :foreground "Gray50" :weight bold :underline t)
    (((class color) (min-colors 88) (background light)) :foreground "dark cyan")
    (((class color) (min-colors 88) (background dark))  :foreground "Aquamarine")
    (((class color) (min-colors 16) (background light)) :foreground "CadetBlue")
    (((class color) (min-colors 16) (background dark))  :foreground "Aquamarine")
    (((class color) (min-colors 8)) :foreground "magenta")
    (t :weight bold :underline t))
  "Font Lock mode face used to highlight constants and labels."
  :group 'font-lock-faces)

(defface font-lock-string-face
  '((((class grayscale) (background light)) :foreground "DimGray" :slant italic)
    (((class grayscale) (background dark))  :foreground "LightGray" :slant italic)
    (((class color) (min-colors 88) (background light)) :foreground "VioletRed4")
    (((class color) (min-colors 88) (background dark))  :foreground "LightSalmon")
    (((class color) (min-colors 16) (background light)) :foreground "RosyBrown")
    (((class color) (min-colors 16) (background dark))  :foreground "LightSalmon")
    (((class color) (min-colors 8)) :foreground "green")
    (t :slant italic))
  "Font Lock mode face used to highlight strings."
  :group 'font-lock-faces)

(defface font-lock-variable-name-face
  '((((class grayscale) (background light))
     :foreground "Gray90" :weight bold :slant italic)
    (((class grayscale) (background dark))
     :foreground "DimGray" :weight bold :slant italic)
    (((class color) (min-colors 88) (background light)) :foreground "sienna")
    (((class color) (min-colors 88) (background dark))  :foreground "LightGoldenrod")
    (((class color) (min-colors 16) (background light)) :foreground "DarkGoldenrod")
    (((class color) (min-colors 16) (background dark))  :foreground "LightGoldenrod")
    (((class color) (min-colors 8)) :foreground "yellow" :weight light)
    (t :weight bold :slant italic))
  "Font Lock mode face used to highlight variable names."
  :group 'font-lock-faces)

;; reference from file "font-lock.el.gz" 
(defface font-lock-evil-normal-face
  '((((class grayscale) (background light)) :foreground "DimGray" :slant italic)
    (((class grayscale) (background dark))  :foreground "LightGray" :slant italic)
    (((class color) (min-colors 88) (background light)) :foreground "VioletRed4")
    (((class color) (min-colors 88) (background dark))  :foreground "LightSalmon")
    (((class color) (min-colors 16) (background light)) :foreground "RosyBrown")
    (((class color) (min-colors 16) (background dark))  :foreground "LightSalmon")
    (((class color) (min-colors 8)) :foreground "green")
    (t :slant italic))
  "Font Lock mode face used to highlight strings."
  :group 'font-lock-evil-faces)
(defface font-lock-evil-insert-face
  '((((class color) (min-colors 88) (background light)) :foreground "Blue1")
    (((class color) (min-colors 88) (background dark))  :foreground "LightSkyBlue")
    (((class color) (min-colors 16) (background light)) :foreground "Blue")
    (((class color) (min-colors 16) (background dark))  :foreground "LightSkyBlue")
    (((class color) (min-colors 8)) :foreground "blue" :weight bold)
    (t :inverse-video t :weight bold))
  "Font Lock mode face used to highlight function names."
  :group 'font-lock-evil-faces)
(defface font-lock-evil-visual-face
  '((((class grayscale) (background light)) :foreground "Gray90" :weight bold)
    (((class grayscale) (background dark))  :foreground "DimGray" :weight bold)
    (((class color) (min-colors 88) (background light)) :foreground "ForestGreen")
    (((class color) (min-colors 88) (background dark))  :foreground "PaleGreen")
    (((class color) (min-colors 16) (background light)) :foreground "ForestGreen")
    (((class color) (min-colors 16) (background dark))  :foreground "PaleGreen")
    (((class color) (min-colors 8)) :foreground "green")
    (t :weight bold :underline t))
  "Font Lock mode face used to highlight type and classes."
  :group 'font-lock-evil-faces)
(defface font-lock-evil-emacs-face
  '((((class grayscale) (background light))
     :foreground "LightGray" :weight bold :underline t)
    (((class grayscale) (background dark))
     :foreground "Gray50" :weight bold :underline t)
    (((class color) (min-colors 88) (background light)) :foreground "dark cyan")
    (((class color) (min-colors 88) (background dark))  :foreground "Aquamarine")
    (((class color) (min-colors 16) (background light)) :foreground "CadetBlue")
    (((class color) (min-colors 16) (background dark))  :foreground "Aquamarine")
    (((class color) (min-colors 8)) :foreground "magenta")
    (t :weight bold :underline t))
  "Font Lock mode face used to highlight constants and labels."
  :group 'font-lock-evil-faces)

;; (defface spaceline-highlight-face
;;   `((t (:background "DarkGoldenrod2"
;;         :foreground "#3E3D31"
;;         :inherit 'mode-line)))
;;   "Default highlight face for spaceline."
;;   :group 'spaceline)

;; Define various other highlight faces
(dolist (s '((spaceline-evil-normal "DarkGoldenrod2" "Evil normal state face.")
             (spaceline-evil-insert "chartreuse3" "Evil insert state face.")
             (spaceline-evil-emacs "SkyBlue2" "Evil emacs state face.")
             (spaceline-evil-replace "chocolate" "Evil replace state face.")
             (spaceline-evil-visual "gray" "Evil visual state face.")
             (spaceline-evil-motion "plum3" "Evil motion state face.")
             (spaceline-unmodified "DarkGoldenrod2" "Unmodified buffer face.")
             (spaceline-modified "SkyBlue2" "Modified buffer face.")
             (spaceline-read-only "plum3" "Read-only buffer face.")))
  (eval `(defface ,(nth 0 s)
           `((t (:background ,(nth 1 s)
                 :foreground "#3E3D31"
                 :inherit 'mode-line)))
           ,(nth 2 s)
           :group 'spaceline)))

(dolist (s '((fwar34/spaceline-evil-normal "DarkGoldenrod2" "Evil normal state face.")
             (fwar34/spaceline-evil-insert "chartreuse3" "Evil insert state face.")
             (fwar34/spaceline-evil-emacs "SkyBlue2" "Evil emacs state face.")
             (fwar34/spaceline-evil-replace "chocolate" "Evil replace state face.")
             (fwar34/spaceline-evil-visual "gray" "Evil visual state face.")
             (fwar34/spaceline-evil-motion "plum3" "Evil motion state face.")
             (fwar34/spaceline-unmodified "DarkGoldenrod2" "Unmodified buffer face.")
             (fwar34/spaceline-modified "SkyBlue2" "Modified buffer face.")
             (fwar34/spaceline-read-only "plum3" "Read-only buffer face.")))
  (eval `(defface ,(nth 0 s)
           `((t (:foreground  ,(nth 1 s)
                 ;; :background "#424242424242"
                 :inherit 'mode-line-buffer-id)))
           ,(nth 2 s)
           :group 'spaceline)))

(provide 'modeline-face)
