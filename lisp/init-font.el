;; -*- coding: utf-8; lexical-binding: t; -*-

;; https://emacs-china.org/t/emacs/15676/21
;; https://emacs-china.org/t/cnfonts/16894/10
(use-package faces
  :custom-face
  (variable-pitch
   ((t (:font ,(font-xlfd-name (font-spec :family "Iosevka"
                                          :foundry "Outline"
                                          ;; If we do not specify this, Emacs
                                          ;; selects ascii and we miss accents
                                          :registry "iso10646-1"))
              :slant normal :weight normal
              :height 138 :width normal))))
  (default
    ((t (:font ,(font-xlfd-name (font-spec :family "Comic Mono"
                                           :foundry "Outline"
                                           ;; If we do not specify this, Emacs
                                           ;; selects ascii and we miss accents
                                           :registry "iso10646-1"))
               :slant normal :weight normal
               :height 140 :width normal))))
  (fixed-pitch
   ((t (:inherit default))))
  :config
  (defun colawithsauce/set-unicode-fonts (&optional frame)
    "Setting unicode-char fonts for emacs."
    ;; Use Noto for everything to get consistent view
    ;; Chinese, simplified
    (set-fontset-font t 'unicode (font-spec :family "Sarasa Mono Slab SC" :weight 'normal) frame)
    ;; Symbols, including b/w emoji
    (set-fontset-font t 'symbol "Apple Color Emoji" frame 'prepend))
  ;; For initial frame
  (add-hook 'after-init-hook 'colawithsauce/set-unicode-fonts)
  (add-hook 'after-make-frame-functions 'colawithsauce/set-unicode-fonts)

  ;; Rescale to restrict font into same height.
  (add-to-list 'face-font-rescale-alist '("Apple Color Emoji" . 0.9))
  (add-to-list 'face-font-rescale-alist '("Sarasa Mono Slab SC" . 0.88))

  ;; Disable font rescale in variable-pitch mode
  (defun colawithsauce/disable-rescale-maybe ()
    "remove stuffs in `face-font-scale-alist' when in buffer-face-mode."
    (if buffer-face-mode
        (setq-local face-font-rescale-alist nil)
      (setq-local face-font-rescale-alist '(("Sarasa Mono Slab SC" . 0.88) ("Apple Color Emoji" . 0.9) ("-cdac$" . 1.3)))))
  (add-hook
   'buffer-face-mode-hook 'colawithsauce/disable-rescale-maybe)
  )

(provide 'init-font)
