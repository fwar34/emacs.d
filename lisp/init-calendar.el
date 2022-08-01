;;; init-calendar.el --- Useful preset transient commands  -*- coding:utf-8; lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(use-package calendar
  :ensure nil
  :config
  (setq calendar-load-hook
        '(lambda ()
           (set-face-foreground 'diary-face "skyblue")
           (set-face-background 'holiday-face "slate blue")
           (set-face-foreground 'holiday-face "white")))
  
  (setq calendar-view-holidays-initially-flag t)
  ;; 设置阴历显示，在 calendar 上用 pC 显示阴历
  (setq chinese-calendar-celestial-stem
          ["甲" "乙" "丙" "丁" "戊" "己" "庚" "辛" "壬" "癸"])
  (setq chinese-calendar-terrestrial-branch
          ["子" "丑" "寅" "卯" "辰" "巳" "戊" "未" "申" "酉" "戌" "亥"]))

(provide 'init-calendar)
;;; init-calendar.el ends here
