;; swiper setting
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)

;; 下面的这些函数可以让你找到不同函数，变量以及快捷键所定义的文件位置
;; 因为非常常用 所以我们建议将其设置为与查找文档类似的快捷键
(global-set-key (kbd "C-h C-f") 'find-function)
(global-set-key (kbd "C-h C-v") 'find-variable)
(global-set-key (kbd "C-h C-k") 'find-function-on-key)

;; minefunc 快速打开配置文件
(global-set-key (kbd "C-x C-m") 'open-init-file)

;;
(global-set-key (kbd "C-c p f") 'counsel-git)

;; 打开recent files
(global-set-key (kbd "C-x C-r") 'recentf-open-files)

;; dired重用buffer
(with-eval-after-load 'dired
      (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file))

(provide 'init-keybindings)
