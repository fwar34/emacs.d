;; -*- coding: utf-8; lexical-binding: t; -*-

(defun my-set-path ()
  (interactive)
  (when (eq window-system 'w32)
    (setenv "PATH"
	    (concat (getenv "PATH")
		    ";D:\\msys64\\mingw64\\bin;D:\\msys64\\usr\\bin;D:\\msys64\\mingw32\\bin;C:\\Windows\\System32")))

  ;; awesome 和 dwm 等一些 window manager 启动的时候 path 不完整，所以这里设置
  (when (equal system-type 'gnu/linux)
    (let ((home (expand-file-name "~")))
      (setenv "PATH"
              (concat (getenv "PATH") ":"
                      home "/bin:"
                      "/snap/bin:"
                      home "/.local/bin:"
                      home "/.local/share/nvim/site/pack/packer/start/fzf/bin:"
                      "/usr/local/go/bin:"
                      home "/go/bin:"
                      home "/.cargo/bin:"
                      home "/bin/maven/bin:"
                      home "/jdk-13.0.2/bin:"
                      "/usr/share/maven/bin"
                      )))
    )
  )

;; (add-hook 'after-init-hook 'my-set-path)
(my-set-path)
(provide 'init-custom)
