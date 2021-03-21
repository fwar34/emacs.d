;; -*- coding: utf-8; lexical-binding: t; -*-

(defun my-set-path ()
  (when (eq window-system 'w32)

    (setenv "PATH"
	    (concat (getenv "PATH")
		    ";D:\\msys64\\mingw64\\bin;D:\\msys64\\usr\\bin;D:\\msys64\\mingw32\\bin;C:\\Windows\\System32"))
    )
  )
(add-hook 'after-init-hook 'my-set-path)
(provide 'init-custom)
