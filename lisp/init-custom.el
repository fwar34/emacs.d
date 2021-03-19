;; -*- coding: utf-8; lexical-binding: t; -*-

(defun my-set-path ()
  (when (eq window-system 'w32)
    )
  )
(add-hook 'after-init-hook 'my-set-path)
(setenv "PATH"
        (concat (getenv "PATH")
                ";D:\\msys64\\mingw64\\bin;D:\\msys64\\usr\\bin;D:\\msys64\\mingw32\\bin;C:\\Windows\\System32"))
(provide 'init-custom)
