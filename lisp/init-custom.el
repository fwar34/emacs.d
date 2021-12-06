;; -*- coding: utf-8; lexical-binding: t; -*-

(defun my-set-path ()
  "Set PATH"
  (when (eq window-system 'w32)
    (setenv "PATH"
	    (concat (getenv "PATH")
		    ";D:\\msys64\\mingw64\\bin;D:\\msys64\\usr\\bin;D:\\msys64\\mingw32\\bin;C:\\Windows\\System32")))

  ;; (when (and (display-graphic-p) (equal system-type 'gnu/linux))
  ;;   (let ((home (expand-file-name "~")))
  ;;     (setenv "PATH"
  ;;             (concat (getenv "PATH") ":"
  ;;                     home "/bin:"
  ;;                     "/snap/bin:"
  ;;                     home "/.local/bin:"
  ;;                     home "/.local/share/nvim/site/pack/packer/start/fzf/bin:"
  ;;                     "/usr/local/go/bin:"
  ;;                     home "/go/bin:"
  ;;                     home "/.cargo/bin:"
  ;;                     home "/bin/maven/bin:"
  ;;                     home "/jdk-13.0.2/bin:"
  ;;                     "/usr/share/maven/bin"
  ;;                     )))
  ;;   )
  )

(my-set-path)
(provide 'init-custom)
