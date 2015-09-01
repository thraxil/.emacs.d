(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)
(set-default 'indicate-empty-lines t)
(setq echo-keystrokes 0.1)
(global-hl-line-mode nil)
(setq redisplay-dont-pause t)
(show-paren-mode 1)
(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (blink-cursor-mode -1))

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'default-black  t)

(provide 'appearance)
