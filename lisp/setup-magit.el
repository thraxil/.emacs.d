;; full screen magit-status

(defun magit-status-fullscreen-advice (orig-fun &rest args)
  "Fullscreen magit-status advice."
  (window-configuration-to-register :magit-fullscreen)
  (apply orig-fun args)
  (delete-other-windows))

(advice-add 'magit-status :around #'magit-status-fullscreen-advice)

(defun magit-quit-session ()
 "Restores the previous window configuration and kills the magit buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))

(provide 'setup-magit)
