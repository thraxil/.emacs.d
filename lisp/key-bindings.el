(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)

(global-set-key (kbd "C-c n") 'cleanup-buffer)

(global-set-key (kbd "C-'") 'toggle-quotes)

(global-set-key (kbd "M-j")
                (lambda ()
                  (interactive)
                  (join-line -1)))

(global-set-key [(control s)] 'isearch-forward-regexp)
(global-set-key [(control r)] 'isearch-backward-regexp)

(global-set-key [remap goto-line] 'goto-line-with-feedback)
(global-set-key (kbd "[") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "(") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "{") 'skeleton-pair-insert-maybe)

(global-set-key (kbd "C-=") 'er/expand-region)

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(global-set-key [(control d)] 'kill-syntax-forward)

(global-set-key [(control ? )] 'hippie-expand)
(global-set-key [(control return)] 'set-mark-command)

(provide 'key-bindings)
