(defun my-elixir-mode-hook ()
	(setq-default indent-tabs-mode nil)
	(setq c-basic-offset 2))
(add-hook 'elixir-mode-hook 'my-elixir-mode-hook)

(provide 'setup-elixir)
