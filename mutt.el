;; in my .muttrc, I do:
;;   set editor = "emacs -nw -l ~/.emacs.d/mutt.el"
(defun axels-mail-mode-hook () 
  (turn-on-auto-fill) ;;; Auto-Fill is necessary for mails
  (turn-on-font-lock) ;;; Font-Lock is always cool *g*
  (flush-lines "^\\(> \n\\)*> -- \n\\(\n?> .*\\)*") ;;; Kills quoted sigs.
  (not-modified) ;;; We haven't changed the buffer, haven't we? *g*
  (mail-text) ;;; Jumps to the beginning of the mail text
  (setq make-backup-files nil) ;;; No backups necessary.
  )
(or (assoc "mutt-" auto-mode-alist) 
    (setq auto-mode-alist (cons '("mutt-" . mail-mode) auto-mode-alist))) 
(add-hook 'mail-mode-hook 'axels-mail-mode-hook) 
(global-whitespace-mode 0)
