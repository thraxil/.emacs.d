(if (getenv "DISPLAY")
    (progn (menu-bar-mode -1)
           (scroll-bar-mode -1)
           (tool-bar-mode -1)))

(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

;;;;;;;;;;;;;;;;;;; load libraries ;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'auto-complete-config)
(require 'package)
(require 'erlang)
(require 'feature-mode)
(require 'go-mode)
(require 'auto-complete-config)
(require 'hippie-exp)
(require 'markdown-mode)
;; ;(require 'clojure-mode)

(add-to-list 'ac-dictionary-directories "/home/anders/emacs/ac-dict")
(ac-config-default)

;;;;;;;;;;;;;;;;;;; melpa ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(package-initialize)
(unless (file-exists-p "~/.emacs.d/elpa/archives/melpa")
  (package-refresh-contents))

(defun require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (package-install package)
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))

(require-package 'expand-region)
(require-package 'magit)

;;;;;;;;;;;;;;;;;;; global settings ;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)

;; full screen magit-status

(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defun magit-quit-session ()
  "Restores the previous window configuration and kills the magit buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))

;; (global-whitespace-mode 1)

(defun cleanup-buffer-safe ()
  "Perform a bunch of safe operations on the whitespace content of a buffer.
Does not indent buffer, because it is used for a before-save-hook, and that
might be bad."
  (interactive)
  (untabify (point-min) (point-max))
  (delete-trailing-whitespace)
  (set-buffer-file-coding-system 'utf-8))

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer.
Including indent-buffer, which should not be called automatically on save."
  (interactive)
  (cleanup-buffer-safe)
  (indent-region (point-min) (point-max)))

(global-set-key (kbd "C-c n") 'cleanup-buffer)

(global-set-key (kbd "M-j")
                (lambda ()
                  (interactive)
                  (join-line -1)))

(global-set-key [(control s)] 'isearch-forward-regexp)
(global-set-key [(control r)] 'isearch-backward-regexp)
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

(global-set-key [remap goto-line] 'goto-line-with-feedback)

(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (goto-line (read-number "Goto line: ")))
    (linum-mode -1)))


;; some sane defaults
(set-default 'indicate-empty-lines t)
(setq echo-keystrokes 0.1)
(auto-compression-mode t)

;; UTF-8 please
(setq locale-coding-system 'utf-8) ; pretty
(set-terminal-coding-system 'utf-8) ; pretty
(set-keyboard-coding-system 'utf-8) ; pretty
(set-selection-coding-system 'utf-8) ; please
(prefer-coding-system 'utf-8) ; with sugar on top

(delete-selection-mode 1)
(winner-mode 1)


;; Save a list of recent files visited. (open recent file with C-x f)
(recentf-mode 1)
(setq recentf-max-saved-items 100) ;; just 20 is too recent

(setq require-final-newline t)
(setq next-line-add-newlines nil)
(setq default-major-mode 'text-mode)
(fset 'yes-or-no-p 'y-or-n-p)
(set-default 'truncate-lines t)
(setq scroll-step 1)
(setq display-time-24hr-format t)
(display-time)
(global-hl-line-mode nil)
(setq blink-matching-paren-distance nil)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(backward-delete-char-untabify-method (quote hungry))
 '(column-number-mode t)
 '(completion-ignored-extensions
   (quote
    ("~" ".aux" ".a" ".bbl" ".blg" ".dvi" ".elc" ".hc" ".hi" ".log" ".mlc" ".o" ".toc" ".pyc")))
 '(font-lock-global-modes t)
 '(font-lock-maximum-decoration t)
 '(global-font-lock-mode t nil (font-lock))
 '(indent-tabs-mode nil)
 '(line-number-mode t)
 '(user-mail-address "anders@columbia.edu")
 '(version-control t)
 '(visible-bell t))

(setq skeleton-pair t)
(global-set-key (kbd "[") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "(") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "{") 'skeleton-pair-insert-maybe)


(global-set-key (kbd "C-=") 'er/expand-region)


(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; don't leave '~' files all over the place. instead,
;; put all of them into a single central directory
(defun ecm-backup-enable-predicate (filename)
  (and (not (string= "/tmp/" (substring filename 0 5)))
       (not (string-match "/Mail/" filename))
       (not (string-match "/News/" filename))))
(setq backup-enable-predicate 'ecm-backup-enable-predicate)
(setq version-control "never")
(setq backup-directory-alist
      (cons '("." . "/home/anders/.backups") backup-directory-alist))
(setq kept-old-versions 0)
(setq kept-new-versions 1)
(setq delete-old-versions t)

;; auto fill for text-mode
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(setq fill-column 72)

;; keep point centered vertically
(add-hook 'post-command-hook
          (lambda ()
            (recenter '("don't redraw"))))

;;;;;;;;;;;;;;;;;;; modes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'erlang-mode-hook '(lambda() (setq indent-tabs-mode nil)))


(add-to-list 'auto-mode-alist '("\.feature$" . feature-mode))

(add-to-list 'auto-mode-alist '("\.go$" . go-mode))
(setq default-tab-width 2)
(setenv "GOPATH" "/home/anders/code/go/")
(setenv "PATH" "/home/anders/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games:/home/anders/code/go/go/bin")

(defun my-go-mode-hook ()
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save)
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v && go vet"))
  (local-set-key (kbd "M-.") 'godef-jump))
(add-hook 'go-mode-hook 'my-go-mode-hook)

(setq c-basic-offset 4)
(setq js-indent-level 4)

(setq auto-mode-alist (cons '("\\.tmpl$" . html-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.html$" . django-html-mode) auto-mode-alist))
(add-to-list 'auto-mode-alist '("\\.djhtml$'" . django-html-mode))
(setq auto-mode-alist (cons '("\\.erl$" . erlang-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
(add-to-list 'auto-mode-alist '("\\.js$" . js-mode))
(setq js-mode-hook
      '(lambda () (progn
                    (set-variable 'indent-tabs-mode nil))))

(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; if i did more Clojure, I'd enable these...

;; ;(add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))
;; ;(setq clojure-src-root "/home/anders/bin")
;; ;(setq swank-clojure-extra-classpaths '())
;; ;(clojure-slime-config)
;; (setq auto-mode-alist (cons '("\\.clj$" . clojure-mode) auto-mode-alist))

;;;;;;;;;;;;;;;;;;; extra functions ;;;;;;;;;;;;;;;;;;;;;;

(global-set-key [(control d)] 'kill-syntax-forward)

(defun kill-syntax-forward ()
  "Kill characters with syntax at point."
  (interactive)
  (kill-region (point)
               (progn (skip-syntax-forward (string (char-syntax (char-after))))
                      (point))))

(global-set-key [(control ? )] 'hippie-expand)
(global-set-key [(control return)] 'set-mark-command)

(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name-partially
        try-complete-file-name
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol
        try-expand-whole-kill))

(fset 'flip-pipe
      (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([1 C-return 19 124 2 23 4 5 124 25 backspace 14] 0 "%d")) arg)))

(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
