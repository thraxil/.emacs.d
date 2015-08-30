(if (getenv "DISPLAY")
    (progn (menu-bar-mode -1)
           (scroll-bar-mode -1)
           (tool-bar-mode -1)))

(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)


;;;;;;;;;;;;;;;;;;; load libraries ;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'appearance)
(require 'key-bindings)
(require 'auto-complete-config)
(require 'package)
(require 'erlang)
(require 'feature-mode)
(require 'go-mode)
(require 'auto-complete-config)
(require 'hippie-exp)
(require 'markdown-mode)
(require 'toggle-quotes)
;; ;(require 'clojure-mode)

(add-to-list 'ac-dictionary-directories
						 (expand-file-name "ac-dict" user-emacs-directory))
(ac-config-default)
(setq ac-show-menu-immediately-on-auto-complete t)

(setq-default abbrev-mode t)
(if (file-exists-p abbrev-file-name)
        (quietly-read-abbrev-file))

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
(require-package 'projectile)
(require-package 'helm)
(require-package 'helm-projectile)
(require-package 'yaml-mode)
(require-package 'elixir-mode)
(require-package 'web-mode)

;;;;;;;;;;;;;;;;;;; global settings ;;;;;;;;;;;;;;;;;;;;;;

(eval-after-load "magit" '(require 'setup-magit))

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

(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (goto-line (read-number "Goto line: ")))
    (linum-mode -1)))


;; some sane defaults
(auto-compression-mode t)

;; UTF-8 please
(setq locale-coding-system 'utf-8) ; pretty
(set-terminal-coding-system 'utf-8) ; pretty
(set-keyboard-coding-system 'utf-8) ; pretty
(set-selection-coding-system 'utf-8) ; please
(prefer-coding-system 'utf-8) ; with sugar on top

(delete-selection-mode 1)
(winner-mode 1)

;; emacs 25's electric-indent-mode does some weird things with python
(defun electric-indent-ignore-python (char)
  "Ignore electric indentation for python-mode"
  (if (equal major-mode 'python-mode)
      'no-indent
    nil))
(add-hook 'electric-indent-functions 'electric-indent-ignore-python)

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
(setq blink-matching-paren-distance nil)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (helm-projectile helm projectile magit expand-region))))

(setq skeleton-pair t)

;; don't leave '~' files all over the place. instead,
;; put all of them into a single central directory
(defun ecm-backup-enable-predicate (filename)
  (and (not (string= "/tmp/" (substring filename 0 5)))
       (not (string-match "/Mail/" filename))
       (not (string-match "/News/" filename))))
(setq backup-enable-predicate 'ecm-backup-enable-predicate)
(setq version-control "never")
(setq backup-directory-alist
      (cons '("~/.backups") backup-directory-alist))
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
(eval-after-load "go-mode" '(require 'setup-go))

(setq c-basic-offset 4)
(setq js-indent-level 4)

(setq auto-mode-alist (cons '("\\.tmpl$" . html-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.erl$" . erlang-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
(add-to-list 'auto-mode-alist '("\\.js$" . js-mode))
(setq js-mode-hook
      '(lambda () (progn
                    (set-variable 'indent-tabs-mode nil))))

(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(add-to-list 'auto-mode-alist '("\\.elixir2\\'" . elixir-mode))
(add-to-list 'auto-mode-alist '("\\.exs\\'" . elixir-mode))

(add-to-list 'auto-mode-alist '("\\.eex\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(setq web-mode-engines-alist
      '(("elixir"    . "\\.eex\\'")
        ("django"  . "\\.html\\'"))
			)

(defun my-web-mode-hook ()
  (setq web-mode-enable-auto-pairing nil))

(add-hook 'web-mode-hook  'my-web-mode-hook)
(add-hook 'web-mode-hook '(lambda() (setq indent-tabs-mode nil)))

(defun sp-web-mode-is-code-context (id action context)
  (when (and (eq action 'insert)
             (not (or (get-text-property (point) 'part-side)
                      (get-text-property (point) 'block-side))))

    t))
(setq web-mode-enable-block-face t)
(setq web-mode-enable-current-element-highlight t)
(setq web-mode-enable-part-face t)
(setq web-mode-enable-current-column-highlight t)

;; salt-stack extension
(add-to-list 'auto-mode-alist '("\\.sls\\'" . yaml-mode))

;; if i did more Clojure, I'd enable these...

;; ;(add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))
;; ;(setq clojure-src-root "~/bin")
;; ;(setq swank-clojure-extra-classpaths '())
;; ;(clojure-slime-config)
;; (setq auto-mode-alist (cons '("\\.clj$" . clojure-mode) auto-mode-alist))

;; Projectile

(require 'projectile)
(projectile-global-mode)
;(require 'ido)
;(ido-mode t)
(helm-mode t)
(helm-autoresize-mode 1)
(require 'helm-projectile)
(setq projectile-completion-system 'helm)
(helm-projectile-on)

(setq compilation-scroll-output t)

;; better dired setup
(setq dired-listing-switches "-laGh1v --group-directories-first")
(defun dired-get-size ()
  (interactive)
  (let ((files (dired-get-marked-files)))
    (with-temp-buffer
      (apply 'call-process "/usr/bin/du" nil t nil "-sch" files)
      (message
       "Size of all marked files: %s"
       (progn
         (re-search-backward "\\(^[ 0-9.,]+[A-Za-z]+\\).*total$")
         (match-string 1))))))

(add-hook 'dired-load-hook '(lambda () (require 'dired-x)))
(setq dired-omit-mode t)
(require 'dired-x)
(setq dired-omit-files 
      (rx (or (seq bol (? ".") "#")         ;; emacs autosave files 
              (seq "~" eol)                 ;; backup-files 
              (seq bol "svn" eol)           ;; svn dirs 
              (seq ".pyc" eol)
              ))) 
(setq-default dired-omit-files-p t)


;;;;;;;;;;;;;;;;;;; extra functions ;;;;;;;;;;;;;;;;;;;;;;

(defun kill-syntax-forward ()
  "Kill characters with syntax at point."
  (interactive)
  (kill-region (point)
               (progn (skip-syntax-forward (string (char-syntax (char-after))))
                      (point))))

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
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'dired-find-alternate-file 'disabled nil)
