(if (getenv "DISPLAY")
    (progn (menu-bar-mode -1)
           (scroll-bar-mode -1)
           (tool-bar-mode -1)))

(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(setq redisplay-dont-pause t)

;;;;;;;;;;;;;;;;;;; load libraries ;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'appearance)
(require 'key-bindings)
(require 'auto-complete-config)
(require 'package)
(require 'erlang)
(require 'feature-mode)
(require 'go-mode)
(require 'hippie-exp)
(require 'markdown-mode)
(require 'toggle-quotes)
(require 'go-autocomplete)
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
(require-package 'alchemist)
(require-package 'powerline)
(require-package 'edit-server)
(require-package 'f)
(require-package 'let-alist)
(require-package 's)
(require-package 'elm-mode)
(require-package 'groovy-mode)
(require-package 'php-mode)
(require-package 'go-eldoc)
(require-package 'go-autocomplete)

;;;;;;;;;;;;;;;;;;; global settings ;;;;;;;;;;;;;;;;;;;;;;

                                        ; tramp still has some issues with helm...
(eval-after-load 'tramp '(setenv "SHELL" "/bin/bash"))
(setq tramp-shell-prompt-pattern "^[^$>\n]*[#$%>] *\\(\[[0-9;]*[a-zA-Z] *\\)*")
(setq tramp-default-method "ssh")
                                        ;(setq tramp-debug-buffer t)
                                        ;(setq tramp-verbose 10)
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

(require 'mode-line)

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
																				;(defun electric-indent-ignore-python (char)
																				;  "Ignore electric indentation for python-mode"
																				;  (if (equal major-mode 'python-mode)
																				;      'no-indent
																				;    nil))
																				;(add-hook 'electric-indent-functions 'electric-indent-ignore-python)

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
 '(org-agenda-files
	 (quote
		("~/org/meetings.org" "~/org/home.org" "~/org/spokehub.org" "~/org/projects.org" "~/org/calendar.org" "~/org/meetings.org")))
 '(package-selected-packages
	 (quote
		(groovy-mode powerline alchemist web-mode elixir-mode yaml-mode magit helm-projectile expand-region)))
 '(send-mail-function (quote smtpmail-send-it)))

(setq skeleton-pair t)

;; don't leave '~' files all over the place. instead,
;; put all of them into a single central directory
(defun ecm-backup-enable-predicate (filename)
  (and (not (string= "/tmp/" (substring filename 0 5)))
       (not (string-match "/Mail/" filename))
       (not (string-match "/News/" filename))))
(setq backup-enable-predicate 'ecm-backup-enable-predicate)

(setq
 backup-by-copying t      ; don't clobber symlinks
 backup-directory-alist
 '(("." . "~/.backups"))    ; don't litter my fs tree
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)

;; auto fill for text-mode
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(setq fill-column 72)

;; keep point centered vertically
(add-hook 'post-command-hook
          (lambda ()
            (unless (eq major-mode 'eshell-mode)
              (recenter '("don't redraw")))))

;;;;;;;;;;;;;;;;;;; modes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(add-hook 'erlang-mode-hook '(lambda() (setq indent-tabs-mode nil)))

(c-add-style "my-style"
             '("k&r"
               (indent-tabs-mode . nil)        ; use spaces rather than tabs
               (c-basic-offset . 4)            ; indent by four spaces
               ))

(defun my-c++-mode-hook ()
  (c-set-style "my-style")        ; use my-style defined above
  (auto-fill-mode)
  (c-toggle-electric-state -1)    ; i like to type RET myself...
  (define-key c++-mode-map (kbd "RET") ; but i still want it to be smart
    'reindent-then-newline-and-indent)
  (c-toggle-auto-hungry-state 1))

(add-hook 'c++-mode-hook 'my-c++-mode-hook)

(add-to-list 'auto-mode-alist '("\.feature$" . feature-mode))

(add-to-list 'auto-mode-alist '("\.go$" . go-mode))
(eval-after-load "go-mode" '(require 'setup-go))
																				;(add-hook 'before-save-hook #'gofmt-before-save)

																				; remember to `go get -u github.com/nsf/gocode`
																				; and make sure your $PATH includes it
																				; godef also wants `go get github.com/rogpeppe/godef`
(defun go-mode-setup ()
  (go-eldoc-setup))

(add-hook 'go-mode-hook 'go-mode-setup)

(setq c-basic-offset 4)
(setq js-indent-level 4)

(setq auto-mode-alist (cons '("\\.tmpl$" . html-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.erl$" . erlang-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
(add-to-list 'auto-mode-alist '("\\.js$" . js-mode))
(setq js-mode-hook
      '(lambda () (progn
                    (set-variable 'indent-tabs-mode nil))))
(setq emacs-lisp-mode
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

(eval-after-load "elixir-mode" '(require 'setup-elixir))

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
                                        ;(helm-mode nil)
                                        ;(helm-autoresize-mode 1)
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

;;;;;;;;;;;;;;;;;;; org mode stuff ;;;;;;;;;;;;;;;;;;;;;;;

(setq org-log-done t)
(setq org-modules (quote (org-bbdb
													org-gnus
                          org-habit
                          )))
(setq org-habit-graph-column 50)
(run-at-time "18:00" 86400 '(lambda () (setq org-habit-show-habits t)))

(setq org-directory "~/org")

(defun get-bullet-dir-today ()
  (expand-file-name (concat "~/org/bullet/" (format-time-string "%Y/"))))


(defun get-bullet-file-today ()
  "Return filename for today's journal entry."
  (let ((daily-dir (get-bullet-dir-today)))
    (make-directory daily-dir t)
    (concat daily-dir (format-time-string "%Y-%m-%W.org"))))

(setq org-agenda-files (list (get-bullet-file-today)
                             "~/org/meetings.org"
                             "~/org/home.org"
                             "~/org/spokehub.org"
                             "~/org/projects.org"
                             "~/org/calendar.org"
                             "~/org/meetings.org"))

(setq org-agenda-custom-commands
      '(("w" todo "WAITING" nil)
        ("n" todo "NEXT" nil)
        ("a" "Agenda + All Captured" ((agenda) (tags-todo "+CATEGORY=\"capture\"")))
        ("d" "Agenda + Next Actions" ((agenda) (todo "NEXT"))))
      )

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
              (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))))
(add-hook 'org-agenda-mode-hook
          (lambda ()
            (add-hook 'auto-save-hook 'org-save-all-org-buffers nil t)
            (auto-save-mode)))

(defun org-sync ()
  (interactive)
  (shell-command-to-string "~/.emacs.d/bin/org-sync.sh"))

(setq org-default-notes-file (get-bullet-file-today))

(defun get-journal-dir-today ()
  (expand-file-name (concat "~/org/journal/" (format-time-string "%Y/%m/"))))

(defun get-journal-file-today ()
  "Return filename for today's journal entry."
  (let ((daily-dir (get-journal-dir-today)))
    (make-directory daily-dir t)
    (concat daily-dir (format-time-string "%Y-%m-%d.org"))))

(defun journal-file-today ()
  "Create and load a journal file based on today's date."
  (interactive)
  (find-file (get-journal-file-today)))

(setq org-capture-templates
      '(("t" "Todo" entry (file (get-bullet-file-today))
         "* TODO %?\n%t\n  %i\n" :kill-buffer t)
        ("d" "Done" entry (file (get-bullet-file-today))
         "* DONE %?\n\t CLOSED: %U\n%t\n  %i\n" :kill-buffer t)
        ("n" "Note" entry (file (get-bullet-file-today))
         "* %?\n  %i\n%U\n" :kill-buffer t)
        ("s" "Schedule" entry (file (get-bullet-file-today))
         "* %?\n  %^T%i\n%U\n" :kill-buffer t)
        ("l" "Link" entry (file+headline "~/org/links.org" "Links")
         "* %?\n  %i\n%U\n" :kill-buffer t)
        ("q" "Quote" entry (file+headline "~/org/quotes.org" "Quotes")
         "* %U\n\n%?\n\n%i\n" :kill-buffer t)
        ("i" "Idea" entry (file+headline "~/org/ideas.org" "Ideas")
         "* %?\n%U\n\n%i\n" :kill-buffer t)
        ("f" "Fact" entry (file+headline "~/org/facts.org" "Facts")
         "* %?\n\n%U\n\n%i\n" :kill-buffer t)
        ("v" "Vocab" entry (file+headline "~/org/vocab.org" "Vocab")
         "* %? :: \n  %i\n" :kill-buffer t)
        ("j" "Journal" entry (file (get-journal-file-today))
         "* Journal: %?\n%t\n  %i\n\n"
         :empty-lines 1 :kill-buffer t)
        ("w" "Mass" table-line (file+headline "~/org/mass.org" "Mass")
         "|%t|%?|")
        ("m" "Meeting" entry (file+datetree "~/org/meetings.org")
         "* %u %?\n%U\n** Present\n- [X] Anders\n** Notes\n** Actions\n** TODO send out notes/PMTS\n" :kill-buffer t)
        ))

(setq org-refile-targets (quote (("~/org/home.org" :maxlevel . 1)
                                 ("~/org/spokehub.org" :maxlevel . 1)
                                 ("~/org/calendar.org" :maxlevel . 1)
                                 ("~/org/meetings.org" :maxlevel . 1)
                                 ("~/org/projects.org" :maxlevel . 1)
                                 )))

(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)
(setq org-agenda-include-diary t)

(eval-after-load "org"
  '(require 'ox-md nil t))

(require 'epa-file)
(epa-file-enable)

(setq org-cycle-separator-lines 0)
(setq org-blank-before-new-entry (quote ((heading)
                                         (plain-list-item . auto))))
(setq org-insert-heading-respect-content nil)
(setq org-yank-adjusted-subtrees t)
(require 'org-protocol)

(setq org-confirm-babel-evaluate nil
      org-src-fontify-natively t
      org-src-tab-acts-natively t)
(require 'ob-python)
(require 'ob-sh)
(require 'ob-elixir)
(require 'ob-go)
(require 'ob-js)

(require 'server)
(or (server-running-p)
    (server-start)
    (setq edit-server-new-frame nil)
    (edit-server-start)
    )

(defun axels-mail-mode-hook ()
  (turn-on-auto-fill)
  (turn-on-font-lock)
  (flush-lines "^\\(> \n\\)*> -- \n\\(\n?> .*\\)*") ;;; Kills quoted sigs.
  (not-modified) ;;; We haven't changed the buffer, haven't we? *g*
  (mail-text) ;;; Jumps to the beginning of the mail text
  (setq make-backup-files nil) ;;; No backups necessary.
  (define-key mail-mode-map [(control c) (control c)]
    (lambda ()
      (interactive)
      (save-buffer)
      (server-edit))))
(or (assoc "mutt-" auto-mode-alist)
    (setq auto-mode-alist (cons '("mutt-" . mail-mode) auto-mode-alist)))
(add-hook 'mail-mode-hook 'axels-mail-mode-hook)


;;; gnus stuff ;;;;;;;;

(setq gnus-summary-line-format "%U%R %d %(%-20,20f%) %4L | %B %s\n")
(setq gnus-use-cache t)
(setq gnus-fetch-old-headers t)
(setq shr-color-visible-luminance-min 90)
(setq shr-color-visible-distance-min 10)

(unless (boundp 'message-fill-column)
  (add-hook 'message-mode-hook
            (lambda ()
              (setq fill-column 72)
              (turn-on-auto-fill))))

(setq gnus-permanently-visible-groups ".*INBOX")
;; (gnus-demon-add-handler 'gnus-group-get-new-news 1 nil)
(setq mm-discouraged-alternatives '("text/html" "text/richtext"))
(setq gnus-use-full-window nil)

(add-hook 'gnus-summary-mode-hook (lambda ()
      (local-set-key (kbd "<tab>") 'gnus-summary-next-unread-article)
      (local-set-key "n"  'gnus-summary-next-article)
      (local-set-key "p"  'gnus-summary-prev-article)
      (local-set-key "d"  'gnus-summary-put-mark-as-expirable-next)
      (local-set-key "u"  'gnus-summary-clear-mark-forward)
      (local-set-key "r"  'gnus-summary-reply-with-original)
      (local-set-key "x"  'gnus-summary-delete-article)))

(add-hook 'gnus-summary-mode-hook (lambda ()
      (unless (gnus-news-group-p gnus-newsgroup-name)
        (set (make-local-variable  'gnus-expirable-mark) ?D)
        (set (make-local-variable  'gnus-canceled-mark)  ?X)
        (set (make-local-variable  'gnus-ancient-mark)   ? )
        (set (make-local-variable  'gnus-read-mark)      ? ))))

(setq nnmail-expiry-wait 0)

(setq gnus-nntp-server nil
        gnus-read-active-file nil
        gnus-save-newsrc-file nil
        gnus-read-newsrc-file nil
        gnus-check-new-newsgroups nil)
(setq gnus-large-newsgroup 'nil)

(setq bbdb-file "~/org/bbdb") ;; org directory already gets sync'd. win.
(require-package 'bbdb)
(bbdb-initialize 'gnus 'message)
(bbdb-insinuate-message)
(add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)
(add-hook 'gnus-startup-hook 'bbdb-insinuate-message)
(setq bbdb-mua-update-interactive-p '(query . create))

;; this one breaks reply-with-contents:
;; (add-hook 'message-setup-hook 'bbdb-define-all-aliases)

;;;;;;;;;;;;;;;;;;; extra functions ;;;;;;;;;;;;;;;;;;;;;;

(defun eshell-here ()
  "Opens up a new shell in the directory associated with the
current buffer's file. The eshell is renamed to match that
directory to make multiple eshell windows easier."
  (interactive)
  (let* ((parent (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                   default-directory))
         (height (/ (window-total-height) 3))
         (name   (car (last (split-string parent "/" t)))))
    (split-window-vertically (- height))
    (other-window 1)
    (eshell "new")
    (rename-buffer (concat "*eshell: " name "*"))

    (insert (concat "ls"))
    (eshell-send-input)))

(global-set-key (kbd "C-!") 'eshell-here)

(defun eshell/x ()
  (insert "exit")
  (eshell-send-input)
  (delete-window))

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

(put 'dired-find-alternate-file 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(which-func ((t (:foreground "#ff6600")))))
(put 'dired-find-alternate-file 'disabled nil)
