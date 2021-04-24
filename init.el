;;; -*- lexical-binding: t -*-


(add-to-list 'load-path "/usr/share/emacs/site-lisp")

(setq custom-file "~/.emacs.d/custom.el")

(setq package-archives
      '(("gnu"   . "http://mirrors.ustc.edu.cn/elpa/gnu/")
        ("melpa" . "http://mirrors.ustc.edu.cn/elpa/melpa/")))

(setq package-selected-packages '(which-key
                                  selectrum
                                  key-chord
                                  avy
                                  wgrep
                                  eglot
                                  htmlize))

(require 'package)

(unless (package-installed-p 'which-key)
  (package-refresh-contents)
  (dolist (pkg package-selected-packages)
    (package-install pkg)))


(setq visible-bell t)

(setq inhibit-splash-screen t)

(menu-bar-mode -1)
(blink-cursor-mode -1)

(load-theme 'modus-vivendi)

(global-set-key (kbd "<f2>") 'tmm-menubar)


(setq confirm-kill-emacs 'y-or-n-p
      vc-handled-backends '(Git)
      vc-make-backup-files t
      version-control 'never
      backup-directory-alist '(("." . "~/.bak")))

(setq auto-save-visited-interval 30)

(auto-save-visited-mode 1)

(setq save-abbrevs nil)

(setq-default abbrev-mode t)

(setcdr (assq 'abbrev-mode minor-mode-alist) '(""))


(setq-default indent-tabs-mode nil)

(show-paren-mode 1)
(electric-pair-mode 1)

(global-set-key (kbd "M-R") 'raise-sexp)
(global-set-key (kbd "M-D") 'delete-pair)


(setq which-key-lighter nil
      which-key-idle-secondary-delay 0
      which-key-sort-order 'which-key-description-order)

(which-key-mode 1)


(setq disabled-command-function nil)

(defmacro +call-interactively (key cmd)
  `(defun ,(intern (concat "+" (symbol-name cmd))) ()
     (interactive)
     (setq last-command-event ,key
           this-command ',cmd
           real-this-command ',cmd)
     (call-interactively ',cmd)))

(global-set-key (kbd "C-z") (+call-interactively ?z repeat))

(repeat-mode 1)

(global-set-key (kbd "C-x U") 'undo-redo)
(define-key undo-repeat-map (kbd "U") 'undo-redo)
(put 'undo-redo 'repeat-map 'undo-repeat-map)

(define-key other-window-repeat-map (kbd "0") 'delete-window)
(define-key other-window-repeat-map (kbd "1") 'delete-other-windows)
(put 'delete-window 'repeat-map 'other-window-repeat-map)

(global-set-key (kbd "M-o") (+call-interactively ?o other-window))


(setq view-read-only t)

(with-eval-after-load 'view
  (define-key view-mode-map (kbd "e") 'View-scroll-line-forward))

(setq avy-styles-alist '((avy-goto-line . words)))

(key-chord-mode 1)

(key-chord-define-global "jk" 'view-mode)
(key-chord-define-global "kk" 'avy-goto-line)
(key-chord-define-global "jj" 'avy-goto-char-timer)


(defun +selectrum-filter (query candidates)
  (let ((regexp (string-join (split-string query) ".*")))
    (condition-case error
        (seq-filter (lambda (candidate) (string-match-p regexp candidate))
                    candidates)
      (invalid-regexp nil))))

(setq selectrum-refine-candidates-function '+selectrum-filter)

(selectrum-mode 1)

(defun +project-switch-project ()
  (interactive)
  (require 'project)
  (let ((default-directory (project-prompt-project-dir))
        (command (lookup-key project-prefix-map
                             (vector (read-char-exclusive)))))
    (if command (call-interactively command))))

(define-key project-prefix-map (kbd "p") '+project-switch-project)

(global-set-key (kbd "C-c C-j") 'imenu)

(defun +tab-completion-filter (command)
  (if (or (use-region-p)
          (<= (current-column)
              (current-indentation)))
      command
    'completion-at-point))

(define-key prog-mode-map (kbd "TAB")
  '(menu-item "" indent-for-tab-command :filter +tab-completion-filter))

(with-eval-after-load 'cc-mode
  (define-key c-mode-base-map (kbd "TAB")
    `(menu-item "" c-indent-line-or-region :filter +tab-completion-filter)))

(with-eval-after-load 'flymake
  (define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
  (define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error))

(setq flymake-cc-command '("gcc" "-x" "c++" "-fsyntax-only" "-"))

(setq eglot-ignored-server-capabilites '(:hoverProvider))


(setq dired-listing-switches "-alh")

(defun +dired-do-xdg-open ()
  (interactive)
  (dolist (file (dired-get-marked-files))
    (call-process-shell-command (concat "xdg-open " file))))

(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "V") '+dired-do-xdg-open))

(defun rg ()
  (interactive)
  (require 'grep)
  (grep--save-buffers)
  (compilation-start
   (read-shell-command "command: " "rg --no-heading " 'grep-history)
   'grep-mode))

(setq wgrep-auto-save-buffer t
      wgrep-change-readonly-file t)


(setq org-modules '(org-tempo)
      org-export-backends '(html latex)
      org-html-postamble nil
      org-html-validation-link nil
      org-special-ctrl-a/e t
      org-link-descriptive nil)

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "<") (lambda () (interactive) (insert ?<))))
