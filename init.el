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
                                  imenu-list
                                  wgrep
                                  eglot
                                  sly-macrostep
                                  htmlize
                                  cdlatex))

(require 'package)

(unless (package-installed-p 'which-key)
  (package-refresh-contents)
  (dolist (pkg package-selected-packages)
    (package-install pkg)))


(setq visible-bell t)

(setq inhibit-splash-screen t)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)

(if (display-graphic-p)
    (load-theme 'tango))

(global-set-key (kbd "<f2>") 'tmm-menubar)
(global-set-key (kbd "<f10>") 'toggle-frame-maximized)


(setq confirm-kill-emacs 'y-or-n-p
      vc-handled-backends '(Git)
      vc-make-backup-files t
      version-control 'never
      backup-directory-alist '(("." . "~/.bak")))

(setq auto-save-visited-interval 30)

(auto-save-visited-mode 1)

(setq save-abbrevs nil)

(setq-default abbrev-mode t)


(setq-default indent-tabs-mode nil)

(show-paren-mode 1)
(electric-pair-mode 1)

(defun +insert-pair (&optional arg)
  (interactive "P")
  (insert-pair (or arg 1)))

(dolist (key '("M-(" "M-[" "M-\""))
  (global-set-key (kbd key) '+insert-pair))

(global-set-key (kbd "M-R") 'raise-sexp)
(global-set-key (kbd "M-D") 'delete-pair)
(global-set-key (kbd "C-M-<backspace>") 'backward-kill-sexp)


(setq which-key-lighter nil
      which-key-prefix-prefix nil
      which-key-replacement-alist nil
      which-key-idle-secondary-delay 0
      which-key-sort-order 'which-key-description-order)

(which-key-mode 1)


(setq view-read-only t
      disabled-command-function nil)

(global-set-key (kbd "C-z") 'repeat)
(global-set-key (kbd "C-?") 'undo-redo)
(global-set-key (kbd "M-o") 'other-window)

(repeat-mode 1)

(define-key undo-repeat-map (kbd "r") 'undo-redo)

(with-eval-after-load 'view
  (define-key view-mode-map (kbd "e") 'View-scroll-line-forward))

(key-chord-mode 1)

(key-chord-define-global "jk" 'view-mode)
(key-chord-define-global "kk" 'avy-goto-line)
(key-chord-define-global "jj" 'avy-goto-char-timer)


(setq selectrum-refine-candidates-function '+selectrum-filter)

(defun +selectrum-filter (query candidates)
  (let ((regexp (string-join (split-string query) ".*")))
    (condition-case error
        (seq-filter (lambda (candidate) (string-match-p regexp candidate))
                    candidates)
      (invalid-regexp nil))))

(selectrum-mode 1)

(global-set-key (kbd "C-x C-z") 'selectrum-repeat)

(defun +project-switch-project ()
  (interactive)
  (require 'project)
  (let ((default-directory (project-prompt-project-dir))
        (command (lookup-key project-prefix-map
                             (vector (read-char-exclusive)))))
    (if command (call-interactively command))))

(define-key project-prefix-map (kbd "p") '+project-switch-project)

(defun +tab-completion-filter (command)
  (if (or (use-region-p)
          (<= (current-column)
              (current-indentation)))
      command
    'completion-at-point))

(define-key prog-mode-map (kbd "<backtab>") 'indent-for-tab-command)
(define-key prog-mode-map (kbd "TAB")
  '(menu-item "" indent-for-tab-command :filter +tab-completion-filter))

(with-eval-after-load 'cc-mode
  (define-key c-mode-base-map (kbd "TAB")
    `(menu-item "" c-indent-line-or-region :filter +tab-completion-filter)))


(setq imenu-list-size 0.2
      imenu-list-position 'left)

(global-set-key (kbd "<f8>") 'imenu-list-smart-toggle)
(global-set-key (kbd "C-c C-j") 'imenu)

(setq dired-listing-switches "-alh")

(defun +dired-do-xdg-open ()
  (interactive)
  (dolist (file (dired-get-marked-files))
    (call-process-shell-command (concat "xdg-open " file))))

(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "V") '+dired-do-xdg-open))

(setq ediff-window-setup-function 'ediff-setup-windows-plain
      ediff-split-window-function 'split-window-horizontally)

(defun rg ()
  (interactive)
  (require 'grep)
  (grep--save-buffers)
  (compilation-start
   (read-shell-command "command: " "rg --no-heading " 'grep-history)
   'grep-mode))

(setq wgrep-auto-save-buffer t
      wgrep-change-readonly-file t)

(setq eglot-ignored-server-capabilites '(:hoverProvider))

(setq inferior-lisp-program "sbcl"
      common-lisp-hyperspec-root
      (concat "file://"
              (expand-file-name
               "~/Documents/hyperspec/HyperSpec/")))

(define-key lisp-mode-shared-map (kbd "C-c e") 'macrostep-expand)


(setq org-modules '(org-tempo)
      org-export-backends '(html latex)
      org-html-postamble nil
      org-html-validation-link nil
      org-special-ctrl-a/e t
      org-use-speed-commands t
      org-id-track-globally nil
      org-attach-method 'lns
      org-link-descriptive nil
      org-link-frame-setup '((file . find-file))
      org-src-window-setup 'current-window)

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "<") (lambda () (interactive) (insert ?<))))

(provide 'texmathp)
(defun texmathp () t)
(add-hook 'org-mode-hook 'org-cdlatex-mode)


(defvar +text-scale-list
  [(9.0 . 9.0) (11.5 . 12.0) (14.0 . 15.0) (16.0 . 16.5)])

(defvar +text-scale-index 1)

(defun +text-scale-set ()
  (when (display-graphic-p)
    (let ((scale (aref +text-scale-list +text-scale-index)))
      (set-face-attribute
       'default nil
       :font (font-spec :name "Ubuntu Mono" :size (car scale)))
      (set-fontset-font
       (frame-parameter nil 'font)
       'han
       (font-spec :name "WenQuanYi Micro Hei Mono" :size (cdr scale))))))

(+text-scale-set)

(defun +text-scale-increase ()
  (interactive)
  (when (< +text-scale-index 3)
    (setq +text-scale-index (1+ +text-scale-index))
    (+text-scale-set)))

(defun +text-scale-decrease ()
  (interactive)
  (when (> +text-scale-index 0)
    (setq +text-scale-index (1- +text-scale-index))
    (+text-scale-set)))

(global-set-key (kbd "C--") '+text-scale-decrease)
(global-set-key (kbd "C-=") '+text-scale-increase)
