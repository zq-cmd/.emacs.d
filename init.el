;;; -*- lexical-binding: t -*-


(setq custom-file "~/.emacs.d/custom.el")

(setq package-archives
      '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
        ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))

(setq package-selected-packages '(which-key
                                  selectrum
                                  key-chord
                                  avy
                                  wgrep
                                  eglot
                                  macrostep
                                  htmlize
                                  cdlatex))

(require 'package)

(unless (package-installed-p 'which-key)
  (package-refresh-contents)
  (dolist (pkg package-selected-packages)
    (package-install pkg)))


(setq visible-bell t)

(setq inhibit-splash-screen t)

(if (display-graphic-p)
    (load-theme 'modus-operandi)
  (menu-bar-mode -1))

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

(repeat-mode 1)

(define-key undo-repeat-map (kbd "r") 'undo-redo)

(global-set-key (kbd "C-z") 'repeat)
(global-set-key (kbd "C-?") 'undo-redo)
(global-set-key (kbd "M-z") 'view-mode)
(global-set-key (kbd "M-o") 'other-window)

(key-chord-mode 1)

(key-chord-define-global "jk" 'view-mode)
(key-chord-define-global "jj" 'avy-goto-char)
(key-chord-define-global "kk" 'avy-goto-line)


(setq selectrum-refine-candidates-function '+selectrum-filter)

(defun +selectrum-filter (query candidates)
  (let ((regexp (string-join (split-string query) ".*")))
    (condition-case error
        (seq-filter (lambda (candidate) (string-match-p regexp candidate))
                    candidates)
      (invalid-regexp nil))))

(selectrum-mode 1)

(global-set-key (kbd "C-x C-z") 'selectrum-repeat)

(global-set-key (kbd "C-c C-j") 'imenu)

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

(define-key prog-mode-map (kbd "TAB")
  '(menu-item "" indent-for-tab-command :filter +tab-completion-filter))


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

(define-key emacs-lisp-mode-map (kbd "C-c e") 'macrostep-expand)
(define-key lisp-interaction-mode-map (kbd "C-c e") 'macrostep-expand)


(setq org-modules '(org-tempo org-mouse)
      org-export-backends '(html latex)
      org-html-postamble nil
      org-html-validation-link nil
      org-special-ctrl-a/e t
      org-use-speed-commands t
      org-id-track-globally nil
      org-attach-method 'lns
      org-link-descriptive nil
      org-link-frame-setup '((file . find-file))
      org-src-window-setup 'current-window
      org-file-apps
      '(("\\.pdf\\'" . #1=(lambda (file link)
                            (call-process-shell-command
                             (concat "xdg-open " file))))
        ("\\.x?html?\\'" . #1#)
        (auto-mode . emacs)
        (directory . emacs)))

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
