;;; -*- lexical-binding: t -*-


(setq custom-file "~/.emacs.d/custom.el")

(setq package-archives
      '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
        ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))

(setq package-selected-packages '(which-key
                                  helm
                                  wgrep-helm
                                  eglot
                                  htmlize
                                  cdlatex
                                  pyim
                                  posframe))

(require 'package)

(unless (package-installed-p 'which-key)
  (package-refresh-contents)
  (dolist (pkg package-selected-packages)
    (package-install pkg)))


(setq inhibit-splash-screen t)

(load-theme 'tango)

(global-set-key (kbd "<f2>") 'tmm-menubar)
(global-set-key (kbd "<f10>") 'toggle-frame-maximized)

(winner-mode 1)


(setq-default indent-tabs-mode nil)

(show-paren-mode 1)
(electric-pair-mode 1)

(global-set-key (kbd "M-R") 'raise-sexp)
(global-set-key (kbd "M-D") 'delete-pair)
(global-set-key (kbd "C-M-<backspace>") 'backward-kill-sexp)


(setq which-key-lighter nil
      which-key-prefix-prefix nil
      which-key-replacement-alist nil
      which-key-idle-secondary-delay 0
      which-key-sort-order 'which-key-description-order)

(which-key-mode 1)


(setq confirm-kill-emacs 'y-or-n-p
      vc-handled-backends '(Git)
      vc-make-backup-files t
      version-control 'never
      backup-directory-alist '(("." . "~/.bak")))

(setq auto-save-visited-interval 30)

(auto-save-visited-mode 1)

(setq recentf-max-saved-items 100)

(recentf-mode 1)

(setq-default abbrev-mode t)


(setq view-read-only t
      disabled-command-function nil)

(global-set-key (kbd "C-z") 'view-mode)
(global-set-key (kbd "C-?") 'undo-redo)
(global-set-key (kbd "M-o") 'other-window)

(with-eval-after-load 'view
  (define-key view-mode-map "j" 'View-scroll-line-forward)
  (define-key view-mode-map "k" 'View-scroll-line-backward))


(setq completion-styles '(helm)
      helm-completion-style 'helm
      helm-turn-on-show-completion nil
      helm-completion-mode-string nil)

(setq helm-allow-mouse t
      helm-inherit-input-method nil
      helm-buffer-max-length 30
      helm-buffer-skip-remote-checking t
      helm-mini-default-sources
      '(helm-source-buffers-list
        helm-source-recentf
        helm-source-bookmarks
        helm-source-buffer-not-found)
      helm-occur-use-ioccur-style-keys t
      helm-grep-ag-command "rg --no-heading %s %s %s"
      helm-ff-preferred-shell-mode 'shell-mode
      helm-ff-guess-ffap-filenames t
      helm-for-files-preferred-list
      '(helm-source-files-in-current-dir))

(helm-mode 1)


(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-c z") 'helm-resume)
(global-set-key (kbd "C-c f") 'helm-for-files)
(global-set-key (kbd "C-c g") 'helm-grep-do-git-grep)
(global-set-key (kbd "C-c o") 'helm-occur)
(global-set-key (kbd "C-c i") 'helm-imenu)
(global-set-key (kbd "C-c t") 'helm-etags-select)
(global-set-key (kbd "C-c r") 'helm-register)
(global-set-key (kbd "C-c y") 'helm-show-kill-ring)
(global-set-key (kbd "C-c m") 'helm-all-mark-rings)
(global-set-key (kbd "C-c C-x") 'helm-run-external-command)

(define-key help-map (kbd "o") 'helm-apropos)

(define-key comint-mode-map (kbd "M-r") 'helm-comint-input-ring)
(define-key comint-mode-map (kbd "C-c C-j") 'helm-comint-prompts)

(ffap-bindings)

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

(with-eval-after-load 'cc-mode
  (define-key c-mode-base-map (kbd "TAB")
    '(menu-item "" c-indent-line-or-region :filter +tab-completion-filter)))


(setq dired-listing-switches "-alh")

(setq ediff-window-setup-function 'ediff-setup-windows-plain
      ediff-split-window-function 'split-window-horizontally)

(setq wgrep-auto-save-buffer t
      wgrep-change-readonly-file t)

(setq eglot-ignored-server-capabilites '(:hoverProvider))


(setq org-modules '(org-tempo org-mouse)
      org-babel-load-languages
      '((emacs-lisp . t) (shell . t) (python . t))
      org-babel-python-command "python3"
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


(setq default-input-method "pyim"
      posframe-mouse-banish nil
      pyim-page-tooltip 'posframe
      pyim-default-scheme 'ziranma-shuangpin
      pyim-autoselector nil
      pyim-enable-shortcode nil
      pyim-fuzzy-pinyin-alist nil)

(add-hook 'org-mode-hook
          (lambda ()
            (setq-local pyim-english-input-switch-functions
                        '(pyim-probe-org-speed-commands
                          pyim-probe-org-structure-template))))

(advice-add 'pyim-punctuation-full-width-p :override 'ignore)

(with-eval-after-load 'pyim
  (setcdr (last (car (last (assq 'ziranma-shuangpin pyim-schemes))))
          '(("aj" "an") ("al" "ai") ("ak" "ao") ("ez" "ei") ("ef" "en") ("ob" "ou")))
  (define-key pyim-mode-map (kbd ".") 'pyim-page-next-page)
  (define-key pyim-mode-map (kbd ",") 'pyim-page-previous-page)
  (pyim-basedict-enable))


(defvar +windowsp (eq system-type 'windows-nt))

(defvar +text-en-name (if +windowsp "Consolas" "Ubuntu Mono"))
(defvar +text-zh-name (if +windowsp "微软雅黑" "WenQuanYi Micro Hei Mono"))

(defvar +text-scale-list
  (if +windowsp
      [(9.0 . 10.0) (11.5 . 11.0) (14.0 . 15.0) (16.0 . 18.0)]
    [(9.0 . 9.0) (11.5 . 12.0) (14.0 . 15.0) (16.0 . 16.5)]))

(defvar +text-scale-index 2)

(defun +text-scale-set ()
  (when (display-graphic-p)
    (let ((scale (aref +text-scale-list +text-scale-index)))
      (set-face-attribute
       'default nil
       :font (font-spec :name +text-en-name :size (car scale)))
      (set-fontset-font
       (frame-parameter nil 'font)
       'han
       (font-spec :name +text-zh-name :size (cdr scale))))))

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

(global-set-key (kbd "C-+") '+text-scale-increase)
(global-set-key (kbd "C-_") '+text-scale-decrease)
