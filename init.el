;;; -*- lexical-binding: t -*-


(setq custom-file "~/.emacs.d/custom.el")

(setq package-archives
      '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
        ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))

(setq package-selected-packages '(which-key
                                  selectrum
                                  orderless
                                  yasnippet
                                  wgrep
                                  eglot
                                  htmlize
                                  pyim
                                  posframe))

(require 'package)

(unless (package-installed-p 'which-key)
  (package-refresh-contents)
  (dolist (pkg package-selected-packages)
    (package-install pkg)))


(setq inhibit-splash-screen t)

(global-set-key (kbd "<f2>") 'tmm-menubar)
(global-set-key (kbd "<f10>") 'toggle-frame-maximized)


(setq-default indent-tabs-mode nil)

(show-paren-mode 1)
(electric-pair-mode 1)

(defun +insert-pair (&optional arg)
  (interactive "P")
  (insert-pair (or arg 1)))

(dolist (key '("(" "[" "{" "`" "'" "\""))
  (global-set-key (kbd (concat "M-" key)) '+insert-pair))

(global-set-key (kbd "M-R") 'raise-sexp)
(global-set-key (kbd "M-D") 'delete-pair)
(global-set-key (kbd "C-M-DEL") 'backward-kill-sexp)
(global-set-key (kbd "C-M-<backspace>") 'backward-kill-sexp)


(setq which-key-lighter nil
      which-key-sort-order '+wk-prefix-then-des-order)

(defun +wk-prefix-then-des-order (acons bcons)
  (let ((apref? (which-key--group-p (cdr acons)))
        (bpref? (which-key--group-p (cdr bcons))))
    (if (xor apref? bpref?)
        apref?
      (which-key-description-order acons bcons))))

(which-key-mode 1)


(setq enable-recursive-minibuffers t
      completion-styles '(orderless)
      orderless-matching-styles '(orderless-regexp)
      orderless-style-dispatchers '(+orderless-without-if-bang)
      selectrum-refine-candidates-function 'orderless-filter
      selectrum-highlight-candidates-function 'orderless-highlight-matches)

(defun +orderless-without-if-bang (pattern _index _total)
  (if (string-prefix-p "!" pattern)
      `(orderless-without-literal . ,(substring pattern 1))))

(selectrum-mode 1)

(global-set-key (kbd "C-x C-z") 'selectrum-repeat)


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

(setq eglot-ignored-server-capabilites '(:hoverProvider))

(global-set-key (kbd "C-c C-j") 'imenu)


(let ((file "~/.emacs.d/rsync/private.el"))
  (if (file-exists-p file) (load-file file)))

(setq bookmark-default-file "~/.emacs.d/rsync/bookmarks")


(setq yas-alias-to-yas/prefix-p nil)

(yas-global-mode 1)

(setcdr (assq 'yas-minor-mode minor-mode-alist) '(""))

(setq hippie-expand-try-functions-list
      '(yas-hippie-try-expand
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol
        try-expand-line
        try-expand-line-all-buffers))

(global-set-key (kbd "M-/") 'hippie-expand)


(setq isearch-lazy-count t
      disabled-command-function nil)

(global-set-key (kbd "C-z") 'repeat)
(global-set-key (kbd "C-?") 'undo-redo)
(global-set-key (kbd "C-.") 'view-mode)
(global-set-key (kbd "M-o") 'other-window)

(ffap-bindings)


(setq confirm-kill-emacs 'y-or-n-p
      auto-save-visited-interval 30
      vc-handled-backends '(Git)
      vc-make-backup-files t
      version-control 'never
      backup-directory-alist '(("." . "~/.bak")))

(auto-save-visited-mode 1)


(setq dired-listing-switches "-alh")

(defun +dired-do-xdg-open ()
  (interactive)
  (dolist (file (dired-get-marked-files))
    (call-process-shell-command
     (concat "xdg-open " file))))

(with-eval-after-load 'dired
  (require 'dired-x)
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


(setq ediff-window-setup-function 'ediff-setup-windows-plain
      ediff-split-window-function 'split-window-horizontally)


(setq org-modules '(org-tempo)
      org-babel-load-languages
      '((emacs-lisp . t) (shell . t) (C . t) (python . t))
      org-babel-python-command "python3"
      org-export-backends '(html latex)
      org-html-postamble nil
      org-html-validation-link nil
      org-special-ctrl-a/e t
      org-link-descriptive nil
      org-link-frame-setup '((file . find-file))
      org-src-preserve-indentation t
      org-src-window-setup 'current-window)

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "<") (lambda () (interactive) (insert ?<))))


(defvar +text-scale-list
  [(9.0  . 9.0)
   (10.0 . 10.5)
   (11.5 . 12.0)
   (12.5 . 13.5)
   (14.0 . 15.0)
   (15.0 . 15.0)
   (16.0 . 16.5)
   (18.0 . 18.0)])

(defvar +text-scale-index 4)

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
  (when (< +text-scale-index 7)
    (setq +text-scale-index (1+ +text-scale-index))
    (+text-scale-set)))

(defun +text-scale-decrease ()
  (interactive)
  (when (> +text-scale-index 0)
    (setq +text-scale-index (1- +text-scale-index))
    (+text-scale-set)))

(global-set-key (kbd "C-+") '+text-scale-increase)
(global-set-key (kbd "C-_") '+text-scale-decrease)


(setq default-input-method "pyim"
      posframe-mouse-banish nil
      pyim-page-tooltip 'posframe
      pyim-default-scheme 'ziranma-shuangpin
      pyim-autoselector nil
      pyim-enable-shortcode nil
      pyim-fuzzy-pinyin-alist nil)

(defun +pyim-probe-god-mode-p () god-local-mode)

(setq-default pyim-english-input-switch-functions '(+pyim-probe-god-mode-p))

(advice-add 'pyim-punctuation-full-width-p :override 'ignore)

(with-eval-after-load 'pyim
  ;; fix zirjma
  (setcdr (last (car (last (assq 'ziranma-shuangpin pyim-schemes))))
          '(("aj" "an") ("al" "ai") ("ak" "ao")
            ("ez" "ei") ("ef" "en") ("ob" "ou")))
  (define-key pyim-mode-map (kbd ".") 'pyim-page-next-page)
  (define-key pyim-mode-map (kbd ",") 'pyim-page-previous-page)
  (pyim-basedict-enable))
