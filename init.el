;;; -*- lexical-binding: t -*-


(setq custom-file "~/.emacs.d/custom.el")

(setq package-archives
      '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
        ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))

(setq package-selected-packages '(god-mode
                                  which-key
                                  yasnippet
                                  eglot
                                  wgrep
                                  htmlize
                                  pdf-tools
                                  pyim
                                  posframe))

(require 'package)

(unless (package-installed-p 'god-mode)
  (package-refresh-contents)
  (dolist (pkg package-selected-packages)
    (package-install pkg)))


(tooltip-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)

(setq visible-bell t)

(setq inhibit-splash-screen t)

(global-set-key (kbd "<f2>") 'tmm-menubar)
(global-set-key (kbd "<f10>") 'toggle-frame-maximized)

(if (display-graphic-p)
    (load-theme 'deeper-blue))


(require 'page-ext)

(winner-mode 1)

(global-set-key (kbd "M-o") 'other-window)

(global-set-key (kbd "C-x C-0") 'delete-window)
(global-set-key (kbd "C-x C-1") 'delete-other-windows)
(global-set-key (kbd "C-x C-2") 'split-window-below)
(global-set-key (kbd "C-x C-3") 'split-window-right)

(global-set-key (kbd "C-x C-4") ctl-x-4-map)
(global-set-key (kbd "C-x C-5") ctl-x-5-map)

(define-key ctl-x-5-map (kbd "C-0") 'delete-frame)
(define-key ctl-x-4-map (kbd "C-b") 'switch-to-buffer-other-window)
(define-key ctl-x-5-map (kbd "C-b") 'switch-to-buffer-other-frame)

(global-set-key (kbd "C-x b") 'list-buffers)
(global-set-key (kbd "C-x C-b") 'switch-to-buffer)


(global-set-key (kbd "C-.") 'imenu)
(global-set-key (kbd "C-z") 'repeat)
(global-set-key (kbd "C-?") 'undo-redo)
(global-set-key (kbd "M-Z") 'zap-up-to-char)

(define-key universal-argument-map (kbd "u") 'universal-argument-more)

(setq isearch-lazy-count t)

(define-key isearch-mode-map (kbd "<right>") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "<left>") 'isearch-repeat-backward)

(define-key special-mode-map (kbd "z") 'repeat)
(define-key special-mode-map (kbd "n") 'next-line)
(define-key special-mode-map (kbd "p") 'previous-line)
(define-key special-mode-map (kbd "s") 'isearch-forward)
(define-key special-mode-map (kbd "r") 'isearch-backward)
(define-key special-mode-map (kbd "x") 'god-mode-self-insert)
(define-key special-mode-map (kbd "c") 'god-mode-self-insert)


(setq god-mode-enable-function-key-translation nil
      god-mode-alist '((nil . "C-") ("g" . "M-") ("h" . "C-M-")))

(require 'god-mode)

(global-set-key (kbd "<escape>") 'god-local-mode)
(global-set-key (kbd "C-x g") 'god-local-mode)

;; god mode remap self-insert-command
(defun +self-insert-command () (interactive) (self-insert-command 1))

;; surround region in god mode
(dolist (key '("(" ")" "[" "]" "{" "}" "`" "'" "\""))
  (define-key god-local-mode-map (kbd key) '+self-insert-command))

(define-key god-local-mode-map (kbd "q") 'quit-window)
(define-key god-local-mode-map (kbd "<") 'beginning-of-buffer)
(define-key god-local-mode-map (kbd ">") 'end-of-buffer)
(define-key god-local-mode-map (kbd "SPC") 'scroll-up-command)
(define-key god-local-mode-map (kbd "S-SPC") 'scroll-down-command)

(god-mode-all)


(defun +wk-prefix-then-des-order (acons bcons)
  (let ((apref? (which-key--group-p (cdr acons)))
        (bpref? (which-key--group-p (cdr bcons))))
    (if (xor apref? bpref?)
        apref?
      (which-key-description-order acons bcons))))

(setq which-key-lighter nil
      which-key-show-early-on-C-h t
      which-key-add-column-padding 2
      which-key-idle-secondary-delay 0
      which-key-sort-order '+wk-prefix-then-des-order)

(which-key-mode 1)

(which-key-enable-god-mode-support)


(setq-default indent-tabs-mode nil)

(show-paren-mode 1)
(electric-pair-mode 1)

(global-set-key (kbd "M-R") 'raise-sexp)
(global-set-key (kbd "M-D") 'delete-pair)


(setq confirm-kill-emacs 'y-or-n-p
      disabled-command-function nil
      vc-handled-backends '(Git)
      vc-make-backup-files t
      version-control 'never
      backup-directory-alist '(("." . "~/.bak"))
      bookmark-default-file "~/.emacs.d/rsync/bookmarks")

(auto-save-visited-mode 1)


(setq dired-listing-switches "-alh")

(defun +dired-do-xdg-open ()
  (interactive)
  (dolist (file (dired-get-marked-files))
    (call-process-shell-command
     (concat "xdg-open " file))))

(with-eval-after-load 'dired
  (require 'dired-x)
  (define-key dired-mode-map (kbd "v") '+dired-do-xdg-open))


(advice-add 'project-eshell :override 'project-shell)


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


(setq yas-alias-to-yas/prefix-p nil
      yas-snippet-dirs '("~/.emacs.d/rsync/snippets"))

(setq yas-minor-mode-map
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "C-x C-a C-s") 'yas-insert-snippet)
        (define-key map (kbd "C-x C-a C-n") 'yas-new-snippet)
        (define-key map (kbd "C-x C-a C-v") 'yas-visit-snippet-file)
        map))

(yas-global-mode 1)

(define-key yas-minor-mode-map (kbd "TAB") yas-maybe-expand)

(setcdr (assq 'yas-minor-mode minor-mode-alist) '(""))


(setq completion-styles '(basic))

(setq hippie-expand-try-functions-list
      '(try-complete-file-name-partially
        try-complete-file-name
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

(global-set-key (kbd "M-/") 'hippie-expand)


(setq recentf-max-saved-items 100)

(recentf-mode 1)

(setq ido-enable-regexp t
      ido-use-virtual-buffers t
      ido-use-url-at-point t
      ido-use-filename-at-point 'guess)

(require 'ido)

(ido-everywhere 1)

(define-key ido-common-completion-map (kbd "SPC")
  (lambda () (interactive) (insert (if ido-enable-regexp ".*" " "))))


(setq enable-recursive-minibuffers t)

(defun +ido-completing-read
    (prompt collection &optional predicate require-match
            initial-input hist def inherit-input-method)
  (let ((choices (all-completions (or initial-input "") collection predicate)))
    (ido-completing-read prompt choices nil require-match
                         nil hist def inherit-input-method)))

(setq completing-read-function '+ido-completing-read)

(defun +ido-completion-in-region (beg end collection &optional predicate)
  (let* ((prefix (buffer-substring beg end))
         (choices (all-completions prefix collection predicate))
         (choice (cond ((not choices) nil)
                       ((not (cdr choices)) (car choices))
                       (t (ido-completing-read "complete: " choices nil t)))))
    (if (string-prefix-p prefix choice)
        (insert (substring choice (- end beg))))))

(setq completion-in-region-function '+ido-completion-in-region)


(defun +tab-completion (&optional arg)
  (interactive "P")
  (if (or (use-region-p)
          (<= (point) (save-excursion (back-to-indentation) (point))))
      (indent-for-tab-command arg)
    (completion-at-point)))

(define-key prog-mode-map (kbd "TAB") '+tab-completion)


(setq python-indent-guess-indent-offset nil
      python-shell-interpreter "python3"
      org-babel-python-command "python3")


(setq org-modules '(org-tempo)
      org-babel-load-languages
      '((emacs-lisp . t) (shell . t) (C . t) (python . t))
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


(with-eval-after-load 'pdf-tools
  (pdf-tools-install))

(autoload 'pdf-view-mode "pdf-tools" "pdf tools" t)

(add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode))


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

(defun +text-scale-set (&optional frame)
  (when (display-graphic-p)
    (let ((scale (aref +text-scale-list +text-scale-index)))
      (set-face-attribute
       'default frame
       :font (font-spec :name "Ubuntu Mono" :size (car scale)))
      (set-fontset-font
       (frame-parameter frame 'font)
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

(add-hook 'org-mode-hook
          (lambda ()
            (setq pyim-english-input-switch-functions
                  '(+pyim-probe-god-mode-p org-inside-LaTeX-fragment-p))))

(advice-add 'pyim-punctuation-full-width-p :override 'ignore)

(with-eval-after-load 'pyim
  ;; fix zirjma
  (setcdr (last (car (last (assq 'ziranma-shuangpin pyim-schemes))))
          '(("aj" "an") ("al" "ai") ("ao" "ak")
            ("ez" "ei") ("ef" "en") ("ou" "ob")))
  (define-key pyim-mode-map (kbd ".") 'pyim-page-next-page)
  (define-key pyim-mode-map (kbd ",") 'pyim-page-previous-page)
  (pyim-basedict-enable))
