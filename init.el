;;; -*- lexical-binding: t -*-


(setq custom-file "~/.emacs.d/custom.el")

(setq package-archives
      '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
        ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))

(setq package-selected-packages '(which-key
                                  selectrum
                                  wgrep
                                  inf-ruby
                                  htmlize
                                  cdlatex
                                  pdf-tools
                                  pyim
                                  posframe))

(require 'package)

(unless (package-installed-p 'which-key)
  (package-refresh-contents)
  (dolist (pkg package-selected-packages)
    (package-install pkg)))


(defvar +graphicp (display-graphic-p))
(defvar +windowsp (eq system-type 'windows-nt))


(setq visible-bell t)

(setq inhibit-splash-screen t)

(if +graphicp
    (load-theme 'tango)
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

(setq-default abbrev-mode t)


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


(setq view-read-only t
      disabled-command-function nil)

(global-set-key (kbd "C-z") 'view-mode)
(global-set-key (kbd "C-?") 'undo-redo)
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-Z") 'zap-up-to-char)

(with-eval-after-load 'view
  (define-key view-mode-map "j" 'View-scroll-line-forward)
  (define-key view-mode-map "k" 'View-scroll-line-backward))


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

(with-eval-after-load 'cc-mode
  (define-key c-mode-base-map (kbd "TAB")
    '(menu-item "" c-indent-line-or-region :filter +tab-completion-filter)))


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
      org-src-window-setup 'current-window)

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "<") (lambda () (interactive) (insert ?<))))

(provide 'texmathp)
(defun texmathp () t)
(add-hook 'org-mode-hook 'org-cdlatex-mode)


(with-eval-after-load 'pdf-tools
  (pdf-tools-install))

(autoload 'pdf-view-mode "pdf-tools" "pdf tools" t)

(unless +windowsp
  (add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode)))


(setq default-input-method "pyim"
      posframe-mouse-banish nil
      pyim-page-tooltip (if +graphicp 'posframe 'popup))

(setq pyim-autoselector nil
      pyim-enable-shortcode nil
      pyim-fuzzy-pinyin-alist nil
      pyim-default-scheme 'ziranma-shuangpin)

(add-hook 'org-mode-hook
          (lambda ()
            (setq-local pyim-english-input-switch-functions
                        '(pyim-probe-org-speed-commands
                          pyim-probe-org-structure-template))))

(advice-add 'pyim-punctuation-full-width-p :override 'ignore)

(with-eval-after-load 'pyim
  (pyim-scheme-add
   '(ziranma-shuangpin
     :document "自然码双拼方案。"
     :class shuangpin
     :first-chars "abcdefghijklmnopqrstuvwxyz"
     :rest-chars "abcdefghijklmnopqrstuvwxyz"
     :prefer-trigger-chars nil
     :keymaps
     (("a" "a" "a")
      ("b" "b" "ou")
      ("c" "c" "iao")
      ("d" "d" "uang" "iang")
      ("e" "e" "e")
      ("f" "f" "en")
      ("g" "g" "eng")
      ("h" "h" "ang")
      ("i" "ch" "i")
      ("j" "j" "an")
      ("k" "k" "ao")
      ("l" "l" "ai")
      ("m" "m" "ian")
      ("n" "n" "in")
      ("o" "o" "uo" "o")
      ("p" "p" "un")
      ("q" "q" "iu")
      ("r" "r" "uan" "er")
      ("s" "s" "iong" "ong")
      ("t" "t" "ue" "ve")
      ("u" "sh" "u")
      ("v" "zh" "v" "ui")
      ("w" "w" "ia" "ua")
      ("x" "x" "ie")
      ("y" "y" "uai" "ing")
      ("z" "z" "ei")
      ("aa" "a")
      ("aj" "an")
      ("ah" "ang")
      ("ai" "ai")
      ("ak" "ao")
      ("al" "ai")
      ("an" "an")
      ("ao" "ao")
      ("ee" "e")
      ("ef" "en")
      ("eg" "eng")
      ("ei" "ei")
      ("en" "en")
      ("er" "er")
      ("ez" "ei")
      ("ob" "ou")
      ("oo" "o")
      ("ou" "ou"))))
  (define-key pyim-mode-map (kbd ".") 'pyim-page-next-page)
  (define-key pyim-mode-map (kbd ",") 'pyim-page-previous-page)
  (pyim-basedict-enable))


(defvar +text-en-name (if +windowsp "Consolas" "Ubuntu Mono"))
(defvar +text-zh-name (if +windowsp "微软雅黑" "WenQuanYi Micro Hei Mono"))

(defvar +text-scale-list
  (if +windowsp
      [(9.0 . 10.5) (11.5 . 12.0) (14.0 . 15.0) (16.0 . 18.0)]
    [(9.0 . 9.0) (11.5 . 12.0) (14.0 . 15.0) (16.0 . 16.5)]))

(defvar +text-scale-index 1)

(defun +text-scale-set ()
  (when +graphicp
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
