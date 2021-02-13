;;; -*- lexical-binding: t -*-


(setq custom-file "~/.emacs.d/custom.el")

(setq package-archives
      '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
        ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))

(setq package-selected-packages '(which-key
                                  yasnippet
                                  helm
                                  wgrep-helm
                                  eglot
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
(global-set-key (kbd "C-M-<backspace>") 'backward-kill-sexp)


(setq which-key-lighter nil
      which-key-idle-secondary-delay 0
      which-key-sort-order '+wk-prefix-then-des-order)

(defun +wk-prefix-then-des-order (acons bcons)
  (let ((apref? (which-key--group-p (cdr acons)))
        (bpref? (which-key--group-p (cdr bcons))))
    (if (xor apref? bpref?)
        apref?
      (which-key-description-order acons bcons))))

(which-key-mode 1)


(setq enable-recursive-minibuffers t
      completion-styles '(helm)
      helm-completion-style 'helm
      helm-completion-mode-string nil)

(setq helm-allow-mouse t
      helm-turn-on-show-completion nil
      helm-buffer-max-length 30
      helm-buffer-skip-remote-checking t
      helm-occur-use-ioccur-style-keys t
      helm-grep-ag-command "rg --no-heading %s %s %s"
      helm-ff-guess-ffap-filenames t
      helm-for-files-preferred-list
      '(helm-source-bookmarks
        helm-source-files-in-current-dir)
      helm-bookmark-default-filtered-sources
      '(helm-source-bookmark-helm-find-files
        helm-source-bookmark-files&dirs
        helm-source-bookmark-uncategorized
        helm-source-bookmark-set))

(helm-mode 1)

(remove-hook 'helm-cleanup-hook 'helm-handle-winner-boring-buffers)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x r g") 'helm-register)
(global-set-key (kbd "C-x r b") 'helm-filtered-bookmarks)

(define-key helm-command-map (kbd "o") 'helm-occur)
(define-key helm-command-map (kbd "g") 'helm-do-grep-ag)
(define-key helm-command-map (kbd "l") 'helm-locate-library)


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


(let ((file "~/sync/emacs/private.el"))
  (if (file-exists-p file) (load-file file)))

(setq bookmark-default-file "~/sync/emacs/bookmarks")


(setq yas-alias-to-yas/prefix-p nil
      yas-prompt-functions '(yas-completing-prompt))

(yas-global-mode 1)

(setcdr (assq 'yas-minor-mode minor-mode-alist) '(""))

(global-set-key (kbd "C-x y") 'yas-insert-snippet)


(setq view-read-only t
      isearch-lazy-count t
      disabled-command-function nil)

(global-set-key (kbd "C-z") 'repeat)
(global-set-key (kbd "C-?") 'undo-redo)
(global-set-key (kbd "M-o") 'other-window)


(setq confirm-kill-emacs 'y-or-n-p
      auto-save-visited-interval 30
      vc-handled-backends '(Git)
      vc-make-backup-files t
      version-control 'never
      backup-directory-alist '(("." . "~/.bak")))

(auto-save-visited-mode 1)


(setq dired-listing-switches "-alh")


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


(define-key comint-mode-map (kbd "M-r") 'helm-comint-input-ring)
(define-key comint-mode-map (kbd "C-c C-j") 'helm-comint-prompts)

(setq eshell-modules-list
      '(eshell-alias
        eshell-basic
        eshell-cmpl
        eshell-dirs
        eshell-glob
        eshell-hist
        eshell-ls
        eshell-pred
        eshell-prompt
        eshell-tramp
        eshell-unix)
      eshell-aliases-file "~/sync/emacs/alias"
      eshell-cd-on-directory nil)

(with-eval-after-load 'em-hist
  (define-key eshell-hist-mode-map (kbd "M-s") nil)
  (define-key eshell-hist-mode-map (kbd "M-r") 'helm-eshell-history)
  (define-key eshell-hist-mode-map (kbd "C-c C-j") 'helm-eshell-prompts))


(setq python-guess-indent nil
      org-babel-python-command "python3")


(setq org-modules '(org-tempo org-mouse ol-eshell)
      org-babel-load-languages
      '((emacs-lisp . t) (shell . t) (eshell . t) (C . t) (python . t))
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


(setq org-capture-bookmark nil
      org-default-notes-file "~/sync/emacs/notes.org")

(defvar +org-global-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "a") 'org-agenda)
    (define-key map (kbd "c") 'org-capture)
    (define-key map (kbd ".") 'org-time-stamp)
    (define-key map (kbd "w") 'org-store-link)
    (define-key map (kbd "l") 'org-insert-link-global)
    (define-key map (kbd "o") 'org-open-at-point-global)
    (define-key map (kbd "n") 'org-next-link)
    (define-key map (kbd "p") 'org-previous-link)
    (define-key map (kbd "g") 'org-mark-ring-goto)
    map))

(global-set-key (kbd "C-x C-a") +org-global-map)


(defun +project-agenda ()
  (interactive)
  (require 'project)
  (let* ((root (project-root (project-current t)))
         (agenda (expand-file-name "agenda.txt" root)))
    (setq org-directory root
          org-agenda-files (if (file-exists-p agenda)
                               agenda (list root))))
  (call-interactively 'org-agenda))

(define-key project-prefix-map (kbd "a") '+project-agenda)

(add-to-list 'project-switch-commands '(+project-agenda "Agenda"))


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

(add-hook 'org-mode-hook
          (lambda ()
            (setq-local pyim-english-input-switch-functions
                        '(pyim-probe-org-speed-commands
                          pyim-probe-org-structure-template))))

(advice-add 'pyim-punctuation-full-width-p :override 'ignore)

(with-eval-after-load 'pyim
  ;; fix zirjma
  (setcdr (last (car (last (assq 'ziranma-shuangpin pyim-schemes))))
          '(("aj" "an") ("al" "ai") ("ak" "ao")
            ("ez" "ei") ("ef" "en") ("ob" "ou")))
  (define-key pyim-mode-map (kbd ".") 'pyim-page-next-page)
  (define-key pyim-mode-map (kbd ",") 'pyim-page-previous-page)
  (pyim-basedict-enable))
