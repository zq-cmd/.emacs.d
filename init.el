;;; -*- lexical-binding: t -*-


(defmacro +menu-item (&rest body)
  `'(menu-item "" nil :filter (lambda (&optional _) ,@body)))

(defmacro +menu-if (&rest body)
  `(+menu-item (if ,@body)))

(defmacro +setq-hook (hook &rest body)
  `(add-hook ,hook (lambda () (setq-local ,@body))))


(setq custom-file "~/.emacs.d/custom.el")

(setq package-archives
      '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
        ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))

(setq package-selected-packages '(god-mode
                                  which-key
                                  avy
                                  expand-region
                                  yasnippet
                                  company
                                  wgrep
                                  pyim
                                  posframe
                                  auctex
                                  cdlatex
                                  htmlize
                                  pdf-tools
                                  markdown-mode
                                  eglot))

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

(load-theme 'deeper-blue)


(require 'page-ext)

(global-set-key (kbd "C-S-l") (lambda () (interactive) (insert ?\C-l)))

(winner-mode 1)

(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-O") 'delete-other-windows)

(global-set-key (kbd "C-x C-o") 'other-window)
(global-set-key (kbd "C-x C-0") 'delete-window)
(global-set-key (kbd "C-x C-1") 'delete-other-windows)
(global-set-key (kbd "C-x C-2") 'split-window-below)
(global-set-key (kbd "C-x C-3") 'split-window-right)

(global-set-key (kbd "C-x C-4") ctl-x-4-map)
(global-set-key (kbd "C-x C-5") ctl-x-5-map)

(global-set-key (kbd "C-x C-8") 'winner-redo)
(global-set-key (kbd "C-x C-9") 'winner-undo)

(define-key ctl-x-5-map (kbd "C-0") 'delete-frame)
(define-key ctl-x-4-map (kbd "C-b") 'switch-to-buffer-other-window)
(define-key ctl-x-5-map (kbd "C-b") 'switch-to-buffer-other-frame)


(setq-default indent-tabs-mode nil)

(show-paren-mode 1)
(electric-pair-mode 1)

(global-set-key (kbd "C-S-r") 'raise-sexp)
(global-set-key (kbd "C-S-d") 'delete-pair)

(global-set-key (kbd "C-=") 'er/expand-region)

(with-eval-after-load 'expand-region
  (add-to-list 'er/try-expand-list 'mark-page))


(setq avy-background t
      avy-single-candidate-jump nil
      avy-goto-word-0-regexp "\\_<\\(\\sw\\|\\s_\\)")

(global-set-key (kbd "C-z") '+avy)
(define-key isearch-mode-map (kbd "C-z") 'avy-isearch)

(defun +avy (&optional arg)
  (interactive "P")
  (require 'avy)
  (if arg
      (avy-resume)
    (let ((char (read-char "char: ")))
      (cond
       ((= char ?\s)
        (avy-goto-word-0 nil))
       ((= char ?\C-m)
        (avy-goto-line))
       ((<= ?A char ?Z)
        (avy-goto-symbol-1 (downcase char)))
       (t
        (avy-with avy-goto-char
          (avy-jump
           (if (and current-input-method
                    (<= ?a char ?z))
               (pyim-cregexp-build char)
             (regexp-quote (string char))))))))))


(global-set-key (kbd "C-.") 'repeat)
(global-set-key (kbd "C-?") 'undo-redo)

(define-key universal-argument-map (kbd "u") 'universal-argument-more)

(define-key special-mode-map (kbd "z") '+avy)
(define-key special-mode-map (kbd ".") 'repeat)
(define-key special-mode-map (kbd "u") 'universal-argument)
(define-key special-mode-map (kbd "n") 'next-line)
(define-key special-mode-map (kbd "p") 'previous-line)
(define-key special-mode-map (kbd "f") 'forward-char)
(define-key special-mode-map (kbd "b") 'backward-char)
(define-key special-mode-map (kbd "a") 'move-beginning-of-line)
(define-key special-mode-map (kbd "e") 'move-end-of-line)
(define-key special-mode-map (kbd "s") 'isearch-forward)
(define-key special-mode-map (kbd "r") 'isearch-backward)
(define-key special-mode-map (kbd "x") 'god-mode-self-insert)
(define-key special-mode-map (kbd "c") 'god-mode-self-insert)


(setq confirm-kill-emacs 'y-or-n-p
      disabled-command-function nil
      vc-handled-backends '(Git)
      vc-make-backup-files t
      backup-directory-alist '(("." . "~/.bak"))
      tramp-completion-use-auth-sources nil
      bookmark-default-file "~/.emacs.d/rsync/bookmarks")

(auto-save-visited-mode 1)

(global-set-key (kbd "C-x w") 'find-file-at-point)
(define-key ctl-x-4-map (kbd "w") 'ffap-other-window)

(global-set-key (kbd "C-x d") 'find-dired)
(global-set-key (kbd "C-x C-d") 'dired)

(global-set-key (kbd "C-x b") 'ibuffer)
(global-set-key (kbd "C-x C-b") 'switch-to-buffer)


(setq dired-listing-switches "-alh")

(defun +dired-do-xdg-open ()
  (interactive)
  (dolist (file (dired-get-marked-files))
    (call-process-shell-command
     (concat "xdg-open " file))))

(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "v") '+dired-do-xdg-open))


(setq ibuffer-show-empty-filter-groups nil
      ibuffer-saved-filter-groups
      '(("default"
         ("exist" (not (name . "\\`[ *]")))
         ("shell" (or (mode . term-mode)
                      (mode . shell-mode)
                      (mode . eshell-mode)
                      (mode . inferior-python-mode))))))

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))

(defun +project-ibuffer ()
  (interactive)
  (require 'project)
  (let ((project (project-root (project-current t))))
    (ibuffer nil (format "*ibuffer %s*" project)
             `((predicate . (file-in-directory-p default-directory
                                                 ,project))))))

(define-key project-prefix-map (kbd "l") '+project-ibuffer)

(add-to-list 'project-switch-commands '(+project-ibuffer "Ibuffer"))


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

(add-hook 'ediff-after-quit-hook-internal 'winner-undo)


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
        eshell-term
        eshell-tramp
        eshell-unix)
      eshell-cd-on-directory nil
      eshell-aliases-file "~/.emacs.d/rsync/alias")

(defun +eshell-history ()
  (interactive)
  (let* ((ido-enable-flex-matching t)
         (hist (completing-read "history: "
                                (ring-elements eshell-history-ring))))
    (eshell-kill-input)
    (insert hist)))

(with-eval-after-load 'em-hist
  (define-key eshell-hist-mode-map (kbd "M-s") nil)
  (define-key eshell-hist-mode-map (kbd "M-r") '+eshell-history))


(defun +browse-kill-ring ()
  (interactive)
  (let ((buffer (get-buffer-create "*browse kill ring*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (dolist (text kill-ring)
          (insert text "\n"))
        (goto-char (point-min))
        (setq buffer-read-only t)
        (set-buffer-modified-p nil))
      (text-mode))
    (switch-to-buffer-other-window buffer)))

(global-set-key (kbd "C-x C-y") '+browse-kill-ring)


(defun +set-register ()
  (interactive)
  (let* ((register (register-read-with-preview "register: "))
         (content (get-register register)))
    (set-register register (read-string (format "register %c: " register)
                                        (if (stringp content) content)))))

(define-key ctl-x-r-map (kbd "s") '+set-register)


(setq abbrev-file-name "~/.emacs.d/rsync/abbrev_defs"
      yas-snippet-dirs '("~/.emacs.d/rsync/snippets")
      yas-alias-to-yas/prefix-p nil
      yas-prompt-functions '(yas-maybe-ido-prompt))

(yas-global-mode 1)

(setcdr (assq 'yas-minor-mode minor-mode-alist) '(""))

(defun +yas-expand (&optional arg)
  (interactive "P")
  (cond (arg
         (let ((snippet (get-register (register-read-with-preview "register: "))))
           (if (stringp snippet)
               (yas-expand-snippet snippet))))
        ((expand-abbrev))
        ((yas-active-snippets)
         (yas-next-field-or-maybe-expand))
        ((yas-expand))
        (t
         (open-line 1))))

(global-set-key (kbd "C-o") '+yas-expand)


(setq completion-styles '(basic)
      dabbrev-case-replace nil
      dabbrev-case-distinction nil
      dabbrev-case-fold-search nil)

(setq company-idle-delay 0.3
      company-dabbrev-downcase nil
      company-dabbrev-ignore-case nil
      company-dabbrev-other-buffers nil
      company-frontends
      '(company-pseudo-tooltip-unless-just-one-frontend
        company-preview-if-just-one-frontend)
      company-backends
      '(company-files (company-dabbrev-code company-keywords) company-dabbrev))

(global-company-mode 1)

(setcdr (assq 'company-mode minor-mode-alist) '(""))


(setq god-mode-enable-function-key-translation nil
      god-mode-alist '((nil . "C-") ("g" . "M-") ("," . "C-M-")))

(require 'god-mode)
(require 'god-mode-isearch)

(defun +self-insert-command ()     ;god mode remap self-insert-command
  (interactive)
  (self-insert-command 1))

(global-set-key (kbd "C-x g") 'god-local-mode)
(global-set-key (kbd "<escape>") 'god-local-mode)
(dolist (key '("(" ")" "[" "]" "{" "}" "`" "'" "\"")) ;surround region in god mode
  (define-key god-local-mode-map (kbd key) '+self-insert-command))
(define-key god-local-mode-map (kbd "q") 'quit-window)
(define-key god-local-mode-map (kbd "<") 'beginning-of-buffer)
(define-key god-local-mode-map (kbd ">") 'end-of-buffer)
(define-key god-local-mode-map (kbd "SPC") 'scroll-up-command)
(define-key god-local-mode-map (kbd "S-SPC") 'scroll-down-command)
(define-key isearch-mode-map (kbd "<escape>") 'god-mode-isearch-activate)
(define-key god-mode-isearch-map (kbd "<escape>") 'god-mode-isearch-disable)
(define-key god-mode-isearch-map (kbd "z") 'avy-isearch)

(god-mode-all)

(add-hook 'god-local-mode-hook 'company-abort)

(require 'which-key)

(setq which-key-lighter nil
      which-key-add-column-padding 2
      which-key-idle-secondary-delay 0
      which-key-show-prefix 'mode-line)

(which-key-mode 1)

(which-key-enable-god-mode-support)


(setq recentf-max-saved-items 100)

(recentf-mode 1)

(setq ido-use-virtual-buffers t)

(require 'ido)

(ido-everywhere 1)

(defun +completing-read-around
    (func prompt collection &optional predicate require-match
          initial-input hist def inherit-input-method)
  (if (bound-and-true-p ido-cur-list)
      (funcall func prompt collection predicate require-match
               initial-input hist def inherit-input-method)
    (if (and (listp collection)         ;ido comp read directly if collection is a string list
             (stringp (car collection)))
        (ido-completing-read prompt collection predicate require-match
                             initial-input hist def inherit-input-method)
      (let ((allcomp (all-completions "" collection predicate))) ;trans collection to string list using all-completions
        (ido-completing-read prompt allcomp nil require-match
                             initial-input hist def inherit-input-method)))))

(advice-add 'completing-read :around '+completing-read-around)

(defun +read-extended-command-around (func)
  (let ((ido-enable-flex-matching t))
    (funcall func)))

(advice-add 'read-extended-command :around '+read-extended-command-around)


(setq-default display-line-numbers-width 4)

(add-hook 'prog-mode-hook 'hs-minor-mode)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(define-key prog-mode-map (kbd "C-c C-j") 'imenu)
(define-key prog-mode-map (kbd "C-c C-i") 'hs-toggle-hiding)


(+setq-hook 'emacs-lisp-mode-hook
            company-backends
            '(company-capf company-files company-dabbrev-code company-dabbrev))


(setq python-indent-guess-indent-offset nil
      python-shell-interpreter "python3"
      org-babel-python-command "python3")

(with-eval-after-load 'python
  (with-eval-after-load 'org
    (require 'ob-python)))


(setq org-modules '(org-tempo ol-eshell)
      org-export-backends '(html latex)
      org-html-postamble nil
      org-html-validation-link nil
      org-special-ctrl-a/e t
      org-link-descriptive nil
      org-footnote-auto-adjust t
      org-link-frame-setup '((file . find-file))
      org-src-preserve-indentation t
      org-src-window-setup 'current-window)

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "<") (lambda () (interactive) (insert ?<))))

(add-hook 'org-mode-hook 'org-cdlatex-mode)


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

(+setq-hook 'org-mode-hook
            pyim-english-input-switch-functions
            '(+pyim-probe-god-mode-p
              org-inside-LaTeX-fragment-p))

(advice-add 'pyim-punctuation-full-width-p :override 'ignore)

(with-eval-after-load 'pyim
  (setcdr (last (car (last (assq 'ziranma-shuangpin pyim-schemes)))) ;fix zirjma
          '(("aj" "an") ("al" "ai") ("ao" "ak")
            ("ez" "ei") ("ef" "en") ("ou" "ob")))
  (define-key pyim-mode-map (kbd ".") 'pyim-page-next-page)
  (define-key pyim-mode-map (kbd ",") 'pyim-page-previous-page)
  (pyim-basedict-enable))
