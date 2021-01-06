;;; -*- lexical-binding: t -*-
;;; package
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

(setq package-selected-packages '(zenburn-theme
                                  god-mode
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

(unless (package-installed-p 'zenburn-theme)
  (package-refresh-contents)
  (dolist (pkg package-selected-packages)
    (package-install pkg)))

;;; ui
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)

(setq visible-bell t)

(setq inhibit-splash-screen t)

(setq-default indent-tabs-mode nil)

(show-paren-mode 1)
(electric-pair-mode 1)

(global-set-key (kbd "C-S-r") 'raise-sexp)
(global-set-key (kbd "C-S-d") 'delete-pair)

(global-set-key (kbd "C-=") 'er/expand-region)

(global-set-key (kbd "<f2>") 'tmm-menubar)
(global-set-key (kbd "<f10>") 'toggle-frame-maximized)

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

(load-theme 'zenburn t)

;;; tool
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
      vc-handled-backends '(Git)
      vc-make-backup-files t
      backup-directory-alist '(("." . "~/.bak"))
      tramp-completion-use-auth-sources nil
      bookmark-default-file "~/.emacs.d/rsync/bookmarks"
      eshell-aliases-file "~/.emacs.d/rsync/alias"
      eshell-modules-list
      '(eshell-alias
        eshell-basic
        eshell-cmpl
        eshell-dirs
        eshell-glob
        eshell-hist
        eshell-ls
        eshell-pred
        eshell-prompt
        eshell-script
        eshell-term
        eshell-tramp
        eshell-unix)
      eshell-cd-on-directory nil)

(auto-save-visited-mode 1)

(global-set-key (kbd "C-x w") 'find-file-at-point)

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
         ("exist" (or (exist)
                      (mode . dired-mode)))
         ("shell" (or (mode . shell-mode)
                      (mode . eshell-mode)
                      (mode . term-char-mode)
                      (mode . term-line-mode)
                      (mode . inferior-python-mode))))))

(add-hook 'ibuffer-mode-hook
          (lambda () (ibuffer-switch-to-saved-filter-groups "default")))

(with-eval-after-load 'ibuf-ext
  (define-ibuffer-filter exist
      "buffers exist"
    (:description "exist")
    (with-current-buffer buf
      buffer-file-name))
  (define-ibuffer-filter project
      "buffers in project"
    (:description "project" :reader (project-prompt-project-dir))
    (with-current-buffer buf
      (file-in-directory-p default-directory qualifier))))

(defun +project-ibuffer ()
  (interactive)
  (let ((project (project-root (project-current t))))
    (ibuffer nil (format "*ibuffer %s*" project)
             (list (cons 'project project)))))

(define-key project-prefix-map (kbd "l") '+project-ibuffer)
(define-key project-prefix-map (kbd "L") 'ibuffer-filter-by-project)

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

(defun +eshell-history ()
  (interactive)
  (let* ((ido-enable-flex-matching t)
         (cmd (completing-read "history: "
                               (ring-elements eshell-history-ring))))
    (eshell-kill-input)
    (insert cmd)))

(with-eval-after-load 'em-hist
  (define-key eshell-hist-mode-map (kbd "M-s") nil)
  (define-key eshell-hist-mode-map (kbd "M-r") '+eshell-history))

(add-hook 'eshell-mode-hook 'outline-minor-mode)

(+setq-hook 'eshell-mode-hook outline-regexp eshell-prompt-regexp)

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

(setcdr (assq 'eldoc-mode minor-mode-alist) '(""))

(setq abbrev-file-name "~/.emacs.d/rsync/abbrev_defs"
      yas-snippet-dirs '("~/.emacs.d/rsync/snippets")
      yas-alias-to-yas/prefix-p nil
      yas-prompt-functions '(yas-maybe-ido-prompt))

(yas-global-mode 1)

(setcdr (assq 'yas-minor-mode minor-mode-alist) '(""))

(defun +yas-expand ()
  (interactive)
  (cond ((expand-abbrev))
        ((yas-active-snippets)
         (yas-next-field-or-maybe-expand))
        ((yas-expand))
        (t
         (open-line 1))))

(defun +yas-temp-expand ()
  (interactive)
  (let ((snippet (get-register ?s)))
    (if (stringp snippet)
        (yas-expand-snippet snippet))))

(defun +yas-temp-edit ()
  (interactive)
  (let ((buffer (get-buffer-create "*yas temp*"))
        (snippet (get-register ?s)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (if (stringp snippet)
            (insert snippet))
        (text-mode)
        (local-set-key (kbd "C-c C-c")
                       (lambda ()
                         (interactive)
                         (set-register ?s (buffer-string))
                         (winner-undo)))))
    (switch-to-buffer-other-window buffer)))

(defvar +yas-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "i") 'yas-insert-snippet)
    (define-key map (kbd "v") 'yas-visit-snippet-file)
    (define-key map (kbd "n") 'yas-new-snippet)
    (define-key map (kbd "e") '+yas-temp-edit)
    map))

(global-set-key (kbd "C-x C-a") +yas-map)
(global-set-key (kbd "C-o") '+yas-expand)
(global-set-key (kbd "C-S-o") '+yas-temp-expand)

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

;;; god
(setq god-mode-enable-function-key-translation nil
      god-mode-alist '((nil . "C-") ("g" . "M-") ("," . "C-M-")))

(require 'god-mode)
(require 'god-mode-isearch)

(defun +self-insert-command ()          ;god mode remap self-insert-command
  (interactive)
  (self-insert-command 1))

(global-set-key (kbd "C-x g") 'god-local-mode)
(global-set-key (kbd "<escape>") 'god-local-mode)
(dolist (key '("(" ")" "[" "]" "{" "}" "`" "'" "\""))
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

;;; ido
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
    (let ((allcomp (all-completions "" collection predicate)))
      (ido-completing-read prompt allcomp nil require-match
                           initial-input hist def inherit-input-method))))

(advice-add 'completing-read :around '+completing-read-around)

(defun +read-extended-command-around (func)
  (let ((ido-enable-flex-matching t))
    (funcall func)))

(advice-add 'read-extended-command :around '+read-extended-command-around)

;;; org
(setq org-modules '(org-tempo ol-eshell)
      org-export-backends '(html)
      org-html-postamble nil
      org-html-validation-link nil
      org-special-ctrl-a/e t
      org-link-descriptive nil
      org-footnote-auto-adjust t
      org-link-frame-setup '((file . find-file))
      org-src-preserve-indentation t
      org-src-window-setup 'current-window
      org-agenda-files '("~/.emacs.d/rsync/org/todos.org")
      org-directory "~/.emacs.d/rsync/org"
      org-default-notes-file "~/.emacs.d/rsync/org/notes.org"
      org-capture-templates '(("n" "NOTE" entry
                               (file "") "* %?\n  %u")
                              ("N" "NOTE[a]" entry
                               (file "") "* %?\n  %u\n  %a")
                              ("t" "TODO" entry
                               (file "todos.org") "* TODO %?\n  %u")
                              ("T" "TODO[a]" entry
                               (file "todos.org") "* TODO %?\n  %u\n  %a")
                              ("k" "KILL" entry
                               (file "") "* %^{heading}\n  %u\n  %c" :immediate-finish t)
                              ("c" "CLIP" entry
                               (file "") "* %^{heading}\n  %u\n  %x" :immediate-finish t)
                              ("r" "REG" entry
                               (file "") "* %^{heading}\n  %u\n  %i" :immediate-finish t)))

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "<") (lambda () (interactive) (insert ?<))))

(add-hook 'org-mode-hook 'org-cdlatex-mode)

(autoload 'org-open-at-point "org" "org open at point" t)

(defvar +org-link-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "o") 'org-open-at-point)
    (define-key map (kbd "w") 'org-store-link)
    (define-key map (kbd "y") 'org-insert-last-stored-link)
    (define-key map (kbd "i") 'org-insert-link)
    (define-key map (kbd "n") 'org-next-link)
    (define-key map (kbd "p") 'org-previous-link)
    (define-key map (kbd "b") 'org-mark-ring-goto)
    (define-key map (kbd ".") 'org-time-stamp)
    (define-key map (kbd "!") 'org-time-stamp-inactive)
    (define-key map (kbd "a") 'org-agenda)
    (define-key map (kbd "c") 'org-capture)
    map))

(global-set-key (kbd "C-x l") +org-link-map)

;;; prog
(setq-default display-line-numbers-width 4)

(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(require 'outline)
(require 'hideshow)

(dolist (mode '(hs-minor-mode
                outline-minor-mode))
  (add-hook 'prog-mode-hook mode)
  (setcdr (assq mode minor-mode-alist) '("")))

(defun +outline-occur ()
  (interactive)
  (occur outline-regexp))

(global-set-key (kbd "M-s M-o") '+outline-occur)

(define-key prog-mode-map (kbd "C-c C-j") 'imenu)
(define-key prog-mode-map (kbd "C-c C-u") 'outline-up-heading)
(define-key prog-mode-map (kbd "C-c C-n") 'outline-next-heading)
(define-key prog-mode-map (kbd "C-c C-p") 'outline-previous-heading)
(define-key prog-mode-map (kbd "C-c C-f") 'outline-forward-same-level)
(define-key prog-mode-map (kbd "C-c C-b") 'outline-backward-same-level)
(define-key prog-mode-map (kbd "C-c C-i")
  (+menu-if (outline-on-heading-p) 'outline-toggle-children 'hs-toggle-hiding))
(define-key outline-minor-mode-map (kbd "<tab>")
  (+menu-if (outline-on-heading-p) 'outline-cycle))
(define-key outline-minor-mode-map (kbd "<backtab>")
  (+menu-if (outline-on-heading-p) 'outline-cycle-buffer))
(define-key outline-minor-mode-map (kbd "M-<up>")
  (+menu-if (outline-on-heading-p) 'outline-move-subtree-up))
(define-key outline-minor-mode-map (kbd "M-<down>")
  (+menu-if (outline-on-heading-p) 'outline-move-subtree-down))
(define-key outline-minor-mode-map (kbd "C-M-h")
  (+menu-if (outline-on-heading-p) 'outline-mark-subtree))

;;; elisp
(+setq-hook 'emacs-lisp-mode-hook
            outline-regexp ";;[;\^L]+ "
            company-backends '(company-capf company-files company-dabbrev-code company-dabbrev))

;;; python
(setq python-indent-guess-indent-offset nil
      python-shell-interpreter "python3"
      org-babel-python-command "python3")

(+setq-hook 'python-mode-hook
            outline-regexp "#[#\^L]+ "
            outline-heading-end-regexp "\n")

(with-eval-after-load 'python
  (define-key python-mode-map (kbd "C-c p") 'run-python)
  (define-key python-mode-map (kbd "C-c C-p") 'outline-previous-heading)
  (define-key python-mode-map (kbd "C-c C-f") 'outline-forward-same-level)
  (with-eval-after-load 'org
    (require 'ob-python)))

;;; pdf
(with-eval-after-load 'pdf-tools
  (pdf-tools-install))

(autoload 'pdf-view-mode "pdf-tools" "pdf tools" t)

(add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode))

;;; cn
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
      pyim-default-scheme 'zirjma
      pyim-autoselector nil
      pyim-enable-shortcode nil
      pyim-fuzzy-pinyin-alist nil)

(defun +pyim-probe-god-mode-p () god-local-mode)

(setq-default pyim-english-input-switch-functions '(+pyim-probe-god-mode-p))

(+setq-hook 'org-mode-hook
            pyim-english-input-switch-functions
            '(+pyim-probe-god-mode-p
              org-inside-LaTeX-fragment-p))

(with-eval-after-load 'pyim
  (defun pyim-punctuation-full-width-p ())
  (define-key pyim-mode-map (kbd ".") 'pyim-page-next-page)
  (define-key pyim-mode-map (kbd ",") 'pyim-page-previous-page)
  (pyim-scheme-add
   '(zirjma
     :document "zirjma"
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
      ("ah" "ang")
      ("aj" "an")
      ("ak" "ao")
      ("al" "ai")
      ("ee" "e")
      ("ef" "en")
      ("eg" "eng")
      ("er" "er")
      ("ez" "ei")
      ("ob" "ou")
      ("oo" "o"))))
  (pyim-basedict-enable))
