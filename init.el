;;; package
(defmacro +menu-item (&rest body)
  `'(menu-item "" nil :filter (lambda (&optional _) ,@body)))

(defmacro +setq-hook (hook &rest body)
  `(add-hook ,hook (lambda () (setq-local ,@body))))

(setq custom-file "~/.emacs.d/custom.el")

(setq package-archives
      '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
	("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))

(setq package-selected-packages '(zenburn-theme
				  god-mode
				  which-key
				  ido-completing-read+
				  amx
				  projectile
				  expand-region
				  multiple-cursors
				  magit
				  rg
				  fd-find
				  pyim
				  posframe
				  auctex
				  cdlatex
				  htmlize
				  elpy
				  flycheck))

(require 'package)

(unless (package-installed-p 'zenburn-theme)
  (package-refresh-contents)
  (dolist (pkg package-selected-packages)
    (package-install pkg)))

;;; ui
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(setq inhibit-splash-screen t)

(setq indent-tabs-mode nil)

(show-paren-mode 1)
(electric-pair-mode 1)

(global-set-key (kbd "<f2>") 'tmm-menubar)

(windmove-default-keybindings)

(winner-mode 1)

(load-theme 'zenburn t)

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

(defvar +text-scale-index 3)

(defun +text-scale-set (&optional frame)
  (when (display-graphic-p)
    (let ((scale (aref +text-scale-list +text-scale-index)))
      (set-face-attribute
       'default frame
       :font (font-spec :anme "Ubuntu Mono" :size (car scale)))
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

(global-set-key (kbd "M-f")
		(+menu-item
		 (if (string-match-p "\\cc" (char-to-string (following-char)))
		     'pyim-forward-word
		   'forward-word)))

(global-set-key (kbd "M-b")
		(+menu-item
		 (if (string-match-p "\\cc" (char-to-string (following-char)))
		     'pyim-backward-word
		   'backward-word)))

(autoload 'pyim-forward-word "pyim" "pyim" t)
(autoload 'pyim-backward-word "pyim" "pyim" t)

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

;;; tool
(setq vc-handled-backends '(Git)
      vc-make-backup-files t
      backup-directory-alist '(("." . "~/.bak"))
      tramp-completion-use-auth-sources nil)

(define-key special-mode-map (kbd "n") 'next-line)
(define-key special-mode-map (kbd "p") 'previous-line)

(global-set-key (kbd "C-x C-w") 'eshell)

(global-set-key (kbd "C-x f") 'find-file-at-point)

(global-set-key (kbd "C-x d") 'fd-dired)
(global-set-key (kbd "C-x C-d") 'dired)

(global-set-key (kbd "C-x b") 'ibuffer)
(global-set-key (kbd "C-x C-b") 'switch-to-buffer)

(global-set-key (kbd "C-x C-o") 'other-window)
(global-set-key (kbd "C-x C-0") 'delete-window)
(global-set-key (kbd "C-x C-1") 'delete-other-windows)
(global-set-key (kbd "C-x C-2") 'split-window-below)
(global-set-key (kbd "C-x C-3") 'split-window-right)

(setq magit-define-global-key-bindings nil)

(global-set-key (kbd "C-x g") 'magit)
(global-set-key (kbd "C-x M-g") 'magit-dispatch)
(global-set-key (kbd "C-c M-g") 'magit-file-dispatch)

(setq wgrep-auto-save-buffer t
      rg-custom-type-aliases nil)

(autoload 'rg-menu "rg" "rg" t)

(global-set-key (kbd "C-c s") 'rg-menu)

(setq ediff-window-setup-function 'ediff-setup-windows-plain
      ediff-split-window-function 'split-window-horizontally)

(add-hook 'ediff-after-quit-hook-internal 'winner-undo)

(defun +browse-kill-ring ()
  (interactive)
  (let ((buffer (or (get-buffer "*browse kill ring*")
		    (generate-new-buffer "*browse kill ring*"))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
	(erase-buffer)
	(dolist (text kill-ring)
	  (insert text "\n"))
	(goto-char (point-min))
	(setq buffer-read-only t)
	(set-buffer-modified-p nil)))
    (switch-to-buffer-other-window buffer)))

(global-set-key (kbd "M-y") '+browse-kill-ring)

;;; help
(setq god-exempt-predicates nil
      god-exempt-major-modes nil
      god-mode-enable-function-key-translation nil
      god-mode-alist '((nil . "C-") ("g" . "M-") ("," . "C-M-") ("." . "C-M-")))

(require 'god-mode)
(require 'god-mode-isearch)

(global-set-key (kbd "<escape>") 'god-mode-all)
(define-key god-local-mode-map (kbd "z") 'repeat)
(define-key god-local-mode-map (kbd "q") 'quit-window)
(define-key isearch-mode-map (kbd "<escape>") 'god-mode-isearch-activate)
(define-key god-mode-isearch-map (kbd "<escape>") 'god-mode-isearch-disable)

(setq-default cursor-type 'hbar)

(defun +god-mode-update-cursor ()
  (setq cursor-type (if god-local-mode 'box 'hbar)))

(dolist (hook '(god-mode-enabled-hook god-mode-disabled-hook))
  (add-hook hook '+god-mode-update-cursor))

(require 'which-key)

(setq which-key-lighter nil
      which-key-add-column-padding 2
      which-key-idle-secondary-delay 0
      which-key-show-prefix 'mode-line)

(which-key-mode 1)

(which-key-enable-god-mode-support)

;;; ido
(setq dabbrev-case-replace nil
      dabbrev-case-distinction nil)

(recentf-mode 1)

(setq ido-use-virtual-buffers t)

(ido-mode 1)
(ido-everywhere 1)
(ido-ubiquitous-mode 1)
(amx-mode 1)

(define-key ido-common-completion-map (kbd "S-SPC") 'ido-restrict-to-matches)

(setq projectile-keymap-prefix (kbd "C-c p"))

(projectile-mode 1)

;;; edit
(global-set-key (kbd "C-=") 'er/expand-region)

(global-set-key (kbd "C-C C-C") 'mc/edit-lines)
(global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-c C-M-<") 'mc/mark-all-like-this-in-defun)

;;; prog
(setq-default display-line-numbers-width 4)

(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(setcdr (assq 'eldoc-mode minor-mode-alist) '(""))

(require 'company)
(require 'outline)
(require 'hideshow)

(setq company-idle-delay 0.3
      company-dabbrev-downcase nil
      company-dabbrev-ignore-case nil
      company-dabbrev-other-buffers nil
      company-frontends
      '(company-pseudo-tooltip-unless-just-one-frontend
	company-preview-if-just-one-frontend)
      company-backends
      '(company-capf company-files company-dabbrev company-keywords))

(dolist (mode '(outline-minor-mode
		hs-minor-mode
		company-mode))
  (add-hook 'prog-mode-hook mode)
  (setcdr (assq mode minor-mode-alist) '("")))

(define-key prog-mode-map (kbd "C-c C-j") 'imenu)
(define-key prog-mode-map (kbd "C-c C-n") 'outline-next-heading)
(define-key prog-mode-map (kbd "C-c C-p") 'outline-previous-heading)
(define-key prog-mode-map (kbd "C-c C-i") (+menu-item
					   (if (outline-on-heading-p)
					       'outline-toggle-children
					     'hs-toggle-hiding)))

(+setq-hook 'emacs-lisp-mode-hook outline-regexp ";;; ")

;;; org
(setq org-modules '(org-tempo)
      org-export-backends '(html)
      org-html-postamble nil
      org-html-validation-link nil
      org-special-ctrl-a/e t
      org-link-descriptive nil
      org-footnote-auto-adjust t
      org-link-frame-setup '((file . find-file))
      org-src-window-setup 'current-window)

(add-hook 'org-mode-hook 'org-cdlatex-mode)

;;; python
(setq python-indent-guess-indent-offset nil
      python-shell-interpreter "python3"
      org-babel-python-command "python3"
      elpy-rpc-python-command "python3"
      elpy-shell-display-buffer-after-send t)

(+setq-hook 'python-mode-hook
	    outline-regexp "## "
	    outline-heading-end-regexp "\n")

(with-eval-after-load 'python
  (with-eval-after-load 'org
    (require 'ob-python))
  (setq elpy-modules
	'(elpy-module-sane-defaults
	  elpy-module-company
	  elpy-module-eldoc
	  elpy-module-yasnippet
	  elpy-module-highlight-indentation
	  elpy-module-pyvenv))
  (require 'elpy)
  (define-key elpy-mode-map (kbd "C-c C-n") 'outline-next-heading)
  (define-key elpy-mode-map (kbd "C-c C-p") 'outline-previous-heading)
  (elpy-enable)
  (add-hook 'elpy-mode-hook 'flycheck-mode))
