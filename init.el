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
                                  wgrep
                                  ace-window
                                  link-hint
                                  pinyinlib
                                  expand-region
                                  multiple-cursors
                                  auto-yasnippet
                                  yasnippet-snippets
                                  company
                                  pyim
                                  posframe
                                  auctex
                                  cdlatex
                                  org2ctex
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

(global-set-key (kbd "<f2>") 'tmm-menubar)

(windmove-default-keybindings)

(winner-mode 1)

(global-set-key (kbd "C-x C-o") 'other-window)
(global-set-key (kbd "C-x C-0") 'delete-window)
(global-set-key (kbd "C-x C-1") 'delete-other-windows)
(global-set-key (kbd "C-x C-2") 'split-window-below)
(global-set-key (kbd "C-x C-3") 'split-window-right)

(setq avy-background t
      aw-dispatch-always t)

(global-set-key (kbd "M-o") 'ace-window)
(global-set-key (kbd "M-O") '+link-hint-dispatch)
(global-set-key (kbd "C-z") (+menu-if current-input-method
                                      '+avy-pinyin-goto-char
                                      'avy-goto-char))
(global-set-key (kbd "M-z") 'avy-goto-word-1)
(global-set-key (kbd "C-M-z") 'avy-goto-line)
(global-set-key (kbd "C-S-z") 'avy-resume)
(define-key isearch-mode-map (kbd "C-z") 'avy-isearch)

(defun +avy-pinyin-goto-char (char &optional arg)
  (interactive (list (read-char "char: " t)
                     current-prefix-arg))
  (require 'avy)
  (require 'pinyinlib)
  (avy-with +avy-pinyin-goto-char
    (avy-jump
     (if (= 13 char)
         "\n"
       (pinyinlib-build-regexp-char char))
     :window-flip arg)))

(defun +link-hint-dispatch (&optional arg)
  (interactive "P")
  (if arg
      (pcase (read-char-exclusive
              "[w]copy | [c]opy multi | [C]opy all | [o]pen multi | [O]pen all")
        (?w (link-hint-copy-link))
        (?c (link-hint-copy-multiple-links))
        (?C (link-hint-copy-all-links))
        (?o (link-hint-open-multiple-links))
        (?O (link-hint-open-all-links)))
    (let ((avy-single-candidate-jump nil))
      (link-hint-open-link))))

(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-S-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-S-c C->") 'mc/mark-all-like-this-in-defun)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)

(load-theme 'zenburn t)

;;; tool
(global-set-key (kbd "C-?") 'undo-redo)

(define-key special-mode-map (kbd "n") 'next-line)
(define-key special-mode-map (kbd "p") 'previous-line)
(define-key special-mode-map (kbd "s") 'isearch-forward)
(define-key special-mode-map (kbd "r") 'isearch-backward)
(define-key special-mode-map (kbd "o") '+link-hint-dispatch)

(setq confirm-kill-emacs 'y-or-n-p
      vc-handled-backends '(Git)
      vc-make-backup-files t
      backup-directory-alist '(("." . "~/.bak"))
      tramp-completion-use-auth-sources nil
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

(global-set-key (kbd "C-x C-w") 'eshell)

(global-set-key (kbd "C-x f") 'find-file-at-point)

(global-set-key (kbd "C-x d") 'find-dired)
(global-set-key (kbd "C-x C-d") 'dired)

(global-set-key (kbd "C-x b") 'ibuffer)
(global-set-key (kbd "C-x C-b") 'switch-to-buffer)

(setq wgrep-auto-save-buffer t)

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

(global-set-key (kbd "C-x C-y") '+browse-kill-ring)

(setcdr (assq 'eldoc-mode minor-mode-alist) '(""))

(setq yas-alias-to-yas/prefix-p nil
      yas-prompt-functions '(yas-maybe-ido-prompt))

(setq yas-minor-mode-map (make-sparse-keymap))

(yas-global-mode 1)

(define-key yas-minor-mode-map (kbd "<tab>") yas-maybe-expand)

(setcdr (assq 'yas-minor-mode minor-mode-alist) '(""))

(defvar +aya-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "i") 'yas-insert-snippet)
    (define-key map (kbd "n") 'yas-new-snippet)
    (define-key map (kbd "v") 'yas-visit-snippet-file)
    (define-key map (kbd "c") 'aya-create)
    (define-key map (kbd "e") 'aya-expand)
    (define-key map (kbd "s") 'aya-persist-snippet)
    (define-key map (kbd "y") 'aya-yank-snippet)
    map))

(global-set-key (kbd "C-x C-a") +aya-map)
(global-set-key (kbd "C-o") 'aya-open-line)
(global-set-key (kbd "C-S-o") 'aya-expand)

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
(setq god-exempt-predicates nil
      god-exempt-major-modes nil
      god-mode-enable-function-key-translation nil
      god-mode-alist '((nil . "C-") ("g" . "M-") ("," . "C-M-")))

(require 'god-mode)
(require 'god-mode-isearch)

(global-set-key (kbd "<escape>") 'god-mode-all)
(define-key god-local-mode-map (kbd ".") 'repeat)
(define-key god-local-mode-map (kbd "q") 'quit-window)
(define-key isearch-mode-map (kbd "<escape>") 'god-mode-isearch-activate)
(define-key god-mode-isearch-map (kbd "<escape>") 'god-mode-isearch-disable)
(define-key god-mode-isearch-map (kbd "z") 'avy-isearch)

(setq-default cursor-type 'hbar)

(defun +god-mode-update-cursor ()
  (setq cursor-type (if god-local-mode 'box 'hbar)))

(dolist (hook '(god-mode-enabled-hook god-mode-disabled-hook))
  (add-hook hook '+god-mode-update-cursor))

(god-mode-all)

(require 'which-key)

(setq which-key-lighter nil
      which-key-add-column-padding 2
      which-key-idle-secondary-delay 0
      which-key-show-prefix 'mode-line)

(which-key-mode 1)

(which-key-enable-god-mode-support)

;;; ido
(recentf-mode 1)

(setq ido-use-virtual-buffers t
      ido-use-url-at-point t
      ido-use-filename-at-point t)

(require 'ido)

(ido-everywhere 1)

(defun +completing-read-around
    (func prompt collection &optional predicate require-match
          initial-input hist def inherit-input-method)
  (if (bound-and-true-p ido-cur-list)
      (funcall func prompt collection predicate require-match
               initial-input hist def inherit-input-method)
    (let ((allcomp (all-completions (or initial-input "") collection predicate)))
      (ido-completing-read prompt allcomp nil require-match initial-input hist def inherit-input-method))))

(advice-add 'completing-read :around '+completing-read-around)

(defun +read-extended-command-around (func)
  (let ((ido-enable-flex-matching t))
    (funcall func)))

(advice-add 'read-extended-command :around '+read-extended-command-around)

;;; org
(setq org-modules '(ol-eww ol-eshell ol-info)
      org-export-backends '(html)
      org-html-postamble nil
      org-html-validation-link nil
      org-special-ctrl-a/e t
      org-descriptive-links nil
      org-footnote-auto-adjust t
      org-link-frame-setup '((file . find-file))
      org-src-window-setup 'current-window)

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "<") (lambda () (interactive) (insert ?<))))

(add-hook 'org-mode-hook 'org-cdlatex-mode)

(autoload 'org-open-at-point "org" "org open at point" t)

(defvar +org-link-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "o") 'org-open-at-point)
    (define-key map (kbd "s") 'org-store-link)
    (define-key map (kbd "l") 'org-insert-link)
    (define-key map (kbd "L") 'org-insert-last-stored-link)
    (define-key map (kbd "n") 'org-next-link)
    (define-key map (kbd "p") 'org-previous-link)
    (define-key map (kbd "b") 'org-mark-ring-goto)
    map))

(global-set-key (kbd "C-c l") +org-link-map)

;;; prog
(setq-default display-line-numbers-width 4)

(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(require 'outline)
(require 'hideshow)

(dolist (mode '(hs-minor-mode
                outline-minor-mode))
  (add-hook 'prog-mode-hook mode)
  (setcdr (assq mode minor-mode-alist) '("")))

(define-key prog-mode-map (kbd "C-c C-j") 'imenu)
(define-key prog-mode-map (kbd "C-c C-u") 'outline-up-heading)
(define-key prog-mode-map (kbd "C-c C-n") 'outline-next-heading)
(define-key prog-mode-map (kbd "C-c C-p") 'outline-previous-heading)
(define-key prog-mode-map (kbd "C-c C-f") 'outline-forward-same-level)
(define-key prog-mode-map (kbd "C-c C-b") 'outline-backward-same-level)
(define-key prog-mode-map (kbd "C-c C-i")
  (+menu-if (outline-on-heading-p) 'outline-toggle-children 'hs-toggle-hiding))
(define-key prog-mode-map (kbd "M-<up>")
  (+menu-if (outline-on-heading-p) 'outline-move-subtree-up))
(define-key prog-mode-map (kbd "M-<down>")
  (+menu-if (outline-on-heading-p) 'outline-move-subtree-down))

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
