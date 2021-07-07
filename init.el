;;; -*- lexical-binding: t -*-


(add-to-list 'load-path (expand-file-name "lib" user-emacs-directory))
(require '+autoload)


(setq text-quoting-style 'grave)
(startup--setup-quote-display 'grave)

(setq visible-bell t)

(setq inhibit-splash-screen t)

(menu-bar-mode -1)
(blink-cursor-mode -1)


(setq confirm-kill-emacs 'y-or-n-p
      vc-handled-backends '(Git)
      vc-make-backup-files t
      version-control t
      kept-old-versions 0
      kept-new-versions 10
      delete-old-versions t
      backup-directory-alist '(("." . "~/.bak")))

(auto-save-visited-mode 1)


(setq-default indent-tabs-mode nil)

(show-paren-mode 1)
(electric-pair-mode 1)

(global-set-key (kbd "M-R") 'raise-sexp)
(global-set-key (kbd "M-D") 'delete-pair)


(setq disabled-command-function nil)

(setq view-read-only t)

(repeat-mode 1)

(global-set-key (kbd "C-x U") 'undo-redo)
(define-key undo-repeat-map (kbd "U") 'undo-redo)
(put 'undo-redo 'repeat-map 'undo-repeat-map)

(dolist (it '(("0" . delete-window)
              ("1" . delete-other-windows)
              ("2" . split-window-below)
              ("3" . split-window-right)))
  (define-key other-window-repeat-map (kbd (car it)) (cdr it))
  (put (cdr it) 'repeat-map 'other-window-repeat-map))

(global-set-key (kbd "M-o") "\C-xo")


(setq evil-undo-system 'undo-tree
      evil-search-module 'evil-search
      evil-want-fine-undo t
      evil-want-C-u-scroll t
      evil-want-C-u-delete t
      evil-want-keybinding nil
      evil-want-integration nil)

(autoload 'evil-local-mode "evil" "evil-local-mode" t)

(global-set-key (kbd "C-z") 'evil-local-mode)

(with-eval-after-load 'evil
  (evil-global-set-key 'replace (kbd "<f1>") 'evil-normal-state)
  (evil-global-set-key 'insert  (kbd "<f1>") 'evil-force-normal-state)

  (evil-define-text-object +evil-textobj-defun (const &optional beg end type)
    (cl-destructuring-bind (beg . end)
        (bounds-of-thing-at-point 'defun)
      (evil-range beg end 'line)))

  (define-key evil-inner-text-objects-map (kbd "f") '+evil-textobj-defun)
  (define-key evil-outer-text-objects-map (kbd "f") '+evil-textobj-defun)

  (evil-define-operator +evil-operator-narrow (beg end)
    :move-point nil
    (interactive "<r>")
    (narrow-to-region beg end))

  (evil-define-operator +evil-operator-comment (beg end)
    :move-point nil
    (interactive "<r>")
    (comment-or-uncomment-region beg end))

  (evil-global-set-key 'motion (kbd "g w") 'widen)
  (evil-global-set-key 'motion (kbd "g n") '+evil-operator-narrow)
  (evil-global-set-key 'normal (kbd "g c") '+evil-operator-comment))

(setq undo-tree-auto-save-history nil)

(global-undo-tree-mode 1)


(global-set-key (kbd "<f2>") 'listify-tab-completion)

(global-set-key (kbd "C-c C-j") 'imenu)

(with-eval-after-load 'flymake
  (define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
  (define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error))

(defun +flymake-cc-command ()
  `("gcc" "-x" ,(if (derived-mode-p 'c++-mode) "c++" "c") "-fsyntax-only" "-"))

(setq flymake-cc-command '+flymake-cc-command)

(setq eglot-ignored-server-capabilites '(:hoverProvider))

(setq company-backends '(company-files company-dabbrev))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq-local company-backends
                        '(company-capf company-files company-dabbrev))))

(add-hook 'prog-mode-hook 'company-mode)


(defun +xclip-save (beg end)
  (interactive "r")
  (call-shell-region beg end "xclip -selection clip"))

(defun +xclip-yank ()
  (interactive "*")
  (insert (shell-command-to-string "xclip -o")))

(global-set-key (kbd "M-W") '+xclip-save)
(global-set-key (kbd "M-Y") '+xclip-yank)

(with-eval-after-load 'diff-mode
  (define-key diff-mode-map (kbd "M-o") nil))

(setq dired-listing-switches "-alh")

(defun +dired-do-xdg-open ()
  (interactive)
  (dolist (file (dired-get-marked-files))
    (call-process-shell-command (concat "xdg-open " file))))

(with-eval-after-load 'dired
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

(setq ispell-dictionary "en")


(setq org-modules '(org-tempo)
      org-use-speed-commands t
      org-special-ctrl-a/e 'reversed)

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "<") "\C-q<"))
