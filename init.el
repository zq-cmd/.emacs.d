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


(setq isearch-lazy-count t)

(setq view-read-only t)

(global-set-key [+jk] 'view-mode)

(defun +input-method-function (first-char)
  (if (and (eq first-char ?j)
           (not view-mode)
           (not executing-kbd-macro)
           (not (sit-for 0.1 'no-redisplay)))
      (let ((next-char
             (let (input-method-function)
               (read-event))))
        (if (eq next-char ?k)
            '(+jk)
          (push next-char unread-command-events)
          '(?j)))
    `(,first-char)))

(setq input-method-function '+input-method-function)

(defun +god-char-to-string (char)
  (or (cdr (assq char '((tab . "TAB")
                        (?\  . "SPC")
                        (backspace . "DEL")
                        (return . "RET"))))
      (char-to-string char)))

(defun +god-lookup-key (&optional key prev-key)
  (let* ((key (+god-char-to-string (or key (read-event prev-key))))
         (trans (cdr (assoc key '(("SPC" . "") ("g" . "M-") ("h" . "C-")))))
         (keys (if trans
                   (let* ((prev-key (concat prev-key " " trans))
                          (key (+god-char-to-string (read-event prev-key))))
                     (if (and (equal trans "C-") (equal key "g"))
                         (let* ((prev-key (concat prev-key "M-"))
                                (key (+god-char-to-string (read-event prev-key))))
                           (concat prev-key key))
                       (concat prev-key key)))
                 (concat prev-key " C-" key)))
         (seq (read-kbd-macro keys t))
         (binding (key-binding seq)))
    (cond ((commandp binding)
           (setq last-command-event (aref seq (1- (length seq))))
           binding)
          ((keymapp binding)
           (+god-lookup-key (read-event keys) keys))
          (t
           (error "God: unknown key binding for `%s'" keys)))))

(defun +god-self-insert ()
  (interactive)
  (condition-case err
      (let* ((keys (this-command-keys-vector))
             (key (aref keys (- (length keys) 1)))
             (binding (+god-lookup-key key)))
        (setq this-command binding
              real-this-command binding)
        (if (commandp binding t)
            (call-interactively binding)
          (execute-kbd-macro binding)))
    (error (message (cadr err)))))

(with-eval-after-load 'view
  (dolist (key '("x" "c" "g" "h"))
    (define-key view-mode-map (kbd key) '+god-self-insert))
  (define-key view-mode-map (kbd ".") 'repeat)
  (define-key view-mode-map (kbd "j") 'View-scroll-line-forward)
  (define-key view-mode-map (kbd "k") 'View-scroll-line-backward)
  (define-key view-mode-map (kbd "f") 'View-scroll-page-forward)
  (define-key view-mode-map (kbd "b") 'View-scroll-page-backward)
  (define-key view-mode-map (kbd "?") 'View-search-regexp-backward)
  (define-key view-mode-map (kbd "N") 'View-search-last-regexp-backward))


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
