;;; -*- lexical-binding: t -*-


(add-to-list 'load-path "/usr/share/emacs/site-lisp")
(add-to-list 'load-path (expand-file-name "lib" user-emacs-directory))


(setq visible-bell t)

(setq inhibit-splash-screen t)

(menu-bar-mode -1)
(blink-cursor-mode -1)

(load-theme 'modus-vivendi)

(global-set-key (kbd "<f5>") 'tmm-menubar)


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

(defun +repeat ()
  (interactive)
  (setq last-command-event ?z
        this-command 'repeat
        real-this-command 'repeat)
  (call-interactively 'repeat))

(global-set-key (kbd "C-z") '+repeat)

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


(setq view-read-only t)

(with-eval-after-load 'view
  (define-key view-mode-map (kbd "N") "1\C-xnp<")
  (define-key view-mode-map (kbd "P") "-\C-xnp<")
  (define-key view-mode-map (kbd "e") 'View-scroll-line-forward))

(global-set-key (kbd "M-z") 'view-mode)

(defun +input-method-function (first-char)
  (if (and (eq first-char ?j)
           (not executing-kbd-macro)
           (not (sit-for 0.1 'no-redisplay)))
      (let ((next-char
             (let (input-method-function)
               (read-event))))
        (if (eq next-char ?k)
            '(?\M-z)
          (push next-char unread-command-events)
          '(?j)))
    `(,first-char)))

(setq input-method-function '+input-method-function)


(defun +completion-in-region (beg end collection &optional predicate)
  (let* ((enable-recursive-minibuffers t)
         (prefix (buffer-substring beg end))
         (choices (all-completions prefix collection predicate))
         (choice (cond ((null choices) nil)
                       ((null (cdr choices)) (car choices))
                       (t (ido-completing-read "complete: " choices)))))
    (when choice
      (let ((count 0)
            (len (length prefix)))
        (while (not (string-prefix-p (substring prefix count) choice))
          (setq count (1+ count)))
        (insert (substring choice (- len count)))))))

(defun +completion-at-point (arg)
  (interactive "*P")
  (let ((completion-in-region-function '+completion-in-region))
    (call-interactively (lookup-key `(,(current-local-map) ,(current-global-map))
                                    (kbd (if arg "M-TAB" "TAB"))))))

(global-set-key (kbd "<f2>") '+completion-at-point)

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
    `(menu-item "" c-indent-line-or-region :filter +tab-completion-filter)))


(global-set-key (kbd "C-c C-j") 'imenu)

(with-eval-after-load 'flymake
  (define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
  (define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error))

(defun +flymake-cc-command ()
  `("gcc" "-x" ,(if (derived-mode-p 'c++-mode) "c++" "c") "-fsyntax-only" "-"))

(setq flymake-cc-command '+flymake-cc-command)

(setq semantic-new-buffer-setup-functions
      '((c-mode . semantic-default-c-setup)
        (c++-mode . semantic-default-c-setup)))


(defun +xclip-save (beg end)
  (interactive "r")
  (call-process-region beg end "xclip"))

(defun +xclip-yank ()
  (interactive "*")
  (insert (shell-command-to-string "xclip -o")))

(global-set-key (kbd "M-W") '+xclip-save)
(global-set-key (kbd "M-Y") '+xclip-yank)

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

(autoload 'wgrep-setup "wgrep")
(add-hook 'grep-setup-hook 'wgrep-setup)

(setq ispell-dictionary "en")


(setq org-modules '(org-tempo)
      org-export-backends '(latex)
      org-use-speed-commands t
      org-special-ctrl-a/e 'reversed)

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "<") "\C-q<"))
