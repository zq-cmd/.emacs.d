;;; -*- lexical-binding: t -*-


(add-to-list 'load-path (expand-file-name "lib" user-emacs-directory))
(require '+autoload)


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


(setq isearch-lazy-count t)

(setq view-read-only t)

(with-eval-after-load 'view
  (define-key view-mode-map (kbd "j") 'View-scroll-line-forward)
  (define-key view-mode-map (kbd "k") 'View-scroll-line-backward))

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


(defvar +occur-alive-p nil)
(defvar +occur-timer nil)

(defun +occur-update ()
  (if +occur-alive-p
      (let ((regexp (minibuffer-contents))
            (buffer (get-buffer "*Occur*")))
        (when (and (not (string-empty-p regexp))
                   (not (and buffer
                             (equal regexp
                                    (with-current-buffer buffer
                                      occur-highlight-regexp)))))
          (with-selected-window (minibuffer-selected-window)
            (condition-case error
                (occur regexp)
              (invalid-regexp nil)))))
    (cancel-timer +occur-timer)))

(defun +occur (&optional init)
  (interactive)
  (let ((+occur-alive-p t))
    (when init (occur init))
    (setq +occur-timer (run-with-idle-timer 0.5 t '+occur-update))
    (read-from-minibuffer "occur: " init))
  (let ((buffer (get-buffer "*Occur*")))
    (when buffer
      (display-buffer buffer))))

(defun +isearch-occur ()
  (interactive)
  (isearch-exit)
  (+occur isearch-string))

(global-set-key (kbd "M-#") '+occur)
(define-key isearch-mode-map (kbd "M-#") '+isearch-occur)


(require 'subr-x)

(defvar +completion-collection nil)
(defvar +completion-window nil)
(defvar +completion-timer nil)

(defun +completion-update (&optional query)
  (interactive)
  (let ((query (or query (minibuffer-contents))))
    (if +completion-window
        (with-selected-window +completion-window
          (unless (equal query header-line-format)
            (setq header-line-format query)
            (erase-buffer)
            (if (string-empty-p query)
                (let ((count 0)
                      (current +completion-collection))
                  (while (and (< count 50) current)
                    (insert (car current) "\n")
                    (setq count (1+ count)
                          current (cdr current))))
              (let ((regexp (string-join (split-string query) ".*")))
                (condition-case error
                    (let ((count 0)
                          (current +completion-collection))
                      (while (and (< count 50) current)
                        (when (string-match regexp (car current))
                          (let ((match-data (match-data))
                                (copyed (copy-sequence (car current))))
                            (font-lock-prepend-text-property
                             (car match-data) (cadr match-data) 'face 'match copyed)
                            (insert copyed "\n"))
                          (setq count (1+ count)))
                        (setq current (cdr current))))
                  (invalid-regexp nil))))
            (goto-char (point-min))
            (hl-line-highlight)))
      (cancel-timer +completion-timer))))

(defun +completion-next ()
  (interactive)
  (with-selected-window +completion-window
    (next-line)
    (hl-line-highlight)))

(defun +completion-prev ()
  (interactive)
  (with-selected-window +completion-window
    (previous-line)
    (hl-line-highlight)))

(defun +completion-complete ()
  (interactive)
  (delete-region (line-beginning-position) (line-end-position))
  (insert (with-selected-window +completion-window
            (buffer-substring-no-properties (line-beginning-position)
                                            (line-end-position)))))

(defun +completion-exit-minibuffer ()
  (interactive)
  (+completion-complete)
  (exit-minibuffer))

(defvar +completion-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-j") 'exit-minibuffer)
    (define-key map (kbd "C-r") '+completion-update)
    (define-key map (kbd "C-n") '+completion-next)
    (define-key map (kbd "C-p") '+completion-prev)
    (define-key map (kbd "TAB") '+completion-complete)
    (define-key map (kbd "RET") '+completion-exit-minibuffer)
    (make-composed-keymap map minibuffer-local-map)))

(defun +completion-read (collection)
  (save-window-excursion
    (let* ((+completion-collection collection)
           (+completion-window
            (with-selected-window (if (window-minibuffer-p)
                                      (minibuffer-selected-window)
                                    (selected-window))
              (switch-to-buffer-other-window "*+completion+*")
              (hl-line-mode 1)
              (selected-window))))
      (+completion-update "")
      (setq +completion-timer (run-with-idle-timer 0.5 t '+completion-update))
      (unwind-protect
          (read-from-minibuffer "complete: " nil +completion-map)
        (kill-buffer (window-buffer +completion-window))))))

(defun +completion-in-region (beg end collection predicate)
  (completion-in-region-mode -1)
  (let* ((enable-recursive-minibuffers t)
         (prefix (buffer-substring beg end))
         (metadata (completion-metadata prefix collection predicate))
         (boundary (+ beg (car (completion-boundaries prefix collection predicate ""))))
         (choices (nconc (completion-all-completions
                          prefix collection predicate (length prefix))
                         nil))
         (choice (cond ((null choices) nil)
                       ((null (cdr choices)) (car choices))
                       (t (+completion-read choices)))))
    (when choice
      (delete-region boundary end)
      (insert (substring-no-properties choice)))))

(defun +completion-at-point ()
  (interactive)
  (let ((completion-in-region-function '+completion-in-region))
    (call-interactively (lookup-key `(,(current-local-map) ,(current-global-map))
                                    (kbd "TAB")))))

(global-set-key (kbd "<f2>") '+completion-at-point)


(defun +indent-or-filter (command)
  (if (or (use-region-p)
          (<= (current-column)
              (current-indentation)))
      'indent-for-tab-command
    command))

(define-key prog-mode-map (kbd "TAB")
  '(menu-item "" completion-at-point :filter +indent-or-filter))

(with-eval-after-load 'cc-mode
  (define-key c-mode-base-map (kbd "TAB")
    '(menu-item "" completion-at-point :filter +indent-or-filter)))

(setq emmet-preview-default t)

(add-hook 'sgml-mode-hook 'emmet-mode)

(with-eval-after-load 'emmet-mode
  (define-key emmet-mode-keymap (kbd "TAB")
    '(menu-item "" emmet-expand-line :filter +indent-or-filter)))


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

(setq eglot-ignored-server-capabilites '(:hoverProvider))


(defun +xclip-save (beg end)
  (interactive "r")
  (call-shell-region beg end "xclip -selection clip"))

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

(setq ispell-dictionary "en")


(setq org-modules '(org-tempo)
      org-use-speed-commands t
      org-special-ctrl-a/e 'reversed)

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "<") "\C-q<"))
