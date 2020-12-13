(setq gc-cons-percentage 0.6
      gc-cons-threshold most-positive-fixnum
      +gc-low-cons-threshold (* 1024 1024)
      file-name-handler-alist-init file-name-handler-alist
      file-name-handler-alist nil)

(add-hook 'after-init-hook
          (lambda ()
            (setq gc-cons-percentage 0.1
                  gc-cons-threshold +gc-low-cons-threshold
                  file-name-handler-alist file-name-handler-alist-init)))

(add-hook 'minibuffer-setup-hook (lambda () (setq gc-cons-threshold most-positive-fixnum)))
(add-hook 'minibuffer-exit-hook (lambda () (setq gc-cons-threshold +gc-low-cons-threshold)))

(setq default-frame-alist '((tool-bar-lines . 0)
			    (menu-bar-lines . 0)
			    (fullscreen . maximized)))

(advice-add 'x-apply-session-resources :override 'ignore)

(setq package-quickstart t)
