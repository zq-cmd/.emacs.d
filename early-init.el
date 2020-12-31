(setq gc-cons-percentage 0.6
      gc-cons-threshold most-positive-fixnum
      file-name-handler-alist-init file-name-handler-alist
      file-name-handler-alist nil)

(add-hook 'after-init-hook
          (lambda ()
            (setq gc-cons-percentage 0.1
                  gc-cons-threshold 20000000
                  file-name-handler-alist file-name-handler-alist-init)))

(setq default-frame-alist '((tool-bar-lines . 0)
			    (menu-bar-lines . 0)))

(advice-add 'x-apply-session-resources :override 'ignore)

(setq package-quickstart t)
