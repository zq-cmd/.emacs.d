;;; +autoload.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:


;;;### (autoloads nil "cmake-mode" "cmake-mode.el" (0 0 0 0))
;;; Generated autoloads from cmake-mode.el

(autoload 'cmake-mode "cmake-mode" "\
Major mode for editing CMake source files.

\(fn)" t nil)

(autoload 'cmake-command-run "cmake-mode" "\
Runs the command cmake with the arguments specified.  The
optional argument topic will be appended to the argument list.

\(fn TYPE &optional TOPIC BUFFER)" t nil)

(autoload 'cmake-help-list-commands "cmake-mode" "\
Prints out a list of the cmake commands." t nil)

(autoload 'cmake-help-command "cmake-mode" "\
Prints out the help message for the command the cursor is on." t nil)

(autoload 'cmake-help-module "cmake-mode" "\
Prints out the help message for the module the cursor is on." t nil)

(autoload 'cmake-help-variable "cmake-mode" "\
Prints out the help message for the variable the cursor is on." t nil)

(autoload 'cmake-help-property "cmake-mode" "\
Prints out the help message for the property the cursor is on." t nil)

(autoload 'cmake-help "cmake-mode" "\
Queries for any of the four available help topics and prints out the appropriate page." t nil)

(add-to-list 'auto-mode-alist '("CMakeLists\\.txt\\'" . cmake-mode))

(add-to-list 'auto-mode-alist '("\\.cmake\\'" . cmake-mode))

(register-definition-prefixes "cmake-mode" '("cmake-"))

;;;***

;;;### (autoloads nil "wgrep" "wgrep.el" (0 0 0 0))
;;; Generated autoloads from wgrep.el

(autoload 'wgrep-setup "wgrep" "\
Setup wgrep preparation." nil nil)

(add-hook 'grep-setup-hook 'wgrep-setup)

(register-definition-prefixes "wgrep" '("wgrep-"))

;;;***

(provide '+autoload)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; +autoload.el ends here
