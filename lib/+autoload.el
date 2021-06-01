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

;;;### (autoloads nil "company" "company.el" (0 0 0 0))
;;; Generated autoloads from company.el

(autoload 'company-mode "company" "\
\"complete anything\"; is an in-buffer completion framework.
Completion starts automatically, depending on the values
`company-idle-delay' and `company-minimum-prefix-length'.

If called interactively, toggle `Company mode'.  If the prefix
argument is positive, enable the mode, and if it is zero or
negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

The mode's hook is called both when the mode is enabled and when
it is disabled.

Completion can be controlled with the commands:
`company-complete-common', `company-complete-selection', `company-complete',
`company-select-next', `company-select-previous'.  If these commands are
called before `company-idle-delay', completion will also start.

Completions can be searched with `company-search-candidates' or
`company-filter-candidates'.  These can be used while completion is
inactive, as well.

The completion data is retrieved using `company-backends' and displayed
using `company-frontends'.  If you want to start a specific backend, call
it interactively or use `company-begin-backend'.

By default, the completions list is sorted alphabetically, unless the
backend chooses otherwise, or `company-transformers' changes it later.

regular keymap (`company-mode-map'):

\\{company-mode-map}
keymap during active completions (`company-active-map'):

\\{company-active-map}

\(fn &optional ARG)" t nil)

(put 'global-company-mode 'globalized-minor-mode t)

(defvar global-company-mode nil "\
Non-nil if Global Company mode is enabled.
See the `global-company-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-company-mode'.")

(custom-autoload 'global-company-mode "company" nil)

(autoload 'global-company-mode "company" "\
Toggle Company mode in all buffers.
With prefix ARG, enable Global Company mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if ARG is
omitted or nil.

Company mode is enabled in all buffers where `company-mode-on' would
do it.

See `company-mode' for more information on Company mode.

\(fn &optional ARG)" t nil)

(autoload 'company-manual-begin "company" nil t nil)

(autoload 'company-complete "company" "\
Insert the common part of all candidates or the current selection.
The first time this is called, the common part is inserted, the second
time, or when the selection has been changed, the selected candidate is
inserted." t nil)

(register-definition-prefixes "company" '("company-"))

;;;***

;;;### (autoloads nil "company-capf" "company-capf.el" (0 0 0 0))
;;; Generated autoloads from company-capf.el

(register-definition-prefixes "company-capf" '("company-"))

;;;***

;;;### (autoloads nil "company-dabbrev" "company-dabbrev.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from company-dabbrev.el

(autoload 'company-dabbrev "company-dabbrev" "\
dabbrev-like `company-mode' completion backend.

\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

(register-definition-prefixes "company-dabbrev" '("company-dabbrev-"))

;;;***

;;;### (autoloads nil "company-files" "company-files.el" (0 0 0 0))
;;; Generated autoloads from company-files.el

(autoload 'company-files "company-files" "\
`company-mode' completion backend existing file names.
Completions works for proper absolute and relative files paths.
File paths with spaces are only supported inside strings.

\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

(register-definition-prefixes "company-files" '("company-file"))

;;;***

;;;### (autoloads nil "eglot" "eglot.el" (0 0 0 0))
;;; Generated autoloads from eglot.el

(autoload 'eglot "eglot" "\
Manage a project with a Language Server Protocol (LSP) server.
The LSP server of CLASS is started (or contacted) via CONTACT.
If this operation is successful, current *and future* file
buffers of MANAGED-MAJOR-MODE inside PROJECT become \"managed\"
by the LSP server, meaning information about their contents is
exchanged periodically to provide enhanced code-analysis via
`xref-find-definitions', `flymake-mode', `eldoc-mode',
`completion-at-point', among others.
Interactively, the command attempts to guess MANAGED-MAJOR-MODE
from current buffer, CLASS and CONTACT from
`eglot-server-programs' and PROJECT from
`project-find-functions'.  The search for active projects in this
context binds `eglot-lsp-context' (which see).
If it can't guess, the user is prompted.  With a single
\\[universal-argument] prefix arg, it always prompt for COMMAND.
With two \\[universal-argument] prefix args, also prompts for
MANAGED-MAJOR-MODE.
PROJECT is a project object as returned by `project-current'.
CLASS is a subclass of `eglot-lsp-server'.
CONTACT specifies how to contact the server.  It is a
keyword-value plist used to initialize CLASS or a plain list as
described in `eglot-server-programs', which see.
LANGUAGE-ID is the language ID string to send to the server for
MANAGED-MAJOR-MODE, which matters to a minority of servers.
INTERACTIVE is t if called interactively.

\(fn MANAGED-MAJOR-MODE PROJECT CLASS CONTACT LANGUAGE-ID &optional INTERACTIVE)" t nil)

(autoload 'eglot-ensure "eglot" "\
Start Eglot session for current buffer if there isn't one." nil nil)

(put 'eglot-workspace-configuration 'safe-local-variable 'listp)

(register-definition-prefixes "eglot" '("eglot-"))

;;;***

;;;### (autoloads nil "listify" "listify.el" (0 0 0 0))
;;; Generated autoloads from listify.el

(autoload 'listify-tab-completion "listify" "\
Tab completion with `listify-completion-in-region'.

\(fn ARG)" t nil)

(register-definition-prefixes "listify" '("listify-"))

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
