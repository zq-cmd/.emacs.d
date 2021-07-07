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

;;;### (autoloads nil "evil-command-window" "evil-command-window.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-command-window.el

(register-definition-prefixes "evil-command-window" '("evil-"))

;;;***

;;;### (autoloads nil "evil-commands" "evil-commands.el" (0 0 0 0))
;;; Generated autoloads from evil-commands.el

(register-definition-prefixes "evil-commands" '("evil-"))

;;;***

;;;### (autoloads nil "evil-common" "evil-common.el" (0 0 0 0))
;;; Generated autoloads from evil-common.el

(register-definition-prefixes "evil-common" '("bounds-of-evil-" "evil-" "forward-evil-"))

;;;***

;;;### (autoloads nil "evil-core" "evil-core.el" (0 0 0 0))
;;; Generated autoloads from evil-core.el
 (autoload 'evil-mode "evil" nil t)

(register-definition-prefixes "evil-core" '("evil-" "turn-o"))

;;;***

;;;### (autoloads nil "evil-digraphs" "evil-digraphs.el" (0 0 0 0))
;;; Generated autoloads from evil-digraphs.el

(register-definition-prefixes "evil-digraphs" '("evil-digraph"))

;;;***

;;;### (autoloads nil "evil-ex" "evil-ex.el" (0 0 0 0))
;;; Generated autoloads from evil-ex.el

(register-definition-prefixes "evil-ex" '("evil-"))

;;;***

;;;### (autoloads nil "evil-jumps" "evil-jumps.el" (0 0 0 0))
;;; Generated autoloads from evil-jumps.el

(register-definition-prefixes "evil-jumps" '("evil-"))

;;;***

;;;### (autoloads nil "evil-macros" "evil-macros.el" (0 0 0 0))
;;; Generated autoloads from evil-macros.el

(register-definition-prefixes "evil-macros" '("evil-"))

;;;***

;;;### (autoloads nil "evil-maps" "evil-maps.el" (0 0 0 0))
;;; Generated autoloads from evil-maps.el

(register-definition-prefixes "evil-maps" '("evil-"))

;;;***

;;;### (autoloads nil "evil-repeat" "evil-repeat.el" (0 0 0 0))
;;; Generated autoloads from evil-repeat.el

(register-definition-prefixes "evil-repeat" '("evil-"))

;;;***

;;;### (autoloads nil "evil-search" "evil-search.el" (0 0 0 0))
;;; Generated autoloads from evil-search.el

(register-definition-prefixes "evil-search" '("evil-"))

;;;***

;;;### (autoloads nil "evil-states" "evil-states.el" (0 0 0 0))
;;; Generated autoloads from evil-states.el

(register-definition-prefixes "evil-states" '("evil-"))

;;;***

;;;### (autoloads nil "evil-types" "evil-types.el" (0 0 0 0))
;;; Generated autoloads from evil-types.el

(register-definition-prefixes "evil-types" '("evil-ex-get-optional-register-and-count"))

;;;***

;;;### (autoloads nil "evil-vars" "evil-vars.el" (0 0 0 0))
;;; Generated autoloads from evil-vars.el

(register-definition-prefixes "evil-vars" '("evil-"))

;;;***

;;;### (autoloads nil "goto-chg" "goto-chg.el" (0 0 0 0))
;;; Generated autoloads from goto-chg.el

(autoload 'goto-last-change "goto-chg" "\
Go to the point where the last edit was made in the current buffer.
Repeat the command to go to the second last edit, etc.

To go back to more recent edit, the reverse of this command, use \\[goto-last-change-reverse]
or precede this command with \\[universal-argument] - (minus).

It does not go to the same point twice even if there has been many edits
there. I call the minimal distance between distinguishable edits \"span\".
Set variable `glc-default-span' to control how close is \"the same point\".
Default span is 8.
The span can be changed temporarily with \\[universal-argument] right before \\[goto-last-change]:
\\[universal-argument] <NUMBER> set current span to that number,
\\[universal-argument] (no number) multiplies span by 4, starting with default.
The so set span remains until it is changed again with \\[universal-argument], or the consecutive
repetition of this command is ended by any other command.

When span is zero (i.e. \\[universal-argument] 0) subsequent \\[goto-last-change] visits each and
every point of edit and a message shows what change was made there.
In this case it may go to the same point twice.

This command uses undo information. If undo is disabled, so is this command.
At times, when undo information becomes too large, the oldest information is
discarded. See variable `undo-limit'.

\(fn ARG)" t nil)

(autoload 'goto-last-change-reverse "goto-chg" "\
Go back to more recent changes after \\[goto-last-change] have been used.
See `goto-last-change' for use of prefix argument.

\(fn ARG)" t nil)

(register-definition-prefixes "goto-chg" '("glc-"))

;;;***

;;;### (autoloads nil "listify" "listify.el" (0 0 0 0))
;;; Generated autoloads from listify.el

(autoload 'listify-tab-completion "listify" "\
Tab completion with `listify-completion-in-region'.

\(fn ARG)" t nil)

(register-definition-prefixes "listify" '("listify-"))

;;;***

;;;### (autoloads nil "queue" "queue.el" (0 0 0 0))
;;; Generated autoloads from queue.el

(defalias 'make-queue 'queue-create "\
Create an empty queue data structure.")

(register-definition-prefixes "queue" '("queue"))

;;;***

;;;### (autoloads nil "undo-tree" "undo-tree.el" (0 0 0 0))
;;; Generated autoloads from undo-tree.el

(autoload 'undo-tree-mode "undo-tree" "\
Toggle undo-tree mode.
With no argument, this command toggles the mode.
A positive prefix argument turns the mode on.
A negative prefix argument turns it off.

If called interactively, toggle `Undo-Tree mode'.  If the prefix
argument is positive, enable the mode, and if it is zero or
negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

The mode's hook is called both when the mode is enabled and when
it is disabled.

Undo-tree-mode replaces Emacs' standard undo feature with a more
powerful yet easier to use version, that treats the undo history
as what it is: a tree.

The following keys are available in `undo-tree-mode':

  \\{undo-tree-map}

Within the undo-tree visualizer, the following keys are available:

  \\{undo-tree-visualizer-mode-map}

\(fn &optional ARG)" t nil)

(put 'global-undo-tree-mode 'globalized-minor-mode t)

(defvar global-undo-tree-mode nil "\
Non-nil if Global Undo-Tree mode is enabled.
See the `global-undo-tree-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-undo-tree-mode'.")

(custom-autoload 'global-undo-tree-mode "undo-tree" nil)

(autoload 'global-undo-tree-mode "undo-tree" "\
Toggle Undo-Tree mode in all buffers.
With prefix ARG, enable Global Undo-Tree mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if ARG is
omitted or nil.

Undo-Tree mode is enabled in all buffers where
`turn-on-undo-tree-mode' would do it.

See `undo-tree-mode' for more information on Undo-Tree mode.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "undo-tree" '("*undo-tree-id-counter*" "buffer-undo-tree" "turn-on-undo-tree-mode" "undo-"))

;;;***

;;;### (autoloads nil "wgrep" "wgrep.el" (0 0 0 0))
;;; Generated autoloads from wgrep.el

(autoload 'wgrep-setup "wgrep" "\
Setup wgrep preparation." nil nil)

(add-hook 'grep-setup-hook 'wgrep-setup)

(register-definition-prefixes "wgrep" '("wgrep-"))

;;;***

;;;### (autoloads nil nil ("evil.el") (0 0 0 0))

;;;***

(provide '+autoload)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; +autoload.el ends here
