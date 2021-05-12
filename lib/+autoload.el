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
`eglot-server-programs' and PROJECT from `project-current'.  If
it can't guess, the user is prompted.  With a single
\\[universal-argument] prefix arg, it always prompt for COMMAND.
With two \\[universal-argument] prefix args, also prompts for
MANAGED-MAJOR-MODE.
PROJECT is a project instance as returned by `project-current'.
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

;;;### (autoloads nil "emmet-mode" "emmet-mode.el" (0 0 0 0))
;;; Generated autoloads from emmet-mode.el

(autoload 'emmet-expand-line "emmet-mode" "\
Replace the current line's emmet expression with the corresponding expansion.
If prefix ARG is given or region is visible call `emmet-preview' to start an
interactive preview.
Otherwise expand line directly.
For more information see `emmet-mode'.

\(fn ARG)" t nil)

(autoload 'emmet-mode "emmet-mode" "\
Minor mode for writing HTML and CSS markup.
With emmet for HTML and CSS you can write a line like
  ul#name>li.item*2
and have it expanded to
  <ul id=\"name\">
    <li class=\"item\"></li>
    <li class=\"item\"></li>
  </ul>
This minor mode defines keys for quick access:
\\{emmet-mode-keymap}
Home page URL `http://www.emacswiki.org/emacs/Emmet'.
See also `emmet-expand-line'.

If called interactively, toggle `Emmet mode'.  If the prefix
argument is positive, enable the mode, and if it is zero or
negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(autoload 'emmet-expand-yas "emmet-mode" nil t nil)

(autoload 'emmet-preview "emmet-mode" "\
Expand emmet between BEG and END interactively.
This will show a preview of the expanded emmet code and you can
accept it or skip it.

\(fn BEG END)" t nil)

(autoload 'emmet-wrap-with-markup "emmet-mode" "\
Wrap region with markup.

\(fn WRAP-WITH)" t nil)

(autoload 'emmet-next-edit-point "emmet-mode" "\


\(fn COUNT)" t nil)

(autoload 'emmet-prev-edit-point "emmet-mode" "\


\(fn COUNT)" t nil)

(register-definition-prefixes "emmet-mode" '("emmet-"))

;;;***

;;;### (autoloads nil "selectrum" "selectrum.el" (0 0 0 0))
;;; Generated autoloads from selectrum.el

(defvar selectrum-complete-in-buffer t "\
If non-nil, use Selectrum for `completion-in-region'.
This option needs to be set before activating `selectrum-mode'.")

(custom-autoload 'selectrum-complete-in-buffer "selectrum" t)

(autoload 'selectrum-select-from-history "selectrum" "\
Submit or insert candidate from minibuffer history.
To insert the history item into the previous session use the
binding for `selectrum-insert-current-candidate'. To submit the
history item and exit use `selectrum-select-current-candidate'." t nil)

(autoload 'selectrum-completing-read "selectrum" "\
Read choice using Selectrum. Can be used as `completing-read-function'.
For PROMPT, COLLECTION, PREDICATE, REQUIRE-MATCH, INITIAL-INPUT,
HIST, DEF, and INHERIT-INPUT-METHOD, see `completing-read'.

\(fn PROMPT COLLECTION &optional PREDICATE REQUIRE-MATCH INITIAL-INPUT HIST DEF INHERIT-INPUT-METHOD)" nil nil)

(autoload 'selectrum-completing-read-multiple "selectrum" "\
Read one or more choices using Selectrum.
Replaces `completing-read-multiple'. For PROMPT, TABLE,
PREDICATE, REQUIRE-MATCH, INITIAL-INPUT, HIST, DEF, and
INHERIT-INPUT-METHOD, see `completing-read-multiple'.
The option `selectrum-completing-read-multiple-show-help' can be
used to control insertion of additional usage information into
the prompt.

\(fn PROMPT TABLE &optional PREDICATE REQUIRE-MATCH INITIAL-INPUT HIST DEF INHERIT-INPUT-METHOD)" nil nil)

(autoload 'selectrum-completion-in-region "selectrum" "\
Complete in-buffer text using a list of candidates.
Can be used as `completion-in-region-function'. For START, END,
COLLECTION, and PREDICATE, see `completion-in-region'.

\(fn START END COLLECTION PREDICATE)" nil nil)

(autoload 'selectrum-read-buffer "selectrum" "\
Read buffer using Selectrum. Can be used as `read-buffer-function'.
Actually, as long as `selectrum-completing-read' is installed in
`completing-read-function', `read-buffer' already uses Selectrum.
Installing this function in `read-buffer-function' makes sure the
buffers are sorted in the default order (most to least recently
used) rather than in whatever order is defined by
`selectrum-preprocess-candidates-function', which is likely to be
less appropriate. It also allows you to view hidden buffers,
which is otherwise impossible due to tricky behavior of Emacs'
completion machinery. For PROMPT, DEF, REQUIRE-MATCH, and
PREDICATE, see `read-buffer'.

\(fn PROMPT &optional DEF REQUIRE-MATCH PREDICATE)" nil nil)

(autoload 'selectrum-read-file-name "selectrum" "\
Read file name using Selectrum. Can be used as `read-file-name-function'.
For PROMPT, DIR, DEFAULT-FILENAME, MUSTMATCH, INITIAL, and
PREDICATE, see `read-file-name'.

\(fn PROMPT &optional DIR DEFAULT-FILENAME MUSTMATCH INITIAL PREDICATE)" nil nil)

(autoload 'selectrum--fix-dired-read-dir-and-switches "selectrum" "\
Make \\[dired] do the \"right thing\" with its default candidate.
By default \\[dired] uses `read-file-name' internally, which
causes Selectrum to provide you with the first file inside the
working directory as the default candidate. However, it would
arguably be more semantically appropriate to use
`read-directory-name', and this is especially important for
Selectrum since this causes it to select the working directory
initially.
To test that this advice is working correctly, type \\[dired] and
accept the default candidate. You should have opened the working
directory in Dired, and not a filtered listing for the current
file.
This is an `:around' advice for `dired-read-dir-and-switches'.
FUNC and ARGS are standard as in any `:around' advice.

\(fn FUNC &rest ARGS)" nil nil)

(autoload 'selectrum-read-library-name "selectrum" "\
Read and return a library name.
Similar to `read-library-name' except it handles `load-path'
shadows correctly." nil nil)

(autoload 'selectrum--fix-minibuffer-message "selectrum" "\
Ensure the cursor stays at the front of the minibuffer message.
This advice adjusts where the cursor gets placed for the overlay
of `minibuffer-message' and ensures the overlay gets displayed at
the right place without blocking the display of candidates.
To test that this advice is working correctly, type \\[find-file]
twice in a row with `enable-recursive-minibuffers' set to nil.
The overlay indicating that recursive minibuffers are not allowed
should appear right after the user input area, not at the end of
the candidate list and the cursor should stay at the front.
This is an `:around' advice for `minibuffer-message'. FUNC and
ARGS are standard as in all `:around' advice.

\(fn FUNC &rest ARGS)" nil nil)

(define-minor-mode selectrum-mode "\
Minor mode to use Selectrum for `completing-read'." :global t (if selectrum-mode (progn (selectrum-mode -1) (setq selectrum-mode t) (setq selectrum--old-completing-read-function (default-value 'completing-read-function)) (setq-default completing-read-function #'selectrum-completing-read) (setq selectrum--old-read-buffer-function (default-value 'read-buffer-function)) (setq-default read-buffer-function #'selectrum-read-buffer) (setq selectrum--old-read-file-name-function (default-value 'read-file-name-function)) (setq-default read-file-name-function #'selectrum-read-file-name) (setq selectrum--old-completion-in-region-function (default-value 'completion-in-region-function)) (when selectrum-complete-in-buffer (setq-default completion-in-region-function #'selectrum-completion-in-region)) (advice-add #'completing-read-multiple :override #'selectrum-completing-read-multiple) (advice-add 'dired-read-dir-and-switches :around #'selectrum--fix-dired-read-dir-and-switches) (advice-add 'read-library-name :override #'selectrum-read-library-name) (advice-add #'minibuffer-message :around #'selectrum--fix-minibuffer-message) (define-key minibuffer-local-map [remap previous-matching-history-element] 'selectrum-select-from-history)) (when (equal (default-value 'completing-read-function) #'selectrum-completing-read) (setq-default completing-read-function selectrum--old-completing-read-function)) (when (equal (default-value 'read-buffer-function) #'selectrum-read-buffer) (setq-default read-buffer-function selectrum--old-read-buffer-function)) (when (equal (default-value 'read-file-name-function) #'selectrum-read-file-name) (setq-default read-file-name-function selectrum--old-read-file-name-function)) (when (equal (default-value 'completion-in-region-function) #'selectrum-completion-in-region) (setq-default completion-in-region-function selectrum--old-completion-in-region-function)) (advice-remove #'completing-read-multiple #'selectrum-completing-read-multiple) (advice-remove 'dired-read-dir-and-switches #'selectrum--fix-dired-read-dir-and-switches) (advice-remove 'read-library-name #'selectrum-read-library-name) (advice-remove #'minibuffer-message #'selectrum--fix-minibuffer-message) (when (eq (lookup-key minibuffer-local-map [remap previous-matching-history-element]) #'selectrum-select-from-history) (define-key minibuffer-local-map [remap previous-matching-history-element] nil))))

(register-definition-prefixes "selectrum" '("selectrum-"))

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
