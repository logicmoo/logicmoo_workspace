;;; ess-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "ess" "ess.el" (0 0 0 0))
;;; Generated autoloads from ess.el

(autoload 'ess-mode "ess" "\
Major mode for editing ESS source.
Optional arg ALIST describes how to customize the editing mode.
Optional arg PROC-NAME is name of associated inferior process.

\\{ess-mode-map}

Extra binding to note:  'ESC C-\\' indent-region.

Entry to this mode runs the hooks in ess-mode-hook.

You can send text to the inferior ESS process from other buffers containing
ESS source.
    `ess-eval-region' sends the current region to the ESS process.
    `ess-eval-buffer' sends the current buffer to the ESS process.
    `ess-eval-function' sends the current function to the ESS process.
    `ess-eval-line' sends the current line to the ESS process.
    `ess-beginning-of-function' and `ess-end-of-function' move the point to
        the beginning and end of the current ESS function.
    `ess-switch-to-ESS' switches the current buffer to the ESS process buffer.
    `ess-switch-to-end-of-ESS' switches the current buffer to the ESS process
        buffer and puts point at the end of it.

    `ess-eval-region-and-go', `ess-eval-buffer-and-go',
        `ess-eval-function-and-go', and `ess-eval-line-and-go' switch to the S
        process buffer after sending their text.

    `ess-load-file' sources a file of commands to the ESS process.

\\[ess-indent-command] indents for ESS code.
\\[backward-delete-char-untabify] converts tabs to spaces as it moves back.
Comments are indented in a similar way to Emacs-lisp mode:
       `###'     beginning of line
       `##'      the same level of indentation as the code
       `#'       the same column on the right, or to the right of such a
                 column if that is not possible.(default value 40).
                 \\[indent-for-comment] command automatically inserts such a
                 `#' in the right place, or aligns such a comment if it is
                 already inserted.
\\[ess-indent-exp] command indents each line of the syntactic unit following point.

Variables controlling indentation style:
 `ess-tab-always-indent'
    Non-nil means TAB in ESS mode should always reindent the current line,
    regardless of where in the line point is when the TAB command is used.
 `ess-auto-newline'
    Non-nil means automatically newline before and after braces inserted in S
    code.
 `ess-indent-offset'
    Indentation of ESS statements within surrounding block.
    The surrounding block's indentation is the indentation of the line on
    which the open-brace appears.
 `ess-offset-block'
    Indentation of blocks opened with curly braces or anonymous parentheses.
 `ess-offset-arguments'
    Indentation of function arguments or bracket indices.
 `ess-offset-arguments-newline'
    Indentation of function arguments or bracket indices when the opening
    delimiter is immediately followed by a newline.
 `ess-offset-continued'
    Indentation style for continued statements.
 `ess-align-nested-calls'
    Functions whose nested calls should be aligned.
 `ess-align-arguments-in-calls'
    Calls in which arguments should be aligned.
 `ess-align-continuations-in-calls'
    Whether ignore indentation after an operator in calls
 `ess-align-blocks'
    Blocks that should always be aligned vertically.
 `ess-indent-from-lhs'
    Whether function calls given as argument should be indented from the
    parameter name.
 `ess-indent-from-chain-start'
    Whether to indent arguments from the first of several consecutive calls.
 `ess-indent-with-fancy-comments'
    Non-nil means distinguish between #, ##, and ### for indentation.

Furthermore, \\[ess-set-style] command enables you to set up predefined ess-mode
indentation style. At present, predefined style are `BSD', `GNU', `K&R', `C++',
`CLB' (quoted from C language style).

\(fn &optional ALIST PROC-NAME IS-DERIVED)" nil nil)

(autoload 'ess-dump-object-into-edit-buffer "ess" "\
Edit an ESS OBJECT in its own buffer.
Without a prefix argument, this simply finds the file pointed to by
`ess-source-directory'.  If this file does not exist, or if a
prefix argument is given, a dump() command is sent to the ESS process to
generate the source buffer.

\(fn OBJECT)" t nil)

(autoload 'ess-version "ess" "\
Return a string with ESS version information." t nil)

(register-definition-prefixes "ess" '("ess-"))

;;;***

;;;### (autoloads nil "ess-arc-d" "ess-arc-d.el" (0 0 0 0))
;;; Generated autoloads from ess-arc-d.el

(register-definition-prefixes "ess-arc-d" '("ARC"))

;;;***

;;;### (autoloads nil "ess-bugs-d" "ess-bugs-d.el" (0 0 0 0))
;;; Generated autoloads from ess-bugs-d.el

(autoload 'ess-bugs-mode "ess-bugs-d" "\
ESS[BUGS]: Major mode for BUGS." t nil)

(add-to-list 'auto-mode-alist '("\\.[Bb][Uu][Gg]\\'" . ess-bugs-mode))

(add-to-list 'auto-mode-alist '("\\.[Bb][Oo][Gg]\\'" . ess-bugs-mode))

(add-to-list 'auto-mode-alist '("\\.[Bb][Mm][Dd]\\'" . ess-bugs-mode))

(register-definition-prefixes "ess-bugs-d" '("ess-"))

;;;***

;;;### (autoloads nil "ess-bugs-l" "ess-bugs-l.el" (0 0 0 0))
;;; Generated autoloads from ess-bugs-l.el

(register-definition-prefixes "ess-bugs-l" '("ess-bugs-"))

;;;***

;;;### (autoloads nil "ess-custom" "ess-custom.el" (0 0 0 0))
;;; Generated autoloads from ess-custom.el

(register-definition-prefixes "ess-custom" '("R-" "Rnw-mode-hook" "S+" "S-" "SAS-mode-hook" "comint-highlight-prompt" "ess-" "inferior-"))

;;;***

;;;### (autoloads nil "ess-dde" "ess-dde.el" (0 0 0 0))
;;; Generated autoloads from ess-dde.el

(register-definition-prefixes "ess-dde" '("ess-"))

;;;***

;;;### (autoloads nil "ess-font-lock" "ess-font-lock.el" (0 0 0 0))
;;; Generated autoloads from ess-font-lock.el

(register-definition-prefixes "ess-font-lock" '("ess-font-lock-"))

;;;***

;;;### (autoloads nil "ess-generics" "ess-generics.el" (0 0 0 0))
;;; Generated autoloads from ess-generics.el

(autoload 'ess-defgeneric "ess-generics" "\
Define a new function, as with `defun', which can be overloaded.
NAME is the name of the function to create. ARGS are the
arguments to the function. DOCSTRING is a documentation string to
describe the function.  The docstring will automatically have
details about its overload symbol appended to the end. BODY is
code that would be run when there is no override defined.  The
default is to signal error if {name}-function is not defined.

\(fn NAME ARGS DOCSTRING &rest BODY)" nil t)

(function-put 'ess-defgeneric 'doc-string-elt '3)

(function-put 'ess-defgeneric 'lisp-indent-function 'defun)

(register-definition-prefixes "ess-generics" '("ess-"))

;;;***

;;;### (autoloads nil "ess-gretl" "ess-gretl.el" (0 0 0 0))
;;; Generated autoloads from ess-gretl.el

(autoload 'gretl-mode "ess-gretl" "\
Major mode for editing gretl source.  See `ess-mode' for more help.

\(fn &optional PROC-NAME)" t nil)

(autoload 'gretl "ess-gretl" "\
Call 'gretl',
Optional prefix (C-u) allows to set command line arguments, such as
--vsize.  This should be OS agnostic.
If you have certain command line arguments that should always be passed
to gretl, put them in the variable `inferior-gretl-args'.

\(fn &optional START-ARGS)" t nil)

(register-definition-prefixes "ess-gretl" '("ess-gretl-post-run-hook" "gretl-" "inferior-gretl-args"))

;;;***

;;;### (autoloads nil "ess-help" "ess-help.el" (0 0 0 0))
;;; Generated autoloads from ess-help.el

(autoload 'ess-display-help-on-object "ess-help" "\
Display documentation for OBJECT in another window.
If prefix arg is given, force an update of the cached help topics
and query the ESS process for the help file instead of reusing an
existing buffer if it exists. Uses the variable
`inferior-ess-help-command' for the actual help command. Prompts
for the object name based on the cursor location for all cases
except the S-Plus GUI. With S-Plus on Windows (both GUI and in an
inferior Emacs buffer) the GUI help window is used. If COMMAND is
suplied, it is used instead of `inferior-ess-help-command'.

\(fn OBJECT &optional COMMAND)" t nil)

(defalias 'ess-help 'ess-display-help-on-object)

(autoload 'ess-helpobjs-at-point "ess-help" "\
Return a list (def obj fun).
Obj is a name at point, fun is the name of the function call
point is in, and def is either obj or fun (in that order) which
has a a help file, i.e. it is a member of slist (string-list).
nil otherwise.

\(fn SLIST)" nil nil)

(autoload 'ess-goto-info "ess-help" "\
Display node NODE from `ess-mode' info.

\(fn NODE)" nil nil)

(autoload 'ess-submit-bug-report "ess-help" "\
Submit a bug report to the ESS maintainers." t nil)

(register-definition-prefixes "ess-help" '("ess-"))

;;;***

;;;### (autoloads nil "ess-inf" "ess-inf.el" (0 0 0 0))
;;; Generated autoloads from ess-inf.el

(autoload 'ess-proc-name "ess-inf" "\
Return name of process N, as a string, with NAME prepended.
If `ess-plain-first-buffername', then initial process is number-free.

\(fn N NAME)" nil nil)

(autoload 'inferior-ess "ess-inf" "\
Start inferior ESS process.

Without a prefix argument, starts a new ESS process, or switches
to the ESS process associated with the current buffer.  With
ESS-START-ARGS (perhaps specified via \\[universal-argument]),
starts the process with those args.  The current buffer is used
if it is an `inferior-ess-mode' or `ess-transcript-mode' buffer.

If `ess-ask-about-transfile' is non-nil, you will be asked for a
transcript file to use.  If there is no transcript file, the
buffer name will be like *R* or *R2*, determined by
`ess-gen-proc-buffer-name-function'.

Takes the program name from the variable `inferior-ess-program'.
An initialization file (dumped into the process) is specified by
`inferior-ess-start-file', and `inferior-ess-start-args' is used
to accompany the call for `inferior-ess-program'.

When creating a new process, the process buffer replaces the
current window if `inferior-ess-same-window' is non-nil.
Alternatively, it can appear in its own frame if
`inferior-ess-own-frame' is non-nil.

\(Type \\[describe-mode] in the process buffer for a list of
commands.)

CUSTOMIZE-ALIST is the list of dialect-specific variables.  When
non-nil, NO-WAIT tells ESS not to wait for the process to finish.
This may be useful for debugging.

\(fn &optional ESS-START-ARGS CUSTOMIZE-ALIST NO-WAIT)" t nil)

(ess-defgeneric ess-load-file (&optional filename) "Load a source file into an inferior ESS process.\nThis handles Tramp when working on a remote." (interactive (list (or (and (memq major-mode '(ess-mode ess-julia-mode)) (buffer-file-name)) (expand-file-name (read-file-name "Load source file: " nil nil t))))) (ess-load-file--normalise-buffer filename) (save-selected-window (ess-switch-to-ESS t)) (:override (let ((file (ess-load-file--normalise-file filename))) (let ((command (ess-build-load-command file nil t))) (ess-send-string (ess-get-process) command t)))))

(autoload 'inferior-ess-mode "ess-inf" "\
Major mode for interacting with an inferior ESS process.
Runs an S interactive job as a subprocess of Emacs, with I/O through an
Emacs buffer.  Variable `inferior-ess-program' controls which S
is run.

Commands are sent to the ESS process by typing them, and pressing
\\[inferior-ess-send-input].  Pressing \\[complete-dynamic-complete]
completes known object names or filenames, as appropriate.  Other
keybindings for this mode are:

\\{inferior-ess-mode-map}

When editing S objects, the use of \\[ess-load-file] is advocated.
`ess-load-file' keeps source files (if `ess-keep-dump-files' is non-nil) in
the directory specified by `ess-source-directory', with the
filename chosen according to `ess-dump-filename-template'. When a file is
loaded, `ess-mode' parses error messages and jumps to the appropriate file
if errors occur. The ess-eval- commands do not do this.

Customization: Entry to this mode runs the hooks on `comint-mode-hook' and
`inferior-ess-mode-hook' (in that order).

You can send text to the inferior ESS process from other buffers
containing source code. The key bindings of these commands can be
found by typing \\[describe-mode].

    `ess-eval-region' sends the current region to the ESS process.
    `ess-eval-buffer' sends the current buffer to the ESS process.
    `ess-eval-function' sends the current function to the ESS process.
    `ess-eval-line' sends the current line to the ESS process.
    `ess-beginning-of-function' and `ess-end-of-function' move the point to
        the beginning and end of the current S function.
    `ess-switch-to-ESS' switches the current buffer to the ESS process buffer.
    `ess-switch-to-end-of-ESS' switches the current buffer to the ESS process
        buffer and puts point at the end of it.

    `ess-eval-region-and-go', `ess-eval-buffer-and-go',
        `ess-eval-function-and-go', and `ess-eval-line-and-go' switch to the S
        process buffer after sending their text.
    `ess-dump-object-into-edit-buffer' moves an S object into a temporary file
        and buffer for editing
    `ess-load-file' sources a file of commands to the ESS process.

Commands:
Return after the end of the process' output sends the text from the
    end of process to point.
Return before the end of the process' output copies the sexp ending at point
    to the end of the process' output, and sends it.
Delete converts tabs to spaces as it moves back.
C-M-q does Tab on each line starting within following expression.
Paragraphs are separated only by blank lines.  Crosshatches start comments.
If you accidentally suspend your process, use \\[comint-continue-subjob]
to continue it." t nil)

(register-definition-prefixes "ess-inf" '("ess-" "inferior-ess-" "update-ess-process-name-list" "with-ess-process-buffer"))

;;;***

;;;### (autoloads nil "ess-jags-d" "ess-jags-d.el" (0 0 0 0))
;;; Generated autoloads from ess-jags-d.el

(autoload 'ess-jags-mode "ess-jags-d" "\
ESS[JAGS]: Major mode for JAGS." t nil)

(add-to-list 'auto-mode-alist '("\\.[Jj][Aa][Gg]\\'" . ess-jags-mode))

(register-definition-prefixes "ess-jags-d" '("ess-jags-"))

;;;***

;;;### (autoloads nil "ess-julia" "ess-julia.el" (0 0 0 0))
;;; Generated autoloads from ess-julia.el

(autoload 'ess-julia-mode "ess-julia" "\
Major mode for editing julia source.  See `ess-mode' for more help.

\(fn)" t nil)

(autoload 'julia "ess-julia" "\
Call 'julia'.
Optional prefix (C-u) allows to set command line arguments, such as
--load=<file>.  This should be OS agnostic.
If you have certain command line arguments that should always be passed
to julia, put them in the variable `inferior-julia-args'.

\(fn &optional START-ARGS)" t nil)

(register-definition-prefixes "ess-julia" '("ac-source-ess-julia-objects" "company-ess-julia-objects" "ess-" "inferior-julia-args"))

;;;***

;;;### (autoloads nil "ess-lsp-l" "ess-lsp-l.el" (0 0 0 0))
;;; Generated autoloads from ess-lsp-l.el

(register-definition-prefixes "ess-lsp-l" '("Lisp-editing-alist"))

;;;***

;;;### (autoloads nil "ess-mouse" "ess-mouse.el" (0 0 0 0))
;;; Generated autoloads from ess-mouse.el

(register-definition-prefixes "ess-mouse" '("ess-"))

;;;***

;;;### (autoloads nil "ess-noweb" "ess-noweb.el" (0 0 0 0))
;;; Generated autoloads from ess-noweb.el

(register-definition-prefixes "ess-noweb" '("ess-"))

;;;***

;;;### (autoloads nil "ess-noweb-font-lock-mode" "ess-noweb-font-lock-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ess-noweb-font-lock-mode.el

(autoload 'ess-noweb-font-lock-mode "ess-noweb-font-lock-mode" "\
Minor mode for syntax highlighting when using `ess-noweb-mode' to edit noweb files.
Each chunk is fontified in accordance with its own mode.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "ess-noweb-font-lock-mode" '("ess-noweb-" "nwfl-donowt"))

;;;***

;;;### (autoloads nil "ess-noweb-mode" "ess-noweb-mode.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from ess-noweb-mode.el

(autoload 'ess-noweb-mode "ess-noweb-mode" "\
Minor meta mode for editing noweb files.
`Meta' refers to the fact that this minor mode is switching major
modes depending on the location of point.

The following special keystrokes are available in noweb mode:

Movement:
\\[ess-noweb-next-chunk] 	goto the next chunk
\\[ess-noweb-previous-chunk] 	goto the previous chunk
\\[ess-noweb-goto-previous] 	goto the previous chunk of the same name
\\[ess-noweb-goto-next] 	goto the next chunk of the same name
\\[ess-noweb-goto-chunk] 		goto a chunk
\\[ess-noweb-next-code-chunk] 		goto the next code chunk
\\[ess-noweb-previous-code-chunk] 		goto the previous code chunk
\\[ess-noweb-next-doc-chunk] 		goto the next documentation chunk
\\[ess-noweb-previous-doc-chunk] 		goto the previous documentation chunk

Copying/Killing/Marking/Narrowing:
\\[ess-noweb-copy-chunk-as-kill] 		copy the chunk the point is in into the kill ring
\\[ess-noweb-copy-chunk-pair-as-kill] 		copy the pair of doc/code chunks the point is in
\\[ess-noweb-kill-chunk] 		kill the chunk the point is in
\\[ess-noweb-kill-chunk-pair] 		kill the pair of doc/code chunks the point is in
\\[ess-noweb-mark-chunk] 		mark the chunk the point is in
\\[ess-noweb-mark-chunk-pair] 		mark the pair of doc/code chunks the point is in
\\[ess-noweb-narrow-to-chunk] 		narrow to the chunk the point is in
\\[ess-noweb-narrow-to-chunk-pair] 		narrow to the pair of doc/code chunks the point is in
\\[widen] 	widen
\\[ess-noweb-toggle-narrowing] 		toggle auto narrowing

Filling and Indenting:
\\[ess-noweb-fill-chunk] 	fill (or indent) the chunk at point according to mode
\\[ess-noweb-fill-paragraph-chunk] 	fill the paragraph at point, restricted to chunk
\\[ess-noweb-indent-line] 	indent the line at point according to mode

Insertion:
\\[ess-noweb-insert-default-mode-line] 	insert a line to set this file's code mode
\\[ess-noweb-new-chunk] 		insert a new chunk at point
\\[ess-noweb-complete-chunk] 	complete the chunk name before point
\\[ess-noweb-electric-@] 		insert a `@' or start a new doc chunk
\\[ess-noweb-electric-<] 		insert a `<' or start a new code chunk

Modes:
\\[ess-noweb-set-doc-mode] 		set the major mode for editing doc chunks
\\[ess-noweb-set-code-mode] 	set the major mode for editing code chunks
\\[ess-noweb-set-this-code-mode] 	set the major mode for editing this code chunk

Misc:
\\[ess-noweb-occur] 		find all occurrences of the current chunk
\\[ess-noweb-update-chunk-vector] 	update the markers for chunks
\\[ess-noweb-describe-mode] 	describe ess-noweb-mode

\(fn &optional ARG)" t nil)

(register-definition-prefixes "ess-noweb-mode" '("ess-noweb-"))

;;;***

;;;### (autoloads nil "ess-omg-d" "ess-omg-d.el" (0 0 0 0))
;;; Generated autoloads from ess-omg-d.el

(autoload 'OMG-mode "ess-omg-d" "\
Major mode for editing Omegahat source.  NOT EVEN STARTED.

\(fn &optional PROC-NAME)" t nil)

(add-to-list 'auto-mode-alist '("\\.omg\\'" . omegahat-mode))

(add-to-list 'auto-mode-alist '("\\.hat\\'" . omegahat-mode))

(register-definition-prefixes "ess-omg-d" '("OMG"))

;;;***

;;;### (autoloads nil "ess-omg-l" "ess-omg-l.el" (0 0 0 0))
;;; Generated autoloads from ess-omg-l.el

(register-definition-prefixes "ess-omg-l" '("OMG-" "ess-"))

;;;***

;;;### (autoloads nil "ess-r-a" "ess-r-a.el" (0 0 0 0))
;;; Generated autoloads from ess-r-a.el

(register-definition-prefixes "ess-r-a" '("ess" "pd::set-up-demo"))

;;;***

;;;### (autoloads nil "ess-r-completion" "ess-r-completion.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from ess-r-completion.el

(register-definition-prefixes "ess-r-completion" '("ac-source-R" "company-R-" "ess-"))

;;;***

;;;### (autoloads nil "ess-r-flymake" "ess-r-flymake.el" (0 0 0 0))
;;; Generated autoloads from ess-r-flymake.el

(register-definition-prefixes "ess-r-flymake" '("ess-r-"))

;;;***

;;;### (autoloads nil "ess-r-gui" "ess-r-gui.el" (0 0 0 0))
;;; Generated autoloads from ess-r-gui.el

(register-definition-prefixes "ess-r-gui" '("Rgui" "ess-" "inferior-"))

;;;***

;;;### (autoloads nil "ess-r-mode" "ess-r-mode.el" (0 0 0 0))
;;; Generated autoloads from ess-r-mode.el

(defvar ess-dev-map (let (ess-dev-map) (define-prefix-command 'ess-dev-map) (define-key ess-dev-map "\23" 'ess-r-set-evaluation-env) (define-key ess-dev-map "s" 'ess-r-set-evaluation-env) (define-key ess-dev-map "T" 'ess-toggle-tracebug) (define-key ess-dev-map "\f" 'ess-r-devtools-load-package) (define-key ess-dev-map "l" 'ess-r-devtools-load-package) (define-key ess-dev-map "`" 'ess-show-traceback) (define-key ess-dev-map "~" 'ess-show-call-stack) (define-key ess-dev-map "\27" 'ess-watch) (define-key ess-dev-map "w" 'ess-watch) (define-key ess-dev-map "\4" 'ess-debug-flag-for-debugging) (define-key ess-dev-map "d" 'ess-debug-flag-for-debugging) (define-key ess-dev-map "\25" 'ess-debug-unflag-for-debugging) (define-key ess-dev-map "u" 'ess-debug-unflag-for-debugging) (define-key ess-dev-map [(control 68)] 'ess-debug-unflag-for-debugging) (define-key ess-dev-map "\2" 'ess-bp-set) (define-key ess-dev-map "b" 'ess-bp-set) (define-key ess-dev-map [(control 66)] 'ess-bp-set-conditional) (define-key ess-dev-map "B" 'ess-bp-set-conditional) (define-key ess-dev-map "\f" 'ess-bp-set-logger) (define-key ess-dev-map "L" 'ess-bp-set-logger) (define-key ess-dev-map "\17" 'ess-bp-toggle-state) (define-key ess-dev-map "o" 'ess-bp-toggle-state) (define-key ess-dev-map "\13" 'ess-bp-kill) (define-key ess-dev-map "k" 'ess-bp-kill) (define-key ess-dev-map "\13" 'ess-bp-kill-all) (define-key ess-dev-map "K" 'ess-bp-kill-all) (define-key ess-dev-map "\16" 'ess-bp-next) (define-key ess-dev-map "n" 'ess-bp-next) (define-key ess-dev-map "i" 'ess-debug-goto-input-event-marker) (define-key ess-dev-map "I" 'ess-debug-goto-input-event-marker) (define-key ess-dev-map "\20" 'ess-bp-previous) (define-key ess-dev-map "p" 'ess-bp-previous) (define-key ess-dev-map "\5" 'ess-debug-toggle-error-action) (define-key ess-dev-map "e" 'ess-debug-toggle-error-action) (define-key ess-dev-map "0" 'ess-electric-selection) (define-key ess-dev-map "1" 'ess-electric-selection) (define-key ess-dev-map "2" 'ess-electric-selection) (define-key ess-dev-map "3" 'ess-electric-selection) (define-key ess-dev-map "4" 'ess-electric-selection) (define-key ess-dev-map "5" 'ess-electric-selection) (define-key ess-dev-map "6" 'ess-electric-selection) (define-key ess-dev-map "7" 'ess-electric-selection) (define-key ess-dev-map "8" 'ess-electric-selection) (define-key ess-dev-map "9" 'ess-electric-selection) (define-key ess-dev-map "?" 'ess-tracebug-show-help) ess-dev-map) "\
Keymap for commands related to development and debugging.")

(autoload 'run-ess-r "ess-r-mode" "\
Call 'R', the 'GNU S' system from the R Foundation.
Optional prefix (\\[universal-argument]) allows to set command line arguments, such as
--vsize.  This should be OS agnostic.
If you have certain command line arguments that should always be passed
to R, put them in the variable `inferior-R-args'.

START-ARGS can be a string representing an argument, a list of
such strings, or any other non-nil value.  In the latter case, you
will be prompted to enter arguments interactively.

\(fn &optional START-ARGS)" t nil)

(defalias 'R #'run-ess-r)

(autoload 'R-mode "ess-r-mode" "\
Major mode for editing R source.  See `ess-mode' for more help.

\(fn &optional PROC-NAME)" t nil)

(defalias 'r-mode #'R-mode)

(defalias 'ess-r-mode #'R-mode)

(add-to-list 'auto-mode-alist '("/R/.*\\.q\\'" . R-mode))

(add-to-list 'auto-mode-alist '("\\.[rR]\\'" . R-mode))

(add-to-list 'auto-mode-alist '("\\.[rR]profile\\'" . R-mode))

(add-to-list 'auto-mode-alist '("NAMESPACE\\'" . R-mode))

(add-to-list 'auto-mode-alist '("CITATION\\'" . R-mode))

(autoload 'Rnw-mode "ess-r-mode" "\
Major mode for editing Sweave(R) source.
See `ess-noweb-mode' and `R-mode' for more help." t nil)

(add-to-list 'auto-mode-alist '("\\.[rR]nw\\'" . Rnw-mode))

(add-to-list 'auto-mode-alist '("\\.[sS]nw\\'" . Snw-mode))

(autoload 'R-transcript-mode "ess-r-mode" "\
Does the right thing." t nil)

(add-to-list 'auto-mode-alist '("\\.[Rr]out" . R-transcript-mode))

(add-to-list 'interpreter-mode-alist '("Rscript" . R-mode))

(add-to-list 'interpreter-mode-alist '("r" . R-mode))

(add-to-list 'auto-mode-alist '("/Makevars\\(\\.win\\)?$" . makefile-mode))

(register-definition-prefixes "ess-r-mode" '("R-" "ess-" "inferior-ess-r-"))

;;;***

;;;### (autoloads nil "ess-r-package" "ess-r-package.el" (0 0 0 0))
;;; Generated autoloads from ess-r-package.el

(register-definition-prefixes "ess-r-package" '("ess-"))

;;;***

;;;### (autoloads nil "ess-r-syntax" "ess-r-syntax.el" (0 0 0 0))
;;; Generated autoloads from ess-r-syntax.el

(register-definition-prefixes "ess-r-syntax" '("backward-ess-r-" "ess-" "forward-ess-r-"))

;;;***

;;;### (autoloads nil "ess-r-xref" "ess-r-xref.el" (0 0 0 0))
;;; Generated autoloads from ess-r-xref.el

(register-definition-prefixes "ess-r-xref" '("ess-r-xref-"))

;;;***

;;;### (autoloads nil "ess-rd" "ess-rd.el" (0 0 0 0))
;;; Generated autoloads from ess-rd.el

(autoload 'Rd-mode "ess-rd" "\
Major mode for editing R documentation source files.

This mode makes it easier to write R documentation by helping with
indentation, doing some of the typing for you (with Abbrev mode) and by
showing keywords, strings, etc. in different faces (with Font Lock mode
on terminals that support it).

Type \\[list-abbrevs] to display the built-in abbrevs for Rd keywords.

Keybindings
===========

\\{Rd-mode-map}

Variables you can use to customize Rd mode
==========================================

`Rd-indent-level'
  Indentation of Rd code with respect to containing blocks.
  Default is 2.

Turning on Rd mode runs the hook `Rd-mode-hook'.

To automatically turn on the abbrev(iate) features, add the
following lines to your `.emacs' file:

  (add-hook 'Rd-mode-hook
            (lambda ()
              (abbrev-mode 1)))" t nil)

(add-to-list 'auto-mode-alist '("\\.Rd\\'" . Rd-mode))

(autoload 'Rd-preview-help "ess-rd" "\
Preview the current Rd buffer contents as help.
If optional VIA-SHELL is set, using `Rd-to-help-command'.
If the current buffer is not associated with a file, create a
temporary one in `temporary-file-directory'.

\(fn &optional VIA-SHELL)" t nil)

(register-definition-prefixes "ess-rd" '("Rd-" "ess"))

;;;***

;;;### (autoloads nil "ess-rdired" "ess-rdired.el" (0 0 0 0))
;;; Generated autoloads from ess-rdired.el

(autoload 'ess-rdired "ess-rdired" "\
Run dired-like mode on R objects.
This is the main function.  See documentation for `ess-rdired-mode' though
for more information!" t nil)

(register-definition-prefixes "ess-rdired" '("ess-rdired-"))

;;;***

;;;### (autoloads nil "ess-roxy" "ess-roxy.el" (0 0 0 0))
;;; Generated autoloads from ess-roxy.el

(register-definition-prefixes "ess-roxy" '("ess-"))

;;;***

;;;### (autoloads nil "ess-rutils" "ess-rutils.el" (0 0 0 0))
;;; Generated autoloads from ess-rutils.el

(register-definition-prefixes "ess-rutils" '("ess-rutils-"))

;;;***

;;;### (autoloads nil "ess-s-lang" "ess-s-lang.el" (0 0 0 0))
;;; Generated autoloads from ess-s-lang.el

(add-to-list 'auto-mode-alist '("\\.[Ss]t\\'" . S-transcript-mode))

(add-to-list 'auto-mode-alist '("\\.Sout" . S-transcript-mode))

(register-definition-prefixes "ess-s-lang" '("S+common-cust-alist" "S-" "ess-" "inferior-S-language-start"))

;;;***

;;;### (autoloads nil "ess-s3-d" "ess-s3-d.el" (0 0 0 0))
;;; Generated autoloads from ess-s3-d.el

(register-definition-prefixes "ess-s3-d" 'nil)

;;;***

;;;### (autoloads nil "ess-s4-d" "ess-s4-d.el" (0 0 0 0))
;;; Generated autoloads from ess-s4-d.el

(register-definition-prefixes "ess-s4-d" 'nil)

;;;***

;;;### (autoloads nil "ess-sas-a" "ess-sas-a.el" (0 0 0 0))
;;; Generated autoloads from ess-sas-a.el

(register-definition-prefixes "ess-sas-a" '("ess-" "sas-program"))

;;;***

;;;### (autoloads nil "ess-sas-d" "ess-sas-d.el" (0 0 0 0))
;;; Generated autoloads from ess-sas-d.el

(autoload 'SAS-mode "ess-sas-d" "\
Major mode for editing SAS source.  See ess-mode for more help.

\(fn &optional PROC-NAME)" t nil)

(add-to-list 'auto-mode-alist '("\\.[Ss][Aa][Ss]\\'" . SAS-mode))

(autoload 'SAS-menu "ess-sas-d" "\
Start SAS from the menu." t nil)

(register-definition-prefixes "ess-sas-d" '("SAS" "ess-" "inferior-SAS-args"))

;;;***

;;;### (autoloads nil "ess-sas-l" "ess-sas-l.el" (0 0 0 0))
;;; Generated autoloads from ess-sas-l.el

(register-definition-prefixes "ess-sas-l" '("SAS-" "backward-page-top-of-window" "beginning-of-sas-" "ess-" "fix-page-breaks" "forward-page-top-of-window" "indent-sas-statement" "next-sas-proc" "sas-" "set-sas-file-" "submit-sas" "switch-to-"))

;;;***

;;;### (autoloads nil "ess-site" "ess-site.el" (0 0 0 0))
;;; Generated autoloads from ess-site.el

(register-definition-prefixes "ess-site" '("ess-"))

;;;***

;;;### (autoloads nil "ess-sp3-d" "ess-sp3-d.el" (0 0 0 0))
;;; Generated autoloads from ess-sp3-d.el

(register-definition-prefixes "ess-sp3-d" '("S+3"))

;;;***

;;;### (autoloads nil "ess-sp4-d" "ess-sp4-d.el" (0 0 0 0))
;;; Generated autoloads from ess-sp4-d.el

(register-definition-prefixes "ess-sp4-d" '("S+4" "Sqpe+4" "inferior-S+4-multipleinstances"))

;;;***

;;;### (autoloads nil "ess-sp5-d" "ess-sp5-d.el" (0 0 0 0))
;;; Generated autoloads from ess-sp5-d.el

(register-definition-prefixes "ess-sp5-d" '("S+5"))

;;;***

;;;### (autoloads nil "ess-sp6-d" "ess-sp6-d.el" (0 0 0 0))
;;; Generated autoloads from ess-sp6-d.el

(autoload 'S+-mode "ess-sp6-d" "\
Major mode for editing S+ source.  See `ess-mode' for more help.

\(fn &optional PROC-NAME)" t nil)

(register-definition-prefixes "ess-sp6-d" '("S+" "ess-"))

;;;***

;;;### (autoloads nil "ess-sp6w-d" "ess-sp6w-d.el" (0 0 0 0))
;;; Generated autoloads from ess-sp6w-d.el

(add-to-list 'auto-mode-alist '("\\.sp\\'" . S-mode))

(add-to-list 'auto-mode-alist '("\\.[qsS]\\'" . S-mode))

(add-to-list 'auto-mode-alist '("\\.ssc\\'" . S-mode))

(add-to-list 'auto-mode-alist '("\\.SSC\\'" . S-mode))

(register-definition-prefixes "ess-sp6w-d" '("S+" "Sqpe+" "ess-sqpe-versions-create" "inferior-S+"))

;;;***

;;;### (autoloads nil "ess-stata-lang" "ess-stata-lang.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from ess-stata-lang.el

(register-definition-prefixes "ess-stata-lang" '("STA-" "ado-set-font-lock-keywords" "ess-" "stata-"))

;;;***

;;;### (autoloads nil "ess-stata-mode" "ess-stata-mode.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from ess-stata-mode.el

(autoload 'STA-mode "ess-stata-mode" "\
Major mode for editing Stata source.  See `ess-mode' for more help.

\(fn &optional PROC-NAME)" t nil)

(add-to-list 'auto-mode-alist '("\\.do\\'" . STA-mode))

(add-to-list 'auto-mode-alist '("\\.ado\\'" . STA-mode))

(register-definition-prefixes "ess-stata-mode" '("STA-" "ess-" "stata"))

;;;***

;;;### (autoloads nil "ess-swv" "ess-swv.el" (0 0 0 0))
;;; Generated autoloads from ess-swv.el

(register-definition-prefixes "ess-swv" '("ess-"))

;;;***

;;;### (autoloads nil "ess-toolbar" "ess-toolbar.el" (0 0 0 0))
;;; Generated autoloads from ess-toolbar.el

(register-definition-prefixes "ess-toolbar" '("ess-"))

;;;***

;;;### (autoloads nil "ess-tracebug" "ess-tracebug.el" (0 0 0 0))
;;; Generated autoloads from ess-tracebug.el

(register-definition-prefixes "ess-tracebug" '("ess-" "inferior-ess-" "org-"))

;;;***

;;;### (autoloads nil "ess-trns" "ess-trns.el" (0 0 0 0))
;;; Generated autoloads from ess-trns.el

(autoload 'ess-transcript-mode "ess-trns" "\
Major mode for manipulating {ESS} transcript files.

Type \\[ess-transcript-send-command] to send a command in the
transcript to the current S process. \\[ess-transcript-copy-command]
copies the command but does not execute it, allowing you to edit it in
the process buffer first.

Type \\[ess-transcript-clean-region] to delete all outputs and prompts
in the region, leaving only the S commands.  Other keybindings are:

\\{ess-transcript-mode-map}

\(fn ALIST &optional PROC)" nil nil)

(autoload 'ess-transcript-clean-region "ess-trns" "\
Strip the transcript in the region, leaving only (R/S/Lsp/..) commands.
Deletes any lines not beginning with a prompt, and then removes the
prompt from those lines that remain.  Prefix argument means to
clean even if the buffer is \\[read-only].

\(fn BEG END EVEN-IF-READ-ONLY)" t nil)

(register-definition-prefixes "ess-trns" '("ess-transcript-"))

;;;***

;;;### (autoloads nil "ess-utils" "ess-utils.el" (0 0 0 0))
;;; Generated autoloads from ess-utils.el

(register-definition-prefixes "ess-utils" '("ess-"))

;;;***

;;;### (autoloads nil "ess-vst-d" "ess-vst-d.el" (0 0 0 0))
;;; Generated autoloads from ess-vst-d.el

(register-definition-prefixes "ess-vst-d" '("VST-" "ViSta"))

;;;***

;;;### (autoloads nil "ess-xls-d" "ess-xls-d.el" (0 0 0 0))
;;; Generated autoloads from ess-xls-d.el

(autoload 'XLS-mode "ess-xls-d" "\
Major mode for editing XLispStat source.  NOT EVEN STARTED.

\(fn &optional PROC-NAME)" t nil)

(add-to-list 'auto-mode-alist '("\\.lsp\\'" . XLS-mode))

(register-definition-prefixes "ess-xls-d" '("XLS" "ess-help-XLS-sec-keys-alist" "xls-transcript-mode"))

;;;***

;;;### (autoloads nil "essd-els" "essd-els.el" (0 0 0 0))
;;; Generated autoloads from essd-els.el

(autoload 'ess-remote "essd-els" "\
Execute this command from within a buffer running a process.  It
runs `ess-add-ess-process' to add the process to
`ess-process-name-alist' and to make it the
`ess-current-process-name'.  It then prompts the user for an ESS
language and sets the editing characteristics appropriately.

To use this command, first start a process on a remote computer by
manual use of telnet, rlogin, ssh, or some other protocol.  Start the
relevant program (\"S\" or \"R\" or \"sas -stdio\") in that buffer.  Once
you are talking to S or R or SAS, then execute `ess-remote' to make
the current buffer an inferior-ess buffer with the right behavior for
the language you are currently working with.  With S and R, use C-c
C-n to send lines over.  With SAS, use C-c i
`ess-eval-line-and-step-invisibly' to send lines over invisibly.

DIALECT is the desired ess-dialect. If nil, ask for dialect

\(fn &optional PROC-NAME DIALECT)" t nil)

(register-definition-prefixes "essd-els" '("S+elsewhere" "ess-" "inferior-ess-remote-pager"))

;;;***

;;;### (autoloads nil "make-regexp" "make-regexp.el" (0 0 0 0))
;;; Generated autoloads from make-regexp.el

(register-definition-prefixes "make-regexp" '("make-regexp" "regexp-span"))

;;;***

;;;### (autoloads nil "mouseme" "mouseme.el" (0 0 0 0))
;;; Generated autoloads from mouseme.el

(autoload 'mouse-me "mouseme" "\
Popup a menu of functions to run on selected string or region.

\(fn EVENT)" t nil)

(register-definition-prefixes "mouseme" '("mouse-me-"))

;;;***

;;;### (autoloads nil nil ("ess-mode.el" "ess-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ess-autoloads.el ends here
