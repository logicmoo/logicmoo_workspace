;;; win-switch-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "win-switch" "win-switch.el" (0 0 0 0))
;;; Generated autoloads from win-switch.el

(defvar win-switch-idle-time 0.75 "\
Cancel window switching mode when idle time exceeds this threshold.
The time is measured in seconds and can be an integer or
floating-point number.")

(custom-autoload 'win-switch-idle-time "win-switch" t)

(defvar win-switch-window-threshold 2 "\
Number of windows above which dispatch always enters switching mode.
When the current frame has more than this many windows,
`win-switch-dispatch' enters window-switching mode
unconditionally; otherwise, it acts like like
`win-switch-other-window-function' (which is `other-window' by
default).

Besides its effect on window switching behavior, this option also
affects how `win-switch-dispatch' interprets its prefix argument.
See the documentation for `win-switch-dispatch' for details.")

(custom-autoload 'win-switch-window-threshold "win-switch" t)

(defvar win-switch-other-window-first t "\
Whether to move to next window before entering window switching mode.
Should be either a boolean or a boolean function that takes no arguments.
If equal to t or if a function and the function returns a non-nil value,
`win-switch-dispatch' calls `win-switch-next-window' before changing
window-switching modes.")

(custom-autoload 'win-switch-other-window-first "win-switch" t)

(defcustom win-switch-wrap-around t "\
Whether movement off the edge of the frame wraps around.

To set this variable in Lisp code, do not set the variable
directly but rather call the function
`win-switch-set-wrap-around' with argument 1 to turn wrapping
on and -1 to turn wrapping off." :type 'boolean :set (lambda (symbol value) (win-switch-set-wrap-around (if value 1 -1))) :initialize (lambda (symbol value) (setq windmove-wrap-around value) (custom-initialize-default symbol value)) :require 'windmove :group 'win-switch)

(custom-autoload 'win-switch-wrap-around "win-switch" nil)

(defvar win-switch-provide-visual-feedback t "\
Whether to provide visual feedback during window switching mode.")

(custom-autoload 'win-switch-provide-visual-feedback "win-switch" t)

(defvar win-switch-feedback-background-color "red" "\
Mode line background color of active window during switching mode.")

(custom-autoload 'win-switch-feedback-background-color "win-switch" t)

(defvar win-switch-feedback-foreground-color "white" "\
Mode line foreground color of active window during switching mode.")

(custom-autoload 'win-switch-feedback-foreground-color "win-switch" t)

(defvar win-switch-on-feedback-function nil "\
Function to turn on visual feedback, or nil for default behavior.
This function of zero arguments is called when entering window
switching mode, and it should set up conditions that make salient
that window switching mode is turned on. Setting this function
should usually be paired with setting
`win-switch-off-feedback-function' to ensure that what is set on
entry is unset on exit. See `win-switch-on-feedback' for the
default behavior.")

(custom-autoload 'win-switch-on-feedback-function "win-switch" t)

(defvar win-switch-off-feedback-function nil "\
Function to turn off visual feedback, or nil for default behavior.
This function of zero arguments is called when exiting window
switching mode, and it should make salient that window switching
mode is turned off and clear any conditions that were set on
entry. Setting this function should usually be paired with
setting `win-switch-on-feedback-function' to ensure that what is
unset on exit had been set on entry. See the function
`win-switch-off-feedback' for the default behavior.")

(custom-autoload 'win-switch-off-feedback-function "win-switch" t)

(defvar win-switch-other-window-function nil "\
Function to switch windows or nil for default, `other-window'.")

(custom-autoload 'win-switch-other-window-function "win-switch" t)

(defvar win-switch-load-hook nil "\
List of functions to be called when win-switch module is loaded.")

(defvar win-switch-on-hook nil "\
List of functions to be called as window switching mode is entered.
These functions are called just before the overriding key map is set up
and before the timer is started.")

(defvar win-switch-off-hook nil "\
List of functions to be called after window switching mode is exited.
These functions are called after the timer is cleared and the
overriding key map is restored")

(defvar win-switch-abort-hook nil "\
List of functions that check if `win-switch-dispatch' should be aborted.
These functions are called in succession at `win-switch-dispatch'
just before entering window-switching mode and after checking the
usual conditions on the window threshold and prefix arguments. If
any function in this list returns a non-nil value,
window-switching mode is not entered. If the returned value is
'abort, then no action is taken; for any other non-nil value
`win-switch-next-window' is still called. The primary purpose of
these hooks is to allow for conditions where the persistent mode
can cause conflicts or other problems.")

(defvar win-switch-up-keys '("i") "\
List of key sequences that select the window above the current one.

To set this variable from Lisp code, do not just set it directly, but
rather use the function `win-switch-set-keys' as follows:

   (win-switch-set-keys <key-list> 'up)

where <key-list> is a list of key bindings.")

(custom-autoload 'win-switch-up-keys "win-switch" nil)

(defvar win-switch-down-keys '("k") "\
List of key sequences that select the window below the current one.

To set this variable from Lisp code, do not just set it directly, but
rather use the function `win-switch-set-keys' as follows:

   (win-switch-set-keys <key-list> 'down)

where <key-list> is a list of key bindings.")

(custom-autoload 'win-switch-down-keys "win-switch" nil)

(defvar win-switch-left-keys '("j") "\
List of key sequences that select the window left of the current one.

To set this variable from Lisp code, do not just set it directly, but
rather use the function `win-switch-set-keys' as follows:

   (win-switch-set-keys <key-list> 'left)

where <key-list> is a list of key bindings.")

(custom-autoload 'win-switch-left-keys "win-switch" nil)

(defvar win-switch-right-keys '("l") "\
List of key sequences that select the window left of the current one.

To set this variable from Lisp code, do not just set it directly, but
rather use the function `win-switch-set-keys' as follows:

   (win-switch-set-keys <key-list> 'right)

where <key-list> is a list of key bindings.")

(custom-autoload 'win-switch-right-keys "win-switch" nil)

(defvar win-switch-next-window-keys '("o") "\
List of key sequences that select the next window.

To set this variable from Lisp code, do not just set it directly, but
rather use the function `win-switch-set-keys' as follows:

   (win-switch-set-keys <key-list> 'next-window)

where <key-list> is a list of key bindings.")

(custom-autoload 'win-switch-next-window-keys "win-switch" nil)

(defvar win-switch-previous-window-keys '("p") "\
List of key sequences that select the next window.

To set this variable from Lisp code, do not just set it directly, but
rather use the function `win-switch-set-keys' as follows:

   (win-switch-set-keys <key-list> 'previous-window)

where <key-list> is a list of key bindings.")

(custom-autoload 'win-switch-previous-window-keys "win-switch" nil)

(defvar win-switch-enlarge-vertically-keys '("I") "\
List of key sequences that vertically enlarges current window.

To set this variable from Lisp code, do not just set it directly, but
rather use the function `win-switch-set-keys' as follows:

   (win-switch-set-keys <key-list> 'enlarge-vertically)

where <key-list> is a list of key bindings.")

(custom-autoload 'win-switch-enlarge-vertically-keys "win-switch" nil)

(defvar win-switch-shrink-vertically-keys '("K") "\
List of key sequences that vertically shrinks current window.

To set this variable from Lisp code, do not just set it directly, but
rather use the function `win-switch-set-keys' as follows:

   (win-switch-set-keys <key-list> 'shrink-vertically)

where <key-list> is a list of key bindings.")

(custom-autoload 'win-switch-shrink-vertically-keys "win-switch" nil)

(defvar win-switch-shrink-horizontally-keys '("J") "\
List of key sequences that horizontally shrinks current window.

To set this variable from Lisp code, do not just set it directly, but
rather use the function `win-switch-set-keys' as follows:

   (win-switch-set-keys <key-list> 'shrink-horizontally)

where <key-list> is a list of key bindings.")

(custom-autoload 'win-switch-shrink-horizontally-keys "win-switch" nil)

(defvar win-switch-enlarge-horizontally-keys '("L") "\
List of key sequences that horizontally enlarges current window.

To set this variable from Lisp code, do not just set it directly, but
rather use the function `win-switch-set-keys' as follows:

   (win-switch-set-keys <key-list> 'enlarge-horizontally)

where <key-list> is a list of key bindings.")

(custom-autoload 'win-switch-enlarge-horizontally-keys "win-switch" nil)

(defvar win-switch-other-frame-keys '(" ") "\
List of key sequences that select the next frame.

To set this variable from Lisp code, do not just set it directly, but
rather use the function `win-switch-set-keys' as follows:

   (win-switch-set-keys <key-list> 'other-frame)

where <key-list> is a list of key bindings.")

(custom-autoload 'win-switch-other-frame-keys "win-switch" nil)

(defvar win-switch-exit-keys '("u" [return]) "\
List of key sequences that will exit window switching mode.

To set this variable from Lisp code, do not just set it directly, but
rather use the function `win-switch-set-keys' as follows:

   (win-switch-set-keys <key-list> 'exit)

where <key-list> is a list of key bindings.")

(custom-autoload 'win-switch-exit-keys "win-switch" nil)

(defvar win-switch-split-horizontally-keys '(";") "\
List of key sequences that horizontally splits current window.

To set this variable from Lisp code, do not just set it directly, but
rather use the function `win-switch-set-keys' as follows:

   (win-switch-set-keys <key-list> 'split-horizontally)

where <key-list> is a list of key bindings.")

(custom-autoload 'win-switch-split-horizontally-keys "win-switch" nil)

(defvar win-switch-split-vertically-keys '("h") "\
List of key sequences that vertically splits current window.

To set this variable from Lisp code, do not just set it directly, but
rather use the function `win-switch-set-keys' as follows:

   (win-switch-set-keys <key-list> 'split-vertically)

where <key-list> is a list of key bindings.")

(custom-autoload 'win-switch-split-vertically-keys "win-switch" nil)

(defvar win-switch-delete-window-keys '("0") "\
List of key sequences that deletes current window.

To set this variable from Lisp code, do not just set it directly, but
rather use the function `win-switch-set-keys' as follows:

   (win-switch-set-keys <key-list> 'delete-window)

where <key-list> is a list of key bindings.")

(custom-autoload 'win-switch-delete-window-keys "win-switch" nil)

(defvar win-switch-emergency-exit-keys '("\207") "\
List of additional key sequences that will exit window switching mode.
This exits window switching without any niceties, feedback, or
hooks and so should be used only as a last resort. It is intended
only as a precaution for cases in which an unexpected
problem (e.g., in user defined hooks or function-valued options)
makes it impossible to exit window switching mode by another way.
This should not need really be necessary and may be removed in
future versions.

To set this variable from Lisp code, do not just set it directly, but
rather use the function `win-switch-set-keys' as follows:

   (win-switch-set-keys <key-list> 'emergency-exit)

where <key-list> is a list of key bindings.")

(custom-autoload 'win-switch-emergency-exit-keys "win-switch" nil)

(defvar win-switch-once-double-next-keys '("u") "\
List of keys that will advance two windows in `win-switch-dispatch-once'.

To set this variable from Lisp code, do not just set it directly, but
rather use the function `win-switch-set-once-keys' as follows:

   (win-switch-set-once-keys <key-list> 'once-double-next)

where <key-list> is a list of key bindings.")

(custom-autoload 'win-switch-once-double-next-keys "win-switch" nil)

(defvar win-switch-once-double-prev-keys '("y") "\
List of keys that will move back two windows in `win-switch-dispatch-once'.

To set this variable from Lisp code, do not just set it directly, but
rather use the function `win-switch-set-once-keys' as follows:

   (win-switch-set-once-keys <key-list> 'once-double-prev)

where <key-list> is a list of key bindings.")

(custom-autoload 'win-switch-once-double-prev-keys "win-switch" nil)

(autoload 'win-switch-custom-set-keys "win-switch" "\
Set specified key list and adjust `win-switch-map' and other key lists.

This is the most general function for changing the bindings of
commands represented in the predefined key lists. See also
`win-switch-set-keys', which is the API entry point to this
function, along with `win-switch-add-key' and
`win-switch-delete-key'. Note that changing `win-switch-map'
directly need not be reliable, and also see
`win-switch-define-key' for a safe way to make general bindings.

KEY-SYM is a symbol that must be represented in the alist
`win-switch-commands'. The corresponding list of keys will be set
to KEY-LIST, which should be a list of keybindings, and each key
in the list will be bound to the corresponding commands.
An error is raised when trying to empty the exit list.

\(fn KEY-SYM KEY-LIST)" nil nil)

(autoload 'win-switch-set-keys "win-switch" "\
Bind specified keys to a command and adjust `win-switch-map'.

KEY-LIST is a list of keys, each acceptable to `define-key'. NAME
is either a string or a symbol, either of the form
win-switch-<cmd>-keys or just <cmd>, where <cmd> is one of `up',
`down', `left', `right', `next-window', `previous-window',
`enlarge-vertically', `shrink-vertically',
`enlarge-horizontally', `shrink-horizontally', `other-frame',
`exit', `split-vertically', `split-horizontally',
`delete-window', or `emergency-exit'. (Specifically, it must be
on of the keys in the alist `win-switch-commands'.)

\(fn KEY-LIST NAME)" t nil)

(autoload 'win-switch-add-key "win-switch" "\
Add KEY to command list associated with NAME.
KEY is a key binding in any form acceptable to `define-key'.
NAME should be a symbol or string for which the variable
`win-switch-NAME-keys' is defined.

\(fn KEY NAME)" t nil)

(autoload 'win-switch-delete-key "win-switch" "\
Remove KEY from command list associated with NAME.
NAME should be a symbol or string for which the variable
`win-switch-NAME-keys' is defined. KEY is a key binding in any
form acceptable to `define-key'. Removing the last exit key
raises an error, and the last of any other key prompts an alert
message.

\(fn KEY NAME)" t nil)

(autoload 'win-switch-define-key "win-switch" "\
Safely bind KEY to DEF in win-switch keymap.
Attempting to bind the last exit key raises an error. KEY and DEF
are a keybinding and definition, respectively, as would be
acceptable to `define-key'. If DEF is a keymap, ensure that
keymap (and all sub-keymaps) have an exit-inducing
default (`win-switch-exit-and-redo'), unless FORCE-NO-DEFAULT is
non-nil.

\(fn KEY DEF &optional FORCE-NO-DEFAULT)" t nil)

(autoload 'win-switch-set-once-keys "win-switch" "\
Bind specified keys to a once-only command and adjust `win-switch-once-map'.
KEY-LIST is a list of keys, each acceptable to `define-key'. NAME
is either a string or a symbol, either of the form
win-switch-once-<cmd>-keys or just <cmd>, where <cmd> is one of
`double-next' or `double-prev'. (Specifically, it must be
on of the keys in the alist `win-switch-once-commands'.)

\(fn KEY-LIST NAME)" t nil)

(autoload 'win-switch-set-wrap-around "win-switch" "\
Toggle or set window wrapping behavior.
When WRAP is nil, toggle setting of `win-switch-wrap-around'.
Otherwise, WRAP should be an integer, negative to turn off
wrapping and non-negative to turn it on. (Value t also
turns it on for convenience.) This function
synchronizes `windmove-wrap-around' accordingly.

\(fn &optional WRAP)" t nil)

(autoload 'win-switch-dispatch "win-switch" "\
Enter window switching mode, or select next window in the frame.

If the variable `win-switch-other-window-first' is non-nil, then
`win-switch-next-window' is called *before* entering window
switching mode.

The variable `win-switch-window-threshold' determines both the
switching behavior and how this function interprets its argument.

  * When `win-switch-window-threshold' is less than or equal to 0,
    window switching mode is always entered and the argument
    MUST-ENTER-OR-PREFIX is interpreted as a raw format prefix
    argument for any calls to `win-switch-next-window'. This only
    matters if `win-switch-other-window-first' is (or returns)
    a non-nil value.

  * When `win-switch-window-threshold' is greater than 0, entry
    to window switching mode occurs if either the number of
    windows in the current frame is above
    `win-switch-window-threshold' or if the prefix
    argument (MUST-ENTER-OR-PREFIX) is non-nil. Any calls to
    `win-switch-next-window' determined by the configuration are
    still made, and they are given nil as an argument. If window
    switching mode is not entered, `win-switch-next-window' is
    called.

While more complicated than ideal, this dichotomy gives maximum
flexibility for several common use cases.

\(fn &optional MUST-ENTER-OR-PREFIX)" t nil)

(defalias 'win-switch-mode 'win-switch-dispatch)

(autoload 'win-switch-dispatch-with "win-switch" "\
Produce a command to execute COMMAND and then `win-switch-dispatch'.
COMMAND can be a (quoted) symbol, a lambda form, or a variable
bound to a function. FORCE-ENTER, if non-nil, forces
`win-switch-dispatch' to enter window switching mode.

Note that the call to COMMAND replaces any automatic calls to
`win-switch-next-window' or `win-switch-other-window-function'
during dispatch.

\(fn COMMAND &optional FORCE-ENTER)" nil t)

(autoload 'win-switch-dispatch-once "win-switch" "\
Prefix command to execute one window-switching operation.
This command does not enter window-switching mode, nor does it
require an exit. Except for the exit commands, which are
excluded, the commands and keys are shared with
`win-switch-dispatch'. In addition, this includes commands to
move forward and backward by two windows. See
`win-shift-once-double-next-keys' and
`win-shift-once-double-prev-keys' for the associated keys. Taken
together, these bindings make it convenient to use a single key
sequence to navigate conveniently with up to five windows.

\(fn &optional ARG)" t nil)

(autoload 'win-switch-remove-split-and-delete-keys "win-switch" "\
Eliminate window-splitting and deleting keys from win-switch mode." nil nil)

(autoload 'win-switch-setup-keys-ijkl "win-switch" "\
Restore default key commands and bind global dispatch keys.
Under this setup, keys i, j, k, and l will switch windows,
respectively, up, left, down, and right, with other functionality
bound to nearby keys. The arguments DISPATCH-KEYS, if non-nil,
should be a list of keys that will be bound globally to
`win-switch-dispatch'.

\(fn &rest DISPATCH-KEYS)" t nil)

(autoload 'win-switch-setup-keys-ijkl-minimal "win-switch" "\
Restore default key commands and bind global dispatch keys.
Split and delete keys are excluded from the map for simplicity.
Under this setup, keys i, j, k, and l will switch windows,
respectively, up, left, down, and right, with other functionality
bound to nearby keys. The arguments DISPATCH-KEYS, if non-nil,
should be a list of keys that will be bound globally to
`win-switch-dispatch'.

\(fn &rest DISPATCH-KEYS)" t nil)

(defalias 'win-switch-setup-keys-default 'win-switch-setup-keys-ijkl)

(autoload 'win-switch-setup-keys-arrows "win-switch" "\
Set arrow keys as both dispatch and direction control.
Under this setup, pressing an arrow key with MODIFIER does a
window switch in the corresponding direction and then calls
`win-switch-dispatch'. When window-switching mode is engaged, the
arrow keys continue to switch windows in the corresponding
direction, with all the other functionality bound to nearby keys.
MODIFIER is a symbol, one of control, meta, alt, hyper, super but
*not* shift, which is used for enlarging. The arguments
DISPATCH-KEYS, if non-nil, should be a list of keys that will be
bound globally to `win-switch-dispatch'.

\(fn MODIFIER &rest DISPATCH-KEYS)" t nil)

(autoload 'win-switch-setup-keys-arrow-ctrl "win-switch" "\
Set arrow keys as both dispatch (w/control modifer) and direction.
With a control modifier, Each arrow key causes a window switch in
the corresponding direction and engages window-switching mode if
the configuration parameters indicate so. When window-switching
mode is engaged, the arrow keys theh continue to switch windows
in the corresponding direction. The arguments DISPATCH-KEYS, if
non-nil, should be a list of keys that will be bound globally to
`win-switch-dispatch'.

\(fn &rest DISPATCH-KEYS)" t nil)

(autoload 'win-switch-setup-keys-arrow-meta "win-switch" "\
Set arrow keys as both dispatch (w/meta modifer) and direction.
With a meta modifier, Each arrow key causes a window switch in
the corresponding direction and engages window-switching mode if
the configuration parameters indicate so. When window-switching
mode is engaged, the arrow keys theh continue to switch windows
in the corresponding direction. The arguments DISPATCH-KEYS, if
non-nil, should be a list of keys that will be bound globally to
`win-switch-dispatch'.

\(fn &rest DISPATCH-KEYS)" t nil)

(autoload 'win-switch-setup-keys-esdf "win-switch" "\
Set left-handed keys mirroring defaults and bind global dispatch keys.
Under this setup, keys e, s, d, and f will switch windows,
respectively, up, left, down, and right, with other functionality
bound to nearby keys. The arguments DISPATCH-KEYS, if non-nil,
should be a list of keys that will be bound globally to
`win-switch-dispatch'.

\(fn &rest DISPATCH-KEYS)" t nil)

(autoload 'win-switch-authors-configuration "win-switch" "\
Win-switch configuration previously preferred by the package author." t nil)

(autoload 'win-switch-authors-new-configuration "win-switch" "\
Win-switch configuration currently preferred by the package author." t nil)

(register-definition-prefixes "win-switch" '("win-switch-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; win-switch-autoloads.el ends here
