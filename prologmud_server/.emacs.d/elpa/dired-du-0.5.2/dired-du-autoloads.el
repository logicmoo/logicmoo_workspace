;;; dired-du-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "dired-du" "dired-du.el" (0 0 0 0))
;;; Generated autoloads from dired-du.el

(autoload 'dired-du-mode "dired-du" "\
Toggle dired-du mode.
Interactively with no argument, this command toggles the mode.
A positive prefix argument enables the mode, any other prefix
argument disables it.  From Lisp, argument omitted or nil enables
the mode, `toggle' toggles the state.

This is a minor mode.  If called interactively, toggle the
`Dired-du mode' mode.  If the prefix argument is positive, enable
the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `dired-du-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

Show the recursive size of directories in Dired buffers.
Once this mode is enabled, every new Dired buffer displays
recursive dir sizes.
The directory size is obtained with `dired-du-used-space-program'.

Note that obtaining the recursive size of all the directories
in a Dired buffer might be slow; thus, it may significantly delay
the time to display a new Dired buffer.

Instead of enabling `dired-du-mode' by default in all Dired buffers
you might prefer to use this mode as a convenient interface to
the `du' program: just enable it in the current Dired buffer,
and disable it once you have finished checking the used space.

\(fn &optional ARG)" t nil)

(autoload 'dired-du-count-sizes "dired-du" "\
Count sizes of files marked with MARK.
If MARK evaluates nil, then use `dired-marker-char'.

Optional arg ALL-MARKS, if non-nil, then accept all mark characters.

Optional arg INCLUDE-DIRS, if non-nil, include the recursive size of the
marked directories.
If called interactively with a prefix, then prompt for previous
args.  Otherwise, all optional arguments but INCLUDE-DIRS are nil, and
INCLUDE-DIRS is set to variable `dired-du-mode'.

Directories '.' '..' are not special: if they are marked, then return
their recursive size.

\(fn MARK &optional ALL-MARKS INCLUDE-DIRS)" t nil)

(autoload 'dired-du-insert-marked-dirs "dired-du" "\
Insert all marked subdirectories." t nil)

(register-definition-prefixes "dired-du" '("dired-du-"))

;;;***

;;;### (autoloads nil nil ("dired-du-pkg.el" "dired-du-tests.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; dired-du-autoloads.el ends here
