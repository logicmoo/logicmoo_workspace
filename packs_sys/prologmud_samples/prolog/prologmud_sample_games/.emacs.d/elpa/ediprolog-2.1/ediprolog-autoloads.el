;;; ediprolog-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "ediprolog" "ediprolog.el" (0 0 0 0))
;;; Generated autoloads from ediprolog.el

(autoload 'ediprolog-dwim "ediprolog" "\
Load current buffer into Prolog or post query (Do What I Mean).
If invoked on a line starting with `:-' or `?-', possibly
preceded by `%' and whitespace, call `ediprolog-interact' with
the query as argument. Otherwise, call `ediprolog-consult'.

With prefix argument 0, kill the Prolog process. With prefix 1,
equivalent to `ediprolog-consult'. With prefix 2, equivalent to
`ediprolog-consult' with a new Prolog process. With prefix 7,
equivalent to `ediprolog-toplevel'. With just C-u, first call
`ediprolog-consult' and then, if point is on a query, call
`ediprolog-interact' with it as argument. Analogously, C-u C-u
for `ediprolog-consult' with a new process. With other prefix
arguments, equivalent to `ediprolog-remove-interactions'.

\(fn &optional ARG)" t nil)

(autoload 'ediprolog-interact "ediprolog" "\
Send QUERY to Prolog process and interact as on a terminal.

You can use \\[keyboard-quit] to unblock Emacs in the case of
longer-running queries. When the query completes and the toplevel
asks for input, use \\[ediprolog-toplevel] to resume interaction
with the Prolog process.

\(fn QUERY)" nil nil)

(autoload 'ediprolog-remove-interactions "ediprolog" "\
Remove all lines starting with `ediprolog-prefix' from buffer.

In transient mark mode, if the region is active, the function
operates on the region.

\(fn)" t nil)

(autoload 'ediprolog-consult "ediprolog" "\
Buffer is loaded into a Prolog process. If NEW-PROCESS is
non-nil, start a new process. Otherwise use the existing process,
if any. In case of errors, point is moved to the position of the
first error, and the mark is left at the previous position.

In transient mark mode, if the region is active, the function
operates on the region.

\(fn &optional NEW-PROCESS)" t nil)

(autoload 'ediprolog-localize "ediprolog" "\
After `ediprolog-localize', any Prolog process started from
this buffer becomes buffer-local.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ediprolog" '("ediprolog-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ediprolog-autoloads.el ends here
