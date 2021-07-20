;;; org-ql-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "org-ql" "org-ql.el" (0 0 0 0))
;;; Generated autoloads from org-ql.el

(autoload 'org-ql "org-ql" "\
Expands into a call to `org-ql-select' with the same arguments.
For convenience, arguments should be unquoted.

\(fn BUFFERS-OR-FILES QUERY &key SORT NARROW ACTION)" nil t)

(function-put 'org-ql 'lisp-indent-function 'defun)

(make-obsolete 'org-ql '"Please use functions `org-ql-select' or `org-ql-query' instead" '"org-ql 0.5")

(autoload 'org-ql-select "org-ql" "\
Return items matching QUERY in BUFFERS-OR-FILES.

BUFFERS-OR-FILES is a file or buffer, a list of files and/or
buffers, or a function which returns such a list.

QUERY is an `org-ql' query sexp (quoted, since this is a
function).

ACTION is a function which is called on each matching entry with
point at the beginning of its heading.  It may be:

- `element' or nil: Equivalent to `org-element-headline-parser'.

- `element-with-markers': Equivalent to calling
  `org-element-headline-parser', with markers added using
  `org-ql--add-markers'.  Suitable for formatting with
  `org-ql-view--format-element', allowing insertion into an Org
  Agenda-like buffer.

- A sexp, which will be byte-compiled into a lambda function.

- A function symbol.

If NARROW is non-nil, buffers are not widened (the default is to
widen and search the entire buffer).

SORT is either nil, in which case items are not sorted; or one or
a list of defined `org-ql' sorting methods (`date', `deadline',
`scheduled', `todo', `priority', or `random'); or a user-defined
comparator function that accepts two items as arguments and
returns nil or non-nil.

\(fn BUFFERS-OR-FILES QUERY &key ACTION NARROW SORT)" nil nil)

(function-put 'org-ql-select 'lisp-indent-function 'defun)

(autoload 'org-ql-query "org-ql" "\
Like `org-ql-select', but arguments are named more like a SQL query.

SELECT corresponds to the `org-ql-select' argument ACTION.  It is
the function called on matching headings, the results of which
are returned by this function.  It may be:

- `element' or nil: Equivalent to `org-element-headline-parser'.

- `element-with-markers': Equivalent to
  `org-element-headline-parser', with markers added using
  `org-ql--add-markers'.  Suitable for formatting with
  `org-ql-view--format-element', allowing insertion into an Org
  Agenda-like buffer.

- A sexp, which will be byte-compiled into a lambda function.

- A function symbol.

FROM corresponds to the `org-ql-select' argument BUFFERS-OR-FILES.
It may be one or a list of file paths and/or buffers.

WHERE corresponds to the `org-ql-select' argument QUERY.  It
should be an `org-ql' query sexp.

ORDER-BY corresponds to the `org-ql-select' argument SORT, which
see.

NARROW corresponds to the `org-ql-select' argument NARROW.

\(fn &key (SELECT \\='element-with-markers) FROM WHERE NARROW ORDER-BY)" nil nil)

(function-put 'org-ql-query 'lisp-indent-function '0)

(register-definition-prefixes "org-ql" '("org-ql-"))

;;;***

;;;### (autoloads nil "org-ql-search" "org-ql-search.el" (0 0 0 0))
;;; Generated autoloads from org-ql-search.el

(autoload 'org-ql-sparse-tree "org-ql-search" "\
Show a sparse tree for QUERY in BUFFER and return number of results.
The tree will show the lines where the query matches, and any
other context defined in `org-show-context-detail', which see.

QUERY is an `org-ql' query sexp (quoted, since this is a
function).  BUFFER defaults to the current buffer.

When KEEP-PREVIOUS is non-nil (interactively, with prefix), the
outline is not reset to the overview state before finding
matches, which allows stacking calls to this command.

Runs `org-occur-hook' after making the sparse tree.

\(fn QUERY &key KEEP-PREVIOUS (BUFFER (current-buffer)))" t nil)

(autoload 'org-ql-search "org-ql-search" "\
Search for QUERY with `org-ql'.
Interactively, prompt for these variables:

BUFFERS-FILES: A list of buffers and/or files to search.
Interactively, may also be:

- `buffer': search the current buffer
- `all': search all Org buffers
- `agenda': search buffers returned by the function `org-agenda-files'
- `directory': search Org files in `org-directory'
- A space-separated list of file or buffer names

QUERY: An `org-ql' query in either sexp or non-sexp form (see
Info node `(org-ql)Queries').

SUPER-GROUPS: An `org-super-agenda' group set.  See variable
`org-super-agenda-groups' and Info node `(org-super-agenda)Group
selectors'.

NARROW: When non-nil, don't widen buffers before
searching. Interactively, with prefix, leave narrowed.

SORT: One or a list of `org-ql' sorting functions, like `date' or
`priority' (see Info node `(org-ql)Listing / acting-on results').

TITLE: An optional string displayed in the header.

BUFFER: Optionally, a buffer or name of a buffer in which to
display the results.  By default, the value of
`org-ql-view-buffer' is used, and a new buffer is created if
necessary.

\(fn BUFFERS-FILES QUERY &key NARROW SUPER-GROUPS SORT TITLE (BUFFER org-ql-view-buffer))" t nil)

(function-put 'org-ql-search 'lisp-indent-function 'defun)

(autoload 'org-ql-search-block "org-ql-search" "\
Insert items for QUERY into current buffer.
QUERY should be an `org-ql' query form.  Intended to be used as a
user-defined function in `org-agenda-custom-commands'.  QUERY
corresponds to the `match' item in the custom command form.

Like other agenda block commands, it searches files returned by
function `org-agenda-files'.  Inserts a newline after the block.

If `org-ql-block-header' is non-nil, it is used as the header
string for the block, otherwise a the header is formed
automatically from the query.

\(fn QUERY)" nil nil)

(defalias 'org-ql-block 'org-ql-search-block)

(register-definition-prefixes "org-ql-search" '("org-ql-"))

;;;***

;;;### (autoloads nil "org-ql-view" "org-ql-view.el" (0 0 0 0))
;;; Generated autoloads from org-ql-view.el

(autoload 'org-ql-view "org-ql-view" "\
Choose and display the `org-ql-views' view NAME.
Interactively, prompt for NAME.

\(fn &optional NAME)" t nil)

(autoload 'org-ql-view-recent-items "org-ql-view" "\
Show items in FILES from last NUM-DAYS days with timestamps of TYPE.
TYPE may be `ts', `ts-active', `ts-inactive', `clocked', or
`closed'.

\(fn &key NUM-DAYS (TYPE \\='ts) (FILES (org-agenda-files)) (GROUPS \\='((:auto-parent t) (:auto-todo t))))" t nil)

(autoload 'org-ql-view-sidebar "org-ql-view" "\
Show `org-ql-view' view list sidebar.

\(fn &key (SLOT org-ql-view-list-slot))" t nil)

(autoload 'org-ql-view-bookmark-handler "org-ql-view" "\
Show Org QL View BOOKMARK in current buffer.

\(fn BOOKMARK)" nil nil)

(register-definition-prefixes "org-ql-view" '("org-ql-view"))

;;;***

;;;### (autoloads nil nil ("org-ql-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; org-ql-autoloads.el ends here
