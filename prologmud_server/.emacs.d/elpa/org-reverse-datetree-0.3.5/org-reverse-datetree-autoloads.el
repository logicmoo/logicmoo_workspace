;;; org-reverse-datetree-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "org-reverse-datetree" "org-reverse-datetree.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from org-reverse-datetree.el

(autoload 'org-reverse-datetree-2 "org-reverse-datetree" "\
Jump to the specified date in a reverse date tree.

TIME is the date to be inserted.  If omitted, it will be today.

LEVEL-FORMATS is a list of formats.
See `org-reverse-datetree-level-formats' for the data type.

Depending on the value of RETURN-TYPE, this function returns the
following values:

\"'marker\":
  Returns the marker of the subtree.

\"point\"
  Returns point of subtree.

\"rfloc\"
  Returns a refile location spec that can be used as the third
  argument of `org-refile' function.

\"created\"
  Returns non-nil if and only if a new tree is created.

If ASC is non-nil, it creates a non-reverse date tree.

\(fn TIME LEVEL-FORMATS RETURN-TYPE &key ASC)" nil nil)

(autoload 'org-reverse-datetree-1 "org-reverse-datetree" "\
Jump to the specified date in a reverse date tree.

This function is deprecated.
Use `org-reverse-datetree-2' instead.

A reverse date tree is a reversed version of the date tree in
`org-capture', i.e. a date tree where the newest date is the first.
This is especially useful for a notes archive, because the latest
entry on a particular topic is displayed at the top in
a command like `helm-org-rifle'.

`org-reverse-datetree-find-function' is used to find or insert trees.

TIME is the date to be inserted.  If omitted, it will be today.

If WEEK-TREE is non-nil, it creates week trees.  Otherwise, it
creates month trees.

For RETURN, see the documentation of `org-reverse-datetree-2'.

\(fn &optional TIME &key WEEK-TREE RETURN)" nil nil)

(autoload 'org-reverse-datetree-goto-date-in-file "org-reverse-datetree" "\
Find or create a heading as configured in the file headers.

This function finds an entry at TIME in a date tree as configured
by file headers of the buffer.  If there is no such configuration,
ask the user for a new configuration.  If TIME is omitted, it is
the current date.  RETURN is the same as in `org-reverse-datetree-1'.

When this function is called interactively, it asks for TIME using
`org-read-date' and go to an entry of the date.

\(fn &optional TIME &key RETURN)" t nil)

(autoload 'org-reverse-datetree-refile-to-file "org-reverse-datetree" "\
Refile the current Org entry into a configured date tree in a file.

This function refiles the current entry into a date tree in FILE
configured in the headers of the file.  The same configuration as
`org-reverse-datetree-goto-date-in-file' is used.

The location in the date tree is specified by TIME, which is an
Emacs time.  If TIME is not set, a timestamp is retrieved from
properties of the current entry using
`org-reverse-datetree--get-entry-time' with ASK-ALWAYS and PREFER
as arguments.

\(fn FILE &optional TIME &key ASK-ALWAYS PREFER)" nil nil)

(autoload 'org-reverse-datetree-archive-subtree "org-reverse-datetree" "\
An org-reverse-datetree equivalent to `org-archive-subtree'.

A prefix argument FIND-DONE should be treated as in
`org-archive-subtree'.

\(fn &optional FIND-DONE)" t nil)

(autoload 'org-reverse-datetree-agenda-archive "org-reverse-datetree" "\
Archive the entry or subtree belonging to the current agenda entry." t nil)

(autoload 'org-reverse-datetree-cleanup-empty-dates "org-reverse-datetree" "\
Delete empty date entries in the buffer.

If NOCONFIRM is non-nil, leaf nodes are deleted without
confirmation. In non-interactive mode, you have to explicitly set
this argument.

If both NOCONFIRM and ANCESTORS are non-nil, upper level nodes
are deleted without confirmation as well.

\(fn &key NOCONFIRM ANCESTORS)" t nil)

(register-definition-prefixes "org-reverse-datetree" '("org-reverse-datetree-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; org-reverse-datetree-autoloads.el ends here
