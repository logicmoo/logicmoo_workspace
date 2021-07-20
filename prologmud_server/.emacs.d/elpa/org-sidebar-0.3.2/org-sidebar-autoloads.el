;;; org-sidebar-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "org-sidebar" "org-sidebar.el" (0 0 0 0))
;;; Generated autoloads from org-sidebar.el

(autoload 'org-sidebar "org-sidebar" "\
Display the Org Sidebar.

Interactively, display the sidebars configured in
`org-sidebar-default-fns'.

BUFFERS may be one or a list of buffers to display in the
sidebar.

FNS may be one or a list of functions, each of which may return a
buffer or a `org-sidebar' struct.

SIDEBARS may be one or a list of `org-sidebar' structs.

When GROUP is non-nil (interactively, with one universal prefix
argument), and when SUPER-GROUPS is nil, call each function with
the `group' keyword argument non-nil.

SUPER-GROUPS may be a list of groups according to
`org-super-agenda-groups', in which case the items in the buffers
will be grouped accordingly (where applicable).  Interactively,
with two universal prefix arguments, the global value of
`org-super-agenda-groups' is used.

\(fn &key BUFFERS FNS SIDEBARS GROUP SUPER-GROUPS)" t nil)

(autoload 'org-sidebar-toggle "org-sidebar" "\
Toggle default sidebar window.
If it is open and shows the view for the current buffer, delete
it.  Otherwise, show it for current buffer." t nil)

(autoload 'org-sidebar-ql "org-sidebar" "\
Display a sidebar for `org-ql' QUERY.
Interactively, with prefix, prompt for these variables:

BUFFERS-FILES: A list of buffers and/or files to search.

NARROW: When non-nil, don't widen buffers before searching.

GROUP-PROPERTY: One of the following symbols: `category',
`parent', `priority', `todo', `ts'.

SORT: One or a list of `org-ql' sorting functions, like `date' or
`priority'.

\(fn &key QUERY BUFFERS-FILES NARROW GROUP-PROPERTY SORT)" t nil)

(autoload 'org-sidebar-tree "org-sidebar" "\
Show tree-view sidebar." t nil)

(autoload 'org-sidebar-tree-toggle "org-sidebar" "\
Toggle tree-view sidebar window.
If it is open and shows the view for the current buffer, delete
it.  Otherwise, show it for current buffer." t nil)

(autoload 'org-sidebar-tree-view-buffer "org-sidebar" "\
Return a tree-view buffer for BUFFER.

\(fn &key (BUFFER (current-buffer)) &allow-other-keys)" nil nil)

(register-definition-prefixes "org-sidebar" '("org-sidebar-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; org-sidebar-autoloads.el ends here
