;;; org-jira-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "jiralib" "jiralib.el" (0 0 0 0))
;;; Generated autoloads from jiralib.el

(register-definition-prefixes "jiralib" '("jiralib-"))

;;;***

;;;### (autoloads nil "org-jira" "org-jira.el" (0 0 0 0))
;;; Generated autoloads from org-jira.el

(autoload 'org-jira-mode "org-jira" "\
Toggle org-jira mode.
With no argument, the mode is toggled on/off.
Non-nil argument turns mode on.
Nil argument turns mode off.

This is a minor mode.  If called interactively, toggle the
`Org-jira mode' mode.  If the prefix argument is positive, enable
the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `org-jira-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

Commands:
\\{org-jira-entry-mode-map}

Entry to this mode calls the value of `org-jira-mode-hook'.

\(fn &optional ARG)" t nil)

(autoload 'org-jira-get-projects "org-jira" "\
Get list of projects." t nil)

(autoload 'org-jira-get-issue-list "org-jira" "\
Get list of issues, using jql (jira query language), invoke CALLBACK after.

Default is unresolved issues assigned to current login user; with
a prefix argument you are given the chance to enter your own
jql.

\(fn &optional CALLBACK)" nil nil)

(autoload 'org-jira-get-summary "org-jira" "\
Get issue summary from point and place next to issue id from jira" t nil)

(autoload 'org-jira-get-summary-url "org-jira" "\
Get issue summary from point and place next to issue id from jira, and make issue id a link" t nil)

(autoload 'org-jira-get-issues-headonly "org-jira" "\
Get list of ISSUES, head only.

The default behavior is to return issues assigned to you and unresolved.

With a prefix argument, allow you to customize the jql.  See
`org-jira-get-issue-list'.

\(fn ISSUES)" t nil)

(autoload 'org-jira-get-issue "org-jira" "\
Get a JIRA issue, allowing you to enter the issue-id first.

\(fn ID)" t nil)

(autoload 'org-jira-get-issues-by-fixversion "org-jira" "\
Get list of issues by FIXVERSION.

\(fn FIXVERSION)" t nil)

(autoload 'org-jira-get-issue-project "org-jira" "\


\(fn ISSUE)" nil nil)

(autoload 'org-jira-get-issues "org-jira" "\
Get list of ISSUES into an org buffer.

Default is get unfinished issues assigned to you, but you can
customize jql with a prefix argument.
See`org-jira-get-issue-list'

\(fn ISSUES)" t nil)

(autoload 'org-jira-get-issues-from-custom-jql "org-jira" "\
Get JQL-LIST list of issues from a custom JQL and PROJ-KEY.

The PROJ-KEY will act as the file name, while the JQL will be any
valid JQL to populate a file to store PROJ-KEY results in.

Please note that this is *not* concurrent or race condition
proof.  If you try to run multiple calls to this function, it
will mangle things badly, as they rely on globals DEFAULT-JQL and
ORG-JIRA-PROJ-KEY-OVERRIDE being set before and after running.

\(fn &optional JQL-LIST)" t nil)

(autoload 'org-jira-update-comment "org-jira" "\
Update a comment for the current issue." t nil)

(autoload 'org-jira-update-worklogs-from-org-clocks "org-jira" "\
Update or add a worklog based on the org clocks." t nil)

(autoload 'org-jira-copy-current-issue-key "org-jira" "\
Copy the current issue's key into clipboard." t nil)

(autoload 'org-jira-unassign-issue "org-jira" "\
Update an issue to be unassigned." t nil)

(autoload 'org-jira-set-issue-reporter "org-jira" "\
Update an issue's reporter interactively." t nil)

(autoload 'org-jira-assign-issue "org-jira" "\
Update an issue with interactive re-assignment." t nil)

(autoload 'org-jira-update-issue "org-jira" "\
Update an issue." t nil)

(autoload 'org-jira-todo-to-jira "org-jira" "\
Convert an ordinary todo item to a jira ticket." t nil)

(autoload 'org-jira-get-subtasks "org-jira" "\
Get subtasks for the current issue." t nil)

(autoload 'org-jira-create-issue "org-jira" "\
Create an issue in PROJECT, of type TYPE, with given SUMMARY and DESCRIPTION.

\(fn PROJECT TYPE SUMMARY DESCRIPTION)" t nil)

(autoload 'org-jira-create-subtask "org-jira" "\
Create a subtask issue for PROJECT, of TYPE, with SUMMARY and DESCRIPTION.

\(fn PROJECT TYPE SUMMARY DESCRIPTION)" t nil)

(autoload 'org-jira-refresh-issue "org-jira" "\
Refresh current issue from jira to org." t nil)

(autoload 'org-jira-progress-issue "org-jira" "\
Progress issue workflow." t nil)

(autoload 'org-jira-progress-issue-next "org-jira" "\
Progress issue workflow." t nil)

(autoload 'org-jira-browse-issue "org-jira" "\
Open the current issue in external browser." t nil)

(autoload 'org-jira-download-attachment "org-jira" "\
Download the attachment under cursor." t nil)

(autoload 'org-jira-get-issues-from-filter "org-jira" "\
Get issues from the server-side stored filter named FILTER.

Provide this command in case some users are not able to use
client side jql (maybe because of JIRA server version?).

\(fn FILTER)" t nil)

(autoload 'org-jira-get-issues-from-filter-headonly "org-jira" "\
Get issues *head only* from saved filter named FILTER.
See `org-jira-get-issues-from-filter'.

\(fn FILTER)" t nil)

(autoload 'org-jira-get-issues-by-board "org-jira" "\
Get list of ISSUES from agile board." t nil)

(autoload 'org-jira-get-issues-by-board-headonly "org-jira" "\
Get list of ISSUES from agile board, head only." t nil)

(autoload 'org-jira-get-boards "org-jira" "\
Get list of boards and their properies." t nil)

(register-definition-prefixes "org-jira" '("ensure-on-" "org-jira-"))

;;;***

;;;### (autoloads nil "org-jira-sdk" "org-jira-sdk.el" (0 0 0 0))
;;; Generated autoloads from org-jira-sdk.el

(register-definition-prefixes "org-jira-sdk" '("org-jira-sdk-"))

;;;***

;;;### (autoloads nil nil ("org-jira-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; org-jira-autoloads.el ends here
