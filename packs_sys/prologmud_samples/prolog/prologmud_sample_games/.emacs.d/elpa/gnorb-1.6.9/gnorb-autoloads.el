;;; gnorb-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "gnorb-bbdb" "gnorb-bbdb.el" (0 0 0 0))
;;; Generated autoloads from gnorb-bbdb.el

(autoload 'gnorb-bbdb-mail "gnorb-bbdb" "\
\\<bbdb-mode-map>Acts just like `bbdb-mail', except runs
RECORDS through `gnorb-bbdb-posting-styles', allowing
customization of message styles for certain records. From the
`bbdb-mail' docstring:

Compose a mail message to RECORDS (optional: using SUBJECT).
Interactively, use BBDB prefix \\[bbdb-do-all-records], see
`bbdb-do-all-records'. By default, the first mail addresses of
RECORDS are used. If prefix N is a number, use Nth mail address
of RECORDS (starting from 1). If prefix N is C-u (t
noninteractively) use all mail addresses of RECORDS. If VERBOSE
is non-nil (as in interactive calls) be verbose.

\(fn RECORDS &optional SUBJECT N VERBOSE)" t nil)

(autoload 'gnorb-bbdb-tag-agenda "gnorb-bbdb" "\
Open an Org agenda tags view from the BBDB buffer, using the
value of the record's org-tags field. This shows only TODOs by
default; a prefix argument shows all tagged headings; a \"*\"
prefix operates on all currently visible records. If you want
both, use \"C-u\" before the \"*\".

\(fn RECORDS)" t nil)

(autoload 'gnorb-bbdb-mail-search "gnorb-bbdb" "\
Initiate a mail search from the BBDB buffer.

Use the prefix arg to edit the search string first, and the \"*\"
prefix to search mails from all visible contacts. When using both
a prefix arg and \"*\", the prefix arg must come first.

\(fn RECORDS)" t nil)

(autoload 'gnorb-bbdb-cite-contact "gnorb-bbdb" "\


\(fn REC)" t nil)

(autoload 'gnorb-bbdb-open-link "gnorb-bbdb" "\
\\<bbdb-mode-map>Call this on a BBDB record to open one of the
links in the message field. By default, the first link will be
opened. Use a prefix arg to open different links. For instance,
M-3 \\[gnorb-bbdb-open-link] will open the third link in the
list. If the %:count escape is present in the message formatting
string (see `gnorb-bbdb-message-link-format-multi' and
`gnorb-bbdb-message-link-format-one'), that's the number to use.

This function also needs to be called on a contact once before
that contact will start collecting links to messages.

\(fn RECORD ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "gnorb-bbdb" '("gnorb-bbdb-" "intern")))

;;;***

;;;### (autoloads nil "gnorb-gnus" "gnorb-gnus.el" (0 0 0 0))
;;; Generated autoloads from gnorb-gnus.el

(autoload 'gnorb-gnus-article-org-attach "gnorb-gnus" "\
Save MIME part N, which is the numerical prefix, of the
  article under point as an attachment to the specified org
  heading.

\(fn N)" t nil)

(autoload 'gnorb-gnus-mime-org-attach "gnorb-gnus" "\
Save the MIME part under point as an attachment to the
  specified org heading.

\(fn)" t nil)

(autoload 'gnorb-gnus-outgoing-do-todo "gnorb-gnus" "\
Use this command to use the message currently being composed
as an email todo action.

If it's a new message, or a reply to a message that isn't
referenced by any TODOs, a new TODO will be created.

If it references an existing TODO, you'll be prompted to trigger
a state-change or a note on that TODO after the message is sent.

You can call it with a prefix arg to force choosing an Org
subtree to associate with.

If you've already called this command, but realize you made a
mistake, you can call this command with a double prefix to reset
the association.

If a new todo is made, it needs a capture template: set
`gnorb-gnus-new-todo-capture-key' to the string key for the
appropriate capture template. If you're using a gnus-based
archive method (ie you have `gnus-message-archive-group' set to
something, and your outgoing messages have a \"Fcc\" header),
then a real link will be made to the outgoing message, and all
the gnus-type escapes will be available (see the Info
manual (org) Template expansion section). If you don't, then the
%:subject, %:to, %:toname, %:toaddress, and %:date escapes for
the outgoing message will still be available -- nothing else will
work.

\(fn &optional ARG)" t nil)

(autoload 'gnorb-gnus-incoming-do-todo "gnorb-gnus" "\
Use the message under point to trigger an action on an Org heading.
This function stores a link to the message, prompts for a related
Org heading, visits the heading, and triggers an action on
it (see `gnorb-org-trigger-actions').

If you've set up message tracking (with
`gnorb-tracking-initialize'), Gnorb can guess which Org heading
you probably want to trigger, which can save some time.  It does
this by looking in the References header, and seeing if any of
the messages referenced there are already being tracked by any
headings.

If you mark several messages before calling this function, or
call it with a numerical prefix arg, those messages will be
\"bulk associated\" with the chosen Org heading: associations
will be made, but you won't be prompted to trigger an action, and
you'll stay in the Gnus summary buffer.

\(fn ARG &optional ID)" t nil)

(autoload 'gnorb-gnus-quick-reply "gnorb-gnus" "\
Compose a reply to the message under point, and associate both
the original message and the reply with the selected heading.
Take no other action.

Use this when you want to compose a reply to a message on the
spot, and track both messages, without having to go through the
hassle of triggering an action on a heading, and then starting a
reply.

\(fn)" t nil)

(autoload 'gnorb-gnus-search-registry "gnorb-gnus" "\
Search for and display messages using the registry.
Prompt for a registry-specific SEARCH-STRING, then create an
ephemeral group containing the resulting messages.  All tracked
registry data keys are acceptable, see (slot-value
gnus-registry-db 'tracked).  Unknown keys will be ignored.  Keys
and search strings should be given as \"key:value\", with extra
quotes around multi-word search values.  Eg:

sender:google.com subject:\"your search results\"

\(fn SEARCH-STRING)" t nil)

(autoload 'gnorb-gnus-tag-message "gnorb-gnus" "\
Tag message or messages with TAGS.
ARG is used to specify which messages to work on (according to
Gnus' process prefix convention).  TAGS should be a list of Org
tags.  The tags are stored under the `org-tags' key in the
registry.  If called from a lisp program, TAGS are added to any
existing tags.

If multiple messages are to be tagged, only the first message's
existing tags are offered as a default.

\(fn ARG &optional TAGS)" t nil)

(autoload 'gnorb-gnus-insert-tagged-messages "gnorb-gnus" "\
Insert articles in this group with tags matching TAGS.
TAGS is a string possibly containing multiple tags to include or
exclude.  See Info node `(org)Matching tags and properties'.

\(fn TAGS)" t nil)

(autoload 'gnorb-gnus-insert-tracked-messages "gnorb-gnus" "\
Insert tracked messages into the Summary buffer.
Only inserts tracked messages belonging to this group.  If
SHOW-ALL (interactively, the prefix arg) is non-nil, insert all
messages; otherwise only insert messages that are tracked by a
heading in a non-DONE state.

\(fn SHOW-ALL)" t nil)

(autoload 'gnorb-gnus-search-messages "gnorb-gnus" "\
Initiate a search for gnus message links in an org subtree.
The arg STR can be one of two things: an Org heading id value
\(IDs should be prefixed with \"id+\"), in which case links will
be collected from that heading, or a string corresponding to an
Org tags search, in which case links will be collected from all
matching headings.

In either case, once a collection of links have been made, they
will all be displayed in an ephemeral Gnus group.  In Emacs 27
and below this requires the presence of an active \"nngnorb\"
server to work.  In Emacs 28 and later, no setup is required.

If PERSIST is non-nil, make a permanent group, and offer
HEAD-TEXT, if present, as its name.  Otherwise create an
ephemeral one, with RET as the value of its quit-config.

\(fn STR PERSIST &optional HEAD-TEXT RET)" t nil)

(autoload 'gnorb-gnus-view "gnorb-gnus" "\
Display the first relevant TODO heading for the message under point

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "gnorb-gnus" '("gnorb-" "intern")))

;;;***

;;;### (autoloads nil "gnorb-helm" "gnorb-helm.el" (0 0 0 0))
;;; Generated autoloads from gnorb-helm.el

(autoload 'gnorb-helm-search-registry "gnorb-helm" "\
Use helm and the Gnus registry to search messages.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "gnorb-helm" '("gnorb-helm-gnus-registry-candidates")))

;;;***

;;;### (autoloads nil "gnorb-org" "gnorb-org.el" (0 0 0 0))
;;; Generated autoloads from gnorb-org.el

(autoload 'gnorb-org-contact-link "gnorb-org" "\
Prompt for a BBDB record and insert a link to that record at
point.

There's really no reason to use this instead of regular old
`org-insert-link' with BBDB completion. But there might be in the
future!

\(fn REC)" t nil)

(autoload 'gnorb-org-handle-mail "gnorb-org" "\
Handle current headline as a mail TODO.
How this function behaves depends on whether you're using Gnorb
for email tracking, also on the prefix ARG, and on the active
region.

If tracking is enabled and there is no prefix arg, Gnorb will
begin a reply to the newest associated message that wasn't sent
by the user -- ie, the Sender header doesn't match
`user-mail-address' or `message-alternative-emails'.

If tracking is enabled and there is a prefix arg, ignore the
tracked messages and instead scan the subtree for mail-related
links. This means links prefixed with gnus:, mailto:, or bbdb:.
See `gnorb-org-mail-scan-scope' to limit the scope of this scan.
Do something appropriate with the resulting links.

With a double prefix arg, ignore all tracked messages and all
links, and compose a blank new message.

If tracking is enabled and you want to reply to a
specific (earlier) message in the tracking history, use
`gnorb-org-view' to open an nnir *Summary* buffer containing all
the messages, and reply to the one you want. Your reply will be
automatically tracked, as well.

If tracking is not enabled and you want to use a specific link in
the subtree as a basis for the email action, then put the region
around that link before you call this message.

TEXT is text to insert into the body of the message being
composed.  FILE is a file to attach to the message.

\(fn ARG &optional TEXT FILE)" t nil)

(autoload 'gnorb-org-email-subtree "gnorb-org" "\
Call on a subtree to export it either to a text string or a file,
then compose a mail message either with the exported text
inserted into the message body, or the exported file attached to
the message.

Export options default to the following: When exporting to a
buffer: async = nil, subtreep = t, visible-only = nil, body-only
= t. Options are the same for files, except body-only is set to
nil. Customize `gnorb-org-email-subtree-text-options' and
`gnorb-org-email-subtree-file-options', respectively.

Customize `gnorb-org-email-subtree-parameters' to your preferred
default set of parameters.

\(fn &optional ARG)" t nil)

(autoload 'gnorb-org-popup-bbdb "gnorb-org" "\
In an `org-tags-view' Agenda buffer, pop up a BBDB buffer
showing records whose `org-tags' field matches the current tags
search.

\(fn &optional STR)" t nil)

(autoload 'gnorb-org-view "gnorb-org" "\
Search the subtree at point for links to gnus messages, and
then show them in an ephemeral group, in Gnus.

With a prefix arg, create a search group that will persist across
Gnus sessions, and can be refreshed.

This won't work unless you've added a \"nngnorb\" server to
your gnus select methods.

\(fn ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "gnorb-org" '("gnorb-")))

;;;***

;;;### (autoloads nil "gnorb-registry" "gnorb-registry.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from gnorb-registry.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "gnorb-registry" '("gnorb-")))

;;;***

;;;### (autoloads nil "gnorb-utils" "gnorb-utils.el" (0 0 0 0))
;;; Generated autoloads from gnorb-utils.el

(autoload 'gnorb-restore-layout "gnorb-utils" "\
Restore window layout and value of point after a Gnorb command.

Some Gnorb commands change the window layout (ie `gnorb-org-view'
or incoming email triggering). This command restores the layout
to what it was. Bind it to a global key, or to local keys in Org
and Gnus and BBDB maps.

\(fn)" t nil)

(autoload 'gnorb-tracking-initialize "gnorb-utils" "\
Start using the Gnus registry to track correspondences between
Gnus messages and Org headings. This requires that the Gnus
registry be in use, and should be called after the call to
`gnus-registry-initialize'.

\(fn)" nil nil)

(autoload 'gnorb-install-defaults "gnorb-utils" "\
Set up sane Gnorb customizations and keybindings.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "gnorb-utils" '("gnorb-")))

;;;***

;;;### (autoloads nil "nngnorb" "nngnorb.el" (0 0 0 0))
;;; Generated autoloads from nngnorb.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "nngnorb" '("nnir-run-gnorb" "nngnorb-" "gnorb-")))

;;;***

;;;### (autoloads nil nil ("gnorb-pkg.el" "gnorb.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; gnorb-autoloads.el ends here
