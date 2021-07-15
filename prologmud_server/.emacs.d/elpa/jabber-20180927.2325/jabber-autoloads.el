;;; jabber-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "jabber" "jabber.el" (0 0 0 0))
;;; Generated autoloads from jabber.el

(defvar jabber-account-list nil "\
List of Jabber accounts.
Each element of the list is a cons cell describing a Jabber account,
where the car is a JID and the CDR is an alist.

JID is a full Jabber ID string (e.g. foo@bar.tld). You can also
specify the resource (e.g. foo@bar.tld/emacs).
The following keys can be present in the alist:

  :password is a string to authenticate ourself against the server.
  It can be empty.  If you don't want to store your password in your
  Emacs configuration, try auth-source (info node `(auth)Top').

  :network-server is a string identifying the address to connect to,
  if it's different from the server part of the JID.

  :port is the port to use (default depends on connection type).

  :connection-type is a symbol. Valid symbols are `starttls',
  `network' and `ssl'.

Only JID is mandatory.  The rest can be guessed at run-time.

Examples:

Two accounts without any special configuration:
\((\"foo@example.com\") (\"bar@example.net\"))

One disabled account with a non-standard port:
\((\"romeo@montague.net\" (:port . 5242) (:disabled . t)))

If you don't have SRV and STARTTLS capabilities in your Emacs,
configure a Google Talk account like this:
\((\"username@gmail.com\" 
  (:network-server . \"talk.google.com\")
  (:connection-type . ssl)))")

(custom-autoload 'jabber-account-list "jabber" t)

(defvar *jabber-current-status* nil "\
the users current presence status")

(defvar *jabber-current-show* nil "\
the users current presence show")

(defvar *jabber-current-priority* nil "\
the user's current priority")

(defconst jabber-presence-faces '(("" . jabber-roster-user-online) ("away" . jabber-roster-user-away) ("xa" . jabber-roster-user-xa) ("dnd" . jabber-roster-user-dnd) ("chat" . jabber-roster-user-chatty) ("error" . jabber-roster-user-error) (nil . jabber-roster-user-offline)) "\
Mapping from presence types to faces")

(autoload 'jabber-customize "jabber" "\
customize jabber options" t nil)

(autoload 'jabber-info "jabber" "\
open jabber.el manual" t nil)

(register-definition-prefixes "jabber" '("*jabber-status-history*" "jabber-"))

;;;***

;;;### (autoloads nil "jabber-activity" "jabber-activity.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from jabber-activity.el

(defvar jabber-activity-mode t "\
Non-nil if Jabber-Activity mode is enabled.
See the `jabber-activity-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `jabber-activity-mode'.")

(custom-autoload 'jabber-activity-mode "jabber-activity" nil)

(autoload 'jabber-activity-mode "jabber-activity" "\
Toggle display of activity in hidden jabber buffers in the mode line.

With a numeric arg, enable this display if arg is positive.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "jabber-activity" '("jabber-activity-"))

;;;***

;;;### (autoloads nil "jabber-ahc" "jabber-ahc.el" (0 0 0 0))
;;; Generated autoloads from jabber-ahc.el

(register-definition-prefixes "jabber-ahc" '("jabber-ahc-"))

;;;***

;;;### (autoloads nil "jabber-ahc-presence" "jabber-ahc-presence.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from jabber-ahc-presence.el

(register-definition-prefixes "jabber-ahc-presence" '("jabber-ahc-presence"))

;;;***

;;;### (autoloads nil "jabber-alert" "jabber-alert.el" (0 0 0 0))
;;; Generated autoloads from jabber-alert.el

(register-definition-prefixes "jabber-alert" '("beep" "define-" "echo" "jabber-"))

;;;***

;;;### (autoloads nil "jabber-autoaway" "jabber-autoaway.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from jabber-autoaway.el

(autoload 'jabber-autoaway-start "jabber-autoaway" "\
Start autoaway timer.
The IGNORED argument is there so you can put this function in
`jabber-post-connect-hooks'.

\(fn &optional IGNORED)" t nil)

(register-definition-prefixes "jabber-autoaway" '("jabber-"))

;;;***

;;;### (autoloads nil "jabber-avatar" "jabber-avatar.el" (0 0 0 0))
;;; Generated autoloads from jabber-avatar.el

(register-definition-prefixes "jabber-avatar" '("avatar" "jabber-"))

;;;***

;;;### (autoloads nil "jabber-awesome" "jabber-awesome.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from jabber-awesome.el

(register-definition-prefixes "jabber-awesome" '("awesome" "jabber-awesome-"))

;;;***

;;;### (autoloads nil "jabber-bookmarks" "jabber-bookmarks.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from jabber-bookmarks.el

(autoload 'jabber-get-conference-data "jabber-bookmarks" "\
Get bookmark data for CONFERENCE-JID.
KEY may be nil or one of :name, :autojoin, :nick and :password.
If KEY is nil, a plist containing the above keys is returned.
CONT is called when the result is available, with JC and the
result as arguments.  If CONT is nil, return the requested data
immediately, and return nil if it is not in the cache.

\(fn JC CONFERENCE-JID CONT &optional KEY)" nil nil)

(autoload 'jabber-parse-conference-bookmark "jabber-bookmarks" "\
Convert a <conference/> tag into a plist.
The plist may contain the keys :jid, :name, :autojoin,
:nick and :password.

\(fn NODE)" nil nil)

(autoload 'jabber-get-bookmarks "jabber-bookmarks" "\
Retrieve bookmarks (if needed) and call CONT.
Arguments to CONT are JC and the bookmark list.  CONT will be
called as the result of a filter function or a timer.
If REFRESH is non-nil, always fetch bookmarks.

\(fn JC CONT &optional REFRESH)" nil nil)

(autoload 'jabber-get-bookmarks-from-cache "jabber-bookmarks" "\
Return cached bookmarks for JC.
If bookmarks have not yet been fetched by `jabber-get-bookmarks',
return nil.

\(fn JC)" nil nil)

(autoload 'jabber-edit-bookmarks "jabber-bookmarks" "\
Create a buffer for editing bookmarks interactively.

\(fn JC)" t nil)

(register-definition-prefixes "jabber-bookmarks" '("jabber-"))

;;;***

;;;### (autoloads nil "jabber-browse" "jabber-browse.el" (0 0 0 0))
;;; Generated autoloads from jabber-browse.el

(register-definition-prefixes "jabber-browse" '("jabber-"))

;;;***

;;;### (autoloads nil "jabber-chat" "jabber-chat.el" (0 0 0 0))
;;; Generated autoloads from jabber-chat.el

(defvar jabber-chatting-with nil "\
JID of the person you are chatting with")

(autoload 'jabber-chat-get-buffer "jabber-chat" "\
Return the chat buffer for chatting with CHAT-WITH (bare or full JID).
Either a string or a buffer is returned, so use `get-buffer' or
`get-buffer-create'.

\(fn CHAT-WITH)" nil nil)

(register-definition-prefixes "jabber-chat" '("jabber-"))

;;;***

;;;### (autoloads nil "jabber-chatbuffer" "jabber-chatbuffer.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from jabber-chatbuffer.el

(defvar jabber-buffer-connection nil "\
The connection used by this buffer.")

(make-variable-buffer-local 'jabber-buffer-connection)

(register-definition-prefixes "jabber-chatbuffer" '("jabber-"))

;;;***

;;;### (autoloads nil "jabber-chatstates" "jabber-chatstates.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from jabber-chatstates.el

(register-definition-prefixes "jabber-chatstates" '("jabber-"))

;;;***

;;;### (autoloads nil "jabber-compose" "jabber-compose.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from jabber-compose.el

(autoload 'jabber-compose "jabber-compose" "\
Create a buffer for composing a Jabber message.

\(fn JC &optional RECIPIENT)" t nil)

(register-definition-prefixes "jabber-compose" '("jabber-compose-send"))

;;;***

;;;### (autoloads nil "jabber-conn" "jabber-conn.el" (0 0 0 0))
;;; Generated autoloads from jabber-conn.el

(register-definition-prefixes "jabber-conn" '("*jabber-virtual-server-function*" "jabber-"))

;;;***

;;;### (autoloads nil "jabber-console" "jabber-console.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from jabber-console.el

(autoload 'jabber-process-console "jabber-console" "\
Log XML-DATA i/o as XML in \"*-jabber-console-JID-*\" buffer

\(fn JC DIRECTION XML-DATA)" nil nil)

(register-definition-prefixes "jabber-console" '("jabber-"))

;;;***

;;;### (autoloads nil "jabber-core" "jabber-core.el" (0 0 0 0))
;;; Generated autoloads from jabber-core.el
 (autoload 'jabber-connect-all "jabber" "Connect to all configured Jabber accounts.\nSee `jabber-account-list'.\nIf no accounts are configured (or ARG supplied), call `jabber-connect' interactively." t)
 (autoload 'jabber-connect "jabber" "Connect to the Jabber server and start a Jabber XML stream.\nWith prefix argument, register a new account.\nWith double prefix argument, specify more connection details." t)

(register-definition-prefixes "jabber-core" '("*jabber-" "jabber-"))

;;;***

;;;### (autoloads nil "jabber-disco" "jabber-disco.el" (0 0 0 0))
;;; Generated autoloads from jabber-disco.el

(eval-after-load "jabber-core" '(add-to-list 'jabber-presence-chain #'jabber-process-caps))

(autoload 'jabber-process-caps "jabber-disco" "\
Look for entity capabilities in presence stanzas.

\(fn JC XML-DATA)" nil nil)

(autoload 'jabber-disco-advertise-feature "jabber-disco" "\


\(fn FEATURE)" nil nil)

(autoload 'jabber-caps-presence-element "jabber-disco" "\


\(fn JC)" nil nil)

(eval-after-load "jabber-presence" '(add-to-list 'jabber-presence-element-functions #'jabber-caps-presence-element))

(register-definition-prefixes "jabber-disco" '("jabber-"))

;;;***

;;;### (autoloads nil "jabber-events" "jabber-events.el" (0 0 0 0))
;;; Generated autoloads from jabber-events.el

(register-definition-prefixes "jabber-events" '("jabber-"))

;;;***

;;;### (autoloads nil "jabber-export" "jabber-export.el" (0 0 0 0))
;;; Generated autoloads from jabber-export.el

(autoload 'jabber-export-roster "jabber-export" "\
Export roster for connection JC.

\(fn JC)" t nil)

(autoload 'jabber-import-roster "jabber-export" "\
Create buffer for roster import for connection JC from FILE.

\(fn JC FILE)" t nil)

(register-definition-prefixes "jabber-export" '("jabber-"))

;;;***

;;;### (autoloads nil "jabber-feature-neg" "jabber-feature-neg.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from jabber-feature-neg.el

(register-definition-prefixes "jabber-feature-neg" '("jabber-fn-"))

;;;***

;;;### (autoloads nil "jabber-ft-client" "jabber-ft-client.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from jabber-ft-client.el

(register-definition-prefixes "jabber-ft-client" '("jabber-ft-"))

;;;***

;;;### (autoloads nil "jabber-ft-common" "jabber-ft-common.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from jabber-ft-common.el

(register-definition-prefixes "jabber-ft-common" '("jabber-ft-"))

;;;***

;;;### (autoloads nil "jabber-ft-server" "jabber-ft-server.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from jabber-ft-server.el

(register-definition-prefixes "jabber-ft-server" '("jabber-ft-"))

;;;***

;;;### (autoloads nil "jabber-gmail" "jabber-gmail.el" (0 0 0 0))
;;; Generated autoloads from jabber-gmail.el

(autoload 'jabber-gmail-subscribe "jabber-gmail" "\
Subscribe to gmail notifications.
See http://code.google.com/apis/talk/jep_extensions/usersettings.html#4

\(fn JC)" t nil)

(autoload 'jabber-gmail-query "jabber-gmail" "\
Request mail information from the Google Talk server (a.k.a. one shot query).
See http://code.google.com/apis/talk/jep_extensions/gmail.html#requestmail

\(fn JC)" t nil)

(register-definition-prefixes "jabber-gmail" '("jabber-gmail-"))

;;;***

;;;### (autoloads nil "jabber-history" "jabber-history.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from jabber-history.el

(register-definition-prefixes "jabber-history" '("jabber-"))

;;;***

;;;### (autoloads nil "jabber-iq" "jabber-iq.el" (0 0 0 0))
;;; Generated autoloads from jabber-iq.el

(register-definition-prefixes "jabber-iq" '("*jabber-open-info-queries*" "jabber-"))

;;;***

;;;### (autoloads nil "jabber-keepalive" "jabber-keepalive.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from jabber-keepalive.el

(let ((loads (get 'jabber-keepalive 'custom-loads))) (if (member '"jabber-keepalive" loads) nil (put 'jabber-keepalive 'custom-loads (cons '"jabber-keepalive" loads))))

(autoload 'jabber-keepalive-start "jabber-keepalive" "\
Activate keepalive.
That is, regularly send a ping request to the server, and
disconnect if it doesn't answer.  See `jabber-keepalive-interval'
and `jabber-keepalive-timeout'.

The JC argument makes it possible to add this function to
`jabber-post-connect-hooks'; it is ignored.  Keepalive is activated
for all accounts regardless of the argument.

\(fn &optional JC)" t nil)

(autoload 'jabber-whitespace-ping-start "jabber-keepalive" "\
Start sending whitespace pings at regular intervals.
See `jabber-whitespace-ping-interval'.

The JC argument is ignored; whitespace pings are enabled for all
accounts.

\(fn &optional JC)" t nil)

(register-definition-prefixes "jabber-keepalive" '("jabber-"))

;;;***

;;;### (autoloads nil "jabber-keymap" "jabber-keymap.el" (0 0 0 0))
;;; Generated autoloads from jabber-keymap.el

(defvar jabber-global-keymap (let ((map (make-sparse-keymap))) (define-key map "\3" 'jabber-connect-all) (define-key map "\4" 'jabber-disconnect) (define-key map "\22" 'jabber-switch-to-roster-buffer) (define-key map "\n" 'jabber-chat-with) (define-key map "\f" 'jabber-activity-switch-to) (define-key map "\1" 'jabber-send-away-presence) (define-key map "\17" 'jabber-send-default-presence) (define-key map "\30" 'jabber-send-xa-presence) (define-key map "\20" 'jabber-send-presence) map) "\
Global Jabber keymap (usually under C-x C-j)")

(define-key ctl-x-map "\n" jabber-global-keymap)

(register-definition-prefixes "jabber-keymap" '("jabber-common-keymap"))

;;;***

;;;### (autoloads nil "jabber-libnotify" "jabber-libnotify.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from jabber-libnotify.el

(register-definition-prefixes "jabber-libnotify" '("jabber-libnotify-" "libnotify"))

;;;***

;;;### (autoloads nil "jabber-logon" "jabber-logon.el" (0 0 0 0))
;;; Generated autoloads from jabber-logon.el

(register-definition-prefixes "jabber-logon" '("jabber-"))

;;;***

;;;### (autoloads nil "jabber-menu" "jabber-menu.el" (0 0 0 0))
;;; Generated autoloads from jabber-menu.el

(defvar jabber-menu (let ((map (make-sparse-keymap "jabber-menu"))) (define-key-after map [jabber-menu-connect] '("Connect" . jabber-connect-all)) (define-key-after map [jabber-menu-disconnect] '(menu-item "Disconnect" jabber-disconnect :enable (bound-and-true-p jabber-connections))) (define-key-after map [jabber-menu-status] `(menu-item "Set Status" ,(make-sparse-keymap "set-status") :enable (bound-and-true-p jabber-connections))) (define-key map [jabber-menu-status jabber-menu-status-chat] '(menu-item "Chatty" (lambda nil (interactive) (jabber-send-presence "chat" (jabber-read-with-input-method "status message: " *jabber-current-status* '*jabber-status-history*) *jabber-current-priority*)) :button (:radio and (boundp '*jabber-current-show*) (equal *jabber-current-show* "chat")))) (define-key map [jabber-menu-status jabber-menu-status-dnd] '(menu-item "Do not Disturb" (lambda nil (interactive) (jabber-send-presence "dnd" (jabber-read-with-input-method "status message: " *jabber-current-status* '*jabber-status-history*) *jabber-current-priority*)) :button (:radio and (boundp '*jabber-current-show*) (equal *jabber-current-show* "dnd")))) (define-key map [jabber-menu-status jabber-menu-status-xa] '(menu-item "Extended Away" jabber-send-xa-presence :button (:radio and (boundp '*jabber-current-show*) (equal *jabber-current-show* "xa")))) (define-key map [jabber-menu-status jabber-menu-status-away] '(menu-item "Away" jabber-send-away-presence :button (:radio and (boundp '*jabber-current-show*) (equal *jabber-current-show* "away")))) (define-key map [jabber-menu-status jabber-menu-status-online] '(menu-item "Online" jabber-send-default-presence :button (:radio and (boundp '*jabber-current-show*) (equal *jabber-current-show* "")))) (define-key-after map [separator] '(menu-item "--")) (define-key-after map [jabber-menu-chat-with] '(menu-item "Chat with..." jabber-chat-with :enable (bound-and-true-p jabber-connections))) (define-key-after map [jabber-menu-nextmsg] '(menu-item "Next unread message" jabber-activity-switch-to :enable (bound-and-true-p jabber-activity-jids))) (define-key-after map [jabber-menu-send-subscription-request] '(menu-item "Send subscription request" jabber-send-subscription-request :enable (bound-and-true-p jabber-connections))) (define-key-after map [jabber-menu-roster] '("Switch to roster" . jabber-switch-to-roster-buffer)) (define-key-after map [separator2] '(menu-item "--")) (define-key-after map [jabber-menu-customize] '("Customize" . jabber-customize)) (define-key-after map [jabber-menu-info] '("Help" . jabber-info)) map))

(defvar jabber-display-menu 'maybe "\
Decide whether the \"Jabber\" menu is displayed in the menu bar.
If t, always display.
If nil, never display.
If maybe, display if jabber.el is installed under `package-user-dir', or
if any of `jabber-account-list' or `jabber-connections' is non-nil.")

(custom-autoload 'jabber-display-menu "jabber-menu" t)

(define-key-after (lookup-key global-map [menu-bar]) [jabber-menu] (list 'menu-item "Jabber" jabber-menu :visible '(or (eq jabber-display-menu t) (and (eq jabber-display-menu 'maybe) (or (bound-and-true-p jabber-account-list) (bound-and-true-p jabber-connections))))))

(register-definition-prefixes "jabber-menu" '("jabber-"))

;;;***

;;;### (autoloads nil "jabber-modeline" "jabber-modeline.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from jabber-modeline.el

(register-definition-prefixes "jabber-modeline" '("jabber-mode-line-"))

;;;***

;;;### (autoloads nil "jabber-muc" "jabber-muc.el" (0 0 0 0))
;;; Generated autoloads from jabber-muc.el

(defvar *jabber-active-groupchats* nil "\
alist of groupchats and nicknames
Keys are strings, the bare JID of the room.
Values are strings.")

(defvar jabber-muc-printers 'nil "\
List of functions that may be able to print part of a MUC message.
This gets prepended to `jabber-chat-printers', which see.")

(autoload 'jabber-muc-get-buffer "jabber-muc" "\
Return the chat buffer for chatroom GROUP.
Either a string or a buffer is returned, so use `get-buffer' or
`get-buffer-create'.

\(fn GROUP)" nil nil)

(autoload 'jabber-muc-private-get-buffer "jabber-muc" "\
Return the chat buffer for private chat with NICKNAME in GROUP.
Either a string or a buffer is returned, so use `get-buffer' or
`get-buffer-create'.

\(fn GROUP NICKNAME)" nil nil)

(autoload 'jabber-muc-vcard-get "jabber-muc" "\
Request vcard from chat with NICKNAME in GROUP.

\(fn JC GROUP NICKNAME)" t nil)

(autoload 'jabber-muc-message-p "jabber-muc" "\
Return non-nil if MESSAGE is a groupchat message.
That does not include private messages in a groupchat, but does
include groupchat invites.

\(fn MESSAGE)" nil nil)

(autoload 'jabber-muc-sender-p "jabber-muc" "\
Return non-nil if JID is a full JID of an MUC participant.

\(fn JID)" nil nil)

(autoload 'jabber-muc-private-message-p "jabber-muc" "\
Return non-nil if MESSAGE is a private message in a groupchat.

\(fn MESSAGE)" nil nil)

(register-definition-prefixes "jabber-muc" '("jabber-"))

;;;***

;;;### (autoloads nil "jabber-muc-nick-coloring" "jabber-muc-nick-coloring.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from jabber-muc-nick-coloring.el

(register-definition-prefixes "jabber-muc-nick-coloring" '("jabber-muc-"))

;;;***

;;;### (autoloads nil "jabber-muc-nick-completion" "jabber-muc-nick-completion.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from jabber-muc-nick-completion.el

(autoload 'jabber-muc-looks-like-personal-p "jabber-muc-nick-completion" "\
Return non-nil if jabber MESSAGE is addresed to me.
Optional argument GROUP to look.

\(fn MESSAGE &optional GROUP)" nil nil)

(register-definition-prefixes "jabber-muc-nick-completion" '("*jabber-muc-participant-last-speaking*" "jabber-" "try-expand-jabber-muc"))

;;;***

;;;### (autoloads nil "jabber-ourversion" "jabber-ourversion.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from jabber-ourversion.el

(register-definition-prefixes "jabber-ourversion" '("jabber-version"))

;;;***

;;;### (autoloads nil "jabber-ping" "jabber-ping.el" (0 0 0 0))
;;; Generated autoloads from jabber-ping.el

(register-definition-prefixes "jabber-ping" '("jabber-p"))

;;;***

;;;### (autoloads nil "jabber-presence" "jabber-presence.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from jabber-presence.el

(autoload 'jabber-send-presence "jabber-presence" "\
Set presence for all accounts.

\(fn SHOW STATUS PRIORITY)" t nil)

(autoload 'jabber-send-default-presence "jabber-presence" "\
Send default presence.
Default presence is specified by `jabber-default-show',
`jabber-default-status', and `jabber-default-priority'.

\(fn &optional IGNORE)" t nil)

(register-definition-prefixes "jabber-presence" '("jabber-"))

;;;***

;;;### (autoloads nil "jabber-private" "jabber-private.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from jabber-private.el

(autoload 'jabber-private-get "jabber-private" "\
Retrieve an item from private XML storage.
The item to retrieve is identified by NODE-NAME (a symbol) and
NAMESPACE (a string).

On success, SUCCESS-CALLBACK is called with JC and the retrieved
XML fragment.

On error, ERROR-CALLBACK is called with JC and the entire IQ
result.

\(fn JC NODE-NAME NAMESPACE SUCCESS-CALLBACK ERROR-CALLBACK)" nil nil)

(autoload 'jabber-private-set "jabber-private" "\
Store FRAGMENT in private XML storage.
SUCCESS-CALLBACK, SUCCESS-CLOSURE-DATA, ERROR-CALLBACK and
ERROR-CLOSURE-DATA are used as in `jabber-send-iq'.

\(fn JC FRAGMENT &optional SUCCESS-CALLBACK SUCCESS-CLOSURE-DATA ERROR-CALLBACK ERROR-CLOSURE-DATA)" nil nil)

(register-definition-prefixes "jabber-private" '("jabber-private-get-1"))

;;;***

;;;### (autoloads nil "jabber-ratpoison" "jabber-ratpoison.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from jabber-ratpoison.el

(register-definition-prefixes "jabber-ratpoison" '("jabber-ratpoison-message" "ratpoison"))

;;;***

;;;### (autoloads nil "jabber-register" "jabber-register.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from jabber-register.el

(register-definition-prefixes "jabber-register" '("jabber-"))

;;;***

;;;### (autoloads nil "jabber-roster" "jabber-roster.el" (0 0 0 0))
;;; Generated autoloads from jabber-roster.el

(autoload 'jabber-switch-to-roster-buffer "jabber-roster" "\
Switch to roster buffer.
Optional JC argument is ignored; it's there so this function can
be used in `jabber-post-connection-hooks'.

\(fn &optional JC)" t nil)

(autoload 'jabber-roster-update "jabber-roster" "\
Update roster, in memory and on display.
Add NEW-ITEMS, update CHANGED-ITEMS and remove DELETED-ITEMS, all
three being lists of JID symbols.

\(fn JC NEW-ITEMS CHANGED-ITEMS DELETED-ITEMS)" nil nil)

(register-definition-prefixes "jabber-roster" '("jabber-"))

;;;***

;;;### (autoloads nil "jabber-rtt" "jabber-rtt.el" (0 0 0 0))
;;; Generated autoloads from jabber-rtt.el

(eval-after-load "jabber-disco" '(jabber-disco-advertise-feature "urn:xmpp:rtt:0"))

(eval-after-load "jabber-core" '(add-to-list 'jabber-message-chain #'jabber-rtt-handle-message t))

(autoload 'jabber-rtt-handle-message "jabber-rtt" "\


\(fn JC XML-DATA)" nil nil)

(autoload 'jabber-rtt-send-mode "jabber-rtt" "\
Show text to recipient as it is being typed.
This lets the recipient see every change made to the message up
until it's sent.  The recipient's client needs to implement
XEP-0301, In-Band Real Time Text.

This is a minor mode.  If called interactively, toggle the
`Jabber-Rtt-Send mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `jabber-rtt-send-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "jabber-rtt" '("jabber-rtt-"))

;;;***

;;;### (autoloads nil "jabber-sasl" "jabber-sasl.el" (0 0 0 0))
;;; Generated autoloads from jabber-sasl.el

(register-definition-prefixes "jabber-sasl" '("jabber-sasl-"))

;;;***

;;;### (autoloads nil "jabber-sawfish" "jabber-sawfish.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from jabber-sawfish.el

(register-definition-prefixes "jabber-sawfish" '("jabber-sawfish-display-" "sawfish"))

;;;***

;;;### (autoloads nil "jabber-screen" "jabber-screen.el" (0 0 0 0))
;;; Generated autoloads from jabber-screen.el

(register-definition-prefixes "jabber-screen" '("jabber-screen-message" "screen"))

;;;***

;;;### (autoloads nil "jabber-search" "jabber-search.el" (0 0 0 0))
;;; Generated autoloads from jabber-search.el

(register-definition-prefixes "jabber-search" '("jabber-"))

;;;***

;;;### (autoloads nil "jabber-si-client" "jabber-si-client.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from jabber-si-client.el

(register-definition-prefixes "jabber-si-client" '("jabber-si-initiate"))

;;;***

;;;### (autoloads nil "jabber-si-common" "jabber-si-common.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from jabber-si-common.el

(register-definition-prefixes "jabber-si-common" '("jabber-si-stream-methods"))

;;;***

;;;### (autoloads nil "jabber-si-server" "jabber-si-server.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from jabber-si-server.el

(register-definition-prefixes "jabber-si-server" '("jabber-si-pro"))

;;;***

;;;### (autoloads nil "jabber-socks5" "jabber-socks5.el" (0 0 0 0))
;;; Generated autoloads from jabber-socks5.el

(register-definition-prefixes "jabber-socks5" '("jabber-socks5"))

;;;***

;;;### (autoloads nil "jabber-time" "jabber-time.el" (0 0 0 0))
;;; Generated autoloads from jabber-time.el

(register-definition-prefixes "jabber-time" '("jabber-"))

;;;***

;;;### (autoloads nil "jabber-tmux" "jabber-tmux.el" (0 0 0 0))
;;; Generated autoloads from jabber-tmux.el

(register-definition-prefixes "jabber-tmux" '("jabber-tmux-message" "tmux"))

;;;***

;;;### (autoloads nil "jabber-truncate" "jabber-truncate.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from jabber-truncate.el

(register-definition-prefixes "jabber-truncate" '("jabber-"))

;;;***

;;;### (autoloads nil "jabber-util" "jabber-util.el" (0 0 0 0))
;;; Generated autoloads from jabber-util.el

(register-definition-prefixes "jabber-util" '("jabber-" "string>-numerical" "url-xmpp"))

;;;***

;;;### (autoloads nil "jabber-vcard" "jabber-vcard.el" (0 0 0 0))
;;; Generated autoloads from jabber-vcard.el

(register-definition-prefixes "jabber-vcard" '("jabber-vcard-"))

;;;***

;;;### (autoloads nil "jabber-vcard-avatars" "jabber-vcard-avatars.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from jabber-vcard-avatars.el

(register-definition-prefixes "jabber-vcard-avatars" '("jabber-vcard-avatars-"))

;;;***

;;;### (autoloads nil "jabber-version" "jabber-version.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from jabber-version.el

(register-definition-prefixes "jabber-version" '("jabber-"))

;;;***

;;;### (autoloads nil "jabber-watch" "jabber-watch.el" (0 0 0 0))
;;; Generated autoloads from jabber-watch.el

(register-definition-prefixes "jabber-watch" '("jabber-"))

;;;***

;;;### (autoloads nil "jabber-widget" "jabber-widget.el" (0 0 0 0))
;;; Generated autoloads from jabber-widget.el

(register-definition-prefixes "jabber-widget" '("jabber-" "jid-complete"))

;;;***

;;;### (autoloads nil "jabber-wmii" "jabber-wmii.el" (0 0 0 0))
;;; Generated autoloads from jabber-wmii.el

(register-definition-prefixes "jabber-wmii" '("jabber-wmii-" "wmii"))

;;;***

;;;### (autoloads nil "jabber-xmessage" "jabber-xmessage.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from jabber-xmessage.el

(register-definition-prefixes "jabber-xmessage" '("jabber-xmessage-" "xmessage"))

;;;***

;;;### (autoloads nil "jabber-xml" "jabber-xml.el" (0 0 0 0))
;;; Generated autoloads from jabber-xml.el

(register-definition-prefixes "jabber-xml" '("jabber-"))

;;;***

;;;### (autoloads nil nil ("jabber-festival.el" "jabber-notifications.el"
;;;;;;  "jabber-osd.el" "jabber-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; jabber-autoloads.el ends here
