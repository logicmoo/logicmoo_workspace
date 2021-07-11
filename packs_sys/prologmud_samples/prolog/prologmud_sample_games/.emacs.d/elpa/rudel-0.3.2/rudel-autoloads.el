;;; rudel-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "adopted-compound" "adopted-compound.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from adopted-compound.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "adopted-compound" '("adopted-compound")))

;;;***

;;;### (autoloads nil "adopted-delete" "adopted-delete.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from adopted-delete.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "adopted-delete" '("adopted-delete")))

;;;***

;;;### (autoloads nil "adopted-insert" "adopted-insert.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from adopted-insert.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "adopted-insert" '("adopted-insert")))

;;;***

;;;### (autoloads nil "adopted-nop" "adopted-nop.el" (0 0 0 0))
;;; Generated autoloads from adopted-nop.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "adopted-nop" '("adopted-nop")))

;;;***

;;;### (autoloads nil "adopted-operation" "adopted-operation.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from adopted-operation.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "adopted-operation" '("adopted-operation")))

;;;***

;;;### (autoloads nil "jupiter" "jupiter.el" (0 0 0 0))
;;; Generated autoloads from jupiter.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "jupiter" '("jupiter-context")))

;;;***

;;;### (autoloads nil "jupiter-compound" "jupiter-compound.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from jupiter-compound.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "jupiter-compound" '("jupiter-compound")))

;;;***

;;;### (autoloads nil "jupiter-delete" "jupiter-delete.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from jupiter-delete.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "jupiter-delete" '("jupiter-delete")))

;;;***

;;;### (autoloads nil "jupiter-insert" "jupiter-insert.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from jupiter-insert.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "jupiter-insert" '("jupiter-insert")))

;;;***

;;;### (autoloads nil "jupiter-nop" "jupiter-nop.el" (0 0 0 0))
;;; Generated autoloads from jupiter-nop.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "jupiter-nop" '("jupiter-nop")))

;;;***

;;;### (autoloads nil "jupiter-operation" "jupiter-operation.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from jupiter-operation.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "jupiter-operation" '("jupiter-operation")))

;;;***

;;;### (autoloads nil "rudel" "rudel.el" (0 0 0 0))
;;; Generated autoloads from rudel.el

(autoload 'rudel-join-session "rudel" "\
Join the collaborative editing session described by INFO.
INFO is a property list that describes the collaborative editing
session in terms of properties like :host, :port
and :encryption. The particular properties and their respective
meanings depend on the used backend.

When called interactively, all data required to join a session
will be prompted for.

\(fn INFO)" t nil)

(autoload 'rudel-host-session "rudel" "\
Host a collaborative editing session described by INFO.
INFO is a property list that describes the collaborative editing
session to be created in terms of properties like :address, :port
and :encryption. The particular properties and their respective
meanings depend on the used backend.

When called interactively, all data required to host a session
will be prompted for.

\(fn INFO)" t nil)

(autoload 'rudel-leave-session "rudel" "\
Leave the current collaborative editing session.

\(fn)" t nil)

(autoload 'rudel-change-color "rudel" "\
Change the color associated with the local user.
Not all backends support this operation.

\(fn)" t nil)

(autoload 'rudel-subscribe "rudel" "\
Subscribe to DOCUMENT offered by a peer in a collaborative editing session.
When called interactively, DOCUMENT is prompted for interactively.

\(fn DOCUMENT)" t nil)

(autoload 'rudel-publish-buffer "rudel" "\
Make the BUFFER available for subscription to peers in a collaborative editing session.
If BUFFER is nil, the current buffer is used.

\(fn &optional BUFFER)" t nil)

(autoload 'rudel-unsubscribe "rudel" "\
Detaches BUFFER from the collaborative editing session.
The most recent version of the content will remain in the
buffer but not be affected by future changes from other
peers. If BUFFER is nil, the current is used.

\(fn &optional BUFFER)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rudel" '("rudel-")))

;;;***

;;;### (autoloads nil "rudel-backend" "rudel-backend.el" (0 0 0 0))
;;; Generated autoloads from rudel-backend.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rudel-backend" '("rudel-backend")))

;;;***

;;;### (autoloads nil "rudel-chat" "rudel-chat.el" (0 0 0 0))
;;; Generated autoloads from rudel-chat.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rudel-chat" '("rudel-chat-")))

;;;***

;;;### (autoloads nil "rudel-color" "rudel-color.el" (0 0 0 0))
;;; Generated autoloads from rudel-color.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rudel-color" '("rudel-")))

;;;***

;;;### (autoloads nil "rudel-debug" "rudel-debug.el" (0 0 0 0))
;;; Generated autoloads from rudel-debug.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rudel-debug" '("rudel-")))

;;;***

;;;### (autoloads nil "rudel-display" "rudel-display.el" (0 0 0 0))
;;; Generated autoloads from rudel-display.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rudel-display" '("rudel-display-object-list-string")))

;;;***

;;;### (autoloads nil "rudel-hooks" "rudel-hooks.el" (0 0 0 0))
;;; Generated autoloads from rudel-hooks.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rudel-hooks" '("rudel-")))

;;;***

;;;### (autoloads nil "rudel-icons" "rudel-icons.el" (0 0 0 0))
;;; Generated autoloads from rudel-icons.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rudel-icons" '("rudel-")))

;;;***

;;;### (autoloads nil "rudel-infinote" "rudel-infinote.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from rudel-infinote.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rudel-infinote" '("rudel-infinote-")))

;;;***

;;;### (autoloads nil "rudel-infinote-client" "rudel-infinote-client.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from rudel-infinote-client.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rudel-infinote-client" '("rudel-infinote-client-connection")))

;;;***

;;;### (autoloads nil "rudel-infinote-document" "rudel-infinote-document.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from rudel-infinote-document.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rudel-infinote-document" '("rudel-infinote-document")))

;;;***

;;;### (autoloads nil "rudel-infinote-group" "rudel-infinote-group.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from rudel-infinote-group.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rudel-infinote-group" '("rudel-infinote-")))

;;;***

;;;### (autoloads nil "rudel-infinote-group-directory" "rudel-infinote-group-directory.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from rudel-infinote-group-directory.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rudel-infinote-group-directory" '("rudel-infinote-")))

;;;***

;;;### (autoloads nil "rudel-infinote-group-document" "rudel-infinote-group-document.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from rudel-infinote-group-document.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rudel-infinote-group-document" '("rudel-infinote-group-document")))

;;;***

;;;### (autoloads nil "rudel-infinote-group-text-document" "rudel-infinote-group-text-document.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from rudel-infinote-group-text-document.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rudel-infinote-group-text-document" '("rudel-infinote-group-text-document")))

;;;***

;;;### (autoloads nil "rudel-infinote-node" "rudel-infinote-node.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from rudel-infinote-node.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rudel-infinote-node" '("rudel-infinote-node")))

;;;***

;;;### (autoloads nil "rudel-infinote-node-directory" "rudel-infinote-node-directory.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from rudel-infinote-node-directory.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rudel-infinote-node-directory" '("rudel-infinote-node-directory")))

;;;***

;;;### (autoloads nil "rudel-infinote-state" "rudel-infinote-state.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from rudel-infinote-state.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rudel-infinote-state" '("rudel-infinote-state")))

;;;***

;;;### (autoloads nil "rudel-infinote-text-document" "rudel-infinote-text-document.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from rudel-infinote-text-document.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rudel-infinote-text-document" '("rudel-infinote-text-document")))

;;;***

;;;### (autoloads nil "rudel-infinote-user" "rudel-infinote-user.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from rudel-infinote-user.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rudel-infinote-user" '("rudel-infinote-")))

;;;***

;;;### (autoloads nil "rudel-infinote-util" "rudel-infinote-util.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from rudel-infinote-util.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rudel-infinote-util" '("rudel-infinote-")))

;;;***

;;;### (autoloads nil "rudel-interactive" "rudel-interactive.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from rudel-interactive.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rudel-interactive" '("rudel-")))

;;;***

;;;### (autoloads nil "rudel-mode" "rudel-mode.el" (0 0 0 0))
;;; Generated autoloads from rudel-mode.el

(autoload 'rudel-header-subscriptions-minor-mode "rudel-mode" "\
Toggle Rudel header subscriptions minor mode.

This mode displays users subscribed to the document associated
with the buffer in the header-line. Depending on the kind of
session, additional information like connection status,
encryption or activity indication may be displayed with each
user.

If ARG is null, toggle Rudel header subscriptions mode.
If ARG is a number greater than zero, turn on Rudel header
subscriptions mode; otherwise, turn it off.

\(fn &optional ARG)" t nil)

(defvar global-rudel-header-subscriptions-mode nil "\
Non-nil if Global Rudel-Header-Subscriptions mode is enabled.
See the `global-rudel-header-subscriptions-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-rudel-header-subscriptions-mode'.")

(custom-autoload 'global-rudel-header-subscriptions-mode "rudel-mode" nil)

(autoload 'global-rudel-header-subscriptions-mode "rudel-mode" "\
Toggle Rudel-Header-Subscriptions minor mode in all buffers.
With prefix ARG, enable Global Rudel-Header-Subscriptions mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Rudel-Header-Subscriptions minor mode is enabled in all buffers where
`rudel-header-subscriptions-minor-mode' would do it.
See `rudel-header-subscriptions-minor-mode' for more information on Rudel-Header-Subscriptions minor mode.

\(fn &optional ARG)" t nil)

(autoload 'rudel-mode-line-publish-state-minor-mode "rudel-mode" "\
Toggle Rudel mode line publish state minor mode.

This mode displays an indicator of the buffer's state with
respect to an associated Rudel document in the mode line. If the
buffer has an attached document, the string \"P\" is displayed
after the remote file indicator. Otherwise, the string \"-\" is
displayed.

If ARG is null, toggle Rudel mode line publish state minor mode.
If ARG is a number greater than zero, turn on Rudel minor mode
line publish state mode; otherwise, turn it off.

\(fn &optional ARG)" t nil)

(defvar global-rudel-mode-line-publish-state-mode nil "\
Non-nil if Global Rudel-Mode-Line-Publish-State mode is enabled.
See the `global-rudel-mode-line-publish-state-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-rudel-mode-line-publish-state-mode'.")

(custom-autoload 'global-rudel-mode-line-publish-state-mode "rudel-mode" nil)

(autoload 'global-rudel-mode-line-publish-state-mode "rudel-mode" "\
Toggle Rudel-Mode-Line-Publish-State minor mode in all buffers.
With prefix ARG, enable Global Rudel-Mode-Line-Publish-State mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Rudel-Mode-Line-Publish-State minor mode is enabled in all buffers where
`rudel-mode-line-publish-state-minor-mode' would do it.
See `rudel-mode-line-publish-state-minor-mode' for more information on Rudel-Mode-Line-Publish-State minor mode.

\(fn &optional ARG)" t nil)

(defvar global-rudel-minor-mode nil "\
Non-nil if Global Rudel minor mode is enabled.
See the `global-rudel-minor-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-rudel-minor-mode'.")

(custom-autoload 'global-rudel-minor-mode "rudel-mode" nil)

(autoload 'global-rudel-minor-mode "rudel-mode" "\
Toggle global Rudel minor mode (No modeline indicator).

If ARG is null, toggle global Rudel mode.
If ARG is a number greater than zero, turn on global Rudel mode;
otherwise, turn it off.

\\{rudel-minor-keymap}

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rudel-mode" '("rudel-")))

;;;***

;;;### (autoloads nil "rudel-obby" "rudel-obby.el" (0 0 0 0))
;;; Generated autoloads from rudel-obby.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rudel-obby" '("rudel-obby-")))

;;;***

;;;### (autoloads nil "rudel-obby-client" "rudel-obby-client.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from rudel-obby-client.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rudel-obby-client" '("rudel-obby-c")))

;;;***

;;;### (autoloads nil "rudel-obby-errors" "rudel-obby-errors.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from rudel-obby-errors.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rudel-obby-errors" '("rudel-obby-error-")))

;;;***

;;;### (autoloads nil "rudel-obby-server" "rudel-obby-server.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from rudel-obby-server.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rudel-obby-server" '("rudel-obby-")))

;;;***

;;;### (autoloads nil "rudel-obby-state" "rudel-obby-state.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from rudel-obby-state.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rudel-obby-state" '("rudel-obby-")))

;;;***

;;;### (autoloads nil "rudel-obby-util" "rudel-obby-util.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from rudel-obby-util.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rudel-obby-util" '("with-parsed-arguments" "rudel-")))

;;;***

;;;### (autoloads nil "rudel-operations" "rudel-operations.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from rudel-operations.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rudel-operations" '("rudel-")))

;;;***

;;;### (autoloads nil "rudel-operators" "rudel-operators.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from rudel-operators.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rudel-operators" '("rudel-")))

;;;***

;;;### (autoloads nil "rudel-overlay" "rudel-overlay.el" (0 0 0 0))
;;; Generated autoloads from rudel-overlay.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rudel-overlay" '("rudel-")))

;;;***

;;;### (autoloads nil "rudel-protocol" "rudel-protocol.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from rudel-protocol.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rudel-protocol" '("rudel-protocol-backend")))

;;;***

;;;### (autoloads nil "rudel-session-initiation" "rudel-session-initiation.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from rudel-session-initiation.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rudel-session-initiation" '("rudel-")))

;;;***

;;;### (autoloads nil "rudel-socket" "rudel-socket.el" (0 0 0 0))
;;; Generated autoloads from rudel-socket.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rudel-socket" '("rudel-")))

;;;***

;;;### (autoloads nil "rudel-speedbar" "rudel-speedbar.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from rudel-speedbar.el

(autoload 'rudel-speedbar "rudel-speedbar" "\
Show connected users and available documents of Rudel session in speedbar.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rudel-speedbar" '("rudel-speedbar-")))

;;;***

;;;### (autoloads nil "rudel-state-machine" "rudel-state-machine.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from rudel-state-machine.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rudel-state-machine" '("rudel-")))

;;;***

;;;### (autoloads nil "rudel-telepathy" "rudel-telepathy.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from rudel-telepathy.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rudel-telepathy" '("rudel-telepathy-")))

;;;***

;;;### (autoloads nil "rudel-tls" "rudel-tls.el" (0 0 0 0))
;;; Generated autoloads from rudel-tls.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rudel-tls" '("rudel-")))

;;;***

;;;### (autoloads nil "rudel-transport" "rudel-transport.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from rudel-transport.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rudel-transport" '("rudel-")))

;;;***

;;;### (autoloads nil "rudel-transport-util" "rudel-transport-util.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from rudel-transport-util.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rudel-transport-util" '("rudel-")))

;;;***

;;;### (autoloads nil "rudel-util" "rudel-util.el" (0 0 0 0))
;;; Generated autoloads from rudel-util.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rudel-util" '("rudel-")))

;;;***

;;;### (autoloads nil "rudel-wave" "rudel-wave.el" (0 0 0 0))
;;; Generated autoloads from rudel-wave.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rudel-wave" '("rudel-wave-")))

;;;***

;;;### (autoloads nil "rudel-xml" "rudel-xml.el" (0 0 0 0))
;;; Generated autoloads from rudel-xml.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rudel-xml" '("rudel-xml-" "do-tag-children" "with-tag-attrs" "string->xml" "xml->string")))

;;;***

;;;### (autoloads nil "rudel-xmpp" "rudel-xmpp.el" (0 0 0 0))
;;; Generated autoloads from rudel-xmpp.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rudel-xmpp" '("rudel-xmpp-")))

;;;***

;;;### (autoloads nil "rudel-xmpp-sasl" "rudel-xmpp-sasl.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from rudel-xmpp-sasl.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rudel-xmpp-sasl" '("rudel-xmpp-s")))

;;;***

;;;### (autoloads nil "rudel-xmpp-state" "rudel-xmpp-state.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from rudel-xmpp-state.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rudel-xmpp-state" '("rudel-xmpp-state")))

;;;***

;;;### (autoloads nil "rudel-xmpp-tls" "rudel-xmpp-tls.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from rudel-xmpp-tls.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rudel-xmpp-tls" '("rudel-xmpp-")))

;;;***

;;;### (autoloads nil "rudel-xmpp-tunnel" "rudel-xmpp-tunnel.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from rudel-xmpp-tunnel.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rudel-xmpp-tunnel" '("rudel-xmpp-tunnel-transport")))

;;;***

;;;### (autoloads nil "rudel-xmpp-util" "rudel-xmpp-util.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from rudel-xmpp-util.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rudel-xmpp-util" '("rudel-xmpp-")))

;;;***

;;;### (autoloads nil "rudel-zeroconf" "rudel-zeroconf.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from rudel-zeroconf.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rudel-zeroconf" '("rudel-zeroconf-")))

;;;***

;;;### (autoloads nil nil ("adopted.el" "rudel-errors.el" "rudel-infinote-display.el"
;;;;;;  "rudel-infinote-errors.el" "rudel-loaddefs.el" "rudel-obby-debug.el"
;;;;;;  "rudel-obby-display.el" "rudel-pkg.el" "rudel-xmpp-debug.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; rudel-autoloads.el ends here
