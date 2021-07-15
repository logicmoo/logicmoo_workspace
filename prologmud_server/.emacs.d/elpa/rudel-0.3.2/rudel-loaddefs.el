;;; rudel-loaddefs.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "rudel-backend" "rudel-backend.el" (0 0 0 0))
;;; Generated autoloads from rudel-backend.el

(eieio-defclass-autoload 'rudel-backend-factory 'nil "rudel-backend" "Factory class that holds an object for each known backend\ncategory. Objects manage backend implementation for one backend\ncategory each.")

(defmacro rudel--with-memoization (place &rest code) (declare (indent 1) (debug t)) (gv-letplace (getter setter) place `(or ,getter ,(macroexp-let2 nil val (macroexp-progn code) `(progn ,(funcall setter val) ,val)))))

(cl-defmethod rudel-get-factory ((this (subclass rudel-backend-factory)) category) "\
Return the factory responsible for CATEGORY.
If there is no responsible factory, create one and return it." (rudel--with-memoization (gethash category (eieio-oref-default this (quote factories))) (make-instance (quote rudel-backend-factory))))

(cl-defmethod rudel-add-backend ((this rudel-backend-factory) name class &optional replace) "\
Add factory class CLASS with name NAME to THIS.
if REPLACE is non-nil, replace a registered implementation of the
same name." (with-slots (backends) this (when (or (not (gethash name backends)) replace) (puthash name class backends))))

(autoload 'rudel-backend-get "rudel-backend" "\
A shortcut for getting backend NAME of category CATEGORY.
The returned backend is of the form (NAME . OBJECT).

\(fn CATEGORY NAME)" nil nil)

(autoload 'rudel-backend-get-factory "rudel-backend" "\
A shortcut for getting the factory object for CATEGORY.

\(fn CATEGORY)" nil nil)

;;;***

;;;### (autoloads nil "rudel-infinote" "rudel-infinote.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from rudel-infinote.el

(eieio-defclass-autoload 'rudel-infinote-backend '(rudel-protocol-backend) "rudel-infinote" "")

(rudel-add-backend (rudel-backend-get-factory 'protocol) 'infinote 'rudel-infinote-backend)

(eval-after-load 'rudel-zeroconf '(rudel-zeroconf-register-service "_infinote._tcp" 'xmpp 'infinote))

;;;***

;;;### (autoloads nil "rudel-obby" "rudel-obby.el" (0 0 0 0))
;;; Generated autoloads from rudel-obby.el

(eieio-defclass-autoload 'rudel-obby-backend '(rudel-protocol-backend) "rudel-obby" "Main class of the Rudel obby backend. Creates obby client\nconnections and creates obby servers.")

(rudel-add-backend (rudel-backend-get-factory 'protocol) 'obby 'rudel-obby-backend)

(eval-after-load 'rudel-zeroconf '(rudel-zeroconf-register-service "_lobby._tcp" 'start-tls 'obby))

;;;***

;;;### (autoloads nil "rudel-session-initiation" "rudel-session-initiation.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from rudel-session-initiation.el

(eieio-defclass-autoload 'rudel-ask-protocol-backend '(rudel-session-initiation-backend) "rudel-session-initiation" "This fallback backend can \"discover\" sessions by letting the\nuser select a suitable backend and asking for connect information\nrequired by the chosen backend.")

(rudel-add-backend (rudel-backend-get-factory 'session-initiation) 'ask-protocol 'rudel-ask-protocol-backend)

(eieio-defclass-autoload 'rudel-configured-sessions-backend '(rudel-session-initiation-backend) "rudel-session-initiation" "This fallback backend can \"discover\" sessions the user has\nconfigured using customization.")

(rudel-add-backend (rudel-backend-get-factory 'session-initiation) 'configured-sessions 'rudel-configured-sessions-backend)

;;;***

;;;### (autoloads nil "rudel-socket" "rudel-socket.el" (0 0 0 0))
;;; Generated autoloads from rudel-socket.el

(eieio-defclass-autoload 'rudel-tcp-backend '(rudel-transport-backend) "rudel-socket" "TCP transport backend.\nThe transport backend is a factory for TCP transport objects.")

(rudel-add-backend (rudel-backend-get-factory 'transport) 'tcp 'rudel-tcp-backend)

;;;***

;;;### (autoloads nil "rudel-telepathy" "rudel-telepathy.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from rudel-telepathy.el

(eieio-defclass-autoload 'rudel-telepathy-backend '(rudel-transport-backend) "rudel-telepathy" "Class rudel-telepathy-backend ")

(rudel-add-backend (rudel-backend-get-factory 'transport) 'telepathy 'rudel-telepathy-backend)

;;;***

;;;### (autoloads nil "rudel-tls" "rudel-tls.el" (0 0 0 0))
;;; Generated autoloads from rudel-tls.el

(eieio-defclass-autoload 'rudel-start-tls-backend '(rudel-transport-backend) "rudel-tls" "STARTTLS transport backend.\nThe transport backend is a factory for transport objects that\nsupport STARTTLS behavior.")

(rudel-add-backend (rudel-backend-get-factory 'transport) 'start-tls 'rudel-start-tls-backend)

;;;***

;;;### (autoloads nil "rudel-wave" "rudel-wave.el" (0 0 0 0))
;;; Generated autoloads from rudel-wave.el

(eieio-defclass-autoload 'rudel-wave-backend '(rudel-protocol-backend) "rudel-wave" "Main class of the Rudel Wave backend. Creates wave client\nconnections.")

(rudel-add-backend (rudel-backend-get-factory 'protocol) 'wave 'rudel-wave-backend)

;;;***

;;;### (autoloads nil "rudel-xmpp" "rudel-xmpp.el" (0 0 0 0))
;;; Generated autoloads from rudel-xmpp.el

(eieio-defclass-autoload 'rudel-xmpp-backend '(rudel-transport-backend) "rudel-xmpp" "Transport backend works by transporting XMPP messages through\nXMPP connections.")

(rudel-add-backend (rudel-backend-get-factory 'transport) 'xmpp 'rudel-xmpp-backend)

;;;***

;;;### (autoloads nil "rudel-xmpp-tunnel" "rudel-xmpp-tunnel.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from rudel-xmpp-tunnel.el

(rudel-add-backend (rudel-backend-get-factory 'transport) 'xmpp 'rudel-xmpp-tunnel-backend)

;;;***

;;;### (autoloads nil "rudel-zeroconf" "rudel-zeroconf.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from rudel-zeroconf.el

(autoload 'rudel-zeroconf-register-service "rudel-zeroconf" "\
Add an entry for TYPE with TRANSPORT-BACKEND and PROTOCOL-BACKEND to the list of service types.
TRANSPORT-BACKEND is the name of the transport backend handling
the service type TYPE.
PROTOCOL-BACKEND is the name of the protocol backend handling the
service type TYPE.

\(fn TYPE TRANSPORT-BACKEND PROTOCOL-BACKEND)" nil nil)

(eieio-defclass-autoload 'rudel-zeroconf-backend '(rudel-session-initiation-backend) "rudel-zeroconf" "")

(rudel-add-backend (rudel-backend-get-factory 'session-initiation) 'zeroconf 'rudel-zeroconf-backend)

;;;***

;;;### (autoloads nil nil ("adopted-compound.el" "adopted-delete.el"
;;;;;;  "adopted-insert.el" "adopted-nop.el" "adopted-operation.el"
;;;;;;  "adopted.el" "jupiter-compound.el" "jupiter-delete.el" "jupiter-insert.el"
;;;;;;  "jupiter-nop.el" "jupiter-operation.el" "jupiter.el" "rudel-autoloads.el"
;;;;;;  "rudel-chat.el" "rudel-color.el" "rudel-debug.el" "rudel-display.el"
;;;;;;  "rudel-errors.el" "rudel-hooks.el" "rudel-icons.el" "rudel-infinote-client.el"
;;;;;;  "rudel-infinote-display.el" "rudel-infinote-document.el"
;;;;;;  "rudel-infinote-errors.el" "rudel-infinote-group-directory.el"
;;;;;;  "rudel-infinote-group-document.el" "rudel-infinote-group-text-document.el"
;;;;;;  "rudel-infinote-group.el" "rudel-infinote-node-directory.el"
;;;;;;  "rudel-infinote-node.el" "rudel-infinote-state.el" "rudel-infinote-text-document.el"
;;;;;;  "rudel-infinote-user.el" "rudel-infinote-util.el" "rudel-interactive.el"
;;;;;;  "rudel-mode.el" "rudel-obby-client.el" "rudel-obby-debug.el"
;;;;;;  "rudel-obby-display.el" "rudel-obby-errors.el" "rudel-obby-server.el"
;;;;;;  "rudel-obby-state.el" "rudel-obby-util.el" "rudel-operations.el"
;;;;;;  "rudel-operators.el" "rudel-overlay.el" "rudel-pkg.el" "rudel-protocol.el"
;;;;;;  "rudel-speedbar.el" "rudel-state-machine.el" "rudel-transport-util.el"
;;;;;;  "rudel-transport.el" "rudel-util.el" "rudel-xml.el" "rudel-xmpp-debug.el"
;;;;;;  "rudel-xmpp-sasl.el" "rudel-xmpp-state.el" "rudel-xmpp-tls.el"
;;;;;;  "rudel-xmpp-util.el" "rudel.el") (0 0 0 0))

;;;***

(provide 'rudel-loaddefs)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; rudel-loaddefs.el ends here
