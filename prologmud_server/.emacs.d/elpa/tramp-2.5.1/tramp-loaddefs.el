;;; tramp-loaddefs.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "tramp-adb" "tramp-adb.el" (0 0 0 0))
;;; Generated autoloads from tramp-adb.el

(defvar tramp-adb-program "adb" "\
Name of the Android Debug Bridge program.")

(custom-autoload 'tramp-adb-program "tramp-adb" t)

(defconst tramp-adb-method "adb" "\
When this method name is used, forward all calls to Android Debug Bridge.")

(tramp--with-startup (add-to-list 'tramp-methods `(,tramp-adb-method (tramp-login-program ,tramp-adb-program) (tramp-login-args (("shell"))) (tramp-direct-async t) (tramp-tmpdir "/data/local/tmp") (tramp-default-port 5555))) (add-to-list 'tramp-default-host-alist `(,tramp-adb-method nil "")) (tramp-set-completion-function tramp-adb-method '((tramp-adb-parse-device-names ""))))

(defconst tramp-adb-file-name-handler-alist '((access-file . tramp-handle-access-file) (add-name-to-file . tramp-handle-add-name-to-file) (copy-directory . tramp-handle-copy-directory) (copy-file . tramp-adb-handle-copy-file) (delete-directory . tramp-adb-handle-delete-directory) (delete-file . tramp-adb-handle-delete-file) (directory-file-name . tramp-handle-directory-file-name) (directory-files . tramp-handle-directory-files) (directory-files-and-attributes . tramp-adb-handle-directory-files-and-attributes) (dired-compress-file . ignore) (dired-uncache . tramp-handle-dired-uncache) (exec-path . tramp-adb-handle-exec-path) (expand-file-name . tramp-handle-expand-file-name) (file-accessible-directory-p . tramp-handle-file-accessible-directory-p) (file-acl . ignore) (file-attributes . tramp-adb-handle-file-attributes) (file-directory-p . tramp-handle-file-directory-p) (file-equal-p . tramp-handle-file-equal-p) (file-executable-p . tramp-handle-file-exists-p) (file-exists-p . tramp-handle-file-exists-p) (file-in-directory-p . tramp-handle-file-in-directory-p) (file-local-copy . tramp-adb-handle-file-local-copy) (file-modes . tramp-handle-file-modes) (file-name-all-completions . tramp-adb-handle-file-name-all-completions) (file-name-as-directory . tramp-handle-file-name-as-directory) (file-name-case-insensitive-p . tramp-handle-file-name-case-insensitive-p) (file-name-completion . tramp-handle-file-name-completion) (file-name-directory . tramp-handle-file-name-directory) (file-name-nondirectory . tramp-handle-file-name-nondirectory) (file-newer-than-file-p . tramp-handle-file-newer-than-file-p) (file-notify-add-watch . tramp-handle-file-notify-add-watch) (file-notify-rm-watch . tramp-handle-file-notify-rm-watch) (file-notify-valid-p . tramp-handle-file-notify-valid-p) (file-ownership-preserved-p . ignore) (file-readable-p . tramp-handle-file-exists-p) (file-regular-p . tramp-handle-file-regular-p) (file-remote-p . tramp-handle-file-remote-p) (file-selinux-context . tramp-handle-file-selinux-context) (file-symlink-p . tramp-handle-file-symlink-p) (file-system-info . tramp-adb-handle-file-system-info) (file-truename . tramp-handle-file-truename) (file-writable-p . tramp-adb-handle-file-writable-p) (find-backup-file-name . tramp-handle-find-backup-file-name) (insert-directory . tramp-handle-insert-directory) (insert-file-contents . tramp-handle-insert-file-contents) (load . tramp-handle-load) (make-auto-save-file-name . tramp-handle-make-auto-save-file-name) (make-directory . tramp-adb-handle-make-directory) (make-directory-internal . ignore) (make-nearby-temp-file . tramp-handle-make-nearby-temp-file) (make-process . tramp-adb-handle-make-process) (make-symbolic-link . tramp-handle-make-symbolic-link) (process-file . tramp-adb-handle-process-file) (rename-file . tramp-adb-handle-rename-file) (set-file-acl . ignore) (set-file-modes . tramp-adb-handle-set-file-modes) (set-file-selinux-context . ignore) (set-file-times . tramp-adb-handle-set-file-times) (set-visited-file-modtime . tramp-handle-set-visited-file-modtime) (shell-command . tramp-handle-shell-command) (start-file-process . tramp-handle-start-file-process) (substitute-in-file-name . tramp-handle-substitute-in-file-name) (temporary-file-directory . tramp-handle-temporary-file-directory) (tramp-get-remote-gid . ignore) (tramp-get-remote-uid . ignore) (tramp-set-file-uid-gid . ignore) (unhandled-file-name-directory . ignore) (vc-registered . ignore) (verify-visited-file-modtime . tramp-handle-verify-visited-file-modtime) (write-region . tramp-adb-handle-write-region)) "\
Alist of handler functions for Tramp ADB method.")

(defsubst tramp-adb-file-name-p (filename) "\
Check if it's a FILENAME for ADB." (and (tramp-tramp-file-p filename) (string= (tramp-file-name-method (tramp-dissect-file-name filename)) tramp-adb-method)))

(autoload 'tramp-adb-file-name-handler "tramp-adb" "\
Invoke the ADB handler for OPERATION.
First arg specifies the OPERATION, second arg is a list of
arguments to pass to the OPERATION.

\(fn OPERATION &rest ARGS)" nil nil)

(tramp--with-startup (tramp-register-foreign-file-name-handler #'tramp-adb-file-name-p #'tramp-adb-file-name-handler))

(autoload 'tramp-adb-parse-device-names "tramp-adb" "\
Return a list of (nil host) tuples allowed to access.

\(fn IGNORE)" nil nil)

;;;***

;;;### (autoloads nil "tramp-archive" "tramp-archive.el" (0 0 0 0))
;;; Generated autoloads from tramp-archive.el

(defconst tramp-archive-file-name-regexp (ignore-errors (tramp-archive-autoload-file-name-regexp)) "\
Regular expression matching archive file names.")

(defconst tramp-archive-method "archive" "\
Method name for archives in GVFS.")

(defconst tramp-archive-file-name-handler-alist '((access-file . tramp-archive-handle-access-file) (add-name-to-file . tramp-archive-handle-not-implemented) (copy-file . tramp-archive-handle-copy-file) (delete-directory . tramp-archive-handle-not-implemented) (delete-file . tramp-archive-handle-not-implemented) (directory-file-name . tramp-archive-handle-directory-file-name) (directory-files . tramp-handle-directory-files) (directory-files-and-attributes . tramp-handle-directory-files-and-attributes) (dired-compress-file . tramp-archive-handle-not-implemented) (dired-uncache . tramp-archive-handle-dired-uncache) (exec-path . ignore) (file-accessible-directory-p . tramp-handle-file-accessible-directory-p) (file-acl . ignore) (file-attributes . tramp-archive-handle-file-attributes) (file-directory-p . tramp-handle-file-directory-p) (file-equal-p . tramp-handle-file-equal-p) (file-executable-p . tramp-archive-handle-file-executable-p) (file-exists-p . tramp-handle-file-exists-p) (file-in-directory-p . tramp-handle-file-in-directory-p) (file-local-copy . tramp-archive-handle-file-local-copy) (file-modes . tramp-handle-file-modes) (file-name-all-completions . tramp-archive-handle-file-name-all-completions) (file-name-case-insensitive-p . ignore) (file-name-completion . tramp-handle-file-name-completion) (file-newer-than-file-p . tramp-handle-file-newer-than-file-p) (file-notify-add-watch . ignore) (file-notify-rm-watch . ignore) (file-notify-valid-p . ignore) (file-ownership-preserved-p . ignore) (file-readable-p . tramp-archive-handle-file-readable-p) (file-regular-p . tramp-handle-file-regular-p) (file-selinux-context . tramp-handle-file-selinux-context) (file-symlink-p . tramp-handle-file-symlink-p) (file-system-info . tramp-archive-handle-file-system-info) (file-truename . tramp-archive-handle-file-truename) (file-writable-p . ignore) (find-backup-file-name . ignore) (insert-directory . tramp-archive-handle-insert-directory) (insert-file-contents . tramp-archive-handle-insert-file-contents) (load . tramp-archive-handle-load) (make-auto-save-file-name . ignore) (make-directory . tramp-archive-handle-not-implemented) (make-directory-internal . tramp-archive-handle-not-implemented) (make-nearby-temp-file . tramp-handle-make-nearby-temp-file) (make-process . ignore) (make-symbolic-link . tramp-archive-handle-not-implemented) (process-file . ignore) (rename-file . tramp-archive-handle-not-implemented) (set-file-acl . ignore) (set-file-modes . tramp-archive-handle-not-implemented) (set-file-selinux-context . ignore) (set-file-times . tramp-archive-handle-not-implemented) (set-visited-file-modtime . tramp-handle-set-visited-file-modtime) (shell-command . tramp-archive-handle-not-implemented) (start-file-process . tramp-archive-handle-not-implemented) (temporary-file-directory . tramp-archive-handle-temporary-file-directory) (tramp-get-remote-gid . ignore) (tramp-get-remote-uid . ignore) (tramp-set-file-uid-gid . ignore) (unhandled-file-name-directory . ignore) (vc-registered . ignore) (verify-visited-file-modtime . tramp-handle-verify-visited-file-modtime) (write-region . tramp-archive-handle-not-implemented)) "\
Alist of handler functions for file archive method.
Operations not mentioned here will be handled by the default Emacs primitives.")

(autoload 'tramp-archive-file-name-handler "tramp-archive" "\
Invoke the file archive related OPERATION.
First arg specifies the OPERATION, second arg ARGS is a list of
arguments to pass to the OPERATION.

\(fn OPERATION &rest ARGS)" nil nil)

;;;***

;;;### (autoloads nil "tramp-cache" "tramp-cache.el" (0 0 0 0))
;;; Generated autoloads from tramp-cache.el

(defvar tramp-cache-data (make-hash-table :test #'equal) "\
Hash table for remote files properties.")

(defvar tramp-connection-properties nil "\
List of static connection properties.
Every entry has the form (REGEXP PROPERTY VALUE).  The regexp
matches remote file names.  It can be nil.  PROPERTY is a string,
and VALUE the corresponding value.  They are used, if there is no
matching entry for PROPERTY in `tramp-cache-data'.  For more
details see the info pages.")

(custom-autoload 'tramp-connection-properties "tramp-cache" t)

(defvar tramp-persistency-file-name (expand-file-name (locate-user-emacs-file "tramp")) "\
File which keeps connection history for Tramp connections.")

(custom-autoload 'tramp-persistency-file-name "tramp-cache" t)

(defconst tramp-cache-undefined 'undef "\
The symbol marking undefined hash keys and values.")

(autoload 'tramp-get-file-property "tramp-cache" "\
Get the PROPERTY of FILE from the cache context of KEY.
Return DEFAULT if not set.

\(fn KEY FILE PROPERTY DEFAULT)" nil nil)

(autoload 'tramp-set-file-property "tramp-cache" "\
Set the PROPERTY of FILE to VALUE, in the cache context of KEY.
Return VALUE.

\(fn KEY FILE PROPERTY VALUE)" nil nil)

(autoload 'tramp-flush-file-property "tramp-cache" "\
Remove PROPERTY of FILE in the cache context of KEY.

\(fn KEY FILE PROPERTY)" nil nil)

(autoload 'tramp-flush-file-properties "tramp-cache" "\
Remove all properties of FILE in the cache context of KEY.

\(fn KEY FILE)" nil nil)

(autoload 'tramp-flush-directory-properties "tramp-cache" "\
Remove all properties of DIRECTORY in the cache context of KEY.
Remove also properties of all files in subdirectories.

\(fn KEY DIRECTORY)" nil nil)

(autoload 'tramp-get-connection-property "tramp-cache" "\
Get the named PROPERTY for the connection.
KEY identifies the connection, it is either a process or a
`tramp-file-name' structure.  A special case is nil, which is
used to cache connection properties of the local machine.
If KEY is `tramp-cache-undefined', or if the value is not set for
the connection, return DEFAULT.

\(fn KEY PROPERTY DEFAULT)" nil nil)

(autoload 'tramp-set-connection-property "tramp-cache" "\
Set the named PROPERTY of a connection to VALUE.
KEY identifies the connection, it is either a process or a
`tramp-file-name' structure.  A special case is nil, which is
used to cache connection properties of the local machine.  If KEY
is `tramp-cache-undefined', nothing is set.
PROPERTY is set persistent when KEY is a `tramp-file-name' structure.
Return VALUE.

\(fn KEY PROPERTY VALUE)" nil nil)

(autoload 'tramp-connection-property-p "tramp-cache" "\
Check whether named PROPERTY of a connection is defined.
KEY identifies the connection, it is either a process or a
`tramp-file-name' structure.  A special case is nil, which is
used to cache connection properties of the local machine.

\(fn KEY PROPERTY)" nil nil)

(autoload 'tramp-flush-connection-property "tramp-cache" "\
Remove the named PROPERTY of a connection identified by KEY.
KEY identifies the connection, it is either a process or a
`tramp-file-name' structure.  A special case is nil, which is
used to cache connection properties of the local machine.
PROPERTY is set persistent when KEY is a `tramp-file-name' structure.

\(fn KEY PROPERTY)" nil nil)

(autoload 'tramp-flush-connection-properties "tramp-cache" "\
Remove all properties identified by KEY.
KEY identifies the connection, it is either a process or a
`tramp-file-name' structure.  A special case is nil, which is
used to cache connection properties of the local machine.

\(fn KEY)" nil nil)

(autoload 'tramp-cache-print "tramp-cache" "\
Print hash table TABLE.

\(fn TABLE)" nil nil)

(autoload 'tramp-list-connections "tramp-cache" "\
Return all known `tramp-file-name' structs according to `tramp-cache'.

\(fn)" nil nil)

(autoload 'tramp-parse-connection-properties "tramp-cache" "\
Return a list of (user host) tuples allowed to access for METHOD.
This function is added always in `tramp-get-completion-function'
for all methods.  Resulting data are derived from connection history.

\(fn METHOD)" nil nil)

(defvar tramp-cache-read-persistent-data (or init-file-user site-run-file) "\
Whether to read persistent data at startup time.")

;;;***

;;;### (autoloads nil "tramp-cmds" "tramp-cmds.el" (0 0 0 0))
;;; Generated autoloads from tramp-cmds.el

(autoload 'tramp-change-syntax "tramp-cmds" "\
Change Tramp syntax.
SYNTAX can be one of the symbols `default' (default),
`simplified' (ange-ftp like) or `separate' (XEmacs like).

\(fn &optional SYNTAX)" t nil)

(defvar tramp-cleanup-connection-hook nil "\
List of functions to be called after Tramp connection is cleaned up.
Each function is called with the current vector as argument.")

(autoload 'tramp-cleanup-connection "tramp-cmds" "\
Flush all connection related objects.
This includes password cache, file cache, connection cache,
buffers, processes.  KEEP-DEBUG non-nil preserves the debug
buffer.  KEEP-PASSWORD non-nil preserves the password cache.
KEEP-PROCESSES non-nil preserves the asynchronous processes.
When called interactively, a Tramp connection has to be selected.

\(fn VEC &optional KEEP-DEBUG KEEP-PASSWORD KEEP-PROCESSES)" t nil)

(autoload 'tramp-cleanup-this-connection "tramp-cmds" "\
Flush all connection related objects of the current buffer's connection.

\(fn)" t nil)

(function-put #'tramp-cleanup-this-connection 'completion-predicate #'tramp-command-completion-p)

(defvar tramp-cleanup-all-connections-hook nil "\
List of functions to be called after all Tramp connections are cleaned up.")

(autoload 'tramp-cleanup-all-connections "tramp-cmds" "\
Flush all Tramp internal objects.
This includes password cache, file cache, connection cache, buffers.

\(fn)" t nil)

(autoload 'tramp-cleanup-all-buffers "tramp-cmds" "\
Kill all remote buffers.

\(fn)" t nil)

(autoload 'tramp-rename-files "tramp-cmds" "\
Replace in all buffers the visiting file name from SOURCE to TARGET.
SOURCE is a remote directory name, which could contain also a
localname part.  TARGET is the directory name SOURCE is replaced
with.  Often, TARGET is a remote directory name on another host,
but it can also be a local directory name.  If TARGET has no
local part, the local part from SOURCE is used.

If TARGET is nil, it is selected according to the first match in
`tramp-default-rename-alist'.  If called interactively, this
match is offered as initial value for selection.

On all buffers, which have a `buffer-file-name' matching SOURCE,
this name is modified by replacing SOURCE with TARGET.  This is
applied by calling `set-visited-file-name'.  The new
`buffer-file-name' is prompted for modification in the
minibuffer.  The buffers are marked modified, and must be saved
explicitly.

If user option `tramp-confirm-rename-file-names' is nil, changing
the file name happens without confirmation.  This requires a
matching entry in `tramp-default-rename-alist'.

Remote buffers related to the remote connection identified by
SOURCE, which are not visiting files, or which are visiting files
not matching SOURCE, are not modified.

Interactively, TARGET is selected from `tramp-default-rename-alist'
without confirmation if the prefix argument is non-nil.

The remote connection identified by SOURCE is flushed by
`tramp-cleanup-connection'.

\(fn SOURCE TARGET)" t nil)

(autoload 'tramp-rename-these-files "tramp-cmds" "\
Replace visiting file names to TARGET.
The current buffer must be related to a remote connection.  In
all buffers, which are visiting a file with the same directory
name, the buffer file name is changed.

Interactively, TARGET is selected from `tramp-default-rename-alist'
without confirmation if the prefix argument is non-nil.

For details, see `tramp-rename-files'.

\(fn TARGET)" t nil)

(function-put #'tramp-rename-these-files 'completion-predicate #'tramp-command-completion-p)

(autoload 'tramp-recompile-elpa-command-completion-p "tramp-cmds" "\
A predicate for `tramp-recompile-elpa'.
It is completed by \"M-x TAB\" only if package.el is loaded, and
Tramp is an installed ELPA package.

\(fn SYMBOL BUFFER)" nil nil)

(autoload 'tramp-recompile-elpa "tramp-cmds" "\
Recompile the installed Tramp ELPA package.
This is needed if there are compatibility problems.

\(fn)" t nil)

(function-put #'tramp-recompile-elpa 'completion-predicate #'tramp-recompile-elpa-command-completion-p)

(autoload 'tramp-version "tramp-cmds" "\
Print version number of tramp.el in echo area or current buffer.

\(fn ARG)" t nil)

(autoload 'tramp-bug "tramp-cmds" "\
Submit a bug report to the Tramp developers.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "tramp-crypt" "tramp-crypt.el" (0 0 0 0))
;;; Generated autoloads from tramp-crypt.el

(defvar tramp-crypt-enabled nil "\
Non-nil when encryption support is available.")

(defconst tramp-crypt-encfs-config ".encfs6.xml" "\
Encfs configuration file name.")

(defvar tramp-crypt-directories nil "\
List of crypted remote directories.")

(defsubst tramp-crypt-file-name-p (name) "\
Return the crypted remote directory NAME belongs to.
If NAME doesn't belong to a crypted remote directory, retun nil." (catch (quote crypt-file-name-p) (and tramp-crypt-enabled (stringp name) (not (tramp-compat-file-name-quoted-p name)) (not (string-suffix-p tramp-crypt-encfs-config name)) (dolist (dir tramp-crypt-directories) (and (string-prefix-p dir (file-name-as-directory (expand-file-name name))) (throw (quote crypt-file-name-p) dir))))))

(defconst tramp-crypt-file-name-handler-alist '((access-file . tramp-crypt-handle-access-file) (add-name-to-file . tramp-handle-add-name-to-file) (copy-directory . tramp-handle-copy-directory) (copy-file . tramp-crypt-handle-copy-file) (delete-directory . tramp-crypt-handle-delete-directory) (delete-file . tramp-crypt-handle-delete-file) (directory-files . tramp-crypt-handle-directory-files) (directory-files-and-attributes . tramp-handle-directory-files-and-attributes) (dired-compress-file . ignore) (dired-uncache . tramp-handle-dired-uncache) (exec-path . ignore) (file-accessible-directory-p . tramp-handle-file-accessible-directory-p) (file-acl . ignore) (file-attributes . tramp-crypt-handle-file-attributes) (file-directory-p . tramp-handle-file-directory-p) (file-equal-p . tramp-handle-file-equal-p) (file-executable-p . tramp-crypt-handle-file-executable-p) (file-exists-p . tramp-handle-file-exists-p) (file-in-directory-p . tramp-handle-file-in-directory-p) (file-local-copy . tramp-handle-file-local-copy) (file-modes . tramp-handle-file-modes) (file-name-all-completions . tramp-crypt-handle-file-name-all-completions) (file-name-case-insensitive-p . ignore) (file-name-completion . tramp-handle-file-name-completion) (file-newer-than-file-p . tramp-handle-file-newer-than-file-p) (file-notify-add-watch . ignore) (file-notify-rm-watch . ignore) (file-notify-valid-p . ignore) (file-ownership-preserved-p . tramp-crypt-handle-file-ownership-preserved-p) (file-readable-p . tramp-crypt-handle-file-readable-p) (file-regular-p . tramp-handle-file-regular-p) (file-selinux-context . ignore) (file-symlink-p . tramp-handle-file-symlink-p) (file-system-info . tramp-crypt-handle-file-system-info) (file-writable-p . tramp-crypt-handle-file-writable-p) (find-backup-file-name . tramp-handle-find-backup-file-name) (insert-directory . tramp-crypt-handle-insert-directory) (load . tramp-handle-load) (make-auto-save-file-name . tramp-handle-make-auto-save-file-name) (make-directory . tramp-crypt-handle-make-directory) (make-directory-internal . ignore) (make-nearby-temp-file . tramp-handle-make-nearby-temp-file) (make-process . ignore) (make-symbolic-link . tramp-handle-make-symbolic-link) (process-file . ignore) (rename-file . tramp-crypt-handle-rename-file) (set-file-acl . ignore) (set-file-modes . tramp-crypt-handle-set-file-modes) (set-file-selinux-context . ignore) (set-file-times . tramp-crypt-handle-set-file-times) (set-visited-file-modtime . tramp-handle-set-visited-file-modtime) (shell-command . ignore) (start-file-process . ignore) (temporary-file-directory . tramp-handle-temporary-file-directory) (tramp-set-file-uid-gid . tramp-crypt-handle-set-file-uid-gid) (unhandled-file-name-directory . ignore) (vc-registered . ignore) (verify-visited-file-modtime . tramp-handle-verify-visited-file-modtime) (write-region . tramp-handle-write-region)) "\
Alist of handler functions for crypt method.
Operations not mentioned here will be handled by the default Emacs primitives.")

(autoload 'tramp-crypt-file-name-handler "tramp-crypt" "\
Invoke the crypted remote file related OPERATION.
First arg specifies the OPERATION, second arg is a list of
arguments to pass to the OPERATION.

\(fn OPERATION &rest ARGS)" nil nil)

(defun tramp-register-crypt-file-name-handler nil "\
Add crypt file name handler to `file-name-handler-alist'." (when (and tramp-crypt-enabled tramp-crypt-directories) (add-to-list (quote file-name-handler-alist) (cons tramp-file-name-regexp (function tramp-crypt-file-name-handler))) (put (function tramp-crypt-file-name-handler) (quote safe-magic) t)))

(autoload 'tramp-crypt-add-directory "tramp-crypt" "\
Mark remote directory NAME for encryption.
Files in that directory and all subdirectories will be encrypted
before copying to, and decrypted after copying from that
directory.  File names will be also encrypted.

\(fn NAME)" t nil)

;;;***

;;;### (autoloads nil "tramp-ftp" "tramp-ftp.el" (0 0 0 0))
;;; Generated autoloads from tramp-ftp.el

(autoload 'tramp-ftp-enable-ange-ftp "tramp-ftp" "\
Reenable Ange-FTP, when Tramp is unloaded.

\(fn)" nil nil)

(defconst tramp-ftp-method "ftp" "\
When this method name is used, forward all calls to Ange-FTP.")

(tramp--with-startup (add-to-list 'tramp-methods (cons tramp-ftp-method nil)) (add-to-list 'tramp-default-method-alist (list "\\`ftp\\." nil tramp-ftp-method)) (add-to-list 'tramp-default-method-alist (list nil "\\`\\(anonymous\\|ftp\\)\\'" tramp-ftp-method)) (tramp-set-completion-function tramp-ftp-method '((tramp-parse-netrc "~/.netrc"))))

(autoload 'tramp-ftp-file-name-handler "tramp-ftp" "\
Invoke the Ange-FTP handler for OPERATION and ARGS.
First arg specifies the OPERATION, second arg is a list of arguments to
pass to the OPERATION.

\(fn OPERATION &rest ARGS)" nil nil)

(defsubst tramp-ftp-file-name-p (filename) "\
Check if it's a FILENAME that should be forwarded to Ange-FTP." (and (tramp-tramp-file-p filename) (string= (tramp-file-name-method (tramp-dissect-file-name filename)) tramp-ftp-method)))

(tramp--with-startup (add-to-list 'tramp-foreign-file-name-handler-alist (cons #'tramp-ftp-file-name-p #'tramp-ftp-file-name-handler)))

;;;***

;;;### (autoloads nil "tramp-gvfs" "tramp-gvfs.el" (0 0 0 0))
;;; Generated autoloads from tramp-gvfs.el

(defvar tramp-gvfs-methods '("afp" "dav" "davs" "gdrive" "mtp" "nextcloud" "sftp") "\
List of methods for remote files, accessed with GVFS.")

(custom-autoload 'tramp-gvfs-methods "tramp-gvfs" t)

(defconst tramp-goa-methods '("gdrive" "nextcloud") "\
List of methods which require registration at GNOME Online Accounts.")

(defvar tramp-media-methods '("afc" "gphoto2" "mtp") "\
List of GVFS methods which are covered by the \"mtp\" method.
They are checked during start up via
`tramp-gvfs-interface-remotevolumemonitor'.")

(when (featurep 'dbusbind) (tramp--with-startup (dolist (method tramp-gvfs-methods) (unless (assoc method tramp-methods) (add-to-list 'tramp-methods `(,method))) (when (member method tramp-goa-methods) (add-to-list 'tramp-default-host-alist `(,method nil ""))))))

(defconst tramp-goa-service "org.gnome.OnlineAccounts" "\
The well known name of the GNOME Online Accounts service.")

(defconst tramp-gvfs-service-afc-volumemonitor "org.gtk.vfs.AfcVolumeMonitor" "\
The well known name of the AFC volume monitor.")

(defconst tramp-gvfs-service-gphoto2-volumemonitor "org.gtk.vfs.GPhoto2VolumeMonitor" "\
The well known name of the GPhoto2 volume monitor.")

(defconst tramp-gvfs-service-mtp-volumemonitor "org.gtk.vfs.MTPVolumeMonitor" "\
The well known name of the MTP volume monitor.")

(defconst tramp-gvfs-file-name-handler-alist '((access-file . tramp-handle-access-file) (add-name-to-file . tramp-handle-add-name-to-file) (copy-directory . tramp-handle-copy-directory) (copy-file . tramp-gvfs-handle-copy-file) (delete-directory . tramp-gvfs-handle-delete-directory) (delete-file . tramp-gvfs-handle-delete-file) (directory-file-name . tramp-handle-directory-file-name) (directory-files . tramp-handle-directory-files) (directory-files-and-attributes . tramp-handle-directory-files-and-attributes) (dired-compress-file . ignore) (dired-uncache . tramp-handle-dired-uncache) (exec-path . ignore) (expand-file-name . tramp-gvfs-handle-expand-file-name) (file-accessible-directory-p . tramp-handle-file-accessible-directory-p) (file-acl . ignore) (file-attributes . tramp-gvfs-handle-file-attributes) (file-directory-p . tramp-handle-file-directory-p) (file-equal-p . tramp-handle-file-equal-p) (file-executable-p . tramp-gvfs-handle-file-executable-p) (file-exists-p . tramp-handle-file-exists-p) (file-in-directory-p . tramp-handle-file-in-directory-p) (file-local-copy . tramp-handle-file-local-copy) (file-modes . tramp-handle-file-modes) (file-name-all-completions . tramp-gvfs-handle-file-name-all-completions) (file-name-as-directory . tramp-handle-file-name-as-directory) (file-name-case-insensitive-p . tramp-handle-file-name-case-insensitive-p) (file-name-completion . tramp-handle-file-name-completion) (file-name-directory . tramp-handle-file-name-directory) (file-name-nondirectory . tramp-handle-file-name-nondirectory) (file-newer-than-file-p . tramp-handle-file-newer-than-file-p) (file-notify-add-watch . tramp-gvfs-handle-file-notify-add-watch) (file-notify-rm-watch . tramp-handle-file-notify-rm-watch) (file-notify-valid-p . tramp-handle-file-notify-valid-p) (file-ownership-preserved-p . ignore) (file-readable-p . tramp-gvfs-handle-file-readable-p) (file-regular-p . tramp-handle-file-regular-p) (file-remote-p . tramp-handle-file-remote-p) (file-selinux-context . tramp-handle-file-selinux-context) (file-symlink-p . tramp-handle-file-symlink-p) (file-system-info . tramp-gvfs-handle-file-system-info) (file-truename . tramp-handle-file-truename) (file-writable-p . tramp-handle-file-writable-p) (find-backup-file-name . tramp-handle-find-backup-file-name) (insert-directory . tramp-handle-insert-directory) (insert-file-contents . tramp-handle-insert-file-contents) (load . tramp-handle-load) (make-auto-save-file-name . tramp-handle-make-auto-save-file-name) (make-directory . tramp-gvfs-handle-make-directory) (make-directory-internal . ignore) (make-nearby-temp-file . tramp-handle-make-nearby-temp-file) (make-process . ignore) (make-symbolic-link . tramp-handle-make-symbolic-link) (process-file . ignore) (rename-file . tramp-gvfs-handle-rename-file) (set-file-acl . ignore) (set-file-modes . tramp-gvfs-handle-set-file-modes) (set-file-selinux-context . ignore) (set-file-times . tramp-gvfs-handle-set-file-times) (set-visited-file-modtime . tramp-handle-set-visited-file-modtime) (shell-command . ignore) (start-file-process . ignore) (substitute-in-file-name . tramp-handle-substitute-in-file-name) (temporary-file-directory . tramp-handle-temporary-file-directory) (tramp-get-remote-gid . tramp-gvfs-handle-get-remote-gid) (tramp-get-remote-uid . tramp-gvfs-handle-get-remote-uid) (tramp-set-file-uid-gid . tramp-gvfs-handle-set-file-uid-gid) (unhandled-file-name-directory . ignore) (vc-registered . ignore) (verify-visited-file-modtime . tramp-handle-verify-visited-file-modtime) (write-region . tramp-handle-write-region)) "\
Alist of handler functions for Tramp GVFS method.
Operations not mentioned here will be handled by the default Emacs primitives.")

(defsubst tramp-gvfs-file-name-p (filename) "\
Check if it's a FILENAME handled by the GVFS daemon." (and (tramp-tramp-file-p filename) (let ((method (tramp-file-name-method (tramp-dissect-file-name filename)))) (and (stringp method) (member method tramp-gvfs-methods)))))

(autoload 'tramp-gvfs-file-name-handler "tramp-gvfs" "\
Invoke the GVFS related OPERATION and ARGS.
First arg specifies the OPERATION, second arg is a list of
arguments to pass to the OPERATION.

\(fn OPERATION &rest ARGS)" nil nil)

(when (featurep 'dbusbind) (tramp--with-startup (tramp-register-foreign-file-name-handler #'tramp-gvfs-file-name-p #'tramp-gvfs-file-name-handler)))

;;;***

;;;### (autoloads nil "tramp-rclone" "tramp-rclone.el" (0 0 0 0))
;;; Generated autoloads from tramp-rclone.el

(defconst tramp-rclone-method "rclone" "\
When this method name is used, forward all calls to rclone mounts.")

(tramp--with-startup (add-to-list 'tramp-methods `(,tramp-rclone-method (tramp-mount-args ("--no-unicode-normalization" "--dir-cache-time" "0s")) (tramp-copyto-args nil) (tramp-moveto-args nil) (tramp-about-args ("--full")))) (add-to-list 'tramp-default-host-alist `(,tramp-rclone-method nil "")) (tramp-set-completion-function tramp-rclone-method '((tramp-rclone-parse-device-names ""))))

(defconst tramp-rclone-file-name-handler-alist '((access-file . tramp-handle-access-file) (add-name-to-file . tramp-handle-add-name-to-file) (copy-directory . tramp-handle-copy-directory) (copy-file . tramp-rclone-handle-copy-file) (delete-directory . tramp-fuse-handle-delete-directory) (delete-file . tramp-fuse-handle-delete-file) (directory-file-name . tramp-handle-directory-file-name) (directory-files . tramp-fuse-handle-directory-files) (directory-files-and-attributes . tramp-handle-directory-files-and-attributes) (dired-compress-file . ignore) (dired-uncache . tramp-handle-dired-uncache) (exec-path . ignore) (expand-file-name . tramp-handle-expand-file-name) (file-accessible-directory-p . tramp-handle-file-accessible-directory-p) (file-acl . ignore) (file-attributes . tramp-fuse-handle-file-attributes) (file-directory-p . tramp-handle-file-directory-p) (file-equal-p . tramp-handle-file-equal-p) (file-executable-p . tramp-fuse-handle-file-executable-p) (file-exists-p . tramp-handle-file-exists-p) (file-in-directory-p . tramp-handle-file-in-directory-p) (file-local-copy . tramp-handle-file-local-copy) (file-modes . tramp-handle-file-modes) (file-name-all-completions . tramp-fuse-handle-file-name-all-completions) (file-name-as-directory . tramp-handle-file-name-as-directory) (file-name-case-insensitive-p . tramp-handle-file-name-case-insensitive-p) (file-name-completion . tramp-handle-file-name-completion) (file-name-directory . tramp-handle-file-name-directory) (file-name-nondirectory . tramp-handle-file-name-nondirectory) (file-newer-than-file-p . tramp-handle-file-newer-than-file-p) (file-notify-add-watch . ignore) (file-notify-rm-watch . ignore) (file-notify-valid-p . ignore) (file-ownership-preserved-p . ignore) (file-readable-p . tramp-fuse-handle-file-readable-p) (file-regular-p . tramp-handle-file-regular-p) (file-remote-p . tramp-handle-file-remote-p) (file-selinux-context . tramp-handle-file-selinux-context) (file-symlink-p . tramp-handle-file-symlink-p) (file-system-info . tramp-rclone-handle-file-system-info) (file-truename . tramp-handle-file-truename) (file-writable-p . tramp-handle-file-writable-p) (find-backup-file-name . tramp-handle-find-backup-file-name) (insert-directory . tramp-handle-insert-directory) (insert-file-contents . tramp-handle-insert-file-contents) (load . tramp-handle-load) (make-auto-save-file-name . tramp-handle-make-auto-save-file-name) (make-directory . tramp-fuse-handle-make-directory) (make-directory-internal . ignore) (make-nearby-temp-file . tramp-handle-make-nearby-temp-file) (make-process . ignore) (make-symbolic-link . tramp-handle-make-symbolic-link) (process-file . ignore) (rename-file . tramp-rclone-handle-rename-file) (set-file-acl . ignore) (set-file-modes . ignore) (set-file-selinux-context . ignore) (set-file-times . ignore) (set-visited-file-modtime . tramp-handle-set-visited-file-modtime) (shell-command . ignore) (start-file-process . ignore) (substitute-in-file-name . tramp-handle-substitute-in-file-name) (temporary-file-directory . tramp-handle-temporary-file-directory) (tramp-get-remote-gid . ignore) (tramp-get-remote-uid . ignore) (tramp-set-file-uid-gid . ignore) (unhandled-file-name-directory . ignore) (vc-registered . ignore) (verify-visited-file-modtime . tramp-handle-verify-visited-file-modtime) (write-region . tramp-handle-write-region)) "\
Alist of handler functions for Tramp RCLONE method.
Operations not mentioned here will be handled by the default Emacs primitives.")

(defsubst tramp-rclone-file-name-p (filename) "\
Check if it's a FILENAME for rclone." (and (tramp-tramp-file-p filename) (string= (tramp-file-name-method (tramp-dissect-file-name filename)) tramp-rclone-method)))

(autoload 'tramp-rclone-file-name-handler "tramp-rclone" "\
Invoke the rclone handler for OPERATION and ARGS.
First arg specifies the OPERATION, second arg is a list of
arguments to pass to the OPERATION.

\(fn OPERATION &rest ARGS)" nil nil)

(tramp--with-startup (tramp-register-foreign-file-name-handler #'tramp-rclone-file-name-p #'tramp-rclone-file-name-handler))

(autoload 'tramp-rclone-parse-device-names "tramp-rclone" "\
Return a list of (nil host) tuples allowed to access.

\(fn IGNORE)" nil nil)

;;;***

;;;### (autoloads nil "tramp-sh" "tramp-sh.el" (0 0 0 0))
;;; Generated autoloads from tramp-sh.el

(defconst tramp-default-remote-shell "/bin/sh" "\
The default remote shell Tramp applies.")

(defconst tramp-display-escape-sequence-regexp "[[:digit:];[]+m" "\
Terminal control escape sequences for display attributes.")

(defconst tramp-initial-end-of-output "#$ " "\
Prompt when establishing a connection.")

(tramp--with-startup (add-to-list 'tramp-methods `("rcp" (tramp-login-program "rsh") (tramp-login-args (("%h") ("-l" "%u"))) (tramp-remote-shell ,tramp-default-remote-shell) (tramp-remote-shell-login ("-l")) (tramp-remote-shell-args ("-c")) (tramp-copy-program "rcp") (tramp-copy-args (("-p" "%k") ("-r"))) (tramp-copy-keep-date t) (tramp-copy-recursive t))) (add-to-list 'tramp-methods `("remcp" (tramp-login-program "remsh") (tramp-login-args (("%h") ("-l" "%u"))) (tramp-remote-shell ,tramp-default-remote-shell) (tramp-remote-shell-login ("-l")) (tramp-remote-shell-args ("-c")) (tramp-copy-program "rcp") (tramp-copy-args (("-p" "%k"))) (tramp-copy-keep-date t))) (add-to-list 'tramp-methods `("scp" (tramp-login-program "ssh") (tramp-login-args (("-l" "%u") ("-p" "%p") ("%c") ("-e" "none") ("%h"))) (tramp-async-args (("-q"))) (tramp-direct-async t) (tramp-remote-shell ,tramp-default-remote-shell) (tramp-remote-shell-login ("-l")) (tramp-remote-shell-args ("-c")) (tramp-copy-program "scp") (tramp-copy-args (("-P" "%p") ("-p" "%k") ("%x") ("-q") ("-r") ("%c"))) (tramp-copy-keep-date t) (tramp-copy-recursive t))) (add-to-list 'tramp-methods `("scpx" (tramp-login-program "ssh") (tramp-login-args (("-l" "%u") ("-p" "%p") ("%c") ("-e" "none") ("-t" "-t") ("-o" "RemoteCommand=\"%l\"") ("%h"))) (tramp-async-args (("-q"))) (tramp-remote-shell ,tramp-default-remote-shell) (tramp-remote-shell-login ("-l")) (tramp-remote-shell-args ("-c")) (tramp-copy-program "scp") (tramp-copy-args (("-P" "%p") ("-p" "%k") ("%x") ("-q") ("-r") ("%c"))) (tramp-copy-keep-date t) (tramp-copy-recursive t))) (add-to-list 'tramp-methods `("rsync" (tramp-login-program "ssh") (tramp-login-args (("-l" "%u") ("-p" "%p") ("%c") ("-e" "none") ("%h"))) (tramp-async-args (("-q"))) (tramp-direct-async t) (tramp-remote-shell ,tramp-default-remote-shell) (tramp-remote-shell-login ("-l")) (tramp-remote-shell-args ("-c")) (tramp-copy-program "rsync") (tramp-copy-args (("-t" "%k") ("-p") ("-r") ("-s") ("-c"))) (tramp-copy-env (("RSYNC_RSH") ("ssh" "%c"))) (tramp-copy-keep-date t) (tramp-copy-keep-tmpfile t) (tramp-copy-recursive t))) (add-to-list 'tramp-methods `("rsh" (tramp-login-program "rsh") (tramp-login-args (("%h") ("-l" "%u"))) (tramp-remote-shell ,tramp-default-remote-shell) (tramp-remote-shell-login ("-l")) (tramp-remote-shell-args ("-c")))) (add-to-list 'tramp-methods `("remsh" (tramp-login-program "remsh") (tramp-login-args (("%h") ("-l" "%u"))) (tramp-remote-shell ,tramp-default-remote-shell) (tramp-remote-shell-login ("-l")) (tramp-remote-shell-args ("-c")))) (add-to-list 'tramp-methods `("ssh" (tramp-login-program "ssh") (tramp-login-args (("-l" "%u") ("-p" "%p") ("%c") ("-e" "none") ("%h"))) (tramp-async-args (("-q"))) (tramp-direct-async t) (tramp-remote-shell ,tramp-default-remote-shell) (tramp-remote-shell-login ("-l")) (tramp-remote-shell-args ("-c")))) (add-to-list 'tramp-methods `("sshx" (tramp-login-program "ssh") (tramp-login-args (("-l" "%u") ("-p" "%p") ("%c") ("-e" "none") ("-t" "-t") ("-o" "RemoteCommand=\"%l\"") ("%h"))) (tramp-async-args (("-q"))) (tramp-remote-shell ,tramp-default-remote-shell) (tramp-remote-shell-login ("-l")) (tramp-remote-shell-args ("-c")))) (add-to-list 'tramp-methods `("telnet" (tramp-login-program "telnet") (tramp-login-args (("%h") ("%p") ("%n"))) (tramp-remote-shell ,tramp-default-remote-shell) (tramp-remote-shell-login ("-l")) (tramp-remote-shell-args ("-c")))) (add-to-list 'tramp-methods `("nc" (tramp-login-program "telnet") (tramp-login-args (("%h") ("%p") ("%n"))) (tramp-remote-shell ,tramp-default-remote-shell) (tramp-remote-shell-login ("-l")) (tramp-remote-shell-args ("-c")) (tramp-copy-program "nc") (tramp-copy-args (("-w" "1") ("-v") ("%h") ("%r"))) (tramp-remote-copy-program "nc") (tramp-remote-copy-args (("-l") ("-p" "%r") ("%n"))))) (add-to-list 'tramp-methods `("su" (tramp-login-program "su") (tramp-login-args (("-") ("%u"))) (tramp-remote-shell ,tramp-default-remote-shell) (tramp-remote-shell-login ("-l")) (tramp-remote-shell-args ("-c")) (tramp-connection-timeout 10))) (add-to-list 'tramp-methods `("sg" (tramp-login-program "sg") (tramp-login-args (("-") ("%u"))) (tramp-remote-shell ,tramp-default-remote-shell) (tramp-remote-shell-args ("-c")) (tramp-connection-timeout 10))) (add-to-list 'tramp-methods `("sudo" (tramp-login-program "sudo") (tramp-login-args (("-u" "%u") ("-s") ("-H") ("-p" "P\"\"a\"\"s\"\"s\"\"w\"\"o\"\"r\"\"d\"\":") ("%l"))) (tramp-remote-shell ,tramp-default-remote-shell) (tramp-remote-shell-login ("-l")) (tramp-remote-shell-args ("-c")) (tramp-connection-timeout 10) (tramp-session-timeout 300))) (add-to-list 'tramp-methods `("doas" (tramp-login-program "doas") (tramp-login-args (("-u" "%u") ("-s"))) (tramp-remote-shell ,tramp-default-remote-shell) (tramp-remote-shell-args ("-c")) (tramp-connection-timeout 10) (tramp-session-timeout 300))) (add-to-list 'tramp-methods `("ksu" (tramp-login-program "ksu") (tramp-login-args (("%u") ("-q"))) (tramp-remote-shell ,tramp-default-remote-shell) (tramp-remote-shell-login ("-l")) (tramp-remote-shell-args ("-c")) (tramp-connection-timeout 10))) (add-to-list 'tramp-methods `("krlogin" (tramp-login-program "krlogin") (tramp-login-args (("%h") ("-l" "%u") ("-x"))) (tramp-remote-shell ,tramp-default-remote-shell) (tramp-remote-shell-login ("-l")) (tramp-remote-shell-args ("-c")))) (add-to-list 'tramp-methods `("plink" (tramp-login-program "plink") (tramp-login-args (("-l" "%u") ("-P" "%p") ("-ssh") ("-t") ("%h") ("\"") (,(format "env 'TERM=%s' 'PROMPT_COMMAND=' 'PS1=%s'" tramp-terminal-type tramp-initial-end-of-output)) ("%l") ("\""))) (tramp-remote-shell ,tramp-default-remote-shell) (tramp-remote-shell-login ("-l")) (tramp-remote-shell-args ("-c")))) (add-to-list 'tramp-methods `("plinkx" (tramp-login-program "plink") (tramp-login-args (("-load") ("%h") ("-t") ("\"") (,(format "env 'TERM=%s' 'PROMPT_COMMAND=' 'PS1=%s'" tramp-terminal-type tramp-initial-end-of-output)) ("%l") ("\""))) (tramp-remote-shell ,tramp-default-remote-shell) (tramp-remote-shell-login ("-l")) (tramp-remote-shell-args ("-c")))) (add-to-list 'tramp-methods `("pscp" (tramp-login-program "plink") (tramp-login-args (("-l" "%u") ("-P" "%p") ("-ssh") ("-t") ("%h") ("\"") (,(format "env 'TERM=%s' 'PROMPT_COMMAND=' 'PS1=%s'" tramp-terminal-type tramp-initial-end-of-output)) ("%l") ("\""))) (tramp-remote-shell ,tramp-default-remote-shell) (tramp-remote-shell-login ("-l")) (tramp-remote-shell-args ("-c")) (tramp-copy-program "pscp") (tramp-copy-args (("-l" "%u") ("-P" "%p") ("-scp") ("-p" "%k") ("-q") ("-r"))) (tramp-copy-keep-date t) (tramp-copy-recursive t))) (add-to-list 'tramp-methods `("psftp" (tramp-login-program "plink") (tramp-login-args (("-l" "%u") ("-P" "%p") ("-ssh") ("-t") ("%h") ("\"") (,(format "env 'TERM=%s' 'PROMPT_COMMAND=' 'PS1=%s'" tramp-terminal-type tramp-initial-end-of-output)) ("%l") ("\""))) (tramp-remote-shell ,tramp-default-remote-shell) (tramp-remote-shell-login ("-l")) (tramp-remote-shell-args ("-c")) (tramp-copy-program "pscp") (tramp-copy-args (("-l" "%u") ("-P" "%p") ("-sftp") ("-p" "%k") ("-q"))) (tramp-copy-keep-date t))) (add-to-list 'tramp-methods `("fcp" (tramp-login-program "fsh") (tramp-login-args (("%h") ("-l" "%u") ("sh" "-i"))) (tramp-remote-shell ,tramp-default-remote-shell) (tramp-remote-shell-login ("-l")) (tramp-remote-shell-args ("-i") ("-c")) (tramp-copy-program "fcp") (tramp-copy-args (("-p" "%k"))) (tramp-copy-keep-date t))) (add-to-list 'tramp-default-method-alist `(,tramp-local-host-regexp "\\`root\\'" "su")) (add-to-list 'tramp-default-user-alist `(,(concat "\\`" (regexp-opt '("su" "sudo" "doas" "ksu")) "\\'") nil "root")) (add-to-list 'tramp-default-user-alist `(,(concat "\\`" (regexp-opt '("rcp" "remcp" "rsh" "telnet" "nc" "krlogin" "fcp")) "\\'") nil ,(user-login-name))))

(defconst tramp-completion-function-alist-rsh '((tramp-parse-rhosts "/etc/hosts.equiv") (tramp-parse-rhosts "~/.rhosts")) "\
Default list of (FUNCTION FILE) pairs to be examined for rsh methods.")

(defconst tramp-completion-function-alist-ssh `((tramp-parse-rhosts "/etc/hosts.equiv") (tramp-parse-rhosts "/etc/shosts.equiv") (tramp-parse-shosts ,(expand-file-name "ssh/ssh_known_hosts" (or (and (eq system-type 'windows-nt) (getenv "ProgramData")) "/etc/"))) (tramp-parse-sconfig ,(expand-file-name "ssh/ssh_config" (or (and (eq system-type 'windows-nt) (getenv "ProgramData")) "/etc/"))) (tramp-parse-shostkeys "/etc/ssh2/hostkeys") (tramp-parse-sknownhosts "/etc/ssh2/knownhosts") (tramp-parse-rhosts "~/.rhosts") (tramp-parse-rhosts "~/.shosts") (tramp-parse-shosts ,(expand-file-name ".ssh/known_hosts" (or (and (eq system-type 'windows-nt) (getenv "USERPROFILE")) "~/"))) (tramp-parse-sconfig ,(expand-file-name ".ssh/config" (or (and (eq system-type 'windows-nt) (getenv "USERPROFILE")) "~/"))) (tramp-parse-shostkeys "~/.ssh2/hostkeys") (tramp-parse-sknownhosts "~/.ssh2/knownhosts")) "\
Default list of (FUNCTION FILE) pairs to be examined for ssh methods.")

(defconst tramp-completion-function-alist-telnet '((tramp-parse-hosts "/etc/hosts")) "\
Default list of (FUNCTION FILE) pairs to be examined for telnet methods.")

(defconst tramp-completion-function-alist-su '((tramp-parse-passwd "/etc/passwd")) "\
Default list of (FUNCTION FILE) pairs to be examined for su methods.")

(defconst tramp-completion-function-alist-sg '((tramp-parse-etc-group "/etc/group")) "\
Default list of (FUNCTION FILE) pairs to be examined for sg methods.")

(defconst tramp-completion-function-alist-putty `((tramp-parse-putty ,(if (eq system-type 'windows-nt) "HKEY_CURRENT_USER\\Software\\SimonTatham\\PuTTY\\Sessions" "~/.putty/sessions"))) "\
Default list of (FUNCTION REGISTRY) pairs to be examined for putty sessions.")

(tramp--with-startup (tramp-set-completion-function "rcp" tramp-completion-function-alist-rsh) (tramp-set-completion-function "remcp" tramp-completion-function-alist-rsh) (tramp-set-completion-function "scp" tramp-completion-function-alist-ssh) (tramp-set-completion-function "scpx" tramp-completion-function-alist-ssh) (tramp-set-completion-function "rsync" tramp-completion-function-alist-ssh) (tramp-set-completion-function "rsh" tramp-completion-function-alist-rsh) (tramp-set-completion-function "remsh" tramp-completion-function-alist-rsh) (tramp-set-completion-function "ssh" tramp-completion-function-alist-ssh) (tramp-set-completion-function "sshx" tramp-completion-function-alist-ssh) (tramp-set-completion-function "telnet" tramp-completion-function-alist-telnet) (tramp-set-completion-function "nc" tramp-completion-function-alist-telnet) (tramp-set-completion-function "su" tramp-completion-function-alist-su) (tramp-set-completion-function "sudo" tramp-completion-function-alist-su) (tramp-set-completion-function "doas" tramp-completion-function-alist-su) (tramp-set-completion-function "ksu" tramp-completion-function-alist-su) (tramp-set-completion-function "sg" tramp-completion-function-alist-sg) (tramp-set-completion-function "krlogin" tramp-completion-function-alist-rsh) (tramp-set-completion-function "plink" tramp-completion-function-alist-ssh) (tramp-set-completion-function "plinkx" tramp-completion-function-alist-putty) (tramp-set-completion-function "pscp" tramp-completion-function-alist-ssh) (tramp-set-completion-function "psftp" tramp-completion-function-alist-ssh) (tramp-set-completion-function "fcp" tramp-completion-function-alist-ssh))

(defconst tramp-sh-file-name-handler-alist '((access-file . tramp-handle-access-file) (add-name-to-file . tramp-sh-handle-add-name-to-file) (copy-directory . tramp-sh-handle-copy-directory) (copy-file . tramp-sh-handle-copy-file) (delete-directory . tramp-sh-handle-delete-directory) (delete-file . tramp-sh-handle-delete-file) (directory-file-name . tramp-handle-directory-file-name) (directory-files . tramp-handle-directory-files) (directory-files-and-attributes . tramp-sh-handle-directory-files-and-attributes) (dired-compress-file . tramp-sh-handle-dired-compress-file) (dired-uncache . tramp-handle-dired-uncache) (exec-path . tramp-sh-handle-exec-path) (expand-file-name . tramp-sh-handle-expand-file-name) (file-accessible-directory-p . tramp-handle-file-accessible-directory-p) (file-acl . tramp-sh-handle-file-acl) (file-attributes . tramp-sh-handle-file-attributes) (file-directory-p . tramp-sh-handle-file-directory-p) (file-equal-p . tramp-handle-file-equal-p) (file-executable-p . tramp-sh-handle-file-executable-p) (file-exists-p . tramp-sh-handle-file-exists-p) (file-in-directory-p . tramp-handle-file-in-directory-p) (file-local-copy . tramp-sh-handle-file-local-copy) (file-modes . tramp-handle-file-modes) (file-name-all-completions . tramp-sh-handle-file-name-all-completions) (file-name-as-directory . tramp-handle-file-name-as-directory) (file-name-case-insensitive-p . tramp-handle-file-name-case-insensitive-p) (file-name-completion . tramp-handle-file-name-completion) (file-name-directory . tramp-handle-file-name-directory) (file-name-nondirectory . tramp-handle-file-name-nondirectory) (file-newer-than-file-p . tramp-handle-file-newer-than-file-p) (file-notify-add-watch . tramp-sh-handle-file-notify-add-watch) (file-notify-rm-watch . tramp-handle-file-notify-rm-watch) (file-notify-valid-p . tramp-handle-file-notify-valid-p) (file-ownership-preserved-p . tramp-sh-handle-file-ownership-preserved-p) (file-readable-p . tramp-sh-handle-file-readable-p) (file-regular-p . tramp-handle-file-regular-p) (file-remote-p . tramp-handle-file-remote-p) (file-selinux-context . tramp-sh-handle-file-selinux-context) (file-symlink-p . tramp-handle-file-symlink-p) (file-system-info . tramp-sh-handle-file-system-info) (file-truename . tramp-sh-handle-file-truename) (file-writable-p . tramp-sh-handle-file-writable-p) (find-backup-file-name . tramp-handle-find-backup-file-name) (insert-directory . tramp-sh-handle-insert-directory) (insert-file-contents . tramp-handle-insert-file-contents) (load . tramp-handle-load) (make-auto-save-file-name . tramp-handle-make-auto-save-file-name) (make-directory . tramp-sh-handle-make-directory) (make-nearby-temp-file . tramp-handle-make-nearby-temp-file) (make-process . tramp-sh-handle-make-process) (make-symbolic-link . tramp-sh-handle-make-symbolic-link) (process-file . tramp-sh-handle-process-file) (rename-file . tramp-sh-handle-rename-file) (set-file-acl . tramp-sh-handle-set-file-acl) (set-file-modes . tramp-sh-handle-set-file-modes) (set-file-selinux-context . tramp-sh-handle-set-file-selinux-context) (set-file-times . tramp-sh-handle-set-file-times) (set-visited-file-modtime . tramp-sh-handle-set-visited-file-modtime) (shell-command . tramp-handle-shell-command) (start-file-process . tramp-handle-start-file-process) (substitute-in-file-name . tramp-handle-substitute-in-file-name) (temporary-file-directory . tramp-handle-temporary-file-directory) (tramp-get-remote-gid . tramp-sh-handle-get-remote-gid) (tramp-get-remote-uid . tramp-sh-handle-get-remote-uid) (tramp-set-file-uid-gid . tramp-sh-handle-set-file-uid-gid) (unhandled-file-name-directory . ignore) (vc-registered . tramp-sh-handle-vc-registered) (verify-visited-file-modtime . tramp-sh-handle-verify-visited-file-modtime) (write-region . tramp-sh-handle-write-region)) "\
Alist of handler functions.
Operations not mentioned here will be handled by the normal Emacs functions.")

(autoload 'tramp-sh-file-name-handler "tramp-sh" "\
Invoke remote-shell Tramp file name handler.
Fall back to normal file name handler if no Tramp handler exists.

\(fn OPERATION &rest ARGS)" nil nil)

(autoload 'tramp-sh-file-name-handler-p "tramp-sh" "\
Whether VEC uses a method from `tramp-sh-file-name-handler'.

\(fn VEC)" nil nil)

(tramp--with-startup (tramp-register-foreign-file-name-handler #'identity #'tramp-sh-file-name-handler 'append))

(autoload 'tramp-convert-file-attributes "tramp-sh" "\
Convert `file-attributes' ATTR generated by perl script, stat or ls.
Convert file mode bits to string and set virtual device number.
Return ATTR.

\(fn VEC ATTR)" nil nil)

;;;***

;;;### (autoloads nil "tramp-smb" "tramp-smb.el" (0 0 0 0))
;;; Generated autoloads from tramp-smb.el

(defconst tramp-smb-method "smb" "\
Method to connect SAMBA and M$ SMB servers.")

(unless (memq system-type '(cygwin windows-nt)) (tramp--with-startup (add-to-list 'tramp-methods `(,tramp-smb-method (tramp-tmpdir "/C$/Temp") (tramp-case-insensitive t)))))

(tramp--with-startup (add-to-list 'tramp-default-user-alist `(,(concat "\\`" tramp-smb-method "\\'") nil nil)) (tramp-set-completion-function tramp-smb-method '((tramp-parse-netrc "~/.netrc"))))

(defconst tramp-smb-file-name-handler-alist '((access-file . tramp-handle-access-file) (add-name-to-file . tramp-smb-handle-add-name-to-file) (copy-directory . tramp-smb-handle-copy-directory) (copy-file . tramp-smb-handle-copy-file) (delete-directory . tramp-smb-handle-delete-directory) (delete-file . tramp-smb-handle-delete-file) (directory-file-name . tramp-handle-directory-file-name) (directory-files . tramp-smb-handle-directory-files) (directory-files-and-attributes . tramp-handle-directory-files-and-attributes) (dired-compress-file . ignore) (dired-uncache . tramp-handle-dired-uncache) (exec-path . ignore) (expand-file-name . tramp-smb-handle-expand-file-name) (file-accessible-directory-p . tramp-handle-file-accessible-directory-p) (file-acl . tramp-smb-handle-file-acl) (file-attributes . tramp-smb-handle-file-attributes) (file-directory-p . tramp-handle-file-directory-p) (file-file-equal-p . tramp-handle-file-equal-p) (file-executable-p . tramp-handle-file-exists-p) (file-exists-p . tramp-handle-file-exists-p) (file-in-directory-p . tramp-handle-file-in-directory-p) (file-local-copy . tramp-smb-handle-file-local-copy) (file-modes . tramp-handle-file-modes) (file-name-all-completions . tramp-smb-handle-file-name-all-completions) (file-name-as-directory . tramp-handle-file-name-as-directory) (file-name-case-insensitive-p . tramp-handle-file-name-case-insensitive-p) (file-name-completion . tramp-handle-file-name-completion) (file-name-directory . tramp-handle-file-name-directory) (file-name-nondirectory . tramp-handle-file-name-nondirectory) (file-newer-than-file-p . tramp-handle-file-newer-than-file-p) (file-notify-add-watch . tramp-handle-file-notify-add-watch) (file-notify-rm-watch . tramp-handle-file-notify-rm-watch) (file-notify-valid-p . tramp-handle-file-notify-valid-p) (file-ownership-preserved-p . ignore) (file-readable-p . tramp-handle-file-exists-p) (file-regular-p . tramp-handle-file-regular-p) (file-remote-p . tramp-handle-file-remote-p) (file-selinux-context . tramp-handle-file-selinux-context) (file-symlink-p . tramp-handle-file-symlink-p) (file-system-info . tramp-smb-handle-file-system-info) (file-truename . tramp-handle-file-truename) (file-writable-p . tramp-smb-handle-file-writable-p) (find-backup-file-name . tramp-handle-find-backup-file-name) (insert-directory . tramp-smb-handle-insert-directory) (insert-file-contents . tramp-handle-insert-file-contents) (load . tramp-handle-load) (make-auto-save-file-name . tramp-handle-make-auto-save-file-name) (make-directory . tramp-smb-handle-make-directory) (make-directory-internal . tramp-smb-handle-make-directory-internal) (make-nearby-temp-file . tramp-handle-make-nearby-temp-file) (make-process . ignore) (make-symbolic-link . tramp-smb-handle-make-symbolic-link) (process-file . tramp-smb-handle-process-file) (rename-file . tramp-smb-handle-rename-file) (set-file-acl . tramp-smb-handle-set-file-acl) (set-file-modes . tramp-smb-handle-set-file-modes) (set-file-selinux-context . ignore) (set-file-times . ignore) (set-visited-file-modtime . tramp-handle-set-visited-file-modtime) (shell-command . tramp-handle-shell-command) (start-file-process . tramp-smb-handle-start-file-process) (substitute-in-file-name . tramp-smb-handle-substitute-in-file-name) (temporary-file-directory . tramp-handle-temporary-file-directory) (tramp-get-remote-gid . ignore) (tramp-get-remote-uid . ignore) (tramp-set-file-uid-gid . ignore) (unhandled-file-name-directory . ignore) (vc-registered . ignore) (verify-visited-file-modtime . tramp-handle-verify-visited-file-modtime) (write-region . tramp-smb-handle-write-region)) "\
Alist of handler functions for Tramp SMB method.
Operations not mentioned here will be handled by the default Emacs primitives.")

(defsubst tramp-smb-file-name-p (filename) "\
Check if it's a FILENAME for SMB servers." (and (tramp-tramp-file-p filename) (string= (tramp-file-name-method (tramp-dissect-file-name filename)) tramp-smb-method)))

(autoload 'tramp-smb-file-name-handler "tramp-smb" "\
Invoke the SMB related OPERATION and ARGS.
First arg specifies the OPERATION, second arg is a list of
arguments to pass to the OPERATION.

\(fn OPERATION &rest ARGS)" nil nil)

(unless (memq system-type '(cygwin windows-nt)) (tramp--with-startup (tramp-register-foreign-file-name-handler #'tramp-smb-file-name-p #'tramp-smb-file-name-handler)))

;;;***

;;;### (autoloads nil "tramp-sshfs" "tramp-sshfs.el" (0 0 0 0))
;;; Generated autoloads from tramp-sshfs.el

(defconst tramp-sshfs-method "sshfs" "\
Tramp method for sshfs mounts.")

(tramp--with-startup (add-to-list 'tramp-methods `(,tramp-sshfs-method (tramp-mount-args (("-C") ("-p" "%p") ("-o" "idmap=user,reconnect"))) (tramp-login-program "ssh") (tramp-login-args (("-q") ("-l" "%u") ("-p" "%p") ("-e" "none") ("%h") ("%l"))) (tramp-direct-async t) (tramp-remote-shell ,tramp-default-remote-shell) (tramp-remote-shell-login ("-l")) (tramp-remote-shell-args ("-c")))) (add-to-list 'tramp-connection-properties `(,(format "/%s:" tramp-sshfs-method) "direct-async-process" t)) (tramp-set-completion-function tramp-sshfs-method tramp-completion-function-alist-ssh))

(defconst tramp-sshfs-file-name-handler-alist '((access-file . tramp-handle-access-file) (add-name-to-file . tramp-handle-add-name-to-file) (copy-directory . tramp-handle-copy-directory) (copy-file . tramp-sshfs-handle-copy-file) (delete-directory . tramp-fuse-handle-delete-directory) (delete-file . tramp-fuse-handle-delete-file) (directory-file-name . tramp-handle-directory-file-name) (directory-files . tramp-fuse-handle-directory-files) (directory-files-and-attributes . tramp-handle-directory-files-and-attributes) (dired-compress-file . ignore) (dired-uncache . tramp-handle-dired-uncache) (exec-path . tramp-sshfs-handle-exec-path) (expand-file-name . tramp-handle-expand-file-name) (file-accessible-directory-p . tramp-handle-file-accessible-directory-p) (file-acl . ignore) (file-attributes . tramp-fuse-handle-file-attributes) (file-directory-p . tramp-handle-file-directory-p) (file-equal-p . tramp-handle-file-equal-p) (file-executable-p . tramp-fuse-handle-file-executable-p) (file-exists-p . tramp-handle-file-exists-p) (file-in-directory-p . tramp-handle-file-in-directory-p) (file-local-copy . tramp-handle-file-local-copy) (file-modes . tramp-handle-file-modes) (file-name-all-completions . tramp-fuse-handle-file-name-all-completions) (file-name-as-directory . tramp-handle-file-name-as-directory) (file-name-case-insensitive-p . tramp-handle-file-name-case-insensitive-p) (file-name-completion . tramp-handle-file-name-completion) (file-name-directory . tramp-handle-file-name-directory) (file-name-nondirectory . tramp-handle-file-name-nondirectory) (file-newer-than-file-p . tramp-handle-file-newer-than-file-p) (file-notify-add-watch . ignore) (file-notify-rm-watch . ignore) (file-notify-valid-p . ignore) (file-ownership-preserved-p . ignore) (file-readable-p . tramp-fuse-handle-file-readable-p) (file-regular-p . tramp-handle-file-regular-p) (file-remote-p . tramp-handle-file-remote-p) (file-selinux-context . tramp-handle-file-selinux-context) (file-symlink-p . tramp-handle-file-symlink-p) (file-system-info . tramp-sshfs-handle-file-system-info) (file-truename . tramp-handle-file-truename) (file-writable-p . tramp-handle-file-writable-p) (find-backup-file-name . tramp-handle-find-backup-file-name) (insert-directory . tramp-handle-insert-directory) (insert-file-contents . tramp-sshfs-handle-insert-file-contents) (load . tramp-handle-load) (make-auto-save-file-name . tramp-handle-make-auto-save-file-name) (make-directory . tramp-fuse-handle-make-directory) (make-directory-internal . ignore) (make-nearby-temp-file . tramp-handle-make-nearby-temp-file) (make-process . tramp-handle-make-process) (make-symbolic-link . tramp-handle-make-symbolic-link) (process-file . tramp-sshfs-handle-process-file) (rename-file . tramp-sshfs-handle-rename-file) (set-file-acl . ignore) (set-file-modes . tramp-sshfs-handle-set-file-modes) (set-file-selinux-context . ignore) (set-file-times . ignore) (set-visited-file-modtime . tramp-handle-set-visited-file-modtime) (shell-command . tramp-handle-shell-command) (start-file-process . tramp-handle-start-file-process) (substitute-in-file-name . tramp-handle-substitute-in-file-name) (temporary-file-directory . tramp-handle-temporary-file-directory) (tramp-get-remote-gid . ignore) (tramp-get-remote-uid . ignore) (tramp-set-file-uid-gid . ignore) (unhandled-file-name-directory . ignore) (vc-registered . ignore) (verify-visited-file-modtime . tramp-handle-verify-visited-file-modtime) (write-region . tramp-sshfs-handle-write-region)) "\
Alist of handler functions for Tramp SSHFS method.
Operations not mentioned here will be handled by the default Emacs primitives.")

(defsubst tramp-sshfs-file-name-p (filename) "\
Check if it's a FILENAME for sshfs." (and (tramp-tramp-file-p filename) (string= (tramp-file-name-method (tramp-dissect-file-name filename)) tramp-sshfs-method)))

(autoload 'tramp-sshfs-file-name-handler "tramp-sshfs" "\
Invoke the sshfs handler for OPERATION and ARGS.
First arg specifies the OPERATION, second arg is a list of
arguments to pass to the OPERATION.

\(fn OPERATION &rest ARGS)" nil nil)

(tramp--with-startup (tramp-register-foreign-file-name-handler #'tramp-sshfs-file-name-p #'tramp-sshfs-file-name-handler))

;;;***

;;;### (autoloads nil "tramp-sudoedit" "tramp-sudoedit.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from tramp-sudoedit.el

(defconst tramp-sudoedit-method "sudoedit" "\
When this method name is used, call sudoedit for editing a file.")

(tramp--with-startup (add-to-list 'tramp-methods `(,tramp-sudoedit-method (tramp-sudo-login (("sudo") ("-u" "%u") ("-S") ("-H") ("-p" "Password:") ("--"))))) (add-to-list 'tramp-default-user-alist '("\\`sudoedit\\'" nil "root")) (tramp-set-completion-function tramp-sudoedit-method tramp-completion-function-alist-su))

(defconst tramp-sudoedit-file-name-handler-alist '((access-file . tramp-handle-access-file) (add-name-to-file . tramp-sudoedit-handle-add-name-to-file) (byte-compiler-base-file-name . ignore) (copy-directory . tramp-handle-copy-directory) (copy-file . tramp-sudoedit-handle-copy-file) (delete-directory . tramp-sudoedit-handle-delete-directory) (delete-file . tramp-sudoedit-handle-delete-file) (diff-latest-backup-file . ignore) (directory-files . tramp-handle-directory-files) (directory-files-and-attributes . tramp-handle-directory-files-and-attributes) (dired-compress-file . ignore) (dired-uncache . tramp-handle-dired-uncache) (exec-path . ignore) (expand-file-name . tramp-sudoedit-handle-expand-file-name) (file-accessible-directory-p . tramp-handle-file-accessible-directory-p) (file-acl . tramp-sudoedit-handle-file-acl) (file-attributes . tramp-sudoedit-handle-file-attributes) (file-directory-p . tramp-handle-file-directory-p) (file-equal-p . tramp-handle-file-equal-p) (file-executable-p . tramp-sudoedit-handle-file-executable-p) (file-exists-p . tramp-sudoedit-handle-file-exists-p) (file-in-directory-p . tramp-handle-file-in-directory-p) (file-local-copy . tramp-handle-file-local-copy) (file-modes . tramp-handle-file-modes) (file-name-all-completions . tramp-sudoedit-handle-file-name-all-completions) (file-name-as-directory . tramp-handle-file-name-as-directory) (file-name-case-insensitive-p . tramp-handle-file-name-case-insensitive-p) (file-name-completion . tramp-handle-file-name-completion) (file-name-directory . tramp-handle-file-name-directory) (file-name-nondirectory . tramp-handle-file-name-nondirectory) (file-newer-than-file-p . tramp-handle-file-newer-than-file-p) (file-notify-add-watch . ignore) (file-notify-rm-watch . ignore) (file-notify-valid-p . ignore) (file-ownership-preserved-p . ignore) (file-readable-p . tramp-sudoedit-handle-file-readable-p) (file-regular-p . tramp-handle-file-regular-p) (file-remote-p . tramp-handle-file-remote-p) (file-selinux-context . tramp-sudoedit-handle-file-selinux-context) (file-symlink-p . tramp-handle-file-symlink-p) (file-system-info . tramp-sudoedit-handle-file-system-info) (file-truename . tramp-sudoedit-handle-file-truename) (file-writable-p . tramp-sudoedit-handle-file-writable-p) (find-backup-file-name . tramp-handle-find-backup-file-name) (insert-directory . tramp-handle-insert-directory) (insert-file-contents . tramp-handle-insert-file-contents) (load . tramp-handle-load) (make-auto-save-file-name . tramp-handle-make-auto-save-file-name) (make-directory . tramp-sudoedit-handle-make-directory) (make-directory-internal . ignore) (make-nearby-temp-file . tramp-handle-make-nearby-temp-file) (make-process . ignore) (make-symbolic-link . tramp-sudoedit-handle-make-symbolic-link) (process-file . ignore) (rename-file . tramp-sudoedit-handle-rename-file) (set-file-acl . tramp-sudoedit-handle-set-file-acl) (set-file-modes . tramp-sudoedit-handle-set-file-modes) (set-file-selinux-context . tramp-sudoedit-handle-set-file-selinux-context) (set-file-times . tramp-sudoedit-handle-set-file-times) (set-visited-file-modtime . tramp-handle-set-visited-file-modtime) (shell-command . ignore) (start-file-process . ignore) (substitute-in-file-name . tramp-handle-substitute-in-file-name) (temporary-file-directory . tramp-handle-temporary-file-directory) (tramp-get-remote-gid . tramp-sudoedit-handle-get-remote-gid) (tramp-get-remote-uid . tramp-sudoedit-handle-get-remote-uid) (tramp-set-file-uid-gid . tramp-sudoedit-handle-set-file-uid-gid) (unhandled-file-name-directory . ignore) (vc-registered . ignore) (verify-visited-file-modtime . tramp-handle-verify-visited-file-modtime) (write-region . tramp-sudoedit-handle-write-region)) "\
Alist of handler functions for Tramp SUDOEDIT method.")

(defsubst tramp-sudoedit-file-name-p (filename) "\
Check if it's a FILENAME for SUDOEDIT." (and (tramp-tramp-file-p filename) (string= (tramp-file-name-method (tramp-dissect-file-name filename)) tramp-sudoedit-method)))

(autoload 'tramp-sudoedit-file-name-handler "tramp-sudoedit" "\
Invoke the SUDOEDIT handler for OPERATION and ARGS.
First arg specifies the OPERATION, second arg is a list of
arguments to pass to the OPERATION.

\(fn OPERATION &rest ARGS)" nil nil)

(tramp--with-startup (tramp-register-foreign-file-name-handler #'tramp-sudoedit-file-name-p #'tramp-sudoedit-file-name-handler))

;;;***

;;;### (autoloads nil "tramp-uu" "tramp-uu.el" (0 0 0 0))
;;; Generated autoloads from tramp-uu.el

(autoload 'tramp-uuencode-region "tramp-uu" "\
UU-encode the region between BEG and END.

\(fn BEG END)" nil nil)

;;;***

;;;### (autoloads nil "trampver" "trampver.el" (0 0 0 0))
;;; Generated autoloads from trampver.el

(defconst tramp-version "2.5.1" "\
This version of Tramp.")

(defconst tramp-bug-report-address "tramp-devel@gnu.org" "\
Email address to send bug reports to.")

;;;***

;;;### (autoloads nil nil ("tramp-compat.el" "tramp-fuse.el" "tramp-integration.el"
;;;;;;  "tramp.el") (0 0 0 0))

;;;***

(provide 'tramp-loaddefs)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; tramp-loaddefs.el ends here
