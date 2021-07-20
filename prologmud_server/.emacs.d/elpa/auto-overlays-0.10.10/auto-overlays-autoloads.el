;;; auto-overlays-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "auto-overlay-flat" "auto-overlay-flat.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from auto-overlay-flat.el

(register-definition-prefixes "auto-overlay-flat" '("auto-o-"))

;;;***

;;;### (autoloads nil "auto-overlay-line" "auto-overlay-line.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from auto-overlay-line.el

(register-definition-prefixes "auto-overlay-line" '("auto-o-"))

;;;***

;;;### (autoloads nil "auto-overlay-nested" "auto-overlay-nested.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from auto-overlay-nested.el

(register-definition-prefixes "auto-overlay-nested" '("auto-o-"))

;;;***

;;;### (autoloads nil "auto-overlay-self" "auto-overlay-self.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from auto-overlay-self.el

(register-definition-prefixes "auto-overlay-self" '("auto-o-"))

;;;***

;;;### (autoloads nil "auto-overlay-word" "auto-overlay-word.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from auto-overlay-word.el

(register-definition-prefixes "auto-overlay-word" '("auto-o-parse-word-match"))

;;;***

;;;### (autoloads nil "auto-overlays" "auto-overlays.el" (0 0 0 0))
;;; Generated autoloads from auto-overlays.el

(autoload 'auto-overlays-in "auto-overlays" "\
Return auto overlays overlapping region between START and END.

If keyword argument :within is non-nil, only overlays entirely
within START and END are returned.

If keyword argument :inactive is non-nil, both active and
inactive overlays are returned (usually inactive ones are
ignored).

If keyword argument :all-overlays is non-nil, all overlays are
returned, not just auto-overlays.

Any remaining arguments (which *must* come after any keyword
arguments) specify property tests, each of which should be a list
with one of the following forms:

  PROPERTY

  (FUNCTION PROPERTY)

  (FUNCTION PROPERTY VALUE)

  (FUNCTION (PROPERTY1 PROPERTY2 ...) (VALUE1 VALUE2 ...))

where PROPERTY indicates an overlay property name (a symbol), and
VALUE indicates an arbitrary value or lisp expression.

For each overlay between START and END, first the values
corresponding to the property names are retrieved from the
overlay, then FUNCTION is called with the properties values
followed by the other values as its arguments. The test is
satisfied if the result is non-nil, otherwise it fails.

A PROPERTY symbol on its own tests whether that property has a
non-null value, equivalent to (identity PROPERTY).

Tests are evaluated in order, but only up to the first failure.
Only overlays that satisfy all property tests are returned.

\(fn START END &rest PROP-TESTS &key WITHIN INACTIVE ALL-OVERLAYS &allow-other-keys)" nil nil)

(autoload 'auto-overlays-at-point "auto-overlays" "\
Return overlays overlapping POINT, defaulting to the point.

If keyword argument :inactive is non-nil, both active and
inactive overlays are returned (usually inactive ones are
ignored).

If keyword argument :all-overlays is non-nil, all overlays are
returned, not just auto-overlays.

Any remaining arguments (which *must* come after any keyword
arguments) specify property tests, each of which should be a list
with one of the following forms:

  (FUNCTION PROPERTY)

  (FUNCTION PROPERTY VALUE)

  (FUNCTION (PROPERTY1 PROPERTY2 ...) (VALUE1 VALUE2 ...))

where PROPERTY indicates an overlay property name (a symbol), and
VALUE indicates an arbitrary value or lisp expression.

For each overlay between START and END, first the values
corresponding to the property names are retrieved from the
overlay, then FUNCTION is called with the properties values
followed by the other values as its arguments. The test is
satisfied if the result is non-nil, otherwise it fails. Tests are
evaluated in order, but only up to the first failure. Only
overlays that satisfy all property tests are returned.

\(fn &optional POINT &rest PROP-TESTS &key INACTIVE ALL-OVERLAYS &allow-other-keys)" nil nil)

(autoload 'auto-overlay-highest-priority-at-point "auto-overlays" "\
Return highest priority overlay at POINT, defaulting to the point.

If two overlays have the same priority, the innermost one takes
precedence (i.e. the one that begins later, or if they begin at
the same point the one that ends earlier).

The remaining arguments are as for `auto-overlays-at' (which see).

\(fn &optional POINT &rest PROP-TESTS &key INACTIVE ALL-OVERLAYS &allow-other-keys)" nil nil)

(autoload 'auto-overlay-local-binding "auto-overlays" "\
Return \"overlay local \" binding of SYMBOL at POINT,
or the current local binding if there is no overlay binding. If
there is no overlay binding and SYMBOL is not bound, return
nil. POINT defaults to the point.

If ONLY-OVERLAY is non-nil, only overlay bindings are
returned. If none exists at POINT, nil is returned

An \"overlay local\" binding is created by giving an overlay a
non-nil value for a property named SYMBOL. If more than one
overlay at POINT has a non-nil SYMBOL property, the value from
the highest priority overlay is returned.

See `auto-overlay-highest-priority-at-point' for a definition of
\"highest priority\".

\(fn SYMBOL &optional POINT ONLY-OVERLAY)" nil nil)

(autoload 'auto-overlay-load-set "auto-overlays" "\
Load the set of auto-overlay DEFINITIONS
into the set identified by SET-ID the current buffer.

DEFINITIONS should be a list of the form:

  (DEFINITION1 DEFINITION2 ... )

The DEFINITION's should be lists of the form:

  (CLASS @optional :id DEFINITION-ID @rest REGEXP1 REGEXP2 ... )

CLASS is a symbol specifying the auto-overlay class. The standard
classes are 'word, 'line, 'self, 'flat and 'nested. The :id
property is optional. It should be a symbol that can be used to
uniquely identify DEFINITION (see
`auto-overlay-unload-definition').

The REGEXP's should be lists of the form:

  (RGXP &optional :edge EDGE :id REGEXP-ID
        &rest PROPERTY1 PROPERTY2 ... )

RGXP is either a single regular expression (a string), or a cons
cell of the form (RGXP . GROUP) where RGXP is a regular
expression and GROUP is an integer specifying which group in the
regular expression forms the delimiter for the auto-overlay. The
rest of the PROPERTY entries should be cons cells of the
form (NAME . VALUE) where NAME is an overlay property name (a
symbol) and VALUE is its value.

The :edge and :id properties are optional. EDGE should be one of
the symbols 'start or 'end. If it is not specified, :edge is
assumed to be 'start. ID property is a symbol that can be used to
uniquely identify REGEXP (see `auto-overlay-unload-regexp').

\(fn SET-ID DEFINITIONS &optional NOPARSE)" nil nil)

(autoload 'auto-overlay-load-definition "auto-overlays" "\
Load DEFINITION into the set of auto-overlay definitions SET-ID
in the current buffer. If SET-ID does not exist, it is created.

If POS is nil, DEFINITION is added at the end of the list of
auto-overlay definitions. If it is t, it is added at the
beginning. If it is an integer, it is added at that position in
the list. The position in the list makes no difference to the
behaviour of the auto-overlays. But it can make a difference to
the speed and efficiency. In general, higher-priority and
exclusive DEFINITIONS should appear earlier in the list.

Returns a unique id for the loaded definition, which can be used
to unload it later using `auto-overlay-unload-definition' (which
see).


DEFINITION should be a list of the form:

  (CLASS @optional :id DEFINITION-ID @rest REGEXP1 REGEXP2 ... )

CLASS is a symbol specifying the auto-overlay class. The standard
classes are `word', `line', `self', `flat' and `nested'. The :id
property is optional. It should be a symbol that uniquely
identifies the DEFINITION within SET-ID (see
`auto-overlay-unload-definition').

REGEXP should be a list of the form:

  (RGXP &optional :edge EDGE :id REGEXP-ID
        &rest PROPERTY1 PROPERTY2 ... )

RGXP is either a single regular expression (a string), or a cons
cell of the form (RGXP . GROUP) where RGXP is a regular
expression and GROUP is an integer specifying which group in the
regular expression forms the delimiter for the auto-overlay. The
rest of the PROPERTY entries should be cons cells of the
form (NAME . VALUE) where NAME is an overlay property name (a
symbol) and VALUE is its value.

The EDGE and ID properties are optional. EDGE should be one of
the symbols `start' or `end'. If it is not specified, :edge is
assumed to be `start'. ID should be a symbol that uniquely
identifies REGEXP within DEFINITION (see
`auto-overlay-unload-regexp').

\(fn SET-ID DEFINITION &optional POS NOPARSE)" nil nil)

(autoload 'auto-overlay-load-regexp "auto-overlays" "\
Load REGEXP into the auto-overlay definition identified by
DEFINITION-ID in the regexp list named SET-ID in the current
buffer.

If POS is nil, REGEXP is added at the end of the definition. If
it is t, it is added at the beginning. If it is an integer, it is
added at that position.


REGEXP should be a list of the form:

  (RGXP &optional :edge EDGE :id REGEXP-ID
        &rest PROPERTY1 PROPERTY2 ... )

RGXP is either a single regular expression (a string), or a cons
cell of the form (RGXP . GROUP) where RGXP is a regular
expression and GROUP is an integer specifying which group in the
regular expression forms the delimiter for the auto-overlay. The
rest of the PROPERTY entries should be cons cells of the
form (NAME . VALUE) where NAME is an overlay property name (a
symbol) and VALUE is its value.

The :edge and :id properties are optional. EDGE should be one of
the symbols `start' or `end'. If it is not specified, :edge is
assumed to be `start'. ID property is a symbol that can be used to
uniquely identify REGEXP (see `auto-overlay-unload-regexp').

\(fn SET-ID DEFINITION-ID REGEXP &optional POS NOPARSE)" nil nil)

(autoload 'auto-overlay-share-regexp-set "auto-overlays" "\
Make TO-BUFFER share the regexp set identified by SET-ID with FROM-BUFFER.
Any changes to that regexp set in either buffer will be reflected in the
other. TO-BUFFER defaults to the current buffer.

\(fn SET-ID FROM-BUFFER &optional TO-BUFFER)" nil nil)

(autoload 'auto-overlay-load-overlays "auto-overlays" "\
Load overlays for BUFFER from FILE.
Returns t if successful, nil otherwise.
Defaults to the current buffer.

If FILE is null, or is a string that only specifies a directory,
the filename is constructed from the buffer's file name and
SET-ID. If the buffer is not associated with a file and FILE
doesn't specify a full filename, an error occurs.

The FILE should be generated by `auto-overlay-save-overlays'. By
default, the buffer contents and regexp definitions for SET-ID
will be checked to make sure neither have changed since the
overlays were saved. If they don't match, the saved overlay data
will not be loaded, and the function will return nil.

If NO-REGEXP-CHECK is non-nil, the check for matching regexp
definitions will be skipped; the saved overlays will be loaded
even if different regexp definitions were active when the
overlays were saved.

\(fn SET-ID &optional BUFFER FILE NO-REGEXP-CHECK)" nil nil)

(register-definition-prefixes "auto-overlays" '("auto-o"))

;;;***

;;;### (autoloads nil nil ("auto-overlays-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; auto-overlays-autoloads.el ends here
