;;; webfeeder-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "webfeeder" "webfeeder.el" (0 0 0 0))
;;; Generated autoloads from webfeeder.el

(autoload 'webfeeder-html-files-to-items "webfeeder" "\
Parse the source HTML-FILES and return a list of webfeeder-items.
PROJECT-DIR is where HTML files are also assumed to reside.
PROJECT-DIR is the local root of the website hosted at URL.
HTML parsing details can be customized through the following
variables:
- `webfeeder-author-function'
- `webfeeder-date-function'
- `webfeeder-title-function'
- `webfeeder-subtitle-function'
- `webfeeder-body-function'.
- `webfeeder-categories-function'.
- `webfeeder-generator-function'.

\(fn PROJECT-DIR URL HTML-FILES)" nil nil)

(autoload 'webfeeder-make-rss "webfeeder" "\
Return a webfeed string in RSS format that contains every FEED-ITEMS.

WEBFEED is the path where the feed is intended to be stored,
relative to URL.

A feed can have a TITLE and DESCRIPTION: if not, the URL will be
used.

When BUILD-DATE is nil, use `current-time'.  Otherwise it can be
a time expression as in `current-time'.  0 means EPOCH.

When provided, FEED-ITEMS are only added if (PREDICATE FEED-ITEM)
returns non-nil.

The number of items is limited to MAX-ENTRIES.  When nil, all
items are included.

FEED-ITEMS can be generated with `webfeeder-html-files-to-items'.

\(fn WEBFEED URL FEED-ITEMS &key TITLE DESCRIPTION AUTHOR GENERATOR BUILD-DATE PREDICATE MAX-ENTRIES &allow-other-keys)" nil nil)

(autoload 'webfeeder-make-atom "webfeeder" "\
Return a webfeed string in Atom format that contains every FEED-ITEMS.

WEBFEED is the path where the feed is intended to be stored,
relative to URL.

A feed can have a TITLE: if not, the URL will be used.

When BUILD-DATE is nil, use `current-time'.  Otherwise it can be
a time expression as in `current-time'.  0 means EPOCH.

When provided, FEED-ITEMS are only added if (PREDICATE FEED-ITEM)
returns non-nil.

The number of items is limited to MAX-ENTRIES.  When nil, all
items are included.

FEED-ITEMS can be generated with `webfeeder-html-files-to-items'.

\(fn WEBFEED URL FEED-ITEMS &key TITLE SUBTITLE AUTHOR GENERATOR BUILD-DATE PREDICATE MAX-ENTRIES &allow-other-keys)" nil nil)

(autoload 'webfeeder-build "webfeeder" "\
Build a WEBFEED out of HTML-FILES.
The WEBFEED is an XML file that contains every blog post in a
machine-readable format.

The WEBFEED file is stored in PROJECT-DIR where HTML-FILES are
also assumed to reside.

For more details on the other arguments, see
`webfeeder-make-atom', `webfeeder-make-rss' and
`webfeeder-html-files-to-items'.

\(fn WEBFEED PROJECT-DIR URL HTML-FILES &key TITLE SUBTITLE DESCRIPTION AUTHOR GENERATOR BUILD-DATE PREDICATE MAX-ENTRIES (BUILDER \\='webfeeder-make-atom))" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "webfeeder" '("webfeeder-")))

;;;***

;;;### (autoloads nil "webfeeder-test" "webfeeder-test.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from webfeeder-test.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "webfeeder-test" '("webfeeder-")))

;;;***

;;;### (autoloads nil "webfeeder-test-gen" "webfeeder-test-gen.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from webfeeder-test-gen.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "webfeeder-test-gen" '("with-directory-excursion" "webfeeder-test-")))

;;;***

;;;### (autoloads nil nil ("webfeeder-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; webfeeder-autoloads.el ends here
