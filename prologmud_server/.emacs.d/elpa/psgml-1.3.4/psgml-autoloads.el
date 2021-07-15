;;; psgml-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "psgml" "psgml.el" (0 0 0 0))
;;; Generated autoloads from psgml.el

(autoload 'sgml-mode "psgml" "\
Major mode for editing SGML.
\\<sgml-mode-map>Makes > display the matching <.  Makes / display matching /.
Use \\[sgml-validate] to validate your document with an SGML parser.

You can find information with:
\\[sgml-show-context]  Show the nesting of elements at cursor position.
\\[sgml-list-valid-tags]  Show the tags valid at cursor position.

Insert tags with completion of contextually valid tags with \\[sgml-insert-tag].
End the current element with \\[sgml-insert-end-tag].  Insert an element (i.e.
both start and end tag) with \\[sgml-insert-element].  Or tag a region with
\\[sgml-tag-region].

To tag a region with the mouse, use transient mark mode or secondary selection.

Structure editing:
\\[sgml-backward-element]  Moves backwards over the previous element.
\\[sgml-forward-element]  Moves forward over the next element.
\\[sgml-down-element]  Move forward and down one level in the element structure.
\\[sgml-backward-up-element]  Move backward out of this element level.
\\[sgml-beginning-of-element]  Move to after the start tag of the current element.
\\[sgml-end-of-element]  Move to before the end tag of the current element.
\\[sgml-kill-element]  Kill the element following the cursor.

Finding interesting positions
\\[sgml-next-data-field]  Move forward to next point where data is allowed.
\\[sgml-next-trouble-spot]  Move forward to next point where something is
	amiss with the structure.

Folding and unfolding
\\[sgml-fold-element]  Fold the lines comprising the current element, leaving
	the first line visible.
\\[sgml-fold-subelement]  Fold the elements in the content of the current element.
	Leaving the first line of every element visible.
\\[sgml-unfold-line]  Show hidden lines in current line.

User options:

sgml-omittag  Set this to reflect OMITTAG in the SGML declaration.
sgml-shorttag  Set this to reflect SHORTTAG in the SGML declaration.
sgml-namecase-general  Set this to reflect NAMECASE GENERAL in the SGML declaration.
sgml-auto-insert-required-elements  If non-nil, automatically insert required
	elements in the content of an inserted element.
sgml-omittag-transparent  If non-nil, will show legal tags inside elements
	with omitable start tags and legal tags beyond omitable end tags.
sgml-leave-point-after-insert  If non-nil, the point will remain after
	inserted tag(s).
sgml-warn-about-undefined-elements  If non-nil, print a warning when a tag
	for a undefined element is found.
sgml-max-menu-size  Max number of entries in Tags and Entities menus before
 	they are split into several panes.
sgml-always-quote-attributes  If non-nil, quote all attribute values
	inserted after finishing edit attributes.
sgml-minimize-attributes  Determines minimization of attributes inserted by
	edit-attributes.
sgml-normalize-trims  If non-nil, sgml-normalize will trim off white space
	from end of element when adding end tag.
sgml-indent-step  How much to increment indent for every element level.
sgml-indent-data  If non-nil, indent in data/mixed context also.
sgml-set-face     If non-nil, psgml will set the face of parsed markup.
sgml-markup-faces The faces used when the above variable is non-nil.
sgml-public-map  Mapping from public identifiers to file names.
sgml-offer-save  If non-nil, ask about saving modified buffers before
		\\[sgml-validate] is run.

All bindings:
\\{sgml-mode-map}

\(fn)" t nil)

(autoload 'xml-mode "psgml" "\
Major mode for editing XML, specialized from SGML mode.
Sets various variables appropriately for XML.

Can be used without a DTD.  In that case, warnings about undefined
elements and entities are suppressed and various commands' behaviour
is modified to account for the lack of information.  For instance, the
element names offered for selection or completion are those in the
parse of the document, but other names may be entered.

Note that without a DTD, indenting lines will only work if
`sgml-indent-data' is non-nil.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "psgml" '("sgml-" "psgml-maintainer-address")))

;;;***

;;;### (autoloads nil "psgml-api" "psgml-api.el" (0 0 0 0))
;;; Generated autoloads from psgml-api.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "psgml-api" '("sgml-")))

;;;***

;;;### (autoloads nil "psgml-debug" "psgml-debug.el" (0 0 0 0))
;;; Generated autoloads from psgml-debug.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "psgml-debug" '("sgml-" "profile-sgml" "test-sgml")))

;;;***

;;;### (autoloads nil "psgml-dtd" "psgml-dtd.el" (0 0 0 0))
;;; Generated autoloads from psgml-dtd.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "psgml-dtd" '("sgml-")))

;;;***

;;;### (autoloads nil "psgml-edit" "psgml-edit.el" (0 0 0 0))
;;; Generated autoloads from psgml-edit.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "psgml-edit" '("sgml-" "psgml-edit--read-ids")))

;;;***

;;;### (autoloads nil "psgml-fs" "psgml-fs.el" (0 0 0 0))
;;; Generated autoloads from psgml-fs.el

(autoload 'style-format "psgml-fs" "\


\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "psgml-fs" '("fs-")))

;;;***

;;;### (autoloads nil "psgml-info" "psgml-info.el" (0 0 0 0))
;;; Generated autoloads from psgml-info.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "psgml-info" '("sgml-")))

;;;***

;;;### (autoloads nil "psgml-lucid" "psgml-lucid.el" (0 0 0 0))
;;; Generated autoloads from psgml-lucid.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "psgml-lucid" '("sgml-")))

;;;***

;;;### (autoloads nil "psgml-maint" "psgml-maint.el" (0 0 0 0))
;;; Generated autoloads from psgml-maint.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "psgml-maint" '("psgml-")))

;;;***

;;;### (autoloads nil "psgml-nofill" "psgml-nofill.el" (0 0 0 0))
;;; Generated autoloads from psgml-nofill.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "psgml-nofill" '("sgml-")))

;;;***

;;;### (autoloads nil "psgml-other" "psgml-other.el" (0 0 0 0))
;;; Generated autoloads from psgml-other.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "psgml-other" '("sgml-")))

;;;***

;;;### (autoloads nil "psgml-parse" "psgml-parse.el" (0 0 0 0))
;;; Generated autoloads from psgml-parse.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "psgml-parse" '("sgml-" "psgml-ids-seen" "xml-parser-syntax")))

;;;***

;;;### (autoloads nil "psgml-vars" "psgml-vars.el" (0 0 0 0))
;;; Generated autoloads from psgml-vars.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "psgml-vars" '("sgml-")))

;;;***

;;;### (autoloads nil "psgml-xpr" "psgml-xpr.el" (0 0 0 0))
;;; Generated autoloads from psgml-xpr.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "psgml-xpr" '("sgml-" "psgml-parse-jps-tag")))

;;;***

;;;### (autoloads nil nil ("psgml-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; psgml-autoloads.el ends here
