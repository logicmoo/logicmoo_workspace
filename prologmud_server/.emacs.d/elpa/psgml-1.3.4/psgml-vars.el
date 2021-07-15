;;; psgml-vars.el --- ???  -*- lexical-binding:t -*-

;; Copyright (C) 2016  Free Software Foundation, Inc.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'custom)

(defgroup psgml ()
  "SGML and XML editing with PSGML mode."
  :group 'languages)


(defcustom sgml-insert-missing-element-comment t
  "If true, and sgml-auto-insert-required-elements also true,
`sgml-insert-element' will insert a comment if there is an element required
but there is more than one to choose from."
  :type 'boolean
  :group 'psgml)

(defcustom sgml-insert-end-tag-on-new-line nil
  "If true, `sgml-insert-element' will put the end-tag on a new line
after the start-tag. Useful on slow terminals if you find the end-tag after
the cursor irritating."
  :type 'boolean
  :group 'psgml)

(defcustom sgml-doctype nil
  "*If set, this should be the name of a file that contains the doctype
declaration to use.
Setting this variable automatically makes it local to the current buffer."
  :tag "Doctype file"
  :type '(choice (const :tag "Off" nil)
                 file)
  :group 'psgml)

(make-variable-buffer-local 'sgml-doctype)

(defcustom sgml-system-identifiers-are-preferred nil
  "*If nil, PSGML will look up external entities by searching the catalogs
in `sgml-local-catalogs' and `sgml-catalog-files' and only if the
entity is not found in the catalogs will a given system identifer be
used. If the variable is non-nil and a system identifer is given, the
system identifier will be used for the entity. If no system identifier
is given the catalogs will searched."
  :type 'boolean
  :group 'psgml)

(defcustom sgml-range-indicator-max-length 9
  "*Maximum number of characters used from the first and last entry
of a submenu to indicate the range of items in that menu."
  :type 'integer
  :group 'psgml)

(defcustom sgml-default-doctype-name nil
  "*Document type name to use if no document type declaration is present."
  :type '(choice (const :tag "Off" nil) string)
  :group 'psgml)


(defcustom sgml-markup-faces
  ;; Fixme: are the font-lock correspondences here the most appopriate
  ;; ones?  I don't recall whence this set came.  -- fx
  `((start-tag . ,(if (facep 'font-lock-function-name-face)
		      'font-lock-function-name-face
		    'bold))
    (end-tag . ,(if (facep 'font-lock-function-name-face)
		    'font-lock-function-name-face
		  'bold))
    (comment . ,(if (facep 'font-lock-comment-face)
		    'font-lock-comment-face
		  'bold))
    (pi . ,(if (facep 'font-lock-type-face)
	       'font-lock-type-face
	     'bold))
    (sgml . ,(if (facep 'font-lock-type-face)
		 'font-lock-type-face
	       'bold))
    (doctype . ,(if (facep 'font-lock-keyword-face)
		    'font-lock-keyword-face
		  'bold))
    (entity . ,(if (facep 'font-lock-string-face)
		   'font-lock-string-face
		 'bold))
    (shortref . ,(if (facep 'font-lock-string-face)
		     'font-lock-string-face
		   'bold))
    (ignored . ,(if (facep 'font-lock-constant-face)
		    'font-lock-constant-face
		  'default))
    (ms-start . ,(if (facep 'font-lock-constant-face)
		    'font-lock-constant-face
		  'default))
    (ms-end . ,(if (facep 'font-lock-constant-face)
		    'font-lock-constant-face
		  'default)))
  "*Alist of markup to face mappings.
Element are of the form (MARKUP-TYPE . FACE).
Possible values for MARKUP-TYPE are:
comment	- comment declaration
doctype	- doctype declaration
end-tag
ignored	- ignored marked section
ms-end	- marked section start, if not ignored
ms-start- marked section end, if not ignored
pi	- processing instruction
sgml	- SGML declaration
start-tag
entity  - general entity reference
shortref- short reference"
  :type '(alist :key-type symbol
                :value-type face)
  :options '(start-tag end-tag comment pi sgml doctype entity shortref
                      ignored ms-start ms-end)
  :group 'psgml)


(defcustom sgml-set-face nil
  "*If non-nil, psgml will set the face of parsed markup."
  :type 'boolean
  :group 'psgml)
;(put 'sgml-set-face 'sgml-desc "Set face of parsed markup")

(defcustom sgml-auto-activate-dtd nil
  "*If non-nil, loading a sgml-file will automatically try to activate its DTD.
Activation means either to parse the document type declaration or to
load a previously saved parsed DTD.  The name of the activated DTD
will be shown in the mode line."
  :type 'boolean
  :group 'psgml)
;;(put 'sgml-auto-activate-dtd 'sgml-desc "Auto Activate DTD")

(defcustom sgml-offer-save t
  "*If non-nil, ask about saving modified buffers before \\[sgml-validate] is run."
  :type 'boolean
  :group 'psgml)

(defvar sgml-parent-document nil
  "*How to handle the current file as part of a bigger document.

The variable describes how the current file's content fit into the element
hierarchy.  The value should have the form

  (PARENT-FILE CONTEXT-ELEMENT* TOP-ELEMENT (HAS-SEEN-ELEMENT*)?)

PARENT-FILE	is a string, the name of the file containing the
		document entity.
CONTEXT-ELEMENT is a string, that is the name of an element type.
		It can occur 0 or more times and is used to set up
		exceptions and short reference map.  Good candidates
		for these elements are the elements open when the
		entity pointing to the current file is used.
TOP-ELEMENT	is a string that is the name of the element type
		of the top level element in the current file.  The file
		should contain one instance of this element, unless
		the last \(Lisp) element of `sgml-parent-document' is a
		list.  If it is a list, the top level of the file
		should follow the content model of top-element.
HAS-SEEN-ELEMENT is a string that is the name of an element type.  This
	        element is satisfied in the content model of top-element.

Setting this variable automatically makes it local to the current buffer.")
(make-variable-buffer-local 'sgml-parent-document)
(put 'sgml-parent-document 'sgml-type 'list)

(defcustom sgml-tag-region-if-active nil
  "*If non-nil, the Tags menu will tag a region if the region is 
considered active by emacs.  If nil, region must be active and
transient-mark-mode must be on for the region to be tagged."
  :type 'boolean
  :group 'psgml)

(defcustom sgml-normalize-trims t
  "*If non-nil, sgml-normalize will trim off white space from end of element
when adding end tag."
  :type 'boolean
  :group 'psgml)

(defcustom sgml-omittag t
  "*Set to non-nil, if you use OMITTAG YES.

Setting this variable automatically makes it local to the current buffer."
  :type 'boolean
  :group 'psgml)

(make-variable-buffer-local 'sgml-omittag)
;;(put 'sgml-omittag 'sgml-desc "OMITTAG")

(defcustom sgml-shorttag t
  "*Set to non-nil, if you use SHORTTAG YES.

Setting this variable automatically makes it local to the current buffer."
  :type 'boolean
  :group 'psgml)

(make-variable-buffer-local 'sgml-shorttag)
;(put 'sgml-shorttag 'sgml-desc "SHORTTAG")

(defcustom sgml-namecase-general t
  "*Set to non-nil, if you use NAMECASE GENERAL YES.

Setting this variable automatically makes it local to the current buffer."
  :type 'boolean
  :group 'psgml)

(make-variable-buffer-local 'sgml-namecase-general)
;(put 'sgml-namecase-general 'sgml-desc "NAMECASE GENERAL")


(defcustom sgml-general-insert-case 'lower
  "*The case that will be used for general names in inserted markup.
This can be the symbol `lower' or `upper'.  Only effective if
`sgml-namecase-general' is true."
  :type '(choice (const lower)
                 (const upper))
  :group 'psgml
  )
(put 'sgml-general-insert-case 'sgml-type '(lower upper))


(defcustom sgml-insert-defaulted-attributes nil
  "*Controls whether defaulted attributes (not #FIXED) are inserted explicitly
or not. nil means don't insert, t means insert."
  :type 'boolean
  :group 'psgml)


(defcustom sgml-minimize-attributes nil
  "*Determines minimization of attributes inserted by edit-attributes.
Actually two things are done
1. If non-nil, omit attribute name, if attribute value is from a token group.
2. If `max', omit attributes with default value.

Setting this variable automatically makes it local to the current buffer."
  :type '(choice (const :tag "No" nil)
                 (const :tag "omit attribute name" t)
                 (const :tag "omit attributes with default value" max))
  :group 'psgml)

(make-variable-buffer-local 'sgml-minimize-attributes)
(put 'sgml-minimize-attributes 'sgml-type
     '(("No" . nil) ("Yes" . t) ("Max" . max)))

(defcustom sgml-always-quote-attributes t
  "*Non-nil means quote all attribute values inserted after editing attributes.
Setting this variable automatically makes it local to the current buffer."
  :type 'boolean
  :group 'psgml)

(make-variable-buffer-local 'sgml-always-quote-attributes)

(defcustom sgml-auto-insert-required-elements t
  "*If non-nil, automatically insert required elements in the content
of an inserted element."
  :type 'boolean
  :group 'psgml)

(defvar sgml-balanced-tag-edit t
  "*If non-nil, context menu inserts start-end tag pairs.")

(defcustom sgml-omittag-transparent (not sgml-balanced-tag-edit)
  "*If non-nil, will show legal tags inside elements with omitable start tags
and legal tags beyond omitable end tags."
  :type 'boolean
  :group 'psgml)

(defcustom sgml-leave-point-after-insert nil
  "*If non-nil, the point will remain after inserted tag(s).
If nil, the point will be placed before the inserted tag(s)."
  :type 'boolean
  :group 'psgml)

(defcustom sgml-warn-about-undefined-elements t
  "*If non-nil, print a warning when a tag for an undefined element is found."
  :type 'boolean
  :group 'psgml)

(defcustom sgml-warn-about-undefined-entities t
  "*If non-nil, print a warning when an undefined entity is found."
  :type 'boolean
  :group 'psgml)

(defcustom sgml-ignore-undefined-elements nil
  "*If non-nil, recover from an undefined element by ignoring the tag.
If nil, recover from an undefined element by assuming it can occur any
where and has content model ANY."
  :type 'boolean
  :group 'psgml)

(defcustom sgml-recompile-out-of-date-cdtd 'ask
  "*If non-nil, out of date compiled DTDs will be automatically recompiled.
If the value is `ask', PSGML will ask before recompiling. A `nil'
value will cause PSGML to silently load an out of date compiled DTD.
A DTD that refers to undefined external entities is always out of
date, thus in such case it can be useful to set this variable to
`nil'."
  :type '(choice (const :tag "No" nil)
                 (const :tag "Yes" t)
                 (const :tag "Ask" ask))
  :group 'psgml)
(put 'sgml-recompile-out-of-date-cdtd 'sgml-type '(("No" . nil)
						   ("Yes" . t)
						   ("Ask" . ask)))

(defcustom sgml-trace-entity-lookup nil
  "*If non-nil, log messages about catalog files used to look for
external entities."
  :type 'boolean
  :group 'psgml) 

(defcustom sgml-indent-step 2
  "*How much to increment indent for every element level.
If nil, no indentation.
Setting this variable automatically makes it local to the current buffer."
  :type '(choice (const :tag "None" nil)
                 integer)
  :group 'psgml)
(make-variable-buffer-local 'sgml-indent-step)
(put 'sgml-indent-step 'sgml-type '(("None" . nil) 0 1 2 3 4 5 6 7 8))

(defcustom sgml-indent-data nil
  "*If non-nil, indent in data/mixed context also.
Setting this variable automatically makes it local to the current buffer."
  :type 'boolean
  :group 'psgml)
(make-variable-buffer-local 'sgml-indent-data)


(defcustom sgml-exposed-tags '()
  "*The list of tag names that remain visible, despite \\[sgml-hide-tags].
Each name is a lowercase string, and start-tags and end-tags must be
listed individually.

`sgml-exposed-tags' is local to each buffer in which it has been set;
use `setq-default' to set it to a value that is shared among buffers."
  :type '(repeat string)
  :group 'psgml)
(make-variable-buffer-local 'sgml-exposed-tags)
(put 'sgml-exposed-tags 'sgml-type 'list)



(defcustom sgml-custom-dtd nil
  "Menu entries to be added to the DTD menu.
The value should be a list of entries to be added to the DTD menu.
Every entry should be a list.  The first element of the entry is a string
used as the menu entry.  The second element is a string containing a
doctype declaration (this can be nil if no doctype).  The rest of the
list should be a list of variables and values.  For backward
compatibility a single string instead of a variable is assigned to
`sgml-default-dtd-file'.  All variables are made buffer local and are also
added to the buffers local variables list.

Example:
   ((\"HTML\" nil
     sgml-default-dtd-file \"~/sgml/html.ced\"
     sgml-omittag nil sgml-shorttag nil)
    (\"HTML+\" \"<!doctype htmlplus system 'htmlplus.dtd'>\"
     \"~/sgml/htmlplus.ced\"
     sgml-omittag t sgml-shorttag nil)
    (\"DOCBOOK\" \"<!doctype docbook system 'docbook.dtd'>\"
     \"~/sgml/docbook.ced\"
     sgml-omittag nil sgml-shorttag t)))
"
  :group 'psgml
  :type '(repeat (list (string :tag "Menu entry")
                  (string :tag "Doctype")
                  (plist :tag "Options" :inline t)))
)


(defcustom sgml-custom-markup nil
  "*Menu entries to be added to the Markup menu.
The value should be a list of lists of two strings.  The first
string is the menu line and the second string is the text inserted
when the menu item is chosen.  The second string can contain a \\r
where the cursor should be left.  Also if a selection is made
according the same rules as for the Tags menu, the selection is
replaced with the second string and \\r is replaced with the
selection.

Example:

  ((\"Version1\" \"<![%Version1[\\r]]>\")
   (\"New page\"  \"<?NewPage>\"))
"
  :group 'psgml
  :type '(repeat (list (string :tag "Menu entry")
                  (choice string sexp)))
)



;;;; Hooks and functions


(defcustom sgml-content-indent-function
  'sgml-indent-according-to-level
  "*Function used to compute indentation level for element content.
Function will be called with one argument, the element.
Should return an integer."
  :group 'psgml
  :type '(choice
          (const :tag "Indented according to nesting level"
                 sgml-indent-according-to-level)
          (const :tag "Indented under start tag"
                 sgml-indent-according-to-stag)
          (const :tag "Indented under start tag after name"
                 sgml-indent-according-to-stag-end)
          (function)))


(defcustom sgml-attribute-indent-function
  'sgml-indent-according-to-stag
  "*Function used to compute indetation level for attributes.
Function will be called with one argument, the element.
Should return an integer."
  :group 'psgml
  :type '(choice
          (const :tag "Indented according to nesting level"
                 sgml-indent-according-to-level)
          (const :tag "Indented under start tag"
                 sgml-indent-according-to-stag)
          (const :tag "Indented under start tag after name"
                 sgml-indent-according-to-stag-end)
          (function)))




(provide 'psgml-vars)
;;; psgml-vars.el ends here
