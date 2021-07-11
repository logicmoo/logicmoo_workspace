;;; docbook.el --- Info-like viewer for DocBook  -*- lexical-binding: t -*-

;; Copyright (C) 2013  Free Software Foundation, Inc.

;; Author: Chong Yidong <cyd@gnu.org>
;; Keywords: docs, help
;; Version: 0.1

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; An Info-like viewer for DocBook manuals.
;;
;; Entry point: M-x docbook-find-file

;;; TODO:

;; table
;; informaltable
;; graphics
;;
;; funcsynopsis
;; classsynopsis
;; citerefentry
;;
;; see, primaryie, secondaryie

;;; Code:

(require 'xml)

(defgroup docbook nil
  "The Emacs DocBook reader."
  :group 'help
  :group 'docs)

(defface docbook-xref
  '((t :inherit button))
  "Face for DocBook cross references."
  :group 'docbook)

(defface docbook-warning
  '((t :inherit font-lock-warning-face))
  "Face for warning text in DocBook documents."
  :group 'docbook)

(defface docbook-emphasis
  '((t :slant italic))
  "Face for emphasized text in DocBook documents."
  :group 'docbook)

(defface docbook-literal
  '((t :inherit (font-lock-constant-face fixed-pitch)))
  "Face for DocBook text marked as being literal."
  :group 'docbook)

(defface docbook-computer
  '((t :inherit (font-lock-type-face fixed-pitch)))
  "Face for DocBook text marked as computer output."
  :group 'docbook)

(defface docbook-computer-term
  '((t :inherit (font-lock-keyword-face fixed-pitch)))
  "Face for DocBook text marked as computer terminology."
  :group 'docbook)

(defface docbook-replaceable
  '((t :inherit (font-lock-string-face bold)))
  "Face for DocBook text marked as replaceable."
  :group 'docbook)

(defface docbook-citation
  '((t :slant italic))
  "Face for DocBook text marked as non-xref citations."
  :group 'docbook)

(defface docbook-label
  '((t :weight bold :underline t))
  "Face for DocBook text marked as labels for Q&A entries,"
  :group 'docbook)

(defface docbook-small '((t :height 0.8))
  "Face for DocBook text marked as small."
  :group 'docbook)

(defface docbook-chapter-title
  '((((type tty pc) (class color) (background light))
     :foreground "green" :weight bold :underline t)
    (((type tty pc) (class color) (background dark))
     :foreground "yellow" :weight bold :underline t)
    (t :height 1.5 :inherit docbook-section-title))
  "Face for DocBook chapter titles."
  :group 'docbook)

(defface docbook-section-title
  '((((type tty pc) (class color))
     :foreground "lightblue" :weight bold :underline t)
    (t :height 1.2 :inherit docbook-subsection-title))
  "Face for DocBook section titles."
  :group 'docbook)

(defface docbook-subsection-title
  '((t :weight bold :height 1.1 :inherit variable-pitch))
  "Face for DocBook subsection titles."
  :group 'docbook)

(defface docbook-misc-title '((t :weight bold :underline t))
  "Face for miscellaneous DocBook titles."
  :group 'docbook)

(defvar docbook-title-markup-alist
  '((book    . docbook-chapter-title)
    (chapter . docbook-chapter-title)
    (sect1   . docbook-section-title)
    (sect2   . docbook-subsection-title)
    (sect3   . docbook-subsection-title)
    (sect4   . docbook-subsection-title)
    (sect5   . docbook-subsection-title)
    (section . docbook-section-title)
    (simplesect . docbook-section-title))
  "Alist mapping DocBook section types to title faces")

(defvar docbook-text-markup-alist
  '((emphasis . docbook-emphasis)
    (foreignphrase . docbook-emphasis)
    (firstterm . docbook-emphasis)
    (bridgehead . docbook-section-title)
    (refname . docbook-section-title)
    (refpurpose . docbook-emphasis)
    (citetitle . docbook-citation)
    (subscript . docbook-small)
    (superscript . docbook-small)
    (replaceable . docbook-replaceable)
    ;; Computer output
    (accel . docbook-computer)
    (computeroutput . docbook-computer)
    (guibutton . docbook-computer)
    (guiicon . docbook-computer)
    (guilabel . docbook-computer)
    (guimenu . docbook-computer)
    (guimenuitem . docbook-computer)
    (guisubmenu . docbook-computer)
    (keycap . docbook-computer)
    (keycode . docbook-computer)
    (keycombo . docbook-computer)
    (keysym . docbook-computer)
    (markup . docbook-computer)
    (menuchoice . docbook-computer)
    (mousebutton . docbook-computer)
    (msgset . docbook-computer)
    (prompt . docbook-computer)
    (shortcut . docbook-computer)
    (tag . docbook-computer)
    (userinput . docbook-computer)
    ;; Computer terminology
    (application . docbook-computer-term)
    (classname . docbook-computer-term)
    (command . docbook-computer-term)
    (constant . docbook-computer-term)
    (database . docbook-computer-term)
    (envar . docbook-computer-term)
    (errorcode . docbook-computer-term)
    (errorname . docbook-computer-term)
    (errortype . docbook-computer-term)
    (filename . docbook-computer-term)
    (function . docbook-computer-term)
    (hardware . docbook-computer-term)
    (option . docbook-computer-term)
    (optional . docbook-computer-term)
    (parameter . docbook-computer-term)
    (property . docbook-computer-term)
    (returnvalue . docbook-computer-term)
    (symbol . docbook-computer-term)
    (systemitem . docbook-computer-term)
    (token . docbook-computer-term)
    (type . docbook-computer-term)
    (varname . docbook-computer-term)
    ;; docbook-literal
    (literal . docbook-literal)
    ;; Admonitions
    (caution . docbook-warning)
    (important . docbook-emphasis)
    (tip . docbook-emphasis)
    (warning . docbook-warning))
  "Alist mapping DocBook element types to markup faces.")

(defvar docbook-page-types
  '(acknowledgements appendix article bibliography book chapter colophon
    dedication glossary part preface sect1 sect2 sect3 sect4 sect5
    section set setindex toc)
  "List of DocBook sectioning element types.
DocBook mode shows one section at a time, as a single page.")

(defvar docbook-block-types
  '(para simpara formalpara equation informalequation
    informalexample figure informalfigure
    blockquote epigraph msgset sidebar
    bridgehead caution important note tip warning
    cmdsynopsis)
  "List of DocBook block types which require no additional processing.")

(defvar docbook-list-types
  '(calloutlist bibliolist glosslist itemizedlist orderedlist
    segmentedlist simplelist variablelist qandaset
    task procedure substeps)
  "List of DocBook block-level list types")

(defvar docbook-literal-block-types
  '(address literallayout programlisting screen screenco
    screenshot synopsis)
  "List of DocBook block element types which preserve whitespace.")

(defvar docbook-suppressed-types
  '(comment info bookinfo chapterinfo sectioninfo articleinfo label
    refmeta refclass)
  "List of DocBook element types which are not printed.")

(defvar docbook-index-separator-column 30
  "Column number of xrefs printed by `docbook--print-index'.")

(defvar docbook-entity-alist
  ;; makeinfo emits these entities, even though the DocBook spec does
  ;; not appear to define them.
  '(("lsquo" . "`")
    ("rsquo" . "'")
    ("ldquo" . "\"")
    ("rdquo" . "\"")
    ("copy" . "(C)")
    ("tex" . "TeX")
    ("latex" . "LaTeX")
    ("hellip" . "...")
    ("period" . ".")
    ("minus" . "-")
    ("colon" . ":")
    ("mdash" . "--")
    ("ndash" . "-"))
  "Alist mapping XML entities to their replacement text.
These elements are added to `xml-entity-alist' while parsing
DocBook documents.")

;;; Buffer setup

(defvar docbook--parse-tree nil
  "Parse tree of the current DocBook document.")

(defvar docbook--id-table nil
  "Hash table mapping DocBook IDs (symbols) to node contents.
Each key should be a Lisp symbol.  Each XML node with an XML ID
is keyed by an interned Lisp symbol with a matching symbol name.
Sectioning (page) nodes which lack their own XML IDs are keyed
using uninterned Lisp symbols created when parsing the XML tree.

Each hash table value has one of these two forms:

 (NODE TITLE-NODE PARENT-ID PREV NEXT SUBSECTIONS)
 (NODE TITLE-NODE PARENT-ID)

The first represents a node corresponding to a DocBook section,
which is displayed as a separate page in the DocBook reader.
The second represents a node which does not correspond to a
DocBook section, e.g. a position within a section for a
cross-reference to jump to.

NODE is the Lisp list tree corresponding to the XML node.
TITLE-NODE is the node corresponding to the node's title (a
string), or nil.
PARENT-ID is the ID of the node's parent page, or nil.
PREV and NEXT are the IDs of the previous and next page.
SUBSECTIONS is a list of IDs of child pages.")

(defvar docbook-id-markers-alist nil
  "Alist mapping DocBook node IDs to markers.
Each key should be a Lisp symbol, but it is not required to be
one of the keys in `docbook--id-table'.  This alist is used to
record the positions of xref'ed elements on the current page.")

(defvar docbook-top-page nil
  "ID of the topmost (root) page in the current DocBook document.
The value should be one of the keys in `docbook--id-table'.")

(defvar docbook-current-page nil
  "ID of the current DocBook page.
The value should be one of the keys in `docbook--id-table'.")

(defvar docbook--last-page-registered)
(defvar docbook--last-page-id-registered)
(defvar docbook--footnotes)
(defvar docbook--indent-level 0)
(defvar docbook--list-context nil)

(defvar docbook--index-alist nil
  "Alist mapping index types to index data.
Each list element has the form (TYPE . ALIST), where TYPE is a
symbol specifying the index type (nil for the default index) and
ALIST is an alist (TERM . ID-LIST).")

(defvar docbook-history nil
  "List of DocBook node IDs which were previously viewed.")

(defvar docbook-history-forward nil
  "List of DocBook node IDs visited with `docbook-history-back'.")

;; Used in place of the interned version of the string "nil".
(defconst docbook--nil (make-symbol "nil"))

(defun docbook-setup (parse-tree)
  "Set up a DocBook buffer using the XML parse tree PARSE-TREE.
PARSE-TREE should be a list of the sort returned by
`xml-parse-file' or `xml-parse-buffer'."
  (docbook-mode)
  (setq docbook--parse-tree parse-tree
	docbook--id-table (make-hash-table :test 'eq)
	docbook--index-alist nil
	docbook-history nil
	docbook-history-forward nil)
  (let ((docbook--last-page-registered nil)
	(docbook--last-page-id-registered nil))
    (docbook-register-node parse-tree nil nil))
  ;; Sort indices
  (dolist (index docbook--index-alist)
    (setcdr index (sort (cdr index) (lambda (a b)
				      (string< (car a) (car b)))))
    (dolist (entry (cdr index))
      (setcdr entry (nreverse (cdr entry))))))

(defun docbook-register-node (node parent-page-id parent-node-id)
  "Register NODE.
NODE should be a cons cell---a subnode of the tree returned by
`xml-parse-file'.  PARENT is the registered node ID of the parent
page (a symbol).  PARENT-NODE-ID is the registered node ID of the
node's immediate parent (which may or may not correspond to a
page node), or nil if the parent has no ID.

If NODE is a page node, return its registered node ID (a symbol).
Otherwise, return nil."
  (let ((type (xml-node-name node)))
    (cond
     ((eq type 'comment))
     ((eq type 'indexterm)
      (docbook--register-indexterm node parent-page-id))
     ((memq type docbook-page-types)
      (docbook--register-page-node node parent-page-id))
     (t
      (docbook--register-nonpage-node node parent-page-id
				      parent-node-id)))))

(defun docbook--register-indexterm (node parent-id)
  (let ((id (docbook--attr 'id node)))
    (if id (puthash id `(,node nil ,parent-id) docbook--id-table))
    ;; HACK: Modify the XML tree to add an indexterm id (a symbol).
    (setq id (make-symbol "indexterm"))
    (setcar (cdr node) (cons (cons 'docbook-indexterm-id id)
			     (xml-node-attributes node)))
    (puthash id `(,node nil ,parent-id) docbook--id-table)
    (let* ((type (docbook--attr 'type node))
	   (index (assq type docbook--index-alist)))
      ;; If there is no index of the indicated type yet, add it.
      (unless index
	(setq docbook--index-alist
	      (cons (setq index (cons type nil))
		    docbook--index-alist)))
      (dolist (subnode (xml-node-children node))
	(cond
	 ((not (consp subnode)))
	 ((memq (xml-node-name subnode) '(primary secondary tertiary))
	  (let* ((term (docbook--node-text subnode))
		 (entry (assoc term (cdr index))))
	    (if entry
		(setcdr entry (cons id (cdr entry)))
	      (setcdr index (cons (list term id) (cdr index))))))))
      nil)))

(defun docbook--register-page-node (node parent-id)
  (let ((id (docbook--attr 'id node)))
    ;; If there is no ID, generate an uninterned symbol as the ID.
    (unless id
      (setq id (make-symbol "Unnamed section")))
    (unless parent-id
      (setq docbook-top-page id))
    ;; Make the node record and update the NEXT record of the last node
    ;; processed.  This must be done before descending into the tree.
    (if docbook--last-page-registered
	(setcar (nthcdr 4 docbook--last-page-registered) id))
    (let ((record (list node nil parent-id
			docbook--last-page-id-registered nil nil)))
      (setq docbook--last-page-registered record
	    docbook--last-page-id-registered id)
      ;; Add the entry for this page node into the hash table.
      (if id (puthash id record docbook--id-table))
      ;; Descend into the children, registering them.
      (let ((subnodes
	     (mapcar (lambda (subnode)
		       (when (consp subnode)
			 (docbook-register-node subnode id id)))
		     (xml-node-children node))))
	;; If this is a section node, update its record with the IDs of
	;; the subsections, then return the ID of this node.
	(setcar (nthcdr 5 record) (delq nil subnodes))))
    id))

(defun docbook--register-nonpage-node (node parent-page-id parent-node-id)
  (let ((id (docbook--attr 'id node)))
    ;; If this is a title node, register it in the parent node.
    (when (and (eq (xml-node-name node) 'title) parent-node-id)
      (let ((parent-record (docbook--node-record parent-node-id)))
	(if parent-record (setcar (cdr parent-record) node))))
    ;; Construct the node record.
    (if id (puthash id `(,node nil ,parent-page-id) docbook--id-table))
    ;; Descend into the children, registering them.
    (dolist (subnode (xml-node-children node))
      (when (consp subnode)
	(docbook-register-node subnode parent-page-id id)))
    nil))

;;; Utility functions

(defsubst docbook--node-record (&optional node-id)
  "Return the record keyed by NODE-ID in `docbook--id-table'.
If NODE-ID is nil, it defaults to ID of the current page."
  (gethash (or node-id docbook-current-page) docbook--id-table))

(defsubst docbook-add-fragment-link (id)
  "If ID is non-nil, add a marker for it to `docbook-id-markers-alist'."
  (if id (push (cons id (point-marker)) docbook-id-markers-alist)))

(defun docbook--attr (attribute node)
  "Return the value of attribute ATTRIBUTE in xml node NODE.
The value is automatically converted to a Lisp symbol.  If the
node lacks the specified attribute, return nil."
  (let ((str (cdr (assq attribute (xml-node-attributes node)))))
    (and (stringp str)
  	 (not (equal str ""))
	 (if (equal str "nil") docbook--nil (intern str)))))

(defun docbook--display-string (base-string fallback)
  "Return a string which displays as BASE-STRING on graphical terminals.
Use a display property so that on non-graphical terminals, the
string displays as the FALLBACK string."
  (propertize base-string
	      'display `(when (not (display-graphic-p)) . ,fallback)))

(defun docbook--node-text (node)
  "Return the contents of the DocBook node NODE, as a string."
  (let ((str (mapconcat
	      (lambda (x)
		(cond ((stringp x)
		       (if (string-match "\\`\\s-+\\'" x) "" x))
		      ((consp x)
		       (docbook--node-text x))))
	      (xml-node-children node)
	      "")))
    (if (string-match "\\`\\s-+" str)
	(setq str (substring str (match-end 0))))
    (if (string-match "\\s-+\\'" str)
	(setq str (substring str 0 (match-beginning 0))))
    str))

(defun docbook--print-block-delimiter ()
  "Insert newlines for the start or end of a DocBook block element."
  (cond
   ((bobp))
   ((looking-back "\n\n"))
   ((eq (char-before) ?\n) (insert ?\n))
   (t (insert "\n\n"))))

(defun docbook--print-string (str &optional literal face)
  "Insert STR (a string) at point, unless it is useless whitespace.
If LITERAL is non-nil, preserve whitespace.  If FACE is non-nil,
apply it as the face for the inserted text."
  (cond ((or literal (not (string-match "\\`\\s-+\\'" str)))
	 (insert (propertize str 'font-lock-face face)))
	((not (or (bolp) (memq (char-before) '(?\s ?\t))))
	 (insert " "))))

(defun docbook--merge-face (base-face face)
  "Return a face or list of faces, by merging BASE-FACE and FACE."
  (cond
   ((null base-face) face)
   ((null face)      base-face)
   ((eq face base-face) base-face)
   (t
    (append (if (consp face) face (list face))
	    (if (consp base-face) base-face (list base-face))))))

(defun docbook--node-face (base-face type &optional parent)
  "Return a face suitable for displaying DocBook node type TYPE.
BASE-FACE is the face specified by the node's parent elements.
If PARENT is non-nil, treat TYPE as the type of the parent node,
and assume that we are looking up the face of a title node."
  (let ((face (if parent
		  (or (cdr (assq type docbook-title-markup-alist))
		      'docbook-misc-title)
		(cdr (assq type docbook-text-markup-alist)))))
    (docbook--merge-face base-face face)))

;;; Rendering DocBook

(defun docbook-print-page (node-id &optional error-msg norecord)
  "Print the DocBook section corresponding to NODE-ID.
If NODE-ID is not a registered DocBook section node, signal an
error.  The optional argument ERROR-MSG, if non-nil, specifies a
default error message.

If optional argument NORECORD is non-nil, do not record this node
in `docbook-history'."
  (let ((node-record (when (and node-id (symbolp node-id))
		       (docbook--node-record node-id))))
    (unless node-record
      (funcall (if (fboundp 'user-error) 'user-error 'error)
	       (or error-msg "Node not found")))
    (unless norecord
      (push node-id docbook-history)
      (setq docbook-history-forward nil))
    (if (= (length node-record) 3)
	;; If the id points to a page fragment, visit the parent page
	;; and jump to the relevant marker within that page.
	(progn
	  (docbook-print-page (nth 2 node-record) nil t)
	  (docbook--visit-xref-marker node-id))
      ;; If the id points to a page, visit it.
      (let* ((inhibit-read-only t)
	     (node (car node-record))
	     (subsections (nth 5 node-record))
	     (docbook--footnotes nil))
	(erase-buffer)
	;; Add a fragment marker to the top of this page.
	(setq docbook-id-markers-alist nil
   	      docbook-current-page node-id)
	(docbook-add-fragment-link node-id)
	;; Each section contains any number of blocks followed by any
	;; number of subsections.  Loop over subnodes, printing
	;; block-level nodes.
	(dolist (subnode (xml-node-children node))
	  (cond ((null subnode))
		((stringp subnode)
		 (docbook--print-string subnode))
		((not (memq (xml-node-name subnode) docbook-page-types))
		 (docbook--print-node subnode (xml-node-name node)))))
	;; If there are footnotes, print them.
	(docbook--print-footnotes)
	;; If there are subsections, print a submenu.
	(when subsections
	  (docbook--print-block-delimiter)
	  (docbook--print-string "Menu" nil 'docbook-misc-title)
	  (insert "\n")
	  (let ((bullet (docbook--display-string "• " "* "))
		opoint)
	    (dolist (id subsections)
	      (setq opoint (point))
	      (insert bullet)
	      (docbook-insert-xref id)
	      (insert ?\n)
	      (put-text-property opoint (point) 'docbook-menu-xref id))))
	(goto-char (point-min))))))

(defun docbook--print-node (node parent-type &optional literal face)
  "Insert the contents of NODE at point.
NODE should be a cons cell---a subnode of the tree returned by
`xml-parse-file'.  PARENT-TYPE should be the node type of the
parent node (a symbol), or nil if this is the topmost node.

Optional arg LITERAL, if non-nil, means to preserve whitespace
and newlines when printing this node.

Optional arg FACE, if non-nil, should be a face or list of faces
to use, by default, for printing this node.  The node may apply
additional markup on top to of the specified FACE."
  (let ((type (xml-node-name node)))
    (cond
     ((memq type docbook-suppressed-types)
      (docbook-add-fragment-link (docbook--attr 'id node)))
     ((eq type 'title)
      (docbook--print-block node literal
			    (docbook--node-face face parent-type t)))
     ((progn
	;; For the sake of all the remaining node types, set FACE to
	;; the markup face for this node's type.
	(setq face (docbook--node-face face type))
	(memq type docbook-block-types))
      (docbook--print-block node literal face))
     ((progn
	;; For the sake of all remaining node types, apply the
	;; fragment ID if any.
	(docbook-add-fragment-link (docbook--attr 'id node))
	(eq type 'xref))
      (docbook--print-xref node literal face))
     ;; Index handling
     ((eq type 'indexterm)
      (docbook-add-fragment-link
       (cdr (assq 'docbook-indexterm-id (xml-node-attributes node)))))
     ((eq type 'index)
      (docbook--print-index (docbook--attr 'type node)))
     ;; Refentry and friends
     ((eq type 'refnamediv)
      (docbook--print-refnamediv node literal face))
     ((eq type 'refsynopsisdiv)
      (docbook--print-refsynopsisdiv node literal face))
     ;; List handling
     ((memq type docbook-list-types)
      (docbook--print-list node literal face))
     ((memq type '(listitem question answer step))
      (docbook--print-listitem node literal face))
     ((memq type '(term glossterm))
      (docbook--print-term node literal face))
     ;; Cross References
     ((memq type '(link ulink))
      (docbook--print-link node literal face))
     ((eq type 'email)
      (docbook--print-email node literal face))
     ;; Misc markup
     ((eq type 'quote)
      (docbook--print-string (docbook--display-string "“" "`")
			     literal face)
      (docbook--print-children node literal face)
      (docbook--print-string (docbook--display-string "”" "'")
			     literal face))
     ((eq type 'footnote)
      (docbook--print-footnote-tag node))
     ((eq type 'subscript)
      (docbook--print-with-display-prop node literal face '(raise -0.2)))
     ((eq type 'superscript)
      (docbook--print-with-display-prop node literal face '(raise 0.2)))
     ((eq type 'arg)
      (docbook--print-arg node literal face))
     ((eq type 'anchor))
     (t
      (docbook--print-children node literal face)))))

(defun docbook--print-block (node literal face)
  (docbook--print-block-delimiter)
  (let* ((type (xml-node-name node))
	 (beg (point)))
    ;; If the block has an ID tag, apply it.
    (docbook-add-fragment-link (docbook--attr 'id node))
    ;; Print the contents of the block.
    (docbook--print-children node literal
			   (docbook--node-face face type))
    (unless literal
      ;; Flush the beginning of the block to column zero, and fill.
      (let ((stop (point)))
	(save-excursion
	  (goto-char beg)
	  (skip-chars-forward "[:space:]" stop)
	  (delete-region beg (point))
	  (setq beg (point))))
      (let ((left-margin docbook--indent-level))
	(fill-region-as-paragraph beg (point))))
    (docbook--print-block-delimiter)))

(defun docbook--print-list (node literal face)
  (docbook--print-block-delimiter)
  (let ((type (xml-node-name node))
	(docbook--indent-level docbook--indent-level)
	(docbook--list-context docbook--list-context))
    (cond
     ((memq type '(procedure substeps))
      ;; We use a version list to denote (sub)steps.
      (let* ((version (if (eq (car-safe docbook--list-context) 'procedure)
			  (append (cdr docbook--list-context) '(1))
			'(1)))
	     (str (mapconcat 'int-to-string version ".")))
	(setq docbook--indent-level (+ (length str) 3 docbook--indent-level)
	      docbook--list-context (cons 'procedure version))))
     ((eq type 'orderedlist)
      (setq docbook--indent-level (+ 4 docbook--indent-level)
	    docbook--list-context 1))
     ((memq type '(glosslist variablelist))
      (setq docbook--indent-level (+ 4 docbook--indent-level)
	    docbook--list-context 'variablelist))
     ((eq type 'qandaset)
      (let ((label (docbook--attr 'defaultlabel node)))
	(setq docbook--indent-level (+ 4 docbook--indent-level)
	      docbook--list-context (cons 'qandaset label))))
     (t
      (setq docbook--indent-level (+ 2 docbook--indent-level)
	    docbook--list-context 'itemizedlist)))
    (docbook--print-children node literal face))
  (docbook--print-block-delimiter))

(defun docbook--print-term (node literal face)
  (when (eq docbook--list-context 'variablelist)
    (unless (eq (char-before) ?\n)
      (insert "\n"))
    (let ((opoint (point)))
      (docbook--print-children node literal face)
      (save-excursion
	(let ((stop (point)))
	  (goto-char opoint)
	  (skip-chars-forward "[:space:]" stop)
	  (delete-region opoint (point))
	  (indent-line-to (- docbook--indent-level 4))
	  (docbook--print-string (docbook--display-string "• " "* ")
				 literal face))))))

(defun docbook--print-listitem (node literal face)
  (let ((opoint (point)))
    (docbook--print-children node literal face)
    (when (not (memq docbook--list-context '(nil variablelist)))
      (cond
       ;; A step in a procedure
       ((eq (car-safe docbook--list-context) 'procedure)
	(let* ((version (cdr docbook--list-context))
	       (str (concat (mapconcat 'int-to-string version ".") ". "))
	       (subversion (nthcdr (1- (length version)) version)))
	  (docbook--print-listitem-1 opoint str (length str)
				     literal face)
	  (setcar subversion (1+ (car subversion)))))
       ;; Question or answer
       ((eq (car-safe docbook--list-context) 'qandaset)
	(let ((subnodes (xml-node-children node))
	      label)
	  ;; Look for a label for the question or answer.
	  (while (and (null label) subnodes)
	    (when (and (consp (car subnodes))
		       (eq (xml-node-name (car subnodes)) 'label))
	      (setq label (docbook--node-text (car subnodes))))
	    (setq subnodes (cdr subnodes)))
	  ;; If there is none, consult the default label.
	  (and (not (stringp label))
	       (eq (cdr docbook--list-context) 'qanda)
	       (setq label (if (eq (xml-node-name node) 'question)
			       "Q:"
			     "A:")))
	  (if (null label)
	      ;; Use a bullet, like an itemizedlist.
	      (docbook--print-listitem-1
	       opoint (docbook--display-string "• " "* ") 2 literal face)
	    (docbook--print-listitem-1
	     opoint label 0 literal
	     (docbook--merge-face face 'docbook-label) " " face))))
       ;; orderedlist
       ((integerp docbook--list-context)
	(docbook--print-listitem-1
	 opoint (format "%2d. " docbook--list-context) 4 literal face)
	(setq docbook--list-context (1+ docbook--list-context)))
       ;; itemizedlist
       (t
	(docbook--print-listitem-1
	 opoint (docbook--display-string "• " "* ") 2 literal face))))))

(defun docbook--print-listitem-1 (opoint bullet bullet-len literal face
				  &optional after-string after-string-face)
  (save-excursion
    (let ((stop (point)))
      (goto-char opoint)
      (skip-chars-forward "[:space:]" stop)
      (indent-line-to (- docbook--indent-level bullet-len))
      (docbook--print-string bullet literal face)
      (if after-string
	  (docbook--print-string after-string literal
				 after-string-face)))))

(defun docbook--print-footnote-tag (node)
  (when (boundp 'docbook--footnotes)
    (let ((n (1+ (length docbook--footnotes)))
	  (tag-id (make-symbol "footnote-id"))
	  (footnote-id (make-symbol "footnote")))
      (docbook-add-fragment-link tag-id)
      (docbook-insert-xref footnote-id (format "(%d)" n))
      (push (list tag-id footnote-id node) docbook--footnotes))))

(defun docbook--print-footnotes ()
  (when (bound-and-true-p docbook--footnotes)
    (docbook--print-block-delimiter)
    (docbook--print-string "--- Footnotes ---")
    (let ((n 1) opoint)
      (dolist (footnote (nreverse docbook--footnotes))
	(docbook--print-block-delimiter)
	(setq opoint (point))
	(docbook--print-children (nth 2 footnote))
	(save-excursion
	  (goto-char opoint)
	  (if (eq (char-after) ?\n) (forward-char))
	  (docbook-add-fragment-link (nth 1 footnote))
	  (docbook-insert-xref (car footnote) (format "(%d)" n))
	  (insert " "))
	(setq n (1+ n))))))

(defun docbook--print-with-display-prop (node literal face prop)
  (let ((opoint (point)))
    (docbook--print-children node literal face)
    (put-text-property opoint (point) 'display prop)))

(defun docbook--print-children (node &optional literal face)
  "Print the child nodes of the DocBook node NODE.
LITERAL and FACE mean the same as in `docbook--print-node'."
  (dolist (subnode (xml-node-children node))
    (cond
     ((null subnode))
     ((stringp subnode)
      (docbook--print-string subnode literal face))
     (t
      (docbook--print-node subnode (xml-node-name node)
			   literal face)))))

(defun docbook--print-refnamediv (node literal face)
  (docbook--print-block-delimiter)
  (let (names purpose)
    (dolist (subnode (xml-node-children node))
      (cond ((not (consp subnode)))
	    ((eq (xml-node-name subnode) 'refname)
	     (push subnode names))
	    ((eq (xml-node-name subnode) 'refpurpose)
	     (setq purpose subnode))))
    (setq names (nreverse names))
    (indent-to docbook--indent-level)
    (while names
      (docbook--print-node (car names) 'refnamediv literal face)
      (setq names (cdr names))
      (if names (docbook--print-string ", " literal face)))
    (when purpose
      (or (eq (char-before) ?\n) (insert ?\n))
      (indent-to docbook--indent-level)
      (docbook--print-node purpose literal face)))
  (docbook--print-block-delimiter))

(defun docbook--print-refsynopsisdiv (node literal face)
  (docbook--print-block-delimiter)
  (indent-to docbook--indent-level)
  (docbook--print-string "Synopsis" nil 'docbook-misc-title)
  (docbook--print-block-delimiter)
  (docbook--print-children node literal face))

(defun docbook--print-arg (node literal face)
  (let ((choice (docbook--attr 'choice node))
	(repeat (docbook--attr 'rep node)))
    (if (eq choice 'opt)
	(docbook--print-string "[ " literal face))
    (docbook--print-children node literal face)
    (if (eq choice 'opt)
	(docbook--print-string " ]" literal face))
    (if (eq repeat 'repeat)
	(docbook--print-string "..." literal face))))

;;; Cross-reference handling

(defun docbook--print-xref (node literal face)
  "Insert the contents of an xref node NODE."
  (let ((target (docbook--attr 'linkend node)))
    (when target
      (let ((endterm (docbook--attr 'endterm node)))
	;; If an endterm attribute is present, print its contents.
	;; FIXME: protect against a recursion bomb.
	(if (and endterm
		 (setq endterm (car (docbook--node-record endterm))))
	    (docbook--print-link endterm literal face target)
	  (docbook-insert-xref target))))))

(defun docbook--print-link (node literal face &optional linkend)
  "Insert the contents of a link node NODE."
  (let ((target (or linkend (docbook--attr 'linkend node)))
	(opoint (point))
	(action 'docbook-xref-button-action))
    (unless target
      ;; If there is no linkend attribute, look for an external URL.
      (let ((attributes (xml-node-attributes node)))
	(setq target
	      (or (cdr (assq 'xlink:href attributes))
		  (cdr (assq 'href attributes))
		  ;; Used by obsolete `url' elements.
		  (cdr (assq 'url attributes))))
	(setq action 'docbook-link-button-action)))
    (docbook--print-children node literal face)
    (make-text-button opoint (point)
		      'action action
		      'docbook-target target)))

(defun docbook--print-email (node literal face)
  "Insert the contents of a link node NODE."
  (let ((opoint (point)))
    (docbook--print-children node literal face)
    (make-text-button opoint (point)
		      'action 'docbook-email-button-action)))

(defun docbook-insert-xref (node-id &optional label)
  "Insert a cross reference to NODE-ID at point.
NODE-ID should be a node ID, as either a symbol or a string.
LABEL, if non-nil, specifies the text label."
  (unless label
    (setq label (docbook-node-label node-id)))
  (insert-text-button label
		      'action 'docbook-xref-button-action
		      'docbook-target node-id))

(defun docbook-node-label (node-id)
  "Return an appropriate label for the node with ID NODE-ID."
  (let* ((record (docbook--node-record node-id))
	 (attributes (xml-node-attributes (car record)))
	 ;; Use the target node's xreflabel attribute.
	 (label (cdr (assq 'xreflabel attributes))))
    (when (memq label '(nil ""))
      ;; Otherwise, use the target node's title.
      (setq label (and (nth 1 record)
		       (docbook--node-text (nth 1 record))))
      (when (memq label '(nil ""))
	;; Otherwise, default to the node ID's name.
	(setq label (symbol-name node-id))))
    label))

(defun docbook--visit-xref-marker (node-id &optional noerror)
  "Visit the position of NODE-ID on the current DocBook page.
Return non-nil if we found the element and jumped to it.
Otherwise, signal an error if NOERROR is nil, and return nil if
NOERROR is non-nil."
  (let ((marker (cdr (assq node-id docbook-id-markers-alist))))
    (cond
     ((markerp marker)
      (goto-char marker))
     ((null noerror)
      (error "Node not found")))))

(defun docbook-visit-xref (node-id)
  (or (docbook--visit-xref-marker node-id t)
      (docbook-print-page node-id)))

(defun docbook-xref-button-action (button)
  "Visit the DocBook node indicated by BUTTON."
  (docbook-visit-xref (button-get button 'docbook-target)))

(defun docbook-link-button-action (button)
  "Call `browse-url' to visit the link indicated by BUTTON."
  (let ((target (button-get button 'docbook-target)))
    (if (string-match "\\`mailto:" target)
	(compose-mail (substring-no-properties target (match-end 0)))
      (browse-url (button-get button 'docbook-target)))))

(defun docbook-email-button-action (button)
  "Send mail to the address indicated by BUTTON."
  (compose-mail (buffer-substring-no-properties
		 (button-start button) (button-end button))))

;; Printing the index and history list

(defun docbook--print-index (type)
  "Insert the DocBook index of type TYPE at point."
  (let ((index (assq type docbook--index-alist))
	(bullet (docbook--display-string "• " "* "))
	opoint)
    (unless (eq (char-before) ?\n) (insert ?\n))
    (dolist (entry (cdr index))
      (setq opoint (point))
      (insert bullet)
      (insert (car entry))
      (let* ((ids (cdr entry))
	     (id (car ids)))
	(indent-to docbook-index-separator-column 2)
	(docbook-insert-xref
	 id (docbook-node-label (nth 2 (docbook--node-record id))))
	(insert ?\n)
	(put-text-property opoint (point) 'docbook-menu-xref id)
	(if (> (length ids) 1)
	    (dolist (id (cdr ids))
	      (setq opoint (point))
	      (indent-to docbook-index-separator-column 2)
	      (docbook-insert-xref
	       id (docbook-node-label
		   (nth 2 (docbook--node-record id))))
	      (insert ?\n)
	      (put-text-property opoint (point) 'docbook-menu-xref id)))))
    (insert ?\n)))

(defun docbook--print-history ()
  "Insert the DocBook navigation history menu at point."
  (let ((bullet (docbook--display-string "◦ " "* ")))
    (dolist (id (reverse (cdr docbook-history)))
      (unless (eq (char-before) ?\n) (insert ?\n))
      (insert bullet)
      (docbook-insert-xref id))
    ;; Indicate the current page with a more prominent bullet.
    (unless (eq (char-before) ?\n) (insert ?\n))
    (insert (docbook--display-string "• " "* "))
    (docbook-insert-xref (car docbook-history))
    (dolist (id docbook-history-forward)
      (unless (eq (char-before) ?\n) (insert ?\n))
      (insert bullet)
      (docbook-insert-xref id))
    (insert ?\n)))

;;; Major mode

(defvar docbook-mode-map
  (let ((map (make-keymap)))
    (set-keymap-parent map (make-composed-keymap button-buffer-map
                                                 special-mode-map))
    (define-key map "." 'beginning-of-buffer)
    (define-key map " " 'docbook-scroll-up)
    (define-key map "\177" 'docbook-scroll-down)
    (define-key map "\C-m" 'docbook-follow-nearest-node)

    (dotimes (n 9)
      (define-key map (number-to-string (1+ n)) 'docbook-nth-menu-item))

    (define-key map "b" 'beginning-of-buffer)
    (define-key map "e" 'end-of-buffer)
    (define-key map "\M-n" 'clone-buffer)

    (define-key map "i" 'docbook-index)
    (define-key map "I" 'docbook-index)
    (define-key map "l" 'docbook-history-back)
    (define-key map "r" 'docbook-history-forward)
    (define-key map "L" 'docbook-history)

    (define-key map "]" 'docbook-forward-page)
    (define-key map "[" 'docbook-backward-page)
    (define-key map "n" 'docbook-forward-page)
    (define-key map "p" 'docbook-backward-page)

    ;; (define-key map "f" 'docbook-follow-reference)
    ;; (define-key map "g" 'docbook-goto-node)
    ;; (define-key map "m" 'docbook-menu)

    ;; (define-key map "s" 'docbook-search)
    ;; (define-key map "S" 'docbook-search-case-sensitively)
    ;; (define-key map "T" 'docbook-toc)
    ;; (define-key map "," 'docbook-index-next)

    (define-key map "t" 'docbook-top-page)
    (define-key map "u" 'docbook-up)
    (define-key map "^" 'docbook-up)
    (define-key map [follow-link] 'mouse-face)
    map)
  "Keymap containing DocBook commands.")

(define-derived-mode docbook-mode special-mode "DocBook"
  "Major mode for viewing DocBook documents.
Type \\[docbook-find-file] to visit DocBook files for viewing.
Most of the commands in DocBook mode are similar to Info mode.

DocBook documents are divided into \"section nodes\" (which
includes chapters, sections, subsections, etc.).  DocBook mode
displays one section node at a time, as a single page.
Navigation commands and hyperlinks can be used to view other
pages.

Moving within a page:
\\[docbook-scroll-up]	Normally, scroll forward a full screen.
	If you have scrolled to the end of this page,
	view the next page.
\\[docbook-scroll-down]	Normally, scroll backward a full screen.
	If you have scrolled to the beginning of this page,
	view the preceding page.
\\[beginning-of-buffer]	Jump to beginning of this page.

Selecting other nodes:
\\[docbook-follow-nearest-node]	Follow a node reference near point.
\\[docbook-backward-page]	View the preceding page.
\\[docbook-forward-page]	View the next page.
\\[docbook-up]	View the parent of the current page.
\\[docbook-top-page]	View the topmost section of this document.
\\[docbook-history-back]	View the last page you were at.
\\[docbook-history-forward]	Move forward in history to the page you were at before using \\[docbook-history-back].
\\[docbook-history]	View a menu of visited pages."
  (make-local-variable 'docbook--parse-tree)
  (make-local-variable 'docbook--id-table)
  (make-local-variable 'docbook-current-page)
  (make-local-variable 'docbook-top-page)
  (make-local-variable 'docbook-id-markers-alist)
  (make-local-variable 'docbook--index-alist)
  (make-local-variable 'docbook-history)
  (make-local-variable 'docbook-history-foward)
  (setq-local adaptive-fill-mode nil)
  (setq indent-tabs-mode nil)
  (setq fill-prefix nil)
  (setq use-hard-newlines t))

;;; Navigation commands

(defun docbook-up ()
  "View the parent of the current DocBook page."
  (interactive)
  (docbook-print-page (nth 2 (docbook--node-record)) "No parent page"))

(defun docbook-top-page ()
  "View the topmost page in the current DocBook document."
  (interactive)
  (docbook-print-page docbook-top-page))

(defun docbook-backward-page ()
  "View the previous DocBook page."
  (interactive)
  (docbook-print-page (nth 3 (docbook--node-record)) "No previous page"))

(defun docbook-forward-page ()
  "View the next DocBook page."
  (interactive)
  (docbook-print-page (nth 4 (docbook--node-record)) "No following page"))

(defun docbook-scroll-up ()
  "Scroll forward, or view the next DocBook page."
  (interactive)
  (condition-case nil
      (scroll-up nil)
    (end-of-buffer (docbook-forward-page))))

(defun docbook-scroll-down ()
  "Scroll backward, or view the preceding DocBook page."
  (interactive)
  (condition-case nil
      (scroll-down nil)
    (beginning-of-buffer (docbook-backward-page))))

(defun docbook-nth-menu-item ()
  "View the Nth menu item, based on the key typed."
  (interactive)
  (let ((n (- (aref (this-command-keys)
		    (1- (length (this-command-keys)))) ?0))
	(node-record (docbook--node-record)))
    (unless node-record
      (funcall (if (fboundp 'user-error) 'user-error 'error)
	       "No menu in this node"))
    (let ((id (nth (1- n) (nth 5 node-record))))
      (unless id
	(funcall (if (fboundp 'user-error) 'user-error 'error)
		 "Too few items in menu"))
      (docbook-visit-xref id))))

(defun docbook-follow-nearest-node ()
  "Follow a node reference near point.
If point is on a reference, follow that reference.  Otherwise,
if point is in a menu item description, follow that menu item."
  (interactive)
  (let ((id (get-text-property (point) 'docbook-menu-xref)))
    (if id
	(docbook-visit-xref id)
      (funcall (if (fboundp 'user-error) 'user-error 'error)
	       "Point neither in reference nor in menu item description"))))

;; History commands

(defun docbook-history-back (n)
  "Go back in history to the previous DocBook page viewed."
  (interactive "p")
  (dotimes (_i n)
    (unless (cdr docbook-history)
      (funcall (if (fboundp 'user-error) 'user-error 'error)
	       "This is the first node you looked at"))
    (push (pop docbook-history) docbook-history-forward)
    (docbook-print-page (car docbook-history) nil t)))

(defun docbook-history-forward (n)
  "Go forward in history to the next DocBook page viewed."
  (interactive "p")
  (dotimes (_i n)
    (if (null docbook-history-forward)
	(funcall (if (fboundp 'user-error) 'user-error 'error)
		 "This is the last node you looked at"))
    (let ((id (pop docbook-history-forward)))
      (push id docbook-history)
      (docbook-print-page id nil t))))

(defun docbook-history ()
  "Display a list of recently-visited DocBook pages."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (docbook--print-string "Recently visited pages"
			   nil 'docbook-chapter-title)
    (insert ?\n ?\n)
    (docbook--print-history)))

;; Misc commands

(defun docbook-index (type)
  "Display a list of index topics fo the current DocBook document.
The argument TYPE is the index type; DocBook documents can define
several indices for different topics.  If called interactively,
prompt for TYPE."
  (interactive (list (if (<= (length docbook--index-alist) 1)
			 (caar docbook--index-alist)
		       (completing-read
			(format "View index type%s: "
				(if (assq nil docbook--index-alist)
				    " (empty input for default index)"
				  ""))
			(cons "" (mapcar (lambda (x) (symbol-name (car x)))
					 docbook--index-alist))
			nil t))))
  (unless (assq type docbook--index-alist)
    (funcall (if (fboundp 'user-error) 'user-error 'error)
	     "Index is empty"))
  (let ((inhibit-read-only t))
    (erase-buffer)
    (docbook--print-string (if type
			       (format "Index: %s" (symbol-name type))
			     "Index")
			   nil 'docbook-chapter-title)
    (insert ?\n ?\n)
    (docbook--print-index type)))

;;;###autoload
(defun docbook-find-file (filename)
  "Visit FILENAME as a DocBook document."
  (interactive "fView DocBook file: ")
  (docbook-setup
   (car (let ((xml-entity-alist (append docbook-entity-alist
					xml-entity-alist)))
	  (xml-parse-file filename))))
  (docbook-print-page docbook-top-page))

;;;; ChangeLog:

;; 2013-02-19  Chong Yidong  <cyd@gnu.org>
;; 
;; 	docbook.el: Add autoload cookie.
;; 
;; 2013-02-18  Chong Yidong  <cyd@gnu.org>
;; 
;; 	docbook: New package (Info-like viewer for DocBook documents).
;; 


(provide 'docbook)

;;; docbook.el ends here
