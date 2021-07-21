;;;; =========================================================================
;;;; RDF Mode
;;;; ========================================================================= 
;;;;
;;;; A derived mode, based on XML mode. Highlighting of RDF/XML is adapted
;;;; to RDF. Requires the psgml package.
;;;;
;;;; ========================================================================
;;;; 
;;;; Copyright (c) 2002, Christoph Wernhard
;;;; 
;;;; Some parts of this are derived from xxml.el by François Pinard, which
;;;; is under GNU General Public License.
;;;; ( http://www.iro.umontreal.ca/~pinard/fp-etc/dist/xxml/xxml.el ).
;;;; 
;;;; This program is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU General Public License
;;;; as published by the Free Software Foundation; either version 2
;;;; of the License, or (at your option) any later version.
;;;; 
;;;; This program is distributed in the hope that it will be useful, but
;;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;;;; General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU Lesser General Public
;;;; License along with this library; if not, write to the Free Software
;;;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
;;;; USA
;;;;
;;;; ========================================================================
;;;; 
;;;; Installation
;;;;
;;;; - You must have installed the psgml package.
;;;;
;;;; - Optionally byte-compile this file.
;;;;
;;;; - Ensure that rdf-mode.elc or rdf-mode.el will be loaded, e.g. by
;;;;   placing it in a directory in the load-path and putting the following
;;;;   call in your .emacs file:
;;;;  
;;;;   (autoload 'rdf-mode "rdf-mode" "Major mode to edit RDF/XML files." t)
;;;;
;;;; - The following call in your .emacs file effects that files *.rdf
;;;;   automatically use RDF mode:
;;;;  
;;;;   (setq auto-mode-alist
;;;;         (append '(("\\.rdf" . rdf-mode)) auto-mode-alist))
;;;;
;;;; ========================================================================

(require 'psgml-api)
(require 'cl)

(define-derived-mode rdf-mode
    xml-mode "RDF"
    "Major mode to edit RDF/XML files.
     \\{rdf-mode-map}")

(add-hook 'rdf-mode-hook
	  (function (lambda()
	    (set (make-local-variable 'font-lock-defaults)
		 '(xrdf-font-lock-keywords t))
	    (set (make-local-variable 'sgml-set-face) nil))))

(make-face 'xrdf-comment-face)
(make-face 'xrdf-starttag-face)
(make-face 'xrdf-endtag-face)
(make-face 'xrdf-entity-face)
(make-face 'xrdf-attname-face)
(make-face 'xrdf-attval-face)
(make-face 'xrdf-bracket-face)
(make-face 'xrdf-procinstr-face)
(make-face 'xrdf-test-face)
(make-face 'xrdf-evenstripe-face)
(make-face 'xrdf-attname-resource-face)
(make-face 'xrdf-attval-resource-face)
(make-face 'xrdf-attname-system-face)
(make-face 'xrdf-attval-system-face)
(make-face 'xrdf-attname-special-face)
(make-face 'xrdf-attval-special-face)


(defvar xrdf-comment-face 'xrdf-comment-face)
(defvar xrdf-starttag-face 'xrdf-starttag-face)
(defvar xrdf-endtag-face 'xrdf-endtag-face)
(defvar xrdf-entity-face 'xrdf-entity-face)
(defvar xrdf-attname-face 'xrdf-attname-face)
(defvar xrdf-attval-face 'xrdf-attval-face)
(defvar xrdf-bracket-face 'xrdf-bracket-face)
(defvar xrdf-procinstr-face 'xrdf-procinstr-face)
(defvar xrdf-test-face 'xrdf-test-face)
(defvar xrdf-evenstripe-face 'xrdf-evenstripe-face)
(defvar xrdf-attname-resource-face 'xrdf-attname-resource-face)
(defvar xrdf-attval-resource-face 'xrdf-attval-resource-face)
(defvar xrdf-attname-system-face 'xrdf-attname-system-face)
(defvar xrdf-attval-system-face 'xrdf-attval-system-face)
(defvar xrdf-attname-special-face 'xrdf-attname-special-face)
(defvar xrdf-attval-special-face 'xrdf-attval-special-face)

(defun xrdf-set-colors-standard ()
  (interactive)
  (set-face-foreground 'xrdf-comment-face "#8B1C62")
  (set-face-foreground 'xrdf-starttag-face "#000080")
  (set-face-foreground 'xrdf-endtag-face "#000080")
  (set-face-foreground 'xrdf-bracket-face "#707070")
  (set-face-foreground 'xrdf-entity-face "#208020")
  (set-face-foreground 'xrdf-attname-face "#505080")
  (set-face-foreground 'xrdf-attval-face "#000000")
  (set-face-foreground 'xrdf-procinstr-face "#008000")
  (set-face-foreground 'xrdf-test-face "red")
  (set-face-foreground 'xrdf-evenstripe-face "#800000")
  (set-face-foreground 'xrdf-attname-resource-face "#805050")
  (set-face-foreground 'xrdf-attval-resource-face "#400000")
  (set-face-foreground 'xrdf-attname-system-face "#707070")
  (set-face-foreground 'xrdf-attval-system-face "#707070")
  (set-face-foreground 'xrdf-attname-special-face "#707070")
  (set-face-foreground 'xrdf-attval-special-face "#DD0000"))

(defun xrdf-set-colors-bright ()
  (interactive)
  (set-face-foreground 'xrdf-comment-face "#ce2b92")
  (set-face-foreground 'xrdf-starttag-face "#0000ce")
  (set-face-foreground 'xrdf-endtag-face "#0000ce")
  (set-face-foreground 'xrdf-bracket-face "#a0a0a0")
  (set-face-foreground 'xrdf-entity-face "#2db72d")
  (set-face-foreground 'xrdf-attname-face "#8282ce")
  (set-face-foreground 'xrdf-attval-face "#000000")
  (set-face-foreground 'xrdf-procinstr-face "#00ce00")
  (set-face-foreground 'xrdf-test-face "red")
  (set-face-foreground 'xrdf-evenstripe-face "#ce0000")
  (set-face-foreground 'xrdf-attname-resource-face "#ce8282")
  (set-face-foreground 'xrdf-attval-resource-face "#800000")
  (set-face-foreground 'xrdf-attname-system-face "#a0a0a0")
  (set-face-foreground 'xrdf-attval-system-face "#a0a0a0")
  (set-face-foreground 'xrdf-attname-special-face "#a0a0a0")
  (set-face-foreground 'xrdf-attval-special-face "#DD0000"))

(xrdf-set-colors-standard)

(defvar xrdf-font-lock-keywords
  '(("\\(<!--\\)\\([^>]*\\)\\(-->\\)"
     (1 xrdf-comment-face)
     (2 xrdf-comment-face)
     (3 xrdf-comment-face))
    ("\\(<[!?]\\)\\([^-]?[^?>]*\\)\\(\\??>\\)"
     (1 xrdf-bracket-face)
     (2 xrdf-procinstr-face)
     (3 xrdf-bracket-face))
    (xrdf-even-stripes-for-highlight
     (1 xrdf-bracket-face)
     (2 xrdf-evenstripe-face))
    ("\\(<\\)\\([a-zA-Z0-9-_:]+\\)"
     (1 xrdf-bracket-face)
     (2 xrdf-starttag-face))
    ("/>" 0 xrdf-bracket-face)
    (">" 0 xrdf-bracket-face)
    ("\\(</\\)\\([a-zA-Z0-9-_:]+\\)\\(>\\)"
     (1 xrdf-bracket-face)
     (2 xrdf-endtag-face)
     (3 xrdf-bracket-face))
    ("&\\([a-zA-Z][a-zA-Z0-9-_.]*\\|#\\([xX][0-9a-fA-F]+\\|[0-9]+\\)\\);?"
     0 xrdf-entity-face)
    (xrdf-attribute-for-highlight-rdfs-comment
     (1 xrdf-attname-face append)
     (2 xrdf-comment-face append))
    (xrdf-attribute-for-highlight-rdf-resource
     (1 xrdf-attname-resource-face append)
     (2 xrdf-attval-resource-face append))
    (xrdf-attribute-for-highlight-system
     (1 xrdf-attname-system-face append)
     (2 xrdf-attval-system-face append))
    (xrdf-attribute-for-highlight-special
     (1 xrdf-attname-special-face append)
     (2 xrdf-attval-special-face append))
    (xrdf-attribute-for-highlight
     (1 xrdf-attname-face append)
     (2 xrdf-attval-face append)))
  "Data to drive fontification in SGML editing mode.")

(defun xrdf-attribute-for-highlight (limit)
  (xrdf-attribute-for-highlight-1 "[-a-zA-Z0-9:]+=" limit))

(defun xrdf-attribute-for-highlight-rdfs-comment (limit)
  (xrdf-attribute-for-highlight-1 "rdfs:comment=" limit))

(defun xrdf-attribute-for-highlight-rdf-resource (limit)
  (xrdf-attribute-for-highlight-1 
   "rdf:resource=\\|rdf:ID=\\|rdf:about=" limit))

(defun xrdf-attribute-for-highlight-special (limit)
  (xrdf-attribute-for-highlight-1 
   "swp:var=" limit))

(defun xrdf-attribute-for-highlight-system (limit)
  (xrdf-attribute-for-highlight-1 
   "rdf:[-a-zA-Z0-9]+=\\|xml[-a-zA-Z0-9:]+=" limit))

(defun xrdf-even-stripes-for-highlight (limit)
  (do ((point (re-search-forward "\\(</?\\)\\([a-zA-Z0-9-_:]+\\)" limit t)
	      (re-search-forward "\\(</?\\)\\([a-zA-Z0-9-_:]+\\)" limit t)))
      ((or (null point)
	   (evenp (save-excursion
		    (save-match-data
		      (sgml-element-level
		       (sgml-find-context-of (point)))))))
       point)))

(defvar xrdf-highlight-tag-alist nil
  "Association list relating tag strings to face for the tag itself.")

(defvar xrdf-highlight-initial-alist
  `(("rdfs:comment" . 'xrdf-comment-face))
  "Association list relating tag strings to face for initial text.")

(defvar xrdf-highlight-recursive-alist nil
  "Association list relating tag strings to face for recursive contents.")

; ;; These are lists as the following excerpt from xxml.el shows.
;
; (defvar xxml-html-highlight-recursive-alist
;   '(;; Block elements.
;     ("title" . xxml-header-1-face)
;     ("h1" . xxml-header-1-face)
;     ("h2" . xxml-header-2-face)))
; (defvar xxml-html-highlight-tag-alist
;   '(;; Text elements - form-fields
;     ("form" . xxml-interaction-face)
;     ("input" . xxml-interaction-face)))

(defvar xrdf-value-regexp
  "\\([-.a-zA-Z0-9]+\\|\"\\(\\\\.\\|[^\"]\\)*\"\\)"
  "Regular expression matching a value assignment to an attribute")

(defun xrdf-append-face-to-trimmed-lines (start end face)
  "To trimmed lines between START and END, append FACE to text properties.
That is, do not append face over starting or ending region, or lines in the
regions.  Point is left at END."
  (goto-char start)
  (skip-chars-forward " \t\n" end)
  (while (< (point) end)
    (setq start (point))
    (unless (search-forward "\n" end t)
      (goto-char end))
    (skip-chars-backward " \t\n")
    (font-lock-append-text-property start (point) 'face face)
    (skip-chars-forward " \t\n" end)))

(defun xrdf-highlight-on-the-fly (tag tag-end limit)
  "Do any specially decided highlighting for tags or their whole contents.
Such TAG should not be recursively used, and must be explicitely ended.
Highlighting usually starts at TAG-END but should not extend beyond LIMIT."
  (setq tag (downcase tag))
  (let ((face-for-tag
	 (let ((pair (assoc tag xrdf-highlight-tag-alist)))
	   (and pair (cdr pair))))
	(face-for-initial
	 (let ((pair (assoc tag xrdf-highlight-initial-alist)))
	   (and pair (cdr pair))))
	(face-for-recursive
	 (let ((pair (assoc tag xrdf-highlight-recursive-alist)))
	   (and pair (cdr pair)))))
    ;; Point always happens to be after the opening bracket of the start tag.
    (let ((here (point)))
      (when (or face-for-tag face-for-recursive)
	;; Find the end tag.  (FIXME: we might not find the correct one!)
	(when (let ((case-fold-search t))
		(re-search-forward (concat "</" tag ">") limit t))
	  (setq limit (match-beginning 0))
	  (when face-for-tag
	    ;; Highlight the end tag.
	    (font-lock-append-text-property (match-beginning 0) (match-end 0)
					    'face face-for-tag))))
      (when face-for-tag
	;; Highlight the start tag.
	(xrdf-append-face-to-trimmed-lines (1- here) tag-end face-for-tag))
      (when face-for-initial
	;; Highlight the text before next tag.
	(goto-char tag-end)
	(when (> (skip-chars-forward "^<" limit) 0)
	  (xrdf-append-face-to-trimmed-lines tag-end (point) face-for-initial))
	(setq tag-end (point)))
      (when face-for-recursive
	;; Highlight the whole remainder of recursive contents.
	(xrdf-append-face-to-trimmed-lines tag-end limit face-for-recursive))
      ;; Restore position.
      (goto-char here))))

(defun xrdf-attribute-for-highlight-1 (attribute-pattern limit)
   "Find next tag attribute to highlight, then set \1 to name and \2 to value.
 Return t if found.  This routine handles tags spanning multiple lines, which
 anchored matches would hardly do.  It might do the job a bit more speedily,
 too.  Also highlight, on the fly, some special tags or embedded contents."
   (let ((pattern (concat "[ \t\n]+\\("
			  attribute-pattern
			  "\\)\\(\\("
			  xrdf-value-regexp
			  "\\)?\\)"))
	tag tag-end)
    ;; Find the end of that tag starting before current position.
    (save-excursion
      (when (search-backward "<" nil t)
	(setq tag (and (looking-at "<\\([-a-zA-Z0-9:]+\\)")
		       (match-string-no-properties 1))
	      tag-end (or (search-forward ">" limit t) limit))))
    (or (and tag
	     (> tag-end (point))
	     ;; We were already within a simple start tag.
	     (progn
	       (xrdf-highlight-on-the-fly tag tag-end limit)
	       (re-search-forward pattern tag-end t)))
	(let (found)
	  ;; Skip over text between tags.
	  (while (and (not found) (search-forward "<" limit t))
	    (setq tag-end (or (save-excursion (search-forward ">" limit t))
			      limit))
	    (if (and (setq tag (and (looking-at "[-a-zA-Z0-9:]+")
				    (match-string-no-properties 0)))
		     ;; Now again within a simple start tag.
		     (progn
		       (xrdf-highlight-on-the-fly tag tag-end limit)
		       (re-search-forward pattern tag-end t)))
		(setq found t)
	      (goto-char tag-end)))
	  found))))

