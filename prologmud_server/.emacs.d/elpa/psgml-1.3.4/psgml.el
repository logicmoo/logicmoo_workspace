;;; psgml.el --- SGML-editing mode with parsing support  -*- lexical-binding:t -*-

;; Copyright (C) 1992-2002, 2016-2017 Free Software Foundation, Inc.

;; Author: Lennart Staflin <lenst@lysator.liu.se>
;; 	James Clark <jjc@clark.com>
;; Maintainer: Lennart Staflin <lstaflin@gmail.com>
;; Keywords: languages
;; Version: 1.3.4

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


;;; Commentary:

;; Major mode for editing the SGML document-markup language.

;; Send bugs to lenst@lysator.liu.se

;; WHAT IT CAN DO

;; - Identify structural errors (but it is not a validator)
;; - Menus for inserting tags with only the contextually valid tags
;; - Edit attribute values in separate window with information about types
;;   and defaults
;; - Hide attributes
;; - Fold elements
;; - Indent according to element nesting depth
;; - Show context
;; - Structure editing: move and kill by element
;; - Find next data context

;; LIMITATIONS

;; - only accepts the reference concrete syntax, though it does allow
;;   unlimited lengths on names


;;; Code:

(defconst psgml-maintainer-address "emacs-devel@gnu.org")

(eval-when-compile (require 'cl-lib))
(require 'easymenu)

(defvar sgml-debug nil)

(defmacro sgml-debug (&rest x)
  (list 'if 'sgml-debug (cons 'message x)))

(require 'psgml-vars)

;;;; Variables

(define-abbrev-table 'sgml-mode-abbrev-table ()
  "Abbrev table in use in SGML mode.")

(eval-and-compile
  (defconst sgml-have-re-char-clases (string-match "[[:alpha:]]" "x")
    "Non-nil if this Emacs supports regexp character classes.
E.g. `[-.[:alnum:]]'."))

(defconst sgml-default-nonsticky (boundp 'text-property-default-nonsticky)
  "Non-nil means use `text-property-default-nonsticky'. locally.
Otherwise put explicit properties.")


(defvar sgml-xml-p nil
  "Is this an XML document?")
(make-variable-buffer-local 'sgml-xml-p)


(defvar sgml-buggy-subst-char-in-region 
  (or (not (boundp 'emacs-minor-version))
      (not (natnump emacs-minor-version))
      (and (eq emacs-major-version 19)
           (< emacs-minor-version 23)))
  "*If non-nil, work around a bug in `subst-char-in-region'.
The bug sets the buffer modified.  If this is set, folding commands
will be slower.")


(defvar sgml-live-element-indicator nil
  "OBSOLETE
If non-nil, indicate current element in mode line.
This may be slow.")



;;[lenst/1998-03-09 19:51:55]
;; sgml-namecase-entity would reflect the NAMECASE ENTITY setting
;; in the SGML Declaration used. Currently only "NO" (encoded as nil)
;; is supported.
(defconst sgml-namecase-entity nil)

;; What letter case to use when inserting entity names when
;; sgml-namecase-entity is non-nil. It's never non-nil so this is kind
;; of useless.
(defvar sgml-entity-insert-case nil)


;;; User settable options:


(defun sgml-parse-colon-path (path)
  "Explode a colon-separated list of directories PATH into a string list."
  (if (null path)
      nil
    (let ((cd-sep ":")
          cd-list (cd-start 0) cd-colon)
      (if (boundp 'path-separator)
          (setq cd-sep path-separator))
      (setq path (concat path cd-sep))
      (while (setq cd-colon (string-match cd-sep path cd-start))
        (setq cd-list
              (nconc cd-list
                     (list (if (= cd-start cd-colon)
                               nil
                             (substitute-in-file-name
                              (substring path cd-start cd-colon))))))
        (setq cd-start (+ cd-colon 1)))
      cd-list)))

(defvar sgml-system-path (sgml-parse-colon-path
			     (or (getenv "SGML_SEARCH_PATH")
				 "."))
  "*List of directories used to look for system identifiers.")
(put 'sgml-system-path 'sgml-type 'file-list)

(defvar sgml-public-map (or (sgml-parse-colon-path (getenv "SGML_PATH"))
			    '("%S" "/usr/local/lib/sgml/%o/%c/%d" ))
  "*Mapping from public identifiers to file names.
This is a list of possible file names.  To find the file for a public
identifier the elements of the list are used one at the time from the
beginning.  If the element is a string a file name is constructed from
the string by substitution of the whole public identifier for %P,
owner for %O, public text class for %C, and public text description
for %D.  The text class will be converted to lower case and the owner
and description will be transliterated according to the variable
`sgml-public-transliterations'.  If the file exists it will be the file
used for the public identifier.  An element can also be a dotted pair
\(regexp . filename), the filename is a string treated as above, but
only if the regular expression, regexp, matches the public
identifier.")
(put 'sgml-public-map 'sgml-type 'list)

(defvar sgml-local-catalogs nil
"*A list of SGML entity catalogs to be searched first when parsing the buffer.
This is used in addition to `sgml-catalog-files',  and `sgml-public-map'.
This variable is automatically local to the buffer.")
(make-variable-buffer-local 'sgml-local-catalogs)
(put 'sgml-local-catalogs 'sgml-type 'file-list)

(defvar sgml-catalog-files (or (delete nil
				       (sgml-parse-colon-path
					(getenv "SGML_CATALOG_FILES")))
			       '("catalog" "/usr/local/lib/sgml/catalog"))
  "*List of catalog entry files.
The files are in the format defined in the SGML Open Draft Technical
Resolution on Entity Management.")
(put 'sgml-catalog-files 'sgml-type 'file-list)

(defvar sgml-ecat-files '("ECAT" "~/sgml/ECAT" "/usr/local/lib/sgml/ECAT")
  "*List of catalog files for PSGML.")
(put 'sgml-ecat-files 'sgml-type 'file-list)

(defvar sgml-local-ecat-files nil
  "*List of local catalog files for PSGML.
Automatically becomes buffer local if set.")

(make-variable-buffer-local 'sgml-local-ecat-files)
(put 'sgml-local-ecat-files 'sgml-type 'file-list)

(defvar sgml-public-transliterations '((? . ?_) (?/ . ?%))
  "*Transliteration for characters that should be avoided in file names.
This is a list of dotted pairs (FROM . TO); where FROM is the the
character to be translated to TO.  This is used when parts of a public
identifier are used to construct a file name.")

(defvar sgml-default-dtd-file nil
  "*This is the default file name for saved DTD.
This is set by sgml-mode from the buffer file name.
Can be changed in the Local variables section of the file.")
(put 'sgml-default-dtd-file 'sgml-type 'string)
(put 'sgml-default-dtd-file 'sgml-desc "Default (saved) DTD File")



;;; Faces used in edit attribute buffer:
(put 'sgml-default 'face 'underline)	; Face for #DEFAULT
(put 'sgml-fixed 'face 'underline)	; Face of #FIXED "..."


;; nsgmls is a free SGML parser in the SP suite available from
;; ftp.jclark.com:pub/sp
;; Its error messages can be parsed by next-error.
;; The -s option suppresses output.

(defvar sgml-validate-command   "nsgmls -s %s %s"
  "*The shell command to validate an SGML document.

This is a `format' control string that by default should contain two
`%s' conversion specifications: the first will be replaced by the
value of `sgml-declaration' \(or the empty string, if nil\); the
second will be replaced by the current buffer's file name \(or the
empty string, if nil\).

If `sgml-validate-files' is non-nil, the format string should contain
one `%s' conversion specification for each element of its result.

If sgml-validate-command is a list, then every element should be a
string.  The strings will be tried in order and %-sequences in the
string will be replaced according to the list below, if the string contains
%-sequences with no replacement value the next string will be tried.

%b means the visited file of the current buffer
%s means the SGML declaration specified in the sgml-declaration variable
%d means the file containing the DOCTYPE declaration, if not in the buffer
")
(make-variable-buffer-local 'sgml-validate-command)

(defvar sgml-xml-validate-command
  (concat "/bin/sh -c \"SP_CHARSET_FIXED=YES SP_ENCODING=XML "
          "nsgmls -wxml -mdeclaration/xml.soc -gues %s %s\"")
  "*The default for `sgml-validate-command' in XML mode.")

(defvar sgml-validate-files nil
  "If non-nil, a function of no arguments that returns a list of file names.
These file names will serve as the arguments to the `sgml-validate-command'
format control string instead of the defaults.")

(defvar sgml-validate-error-regexps
  '((".*:\\(.+\\):\\([0-9]+\\):\\([0-9]+\\):[EXW]: " 1 2 3)
    ("\\(error\\|warning\\) at \\([^,]+\\), line \\([0-9]+\\)" 2 3)
    ("\n[a-zA-Z]?:?[^0-9 \n\t:]+:[ \t]*\\([^ \n\t:]+\\):\
\\([0-9]+\\):\\(\\([0-9]+\\)[: \t]\\)?" 1 2 4))
  "Alist of regexps to recognize error messages from `sgml-validate'.
See `compilation-error-regexp-alist'.")

(defvar sgml-declaration nil
  "*If non-nil, the name of the SGML declaration file.")
(put 'sgml-declaration 'sgml-type 'file-or-nil)

(defvar sgml-xml-declaration nil
  "*If non-nil, the name of the SGML declaration for XML files.")
(put 'sgml-xml-declaration 'sgml-type 'file-or-nil)

(defvar sgml-mode-hook nil
  "A hook or list of hooks to be run when entering sgml-mode")

(defvar sgml-show-context-function
  #'sgml-show-context-standard
  "*Function to called to show context of and element.
Should return a string suitable form printing in the echo area.")

(defconst sgml-file-options
  '(
    sgml-omittag
    sgml-shorttag
    sgml-namecase-general
    sgml-general-insert-case
    sgml-minimize-attributes
    sgml-always-quote-attributes
    sgml-indent-step
    sgml-indent-data
    sgml-doctype
    sgml-parent-document
    sgml-default-dtd-file
    sgml-exposed-tags
    sgml-local-catalogs
    sgml-local-ecat-files
    )
  "Options for the current file, can be saved or set from menu."
  )

(defconst sgml-user-options
  '(
    sgml-set-face
    sgml-auto-activate-dtd
    sgml-offer-save
    sgml-tag-region-if-active
    sgml-normalize-trims
    sgml-auto-insert-required-elements
    sgml-balanced-tag-edit
    sgml-omittag-transparent
    sgml-leave-point-after-insert
    sgml-insert-missing-element-comment
    sgml-insert-end-tag-on-new-line
    sgml-warn-about-undefined-elements
    sgml-warn-about-undefined-entities
    sgml-ignore-undefined-elements
    sgml-recompile-out-of-date-cdtd
    sgml-default-doctype-name
    sgml-declaration
    sgml-validate-command
    sgml-markup-faces
    sgml-system-identifiers-are-preferred
    sgml-trace-entity-lookup
    sgml-public-map
    sgml-catalog-files
    sgml-ecat-files
    sgml-general-insert-case
    )
  "User options that can be saved or set from menu."
  )

;;; Internal variables

(defvar sgml-validate-command-history nil
  "The minibuffer history list for `sgml-validate''s COMMAND argument.")

(defvar sgml-active-dtd-indicator nil
  "Displayed in the mode line")


;;;; User options handling

(defun sgml-variable-description (var)
  (or (get var 'sgml-desc)
      (let ((desc (symbol-name var)))
	(if (string= "sgml-" (substring desc 0 5))
	    (setq desc (replace-regexp-in-string "-" " " (substring desc 5))))
	(capitalize desc))))

(defun sgml-variable-type (var)
  (or (get var 'sgml-type)
      (if (memq (symbol-value var) '(t nil))
	  'toggle)))

(defun sgml-set-local-variable (var val)
  "Set the value of variable VAR to VAL in buffer and local variables list."
  (set (make-local-variable var) val)
  (save-excursion
    (let ((prefix "")
	  (suffix "")
	  (case-fold-search t))
      (goto-char (max (point-min) (- (point-max) 3000)))
      (cond ((search-forward "Local Variables:" nil t)
	     (setq suffix (buffer-substring (point)
					    (save-excursion (end-of-line 1)
							    (point))))
	     (setq prefix
		   (buffer-substring (save-excursion (beginning-of-line 1)
						     (point))
				     (match-beginning 0))))
	    (t
	     (goto-char (point-max))
	     (unless (bolp)
	       (insert ?\n))
	     (insert
	      "<!-- Keep this comment at the end of the file\n"
	      "Local variables:\n"
	      (if sgml-xml-p
		  "mode: xml\n"
		"mode: sgml\n")
	      "End:\n"
	      "-->\n")
	     (forward-line -3)))
      (let* ((endpos (save-excursion
		       (search-forward (format "\n%send:" prefix))))
	     (varpos (search-forward (format "\n%s%s:" prefix var) endpos t)))
	(cond (varpos
	       (delete-region (point)
			      (save-excursion (end-of-line 1)
					      (point)))
	       (insert (format "%S" val) suffix))
	      (t
	       (goto-char endpos)
	       (beginning-of-line 1)
	       (insert prefix (format "%s:%S" var val) suffix ?\n)))))))

(defun sgml-valid-option (var)
  (let ((type (sgml-variable-type var))
	(val (symbol-value var)))
    (cond ((eq 'string type)
	   (stringp val))
	  ((eq 'list-or-string type)
	   (or (stringp val)
	       (consp val)))
	  (t
	   t))))

(defun sgml-save-options ()
  "Save user options for SGML mode that have buffer local values."
  (interactive)
  (dolist (var sgml-file-options)
    (when (sgml-valid-option var)
      (sgml-set-local-variable var (symbol-value var)))))


;;;; SGML mode: template functions

(defun sgml-markup (entry text)
  (cons entry
	`(lambda ()
           (interactive)
           (sgml-insert-markup ,text))))

(defun sgml-insert-markup (text)
  (let ((end (sgml-mouse-region))
	before after
	old-text)
    (when end
      (setq old-text (buffer-substring (point) end))
      (delete-region (point) end))
    (setq before (point))
    (if (stringp text)
	(insert text)
      (eval text))
    (setq after (point))
    (goto-char before)
    (when (search-forward "\r" after t)
      (delete-char -1))
    (when old-text (insert old-text))))

(defun sgml-mouse-region ()
  (let (start end)
    (cond
     ((featurep 'xemacs)
      (cond
       ((null (mark-marker)) nil)
       (t (setq start (region-beginning)
 		end (region-end)))))
     ((and transient-mark-mode
	   mark-active)
      (setq start (region-beginning)
	    end (region-end)))
     ((and mouse-secondary-overlay
	   (eq (current-buffer)
	       (overlay-buffer mouse-secondary-overlay)))
      (setq start (overlay-start mouse-secondary-overlay)
	    end (overlay-end mouse-secondary-overlay))
      (delete-overlay mouse-secondary-overlay)))
    (when start
      (goto-char start))
    end))


;;;; SGML mode: indentation

(defun sgml-indent-or-tab ()
  "Indent line in proper way for current major mode."
  (interactive)
  (if (null sgml-indent-step)
      (insert-tab)
    (funcall indent-line-function)))

;;;; Bug reporting

(eval-and-compile
  (autoload 'reporter-submit-bug-report "reporter"))

(defun sgml-submit-bug-report ()
  "Submit via mail a bug report on PSGML."
  (interactive)
  (and (y-or-n-p "Do you really want to submit a report on PSGML? ")
       (reporter-submit-bug-report
	psgml-maintainer-address
	(concat "psgml.el <ELPA>")
	(list
	 'major-mode
	 'sgml-always-quote-attributes
	 'sgml-auto-activate-dtd
	 'sgml-auto-insert-required-elements
	 'sgml-balanced-tag-edit
	 'sgml-catalog-files
	 'sgml-declaration
	 'sgml-doctype
	 'sgml-ecat-files
	 'sgml-indent-data
	 'sgml-indent-step
	 'sgml-leave-point-after-insert
	 'sgml-local-catalogs
	 'sgml-local-ecat-files
	 'sgml-markup-faces
	 'sgml-minimize-attributes
	 'sgml-normalize-trims
	 'sgml-omittag
	 'sgml-omittag-transparent
	 'sgml-parent-document
	 'sgml-public-map
	 'sgml-set-face
	 'sgml-shorttag
	 'sgml-namecase-general
	 'sgml-tag-region-if-active
         'sgml-use-text-properties
	 ))))

;;;; SGML mode: syntax table

(defvar sgml-mode-syntax-table
  (let ((s (copy-syntax-table text-mode-syntax-table)))
    (modify-syntax-entry ?< "."  s)
    (modify-syntax-entry ?> "."  s)
    s))


;;;; SGML mode: keys and menus

(defvar sgml-mode-map
  (let ((map (make-sparse-keymap))

        ;; FIXME: Are these two explicit prefix map settings really needed?
        (f-map (make-sparse-keymap))
        (u-map (make-sparse-keymap)))
    (define-key map "\C-c\C-f" f-map)
    (define-key map "\C-c\C-u" u-map)

    ;; Key commands
    (define-key map "\t"    'sgml-indent-or-tab)
    ;; (define-key map "<" 	  'sgml-insert-tag)
    (define-key map ">"     'sgml-close-angle)
    (define-key map "/"     'sgml-slash)
    (define-key map "\C-c#"    'sgml-make-character-reference)
    (define-key map "\C-c-"    'sgml-untag-element)
    (define-key map "\C-c+"    'sgml-insert-attribute)
    (define-key map "\C-c/"    'sgml-insert-end-tag)
    (define-key map "\C-c<"    'sgml-insert-tag)
    (define-key map "\C-c="    'sgml-change-element-name)
    (define-key map "\C-c\C-a" 'sgml-edit-attributes)
    (define-key map "\C-c\C-c" 'sgml-show-context)
    (define-key map "\C-c\C-d" 'sgml-next-data-field)
    (define-key map "\C-c\C-e" 'sgml-insert-element)
    (define-key map "\C-c\C-f\C-e" 'sgml-fold-element)
    (define-key map "\C-c\C-f\C-r" 'sgml-fold-region)
    (define-key map "\C-c\C-f\C-s" 'sgml-fold-subelement)
    (define-key map "\C-c\C-f\C-x" 'sgml-expand-element)
    (define-key map "\C-c\C-i" 'sgml-add-element-to-element)
    (define-key map "\C-c\C-k" 'sgml-kill-markup)
    (define-key map "\C-c\r"   'sgml-split-element)
    (define-key map "\C-c\C-n" 'sgml-up-element)
    (define-key map "\C-c\C-o" 'sgml-next-trouble-spot)
    (define-key map "\C-c\C-p" 'sgml-load-doctype)
    (define-key map "\C-c\C-q" 'sgml-fill-element)
    (define-key map "\C-c\C-r" 'sgml-tag-region)
    (define-key map "\C-c\C-s" 'sgml-show-structure)
    ;;(define-key map "\C-c\C-t" 'sgml-list-valid-tags)
    (define-key map "\C-c\C-t" 'sgml-show-current-element-type)
    (define-key map "\C-c\C-u\C-a" 'sgml-unfold-all)
    (define-key map "\C-c\C-u\C-d" 'sgml-custom-dtd)
    (define-key map "\C-c\C-u\C-e" 'sgml-unfold-element)
    (define-key map "\C-c\C-u\C-l" 'sgml-unfold-line)
    (define-key map "\C-c\C-u\C-m" 'sgml-custom-markup)
    (define-key map "\C-c\C-v" 'sgml-validate)
    (define-key map "\C-c\C-w" 'sgml-what-element)
    (define-key map "\C-c\C-z" 'sgml-trim-and-leave-element)

    (define-key map "\e\C-a"   'sgml-beginning-of-element)
    (define-key map "\e\C-e"   'sgml-end-of-element)
    (define-key map "\e\C-f"   'sgml-forward-element)
    (define-key map "\e\C-b"   'sgml-backward-element)
    (define-key map "\e\C-d"   'sgml-down-element)
    (define-key map "\e\C-u"   'sgml-backward-up-element)
    (define-key map "\e\C-k"   'sgml-kill-element)
    (define-key map "\e\C-@"   'sgml-mark-element)
    ;;(define-key map [?\M-\C-\ ] 'sgml-mark-element)
    (define-key map [(meta control h)] 'sgml-mark-current-element)
    (define-key map "\e\C-t"   'sgml-transpose-element)
    (define-key map "\M-\t"    'sgml-complete)

    (if (featurep 'xemacs)
        (define-key map [button3] 'sgml-tags-menu)
      (define-key map [?\M-\C-\ ] 'sgml-mark-element)

      ;;(define-key map [S-mouse-3] 'sgml-tags-menu)
      (define-key map [S-mouse-3] 'sgml-right-menu))

    map)
  "Main keymap for PSGML mode.")

;;;; Menu bar

(easy-menu-define
 sgml-main-menu sgml-mode-map "Main menu"
 '("SGML"
   ["Load Doctype"              sgml-load-doctype               t]
   ("DTD Info"
    ["Describe DTD"             sgml-describe-dtd               t]
    ["Describe element type"	sgml-describe-element-type	t]
    ["Describe entity"		sgml-describe-entity		t]
    ["List elements" 		sgml-list-elements 		t]
    ["List attributes"          sgml-list-attributes 		t]
    ["List terminals" 		sgml-list-terminals 		t]
    ["List content elements" 	sgml-list-content-elements 	t]
    ["List occur in elements" 	sgml-list-occur-in-elements 	t])
   ("Insert DTD" :filter sgml-compute-insert-dtd-items)
   ("Insert Markup"
    ["Insert Element"           sgml-element-menu	t]
    ["Insert Start-Tag"         sgml-start-tag-menu	t]
    ["Insert End-Tag"           sgml-end-tag-menu	t]
    ["End Current Element"	sgml-insert-end-tag     t]
    ["Tag Region"               sgml-tag-region-menu	t]
    ["Insert Attribute"         sgml-attrib-menu	t]
    ["Insert Entity"            sgml-entities-menu	t]
    ["Add Element to Element"	sgml-add-element-menu	t])
   ("Custom markup" :filter sgml-compute-custom-markup-items)
   "--"
   ["Show Context"              sgml-show-context       t]
   ["What Element"              sgml-what-element       t]
   ["List Valid Tags"           sgml-list-valid-tags    t]
   ["Validate"                  sgml-validate           t]
   ["Show Structure"            sgml-show-structure     t]
   "--"
   ("Move"
    ["Next trouble spot"        sgml-next-trouble-spot  t]
    ["Next data field"          sgml-next-data-field    t]
    ["Forward element"          sgml-forward-element    t]
    ["Backward element"         sgml-backward-element   t]
    ["Up element"               sgml-up-element         t]
    ["Down element"             sgml-down-element       t]
    ["Backward up element"      sgml-backward-up-element        t]
    ["Beginning of element"     sgml-beginning-of-element       t]
    ["End of element"           sgml-end-of-element     t])
   ("View"
    ["Fold Element"             sgml-fold-element       t]
    ["Fold Subelement"          sgml-fold-subelement    t]
    ["Unfold Line"              sgml-unfold-line        t]
    ["Unfold Element"           sgml-unfold-element     t]
    ["Expand"                   sgml-expand-element     t]
    ["Fold Region"              sgml-fold-region        t]
    ["Unfold All"               sgml-unfold-all         t]
    ["Hide Tags"		sgml-hide-tags		t]
    ["Hide Attributes"          sgml-hide-attributes    t]
    ["Show All Tags"            sgml-show-tags  t])
   "--"
   ["Normalize Document"        sgml-normalize	t]
   ["Normalize Element"		sgml-normalize-element t]
   ["Expand All Short References" sgml-expand-all-shortrefs (not sgml-xml-p)]
   ["Expand Entity Reference"	sgml-expand-entity-reference t]
   ["Make Character Reference"	sgml-make-character-reference t]
   ["Unmake Character Reference"	(sgml-make-character-reference t) t]
   ["Fill Element"		sgml-fill-element t]
   ["Change Element Name..."	sgml-change-element-name t]
   ["Edit Attributes..."	sgml-edit-attributes t]
   ["Kill Markup"		sgml-kill-markup t]
   ["Kill Element"		sgml-kill-element t]
   ["Untag Element"		sgml-untag-element t]
   ["Trim and leave element"	sgml-trim-and-leave-element t]
   ;; FIXME: These 2 commands used to be in psgml-charent, but I wasn't able to
   ;; get their author's paperwork (Steinar Bang and Falch Hurtigtrykk).
   ;; We should rewrite them, but I can't understand DTD enough to know how
   ;; it's really supposed to work.  --Stef
   ;;["Decode Character Entities"  sgml-charent-to-display-char t]
   ;;["Encode Characters"		sgml-display-char-to-charent t]
   "--"
   ("File Options"   "---")
   ("User Options"   "---")
   ["Reset Buffer"	normal-mode t]
   ["Submit Bug Report"  sgml-submit-bug-report t]
   ))


(defun sgml-options-menu-items (vars)
  (mapcar (lambda (var)
            (let ((desc (format "%s [%s]"
                                (sgml-variable-description var)
                                (sgml-option-value-indicator var)))
                  (type (sgml-variable-type var)))
              (cond ((consp type)
                     (cons desc
                           (mapcar (lambda (c)
                                     (vector
                                      (if (consp c) (car c) (format "%s" c))
                                      `(setq ,var ',(if (consp c) (cdr c) c))
                                      t))
                                   type)))
                    (t
                     (vector desc `(sgml-do-set-option ',var) t)))))
	  vars))

(defun sgml-option-value-indicator (var)
  (let ((type (sgml-variable-type var))
	(val (symbol-value var)))
    (cond
     ((eq type 'toggle)
      (if val "Yes" "No"))
     ((eq type 'string)
      (if (stringp val)
	  (substring val 0 (min (length val) 4))
	"-"))
     ((and (atom type) val)
      "...")
     ((consp type)
      (or (car (rassq val type))
	  val))
     (t
      "-"))))

(defvar sgml-last-options-menu-values ())

(defun sgml-any-option-changed (oldvalues vars)
  (not (cl-loop for val in oldvalues
                for var in vars
                always (eq val (symbol-value var)))))

;; FIXME: Use a keymap filter!
(defun sgml-update-options-menu (menuname option-vars &optional save-func)
  (let ((last-values (assoc menuname sgml-last-options-menu-values)))
    (when (or (null last-values)
              (sgml-any-option-changed (cdr last-values)
                                       option-vars))
      (condition-case-unless-debug err
          (easy-menu-change '("SGML") menuname
                      (nconc (sgml-options-menu-items option-vars)
                             (if save-func
                                 (list "---"
                                       (vector (format "Save %s" menuname)
                                               save-func t)))))
        (error
         (message "Error in update menu: %s" err)))
      (unless last-values
        (setq last-values (cons menuname nil))
        (push last-values sgml-last-options-menu-values))
      (setf (cdr last-values) (mapcar (function symbol-value) option-vars)))))


(defun sgml-update-all-options-menus ()
  (sgml-update-options-menu "File Options" sgml-file-options
			    'sgml-save-options)
  (sgml-update-options-menu "User Options" sgml-user-options)
  nil)

(defun sgml-compute-insert-dtd-items (&optional _menu)
  (if (null sgml-custom-dtd)
      '(["-- No custom entries --" nil :enable nil])
    (mapcar (lambda (e)
              (vector (car e)
                      `(sgml-doctype-insert ,(cadr e) ',(cddr e))
                      t))
            sgml-custom-dtd)))

(defun sgml-compute-custom-markup-items (&optional _menu)
  (if (null sgml-custom-markup)
      '(["-- No custom entries --" nil :enable nil])
    (mapcar (lambda (e)
              (vector (car e)
                      `(sgml-insert-markup  ,(cadr e))
                      t))
             sgml-custom-markup)))


;;;; Post command hook

(defvar sgml-auto-activate-dtd-tried nil)
(make-variable-buffer-local 'sgml-auto-activate-dtd-tried)

(defvar sgml-buffer-parse-state nil
  "If the buffers DTD has been activated this contains the parser state.
The parser state has been created with `sgml-make-pstate' and contains
the information about the DTD and the parse tree.  This parse state is
actually only the state that persists between commands.")
(make-variable-buffer-local 'sgml-buffer-parse-state)

(eval-and-compile			; Interface to psgml-parse
  (dolist (fun '(sgml-need-dtd sgml-update-display
                 sgml-fontify-buffer
                 sgml-subst-expand
                 sgml-declaration))
    (autoload fun "psgml-parse")))


(defun sgml-command-post ()
  (when (and (null sgml-buffer-parse-state)
	     sgml-auto-activate-dtd
	     (null sgml-auto-activate-dtd-tried)
	     (not (zerop (buffer-size)))
	     (looking-at ".*<"))
    (setq sgml-auto-activate-dtd-tried t)
    (with-demoted-errors "PSGML post command: %S"
      (sgml-need-dtd)
      (sgml-fontify-buffer 0)))
  (when sgml-buffer-parse-state
    (sgml-update-display)))


;;;; SGML mode: major mode definition

;;; This section is mostly from sgml-mode by James Clark.

;;;###autoload
(define-derived-mode sgml-mode nil "PSGML"
  "Major mode for editing SGML.
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
\\{sgml-mode-map}"
  (setq sgml-xml-p nil)

  ;; A start or end tag by itself on a line separates a paragraph.
  ;; This is desirable because SGML discards a newline that appears
  ;; immediately after a start tag or immediately before an end tag.

  (set (make-local-variable 'paragraph-separate)
       (if sgml-have-re-char-clases
           "^[ \t\n]*$\\|\
^[ \t]*</?\\([_[:alpha:]]\\([-:._[:alnum:]= \t\n]\\|\
\"[^\"]*\"\\|'[^']*'\\)*\\)?>$"
         "^[ \t\n]*$\\|\
^[ \t]*</?\\([_A-Za-z]\\([-:._A-Za-z0-9= \t\n]\\|\
\"[^\"]*\"\\|'[^']*'\\)*\\)?>$"))
  (set (make-local-variable 'paragraph-start)
       paragraph-separate)

  (set-syntax-table sgml-mode-syntax-table)
  (set (make-local-variable 'comment-start) "<!-- ")
  (set (make-local-variable 'comment-end) " -->")
  (set (make-local-variable 'comment-indent-function) 'sgml-comment-indent)
  ;; This will allow existing comments within declarations to be
  ;; recognized.  [Does not work well with auto-fill, Lst/940205]
  ;;(setq comment-start-skip "--[ \t]*")
  (set (make-local-variable 'comment-start-skip) "<!--[ \t]*")
  ;; Added for psgml:
  (set (make-local-variable 'indent-line-function) 'sgml-indent-line)
  (when (set (make-local-variable 'sgml-default-dtd-file)
             (sgml-default-dtd-file))
    (unless (file-exists-p sgml-default-dtd-file)
      (setq sgml-default-dtd-file nil)))
  ;; This doesn't DTRT with Emacs 21.1 newcomment -- intermediate lines
  ;; are prefixed by `!--'.  -- fx
  ;;(set (make-local-variable 'comment-style) 'multi-line)
  (when sgml-default-nonsticky
    (make-local-variable 'text-property-default-nonsticky)
    ;; see `sgml-set-face-for':
    (add-to-list 'text-property-default-nonsticky '(face . t)))
  (add-hook 'post-command-hook #'sgml-command-post 'append 'local)
  (add-hook 'activate-menubar-hook #'sgml-update-all-options-menus
	    nil 'local)
  (add-hook 'which-func-functions #'sgml-current-element-name nil t)
  (easy-menu-add sgml-main-menu))

;; It would be nice to generalize the `auto-mode-interpreter-regexp'
;; machinery so that we could select xml-mode on the basis of the
;; leading xml PI.  -- fx

;;;###autoload
(define-derived-mode xml-mode sgml-mode "XML"
  "Major mode for editing XML, specialized from SGML mode.
Sets various variables appropriately for XML.

Can be used without a DTD.  In that case, warnings about undefined
elements and entities are suppressed and various commands' behaviour
is modified to account for the lack of information.  For instance, the
element names offered for selection or completion are those in the
parse of the document, but other names may be entered.

Note that without a DTD, indenting lines will only work if
`sgml-indent-data' is non-nil."
  (setq sgml-xml-p t)
  ;; XML-friendly settings
  (setq sgml-omittag nil)
  (setq sgml-shorttag nil)
  (setq sgml-namecase-general nil)
  (setq sgml-minimize-attributes nil)
  (setq sgml-always-quote-attributes t)
  (setq sgml-validate-command sgml-xml-validate-command)
  (set (make-local-variable 'sgml-declaration) sgml-xml-declaration))


(defun sgml-default-dtd-file ()
  (and (buffer-file-name)
       (let ((base (file-name-nondirectory (buffer-file-name))))
	 (concat
	  (cond ((string-match "\\.[^.]+$" base)
		 (substring base 0 (match-beginning 0)))
		(t
		 base))
	  ".ced"))))

(defun sgml-comment-indent ()
  (if (and (looking-at "--")
	   (not (and (eq (char-after (1- (point))) ?!)
		     (eq (char-after (- (point) 2)) ?<))))
      (progn
	(skip-chars-backward " \t")
	(max comment-column (1+ (current-column))))
    0))

(defconst sgml-start-tag-regex
  (if sgml-have-re-char-clases
      "<[_[:alpha:]]\\([-:.[:alnum:]= \n\t]\\|\"[^\"]*\"\\|'[^']*'\\)*"
    "<[_A-Za-z]\\([-:.A-Za-z0-9= \n\t]\\|\"[^\"]*\"\\|'[^']*'\\)*")
  "Regular expression that matches a non-empty start tag.
Any terminating > or / is not matched.")

(defvar sgml-mode-markup-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?< "(>" st)
    (modify-syntax-entry ?> ")<" st)
    (modify-syntax-entry ?- "_ 1234" st)
    (modify-syntax-entry ?\' "\"" st)
    st)
  "Syntax table used for scanning SGML markup.")

(defvar sgml-angle-distance 4000
  "*If non-nil, is the maximum distance to search for matching <.")

(defun sgml-close-angle (arg)
  "Insert > and display matching <."
  (interactive "p")
  (insert-char ?> arg)
  (if (> arg 0)
      (let ((oldpos (point))
	    (blinkpos))
	(save-excursion
	  (save-restriction
	    (if sgml-angle-distance
		(narrow-to-region (max (point-min)
				       (- (point) sgml-angle-distance))
				  oldpos))
	    ;; See if it's the end of a marked section.
	    (and (> (- (point) (point-min)) 3)
		 (eq (char-after (- (point) 2)) ?\])
		 (eq (char-after (- (point) 3)) ?\])
		 (re-search-backward (if sgml-have-re-char-clases
					 "<!\\[\\(-?[[:alnum:]. \t\n&;]\\|\
--\\([^-]\\|-[^-]\\)*--\\)*\\["
				       "<!\\[\\(-?[A-Za-z0-9. \t\n&;]\\|\
--\\([^-]\\|-[^-]\\)*--\\)*\\[")
				     (point-min)
				     t)
		 (let ((msspos (point)))
		   (if (and (search-forward "]]>" oldpos t)
			    (eq (point) oldpos))
		       (setq blinkpos msspos))))
	    ;; This handles cases where the > ends one of the following:
	    ;; markup declaration starting with <! (possibly including a
	    ;; declaration subset); start tag; end tag; SGML declaration.
	    (if blinkpos
		()
	      (goto-char oldpos)
	      (condition-case ()
		  (let ((oldtable (syntax-table))
			(parse-sexp-ignore-comments t))
		    (unwind-protect
			(progn
			  (set-syntax-table sgml-mode-markup-syntax-table)
			  (setq blinkpos (scan-sexps oldpos -1)))
		      (set-syntax-table oldtable)))
		(error nil))
	      (and blinkpos
		   (goto-char blinkpos)
		   (or
		    ;; Check that it's a valid delimiter in context.
		    (not (looking-at
			  (if sgml-have-re-char-clases
			      "<\\(\\?\\|/?[[:alpha:]>]\\|!\\([[[:alpha:]]\\|--\\)\\)"
			    "<\\(\\?\\|/?[A-Za-z>]\\|!\\([[A-Za-z]\\|--\\)\\)")))
		    ;; Check that it's not a net-enabling start tag
		    ;; nor an unclosed start-tag.
		    (looking-at (concat sgml-start-tag-regex "[/<]"))
		    ;; Nor an unclosed end-tag.
		    (looking-at (if sgml-have-re-char-clases
				    "</[[:alpha:]][-:.[:alnum:]]*[ \t]*<"
				  "</[A-Za-z][-:.A-Za-z0-9]*[ \t]*<")))
		   (setq blinkpos nil)))
	    (if blinkpos
		()
	      ;; See if it's the end of a processing instruction.
	      (goto-char oldpos)
	      (if (search-backward "<?" (point-min) t)
		  (let ((pipos (point)))
		    (if (and (search-forward ">" oldpos t)
			     (eq (point) oldpos))
			(setq blinkpos pipos))))))
	  (if blinkpos
	      (progn
		(goto-char blinkpos)
		(if (pos-visible-in-window-p)
		    (sit-for 1)
		  (message "Matches %s"
			   (buffer-substring blinkpos
					     (progn (end-of-line)
						    (point)))))))))))

;;; I doubt that null end tags are used much for large elements,
;;; so use a small distance here.
(defvar sgml-slash-distance 1000
  "*If non-nil, is the maximum distance to search for matching /.")

(defun sgml-slash (arg)
  "Insert / and display any previous matching /.
Two /s are treated as matching if the first / ends a net-enabling
start tag, and the second / is the corresponding null end tag."
  (interactive "p")
  (insert-char ?/ arg)
  (if (> arg 0)
      (let ((oldpos (point))
	    (blinkpos)
	    (level 0))
	(save-excursion
	  (save-restriction
	    (if sgml-slash-distance
		(narrow-to-region (max (point-min)
				       (- (point) sgml-slash-distance))
				  oldpos))
	    (if (and (re-search-backward sgml-start-tag-regex (point-min) t)
		     (eq (match-end 0) (1- oldpos)))
		()
	      (goto-char (1- oldpos))
	      (while (and (not blinkpos)
			  (search-backward "/" (point-min) t))
		(let ((tagend (save-excursion
				(if (re-search-backward sgml-start-tag-regex
							(point-min) t)
				    (match-end 0)
				  nil))))
		  (if (eq tagend (point))
		      (if (eq level 0)
			  (setq blinkpos (point))
			(setq level (1- level)))
		    (setq level (1+ level)))))))
	  (if blinkpos
	      (progn
		(goto-char blinkpos)
		(if (pos-visible-in-window-p)
		    (sit-for 1)
		  (message "Matches %s"
			   (buffer-substring (progn
					       (beginning-of-line)
					       (point))
					     (1+ blinkpos))))))))))

(eval-and-compile
  (autoload 'compile-internal "compile" ""))

(defun sgml-default-validate-command ()
  (cond
   ((consp sgml-validate-command)
    (let ((validate-subst
	   (list
	    (cons ?b (and (buffer-file-name)
			  (file-name-nondirectory (buffer-file-name))))
	    (cons ?s (sgml-declaration))
	    (cons ?v sgml-declaration)
	    (cons ?d sgml-doctype))))
      (cl-loop for template in sgml-validate-command
	    thereis
	    (sgml-subst-expand template validate-subst))))
   (t
    (apply #'format sgml-validate-command
	   (if sgml-validate-files
	       (funcall sgml-validate-files)
	     (list (or sgml-declaration "")
		   (let ((name (buffer-file-name)))
		     (if name
			 (file-name-nondirectory name)
		       ""))))))))

(defun sgml-validate (command)
  "Validate an SGML document.
Runs COMMAND, a shell command, in a separate process asynchronously
with output going to the buffer *compilation*.
You can then use the command \\[next-error] to find the next error message
and move to the line in the SGML document that caused it."
  (interactive
   (list (read-from-minibuffer "Validate command: "
			       (sgml-default-validate-command)
			       nil nil 'sgml-validate-command-history)))
  (if sgml-offer-save
      (save-some-buffers nil nil))
  (compile-internal command "No more errors" "SGML validation"
		    nil
		    sgml-validate-error-regexps))


;;;; Autoloads and hooks

(autoload 'sgml-doctype-insert "psgml-edit"
	  nil
	  nil nil)
(autoload 'sgml-indent-line "psgml-edit" nil)
(autoload 'sgml-element-endable-p "psgml-edit" nil)
(autoload 'sgml-do-set-option "psgml-edit" nil)

;;; Generated by sgml-build-autoloads

(autoload 'sgml-load-dtd "psgml-parse" "Load a saved DTD from FILE." t)
(autoload 'sgml-show-or-clear-log "psgml-parse" "Show the *SGML LOG* buffer if it is not showing, or clear and
remove it if it is showing." t)
(autoload 'sgml-load-doctype "psgml-parse" "Load the documents DTD.
Either from parent document or by parsing the document prolog." t)
(autoload 'sgml-parse-prolog "psgml-parse" "Parse the document prolog to learn the DTD." t)
(autoload 'sgml-beginning-of-element "psgml-edit" "Move to after the start-tag of the current element.
If the start-tag is implied, move to the start of the element." t)
(autoload 'sgml-end-of-element "psgml-edit" "Move to before the end-tag of the current element." t)
(autoload 'sgml-backward-up-element "psgml-edit" "Move backward out of this element level.
That is move to before the start-tag or where a start-tag is implied." t)
(autoload 'sgml-up-element "psgml-edit" "Move forward out of this element level.
That is move to after the end-tag or where an end-tag is implied." t)
(autoload 'sgml-forward-element "psgml-edit" "Move forward over next element." t)
(autoload 'sgml-backward-element "psgml-edit" "Move backward over previous element at this level.
With implied tags this is ambiguous." t)
(autoload 'sgml-down-element "psgml-edit" "Move forward and down one level in the element structure." t)
(autoload 'sgml-kill-element "psgml-edit" "Kill the element following the cursor." t)
(autoload 'sgml-transpose-element "psgml-edit" "Interchange element before point with element after point, leave point after." t)
(autoload 'sgml-mark-element "psgml-edit" "Set mark after next element." t)
(autoload 'sgml-mark-current-element "psgml-edit" "Set mark at end of current element, and leave point before current element." t)
(autoload 'sgml-change-element-name "psgml-edit" "Replace the name of the current element with a new name.
Eventual attributes of the current element will be translated if
possible." t)
(autoload 'sgml-untag-element "psgml-edit" "Remove tags from current element." t)
(autoload 'sgml-kill-markup "psgml-edit" "Kill next tag, markup declaration or process instruction." t)
(autoload 'sgml-fold-region "psgml-edit" "Hide (or if prefixarg unhide) region.
If called from a program first two arguments are start and end of
region. And optional third argument true unhides." t)
(autoload 'sgml-fold-element "psgml-edit" "Fold the lines comprising the current element, leaving the first line visible.
This uses the selective display feature." t)
(autoload 'sgml-fold-subelement "psgml-edit" "Fold all elements current elements content, leaving the first lines visible.
This uses the selective display feature." t)
(autoload 'sgml-unfold-line "psgml-edit" "Show hidden lines in current line." t)
(autoload 'sgml-unfold-element "psgml-edit" "Show all hidden lines in current element." t)
(autoload 'sgml-expand-element "psgml-edit" "As sgml-fold-subelement, but unfold first." t)
(autoload 'sgml-unfold-all "psgml-edit" "Show all hidden lines in buffer." t)
(autoload 'sgml-next-data-field "psgml-edit" "Move forward to next point where data is allowed." t)
(autoload 'sgml-next-trouble-spot "psgml-edit" "Move forward to next point where something is amiss with the structure." t)
(autoload 'sgml-list-valid-tags "psgml-edit" "Display a list of the contextually valid tags." t)
(autoload 'sgml-show-context "psgml-edit" "Display where the cursor is in the element hierarchy." t)
(autoload 'sgml-what-element "psgml-edit" "Display what element is under the cursor." t)
(autoload 'sgml-insert-tag "psgml-edit" "Insert a tag, reading tag name in minibuffer with completion.
 If sgml-leave-point-after-insert is t, the point is left after the
inserted tag(s), unless the element has some required content. If
sgml-leave-point-after-insert is nil the point is left after the first
tag inserted." t)
(autoload 'sgml-insert-element "psgml-edit" "Reads element name from minibuffer and inserts start and end tags." t)
(autoload 'sgml-tag-region "psgml-edit" "Reads element name from minibuffer and inserts start and end tags." t)
(autoload 'sgml-insert-end-tag "psgml-edit" "Insert end-tag for the current open element." t)
(autoload 'sgml-insert-attribute "psgml-edit" "Read attribute name and value from minibuffer and insert attribute spec." t)
(autoload 'sgml-split-element "psgml-edit" "Split the current element at point.
If repeated, the containing element will be split before the beginning
of then current element." t)
(autoload 'sgml-custom-dtd "psgml-edit" "Insert a DTD declaration from the sgml-custom-dtd alist." t)
(autoload 'sgml-custom-markup "psgml-edit" "Insert markup from the sgml-custom-markup alist." t)
(autoload 'sgml-tags-menu "psgml-edit" "Pop up a menu with valid tags and insert the chosen tag.
If the variable sgml-balanced-tag-edit is t, also inserts the
corresponding end tag. If sgml-leave-point-after-insert is t, the point
is left after the inserted tag(s), unless the element has some required
content.  If sgml-leave-point-after-insert is nil the point is left
after the first tag inserted." t)
(autoload 'sgml-element-menu "psgml-edit" "Pop up a menu with valid elements and insert choice.
If sgml-leave-point-after-insert is nil the point is left after the first
tag inserted." t)
(autoload 'sgml-add-element-menu "psgml-edit" nil t)
(autoload 'sgml-start-tag-menu "psgml-edit" "Pop up a menu with valid start-tags and insert choice." t)
(autoload 'sgml-end-tag-menu "psgml-edit" "Pop up a menu with valid end-tags and insert choice." t)
(autoload 'sgml-tag-region-menu "psgml-edit" "Pop up a menu with valid elements and tag current region with the choice." t)
(autoload 'sgml-entities-menu "psgml-edit" nil t)
(autoload 'sgml-attrib-menu "psgml-edit" "Pop up a menu of the attributes of the current element
\(or the element with start-tag before point)." t)
(autoload 'sgml-right-menu "psgml-edit" "Pop up a menu with valid tags and insert the choosen tag.
If the variable sgml-balanced-tag-edit is t, also inserts the
corresponding end tag. If sgml-leave-point-after-insert is t, the point
is left after the inserted tag(s), unless the element has som required
content.  If sgml-leave-point-after-insert is nil the point is left
after the first tag inserted." t)
(autoload 'sgml-fill-element "psgml-edit" "Fill biggest enclosing element with mixed content.
If current element has pure element content, recursively fill the
subelements." t)
(autoload 'sgml-edit-attributes "psgml-edit" "Edit attributes of current element.
Editing is done in a separate window." t)
(autoload 'sgml-edit-attrib-finish "psgml-edit" "Finish editing and insert attribute values in original buffer." t)
(autoload 'sgml-edit-attrib-default "psgml-edit" "Set current attribute value to default." t)
(autoload 'sgml-edit-attrib-clear "psgml-edit" "Kill the value of current attribute." t)
(autoload 'sgml-edit-attrib-field-start "psgml-edit" "Go to the start of the attribute value field." t)
(autoload 'sgml-edit-attrib-field-end "psgml-edit" "Go to the end of the attribute value field." t)
(autoload 'sgml-edit-attrib-next "psgml-edit" "Move to next attribute value." t)
(autoload 'sgml-hide-tags "psgml-edit" "Hide all tags in buffer." t)
(autoload 'sgml-show-tags "psgml-edit" "Show hidden tags in buffer." t)
(autoload 'sgml-hide-attributes "psgml-edit" "Hide all attribute specifications in the buffer." t)
(autoload 'sgml-show-attributes "psgml-edit" "Show all attribute specifications in the buffer." t)
(autoload 'sgml-expand-all-shortrefs "psgml-edit" "Expand all short references in the buffer.
Short references to text entities are expanded to the replacement text
of the entity; other short references are expanded into general entity
references.  If argument TO-ENTITY is non-nil, or if called
interactively with a numeric prefix argument, all short references are
replaced by general entity references." t)
(autoload 'sgml-normalize "psgml-edit" "Normalize buffer by filling in omitted tags and expanding empty tags.
Argument TO-ENTITY controls how short references are expanded as with
`sgml-expand-all-shortrefs'.  An optional argument ELEMENT can be the
element to normalize instead of the whole buffer, if used no short
references will be expanded." t)
(autoload 'sgml-normalize-element "psgml-edit" nil t)
(autoload 'sgml-make-character-reference "psgml-edit" "Convert character after point into a character reference.
If called with a numeric argument, convert a character reference back
to a normal character.  If called from a program, set optional
argument INVERT to non-nil." t)
(autoload 'sgml-expand-entity-reference "psgml-edit" "Insert the text of the entity referenced at point." t)
(autoload 'sgml-trim-and-leave-element "psgml-edit" "Remove blanks at end of current element and move point to after element." t)
(autoload 'sgml-edit-external-entity "psgml-edit" "Open	a new window and display the external entity at the point." t)
(autoload 'sgml-complete "psgml-edit" "Complete the word/tag/entity before point.
If it is a tag (starts with < or </) complete with valid tags.
If it is an entity (starts with &) complete with declared entities.
If it is a markup declaration (starts with <!) complete with markup
declaration names.
If it is something else complete with ispell-complete-word." t)
(autoload 'sgml-file-options-menu "psgml-edit" nil t)
(autoload 'sgml-user-options-menu "psgml-edit" nil t)
(autoload 'sgml-add-element-to-element "psgml-edit" "Add an element of type GI to the current element.
The element will be added at the last legal position if FIRST is `nil',
otherwise it will be added at the first legal position." t)
(autoload 'sgml-show-current-element-type "psgml-edit" "Show information about the current element and its type." t)
(autoload 'sgml-show-structure "psgml-edit" "Show the document structure in a separate buffer." t)
(autoload 'sgml-save-dtd "psgml-dtd" "Save the parsed dtd on FILE." t)
(autoload 'sgml-list-elements "psgml-info" "List the elements and their attributes in the current DTD." t)
(autoload 'sgml-list-attributes "psgml-info" "List the attributes and in which elements they occur." t)
(autoload 'sgml-list-terminals "psgml-info" "List the elements that can have data in their content." t)
(autoload 'sgml-list-content-elements "psgml-info" "List all element types and the element types that can occur in its content." t)
(autoload 'sgml-list-occur-in-elements "psgml-info" "List all element types and where it can occur." t)
(autoload 'sgml-describe-entity "psgml-info" "Describe the properties of an entity as declared in the current DTD." t)
(autoload 'sgml-describe-element-type "psgml-info" "Describe the properties of an element type as declared in the current DTD." t)
(autoload 'sgml-describe-dtd "psgml-info" "Display information about the current DTD." t)


;;;; Last provisions

(provide 'psgml)
;;; psgml.el ends here
