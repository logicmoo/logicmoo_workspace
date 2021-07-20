;;; face-explorer.el --- Library and tools for faces and text properties

;; Copyright (C) 2014-2017  Anders Lindgren

;; Author: Anders Lindgren
;; Keywords: faces
;; Package-Version: 20190517.1857
;; Package-Commit: ad1300e13e5643e4c246cabfd91f833d39113052
;; Version: 0.0.4
;; Created: 2017-02-24 (Based code from e2ansi created 2014-12-09)
;; URL: https://github.com/Lindydancer/face-explorer

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Library and tools for faces and text properties.
;;
;; This library is useful for packages that convert syntax highlighted
;; buffers to other formats.  The functions can be used to determine
;; how a face or a face text property looks, in terms of primitive
;; face attributes (e.g. foreground and background colors).  Two sets
;; of functions are provided, one for existing frames and one for
;; fictitious displays, like 8 color tty.
;;
;; In addition, the following tools are provided:
;;
;; - `face-explorer-list-faces' -- list all available faces.  Like
;;   `list-faces-display' but with information on how a face is
;;   defined.  In addition, a sample for the selected frame and for a
;;   fictitious display is shown.
;;
;; - `face-explorer-describe-face' -- Print detailed information on
;;   how a face is defined, and list all underlying definitions.
;;
;; - `face-explorer-describe-face-prop' -- Describe the `face' text
;;   property at the point in terms of primitive face attributes.
;;   Also show how it would look on a fictitious display.
;;
;; - `face-explorer-list-display-features' -- Show which features a
;;   display supports.  Most graphical displays support all, or most,
;;   features.  However, many tty:s don't support, for example,
;;   strike-through.  Using specially constructed faces, the resulting
;;   buffer will render differently in different displays, e.g. a
;;   graphical frame and a tty connected using `emacsclient -nw'.
;;
;; - `face-explorer-list-face-prop-examples' -- Show a buffer with an
;;   assortment of `face' text properties.  A sample text is shown in
;;   four variants: Native, a manually maintained reference vector,
;;   the result of `face-explorer-face-prop-attributes' and
;;   `face-explorer-face-prop-attributes-for-fictitious-display'.  Any
;;   package that convert a buffer to another format (like HTML, ANSI,
;;   or LaTeX) could use this buffer to ensure that everything work as
;;   intended.
;;
;; - `face-explorer-list-overlay-examples' -- Show a buffer with a
;;   number of examples of overlays, some are mixed with `face' text
;;   properties.  Any package that convert a buffer to another format
;;   (like HTML, ANSI, or LaTeX) could use this buffer to ensure that
;;   everything work as intended.
;;
;; - `face-explorer-tooltip-mode' -- Minor mode that shows tooltips
;;   containing text properties and overlays at the mouse pointer.
;;
;; - `face-explorer-simulate-display-mode' -- Minor mode for make a
;;   buffer look like it would on a fictitious display.  Using this
;;   you can, for example, see how a theme would look in using dark or
;;   light background, a 8 color tty, or on a grayscale graphical
;;   monitor.

;; Fictitious displays:
;;
;; Emacs supports a variety of displays, from graphical frames to
;; terminals with 8 colors.  Emacs itself provides query functions for
;; existing displays.  This library provides query functions for any
;; kind of display, allowing you to play the "what if" game.  (For
;; example, how would a face look on a grayscale graphical display or
;; an 8 color tty.)
;;
;; It is possible to use the query functions for fictitious displays
;; in batch mode, when the normal face and colors system are severely
;; restricted.

;; Library functions:
;;
;; Function for existing frames:
;;
;; - `face-explorer-face-attributes' -- The face attributes
;;   of a face, after expanding all inherited faces.
;;
;; - `face-explorer-face-prop-attributes' -- The primitive face
;;   attributes of a face specification, as used by the `face' text
;;   property.  Effectively, this can tell how a piece of text look to
;;   the user, expressed in terms of foreground color, background
;;   color, underline etc.  Face remappings performed by
;;   `face-remapping-alist' are handled.
;;
;; - `face-explorer-face-attributes-at' -- The primitive face
;;   attributes at a specific position in a buffer.
;;
;; Support for a fictitious display:
;;
;; The following variables defines a fictitious display.  The
;; face-explorer tools use the global variants of these variables.
;; However, when calling the functions in the library, it's possible
;; to dynamically bind them using `let'.
;;
;; - `face-explorer-number-of-colors' -- Number of colors.
;;
;; - `face-explorer-background-mode' -- The background mode, either
;;   `light' or `dark'.
;;
;; - `face-explorer-color-class' -- The color class: `color',
;;   `grayscale', or `mono'.
;;
;; - `face-explorer-window-system-type' -- The window system type,
;;   either a symbol like `tty' or a list like `(graphic ns)'.
;;
;; - `face-explorer-match-supports-function' -- A function to call to
;;   determine the features a display supports.  By default, a
;;   graphical display supports everything and a tty supports things
;;   like underline and inverse video.
;;
;; The following functions can be used to query face-related
;; information for fictitious displays:
;;
;; - `face-explorer-face-attributes-for-fictitious-display'
;;   -- The face attributes of a face, after expanding all inherited
;;   faces, for a fictitious display.
;;
;; - `face-explorer-face-prop-attributes-for-fictitious-display' --
;;   The primitive face attributes of a face specification, as used by
;;   the `face' text property, for a fictitious display.  Effectively,
;;   this can tell how a piece of text look to the user, expressed in
;;   terms of foreground color, background color, underline etc.
;;
;; - `face-explorer-face-attributes-for-fictitious-display-at' -- The
;;   primitive face attributes at a specific position in a buffer, for
;;   a fictitious display.

;; Face-related tools:
;;
;; This package provide a number of face-related tools.  Most of them
;; display information about a face both in the selected frame and as
;; it would look on a fictitious display.
;;
;; Key bindings:
;;
;; In the tool buffers, you can use the following keys to change the
;; settings:
;;
;; - `-', `+', and `#' -- Decrease, increase, and set the number of
;;   colors of the fictitious display.  The increase and decrease
;;   commands use 8, 16, 256, and "infinite" colors.
;;
;; - `b' -- Toggle between light and dark background mode.
;;
;; - `c' -- Step to the next color class.
;;
;; - `r' -- Reset the fictitious display to match the selected frame.
;;
;; - `g' -- Toggle the window system between that of the selected frame
;;   and a terminal.

;; Face verifiers:
;;
;; The `face-explorer-list-faces' and `face-explorer-describe-face'
;; tools warn about inappropriately defined faces.  You can use the
;; following keys to handle these warnings:
;;
;;    - `wd' -- Disable verifier
;;
;;    - `we' -- Enable verifier
;;
;;    - `wa' -- Enable all verifiers
;;
;;    - `wn' -- Disable all verifiers
;;
;;    - `wx' -- Describe verifier.

;; The `face-explorer-list-faces' tool:
;;
;; List all available faces.  Like `list-faces-display' but with
;; information on how a face is defined.  In addition, a sample for
;; the both the selected frame and for the current fictitious display
;; is shown.
;;
;; Additional keys:
;;
;; - `RET' -- Open the `face-explorer-describe-face' tool for the face
;;   on the line of the cursor.

;; The `face-explorer-describe-face' tool:
;;
;; Display information about a face.  This includes:
;;
;; - The documentation
;;
;; - The face attributes in the selected frame
;;
;; - Ditto, but with inheritances resolved
;;
;; - Primitive face attributes and samples for the current fictitious display
;;
;; - Samples for a number of typical fictitious displays
;;
;; - The face specifications used to define the face, originating from
;;   `defface', `custom-theme-set-faces' etc.

;; The `face-explorer-describe-face-prop' tool:
;;
;; Display information about a `face' text property.  This includes:
;;
;; - The value of the property
;;
;; - The corresponding primitive face attributes in the selected frame,
;;   with samples.
;;
;; - Ditto for the current fictitious display
;;
;; - Samples how the `face' text property would look in a number of
;;   typical fictitious displays.

;; The `face-explorer-list-display-features' tool:
;;
;; Display a buffer contains text using specially constructed faces
;; that will look differently depending on available display features.
;; For example, if you run `emacsclient -nw' from a terminal, this
;; buffer will look differently than it does in a graphical frame.

;; The `face-explorer-list-face-prop-examples' tool:
;;
;; List sample text with face text properties in various variants.
;;
;; This is useful for two reasons:
;;
;; - It can be used to investigate how Emacs, rally, displays various
;;   faces and text properties.
;;
;; - It can be used to test packages that convert text with text
;;   properties to various other format, like PostScript, HTML, ANSI,
;;   LaTeX etc.

;; The `face-explorer-list-overlay-examples' tool:
;;
;; List sample text with overlays in various variants.

;; The `face-explorer-tooltip-mode' tool:
;;
;; A minor mode that shows information about text
;; properties and overlays in a tooltip.
;;
;; This is enabled in all buffers displayed by the tools in this
;; module.
;;
;; `face-explorer-tooltip-global-mode' can be used to enable this mode
;; for all buffers.

;; The `face-explorer-simulate-display-mode' tool:
;;
;; A minor mode used to show how the current buffer would look in a
;; fictitious display.  This can be used, for example, to check if a
;; face theme would look good in a 8 color tty or in a grayscale
;; graphical display.
;;
;; The key bindings describe above above are available after the
;; prefix `C-c !'.
;;
;; Note: The mode will not restrict colors to the fictitious display.
;; For example, if a face is defined as "red" it will be shown in red
;; even when the fictitious display is set to `mono' or `grayscale'.
;;
;; `face-explorer-simulate-display-global-mode' can be used to enable
;; this mode for all buffers.

;; Vocabulary:
;;
;; Primitive face attributes:
;;
;; Normally, a face or face specification can be quite complex, for
;; example a face can inherit from other faces and a face
;; specification can contain several faces.  "Primitive face
;; attributes" corresponds to how the face or face specification will
;; look to the user, the foreground and background color it has,
;; whether it is bold or italic etc.  Unspecified properties are not
;; included.  Properties like :height could be relative (like 1.2) or
;; absolute (like 10).  Primitive face attributes never contain the
;; `:inherit' attribute.

;; Technical background:
;;
;; Face definitions:
;;
;; The attributes associated with a face can originate from the
;; following locations:
;;
;; - `defface' -- This construct defines a customizable face.  The
;;   information is stored in the property `face-default-spec' in the
;;   face symbol.  Normally it is accessed using the function
;;   `face-default-spec'.
;;
;; - Customize -- A user can customize a face using
;;   `custom-theme-set-faces'.  The `theme-face' property of the face
;;   symbol contains an alist from active themes to display
;;   requirements face specifications.
;;
;; - Overrides -- Face attributes can be overridden using the function
;;   `face-spec-set'.  This information is stored in the
;;   `face-override-spec' property in the face symbol.
;;
;; - Future frames -- The function `set-face-attribute' can specify
;;   that an attribute should be set for future frames, by passing nil
;;   or `t' as FRAME.  This information can be accessed using
;;   `face-attribute' by passing `t' as the FRAME.
;;
;; - Existing frames -- A face attribute can be changed in existing
;;   frames by `set-face-attribute' by passing a frame or nil as
;;   FRAME.  This information can be accessed using `face-attribute'.
;;
;; Face aliases:
;;
;; The `face-alias' property of the face symbol can contain another
;; symbol which the face is aliased to.
;;
;; Distant foreground:
;;
;; A "distant foreground" is an alternative color used to render the
;; foreground when the normal foreground color would be too close to
;; the background.  Unfortunately, there is no clear definition of
;; what "too close" really means, so it is hard to simulate it.
;; Currently, this package return both the `:foreground' and
;; `:distant-foreground' attributes.  Hopefully, in the future, it
;; might be possible to deduce the color that is shown and use
;; `:foreground' to represent that color.

;; Other Font Lock Tools:
;;
;; This package is part of a suite of font-lock tools.  The other
;; tools in the suite are:
;;
;;
;; Font Lock Studio:
;;
;; Interactive debugger for font-lock keywords (Emacs syntax
;; highlighting rules).
;;
;; Font Lock Studio lets you *single-step* Font Lock keywords --
;; matchers, highlights, and anchored rules, so that you can see what
;; happens when a buffer is fontified.  You can set *breakpoints* on
;; or inside rules and *run* until one has been hit.  When inside a
;; rule, matches are *visualized* using a palette of background
;; colors.  The *explainer* can describe a rule in plain-text English.
;; Tight integration with *Edebug* allows you to step into Lisp
;; expressions that are part of the Font Lock keywords.
;;
;;
;; Font Lock Profiler:
;;
;; A profiler for font-lock keywords.  This package measures time and
;; counts the number of times each part of a font-lock keyword is
;; used.  For matchers, it counts the total number and the number of
;; successful matches.
;;
;; The result is presented in table that can be sorted by count or
;; time.  The table can be expanded to include each part of the
;; font-lock keyword.
;;
;; In addition, this package can generate a log of all font-lock
;; events.  This can be used to verify font-lock implementations,
;; concretely, this is used for back-to-back tests of the real
;; font-lock engine and Font Lock Studio, an interactive debugger for
;; font-lock keywords.
;;
;;
;; Highlight Refontification:
;;
;; Minor mode that visualizes how font-lock refontifies a buffer.
;; This is useful when developing or debugging font-lock keywords,
;; especially for keywords that span multiple lines.
;;
;; The background of the buffer is painted in a rainbow of colors,
;; where each band in the rainbow represent a region of the buffer
;; that has been refontified.  When the buffer is modified, the
;; rainbow is updated.
;;
;;
;; Faceup:
;;
;; Emacs is capable of highlighting buffers based on language-specific
;; `font-lock' rules.  This package makes it possible to perform
;; regression test for packages that provide font-lock rules.
;;
;; The underlying idea is to convert text with highlights ("faces")
;; into a plain text representation using the Faceup markup
;; language.  This language is semi-human readable, for example:
;;
;;     «k:this» is a keyword
;;
;; By comparing the current highlight with a highlight performed with
;; stable versions of a package, it's possible to automatically find
;; problems that otherwise would have been hard to spot.
;;
;; This package is designed to be used in conjunction with Ert, the
;; standard Emacs regression test system.
;;
;; The Faceup markup language is a generic markup language, regression
;; testing is merely one way to use it.
;;
;;
;; Font Lock Regression Suite:
;;
;; A collection of example source files for a large number of
;; programming languages, with ERT tests to ensure that syntax
;; highlighting does not accidentally change.
;;
;; For each source file, font-lock reference files are provided for
;; various Emacs versions.  The reference files contains a plain-text
;; representation of source file with syntax highlighting, using the
;; format "faceup".
;;
;; Of course, the collection source file can be used for other kinds
;; of testing, not limited to font-lock regression testing.

;; History:
;;
;; Part of the code of this package was originally published as part
;; of the package `e2ansi', a package that can emit highlighted text
;; to a terminal using ANSI sequences.  `e2ansi' can be configured to
;; be used with the command line command `more' and `less' to syntax
;; highlight anything viewed using those command.

;;; Code:

;; References:
;;
;; - `defface': "For backward compatibility, elements of SPEC can be
;;   written as (DISPLAY ATTS) instead of (DISPLAY . ATTS)." However,
;;   in most cases, it looks like the former is used, at least for
;;   `custom-theme-set-faces', for all themes shipped with Emacs 25.
;;   Unfortunately, it affects how the property `theme-faces' look.
;;
;; - The two variables below are automatically applied to
;;   `get-text-property' (even though it's documentation doesn't
;;   mention this).
;;
;;   `char-property-alias-alist': Alist of alternative face properties
;;   (e.g. `((face font-lock-face))'.
;;
;;   `default-text-properties': Property list of default text
;;   properties (e.g. `(face (:underline t))').
;;
;;   WARNING: This is also active when getting accessing text
;;   properties of STRINGS, hence the return value will depend on the
;;   value if this variable in the buffer that happens to be current,
;;   unless these variables explicitly are let-bound.
;;
;; - `get-char-property' is returns the value of text properties or
;;   overlays.  However, it only returns the value of one text
;;   property/overlay, which makes it unsuitable for the `face'
;;   property, as Emacs joins all occurrences when displaying the
;;   result.  This package provide `face-explorer-face-text-props-at'
;;   that returns the combined value.
;;
;; - `next-single-property-change' finds the next text property change
;;   and `next-char-property-change' the same for text properties or
;;   overlays.  Note that they behave differently when the text
;;   property stretch to the end of the buffer, the former return
;;   `nil' whereas the latter `point-max'.
;;
;; - A "display requirement" is an alist of the form `((DISPLAY-REQ
;;   . (ATTR VALUE ...)  ...)'  which is the same as `((DISPLAY-REQ
;;   ATTR VALUE ...) ...)'.  DISPLAY-REQ is a form specifying
;;   conditions to match a display.  The first element may use
;;   `default' as DISPLAY-REQ, meaning that the attributes it
;;   specifies default values for the following elements.

;; Emacs bugs:
;;
;; The documentation of `defface' doesn't mention that `tty' is a
;; valid value for `type'.
;;
;; Manual specifies that all attributes of the `default' face should
;; be defined, but :distant-foreground isn't.
;;
;; When a face with a :distant-foreground is themed,
;; :distant-foreground is incorrectly retained.
;;
;; Re-evaluating a theme definition (?) hides it from `disable-theme'.

;; Bad faces:
;;
;; The following is a list of faces I've noticed contain a
;; questionable definition.  The list is by no means complete.
;;
;; - `font-lock-keyword-face' use "cyan" as foreground color in light
;;   displays in 8 color mode.
;;
;; - `font-lock-variable-name-face' use "yellow" as foreground color
;;   in light displays in 8 color mode.


(defgroup face-explorer nil
  "Library and tools for investigating faces."
  :group 'faces)


(defcustom face-explorer-sample-text "Abc01234il()"
  "Sample text for `face-explorer' commands."
  :type 'string
  :group 'face-explorer)


(defvar face-explorer-light-mode-colors '(default ("black" . "white"))
  "List describing how to set sample colors in light background mode.

Each entry in the list can be:

 - A face -- this face is used, but only if it provides different
   colors in light and dark background mode.

 - A cons pair of colors (strings) -- the first element is the
   foreground color and the second the background.")


(defvar face-explorer-dark-mode-colors  '(default ("white" . "black"))
  "List describing how to set sample colors in dark background mode.

See `face-explorer-light-mode-colors' for more information.")


;; -------------------------------------------------------------------
;; Support functions.
;;


(defalias 'face-explorer-user-error
  (if (fboundp 'user-error)
      'user-error
    'error))


(defun face-explorer-plist-set (plist attr value)
  "Change value in PLIST of ATTR to VALUE, nondestructively.

Like `plist-put', but does not destructively change PLIST."
  (let ((rest (plist-member plist attr)))
    (if rest
        (let ((new-plist (cons attr (cons value (cdr (cdr rest)))))
              (head '()))
          (while (not (eq plist rest))
            (push (pop plist) head)
            (push (pop plist) head))
          (nconc (nreverse head) new-plist))
      (cons attr (cons value plist)))))


(defun face-explorer-join-face-attributes (plist1 plist2)
  "Join face attributes PLIST1 and PLIST2, PLIST1 takes precedence."
  (while plist2
    (let ((key (pop plist2))
          (value (pop plist2)))
      ;; Height:s are accumulative.
      (if (and (eq key :height)
               (floatp value)
               (floatp (plist-get plist1 :height)))
          (setq plist1
                (face-explorer-plist-set
                 plist1 :height (* value
                                   (plist-get plist1 :height))))
        (unless (plist-member plist1 key)
          (setq plist1 (cons key (cons value plist1)))))))
  plist1)


(defun face-explorer-join-face-xattributes (xplist1 xplist2)
  "Join XPLIST1 and XPLIST2, XPLIST1 takes precedence.

Both arguments are on the form (FLAG . PLIST), where FLAG is a
non-nil if the face inherited (directly or indirectly) from the
`default' face, and PLIST is a property list of primitive face
attributes."
  (if (car xplist1)
      xplist1
    (cons (car xplist2)
          (face-explorer-join-face-attributes (cdr xplist1)
                                              (cdr xplist2)))))


(defun face-explorer-keep-well-formed-attributes (plist)
  "Remove incorrectly formed entries from PLIST.

Only plists entries where the key is a keywords (:name) are retained.

Return a plist, the returned plist may be completely or partially
the same as the original PLIST."
  (if (null plist)
      plist
    (if (keywordp (car plist))
        (let* ((orig-rest (cdr-safe (cdr plist)))
               (rest (face-explorer-keep-well-formed-attributes orig-rest)))
          (if (eq orig-rest rest)
              plist
            `(,(nth 0 plist) ,(nth 1 plist) ,@rest)))
      ;; Incorrectly formed entry, drop it.
      (face-explorer-keep-well-formed-attributes (cdr-safe (cdr plist))))))


;; C.f. `custom-fix-face-spec'.
(defun face-explorer-fix-face-attributes (plist)
  "Convert deprecated face attributes in PLIST to modern variants."
  (if (null plist)
      plist
    (let ((orig plist)
          (key (pop plist))
          (value (pop plist))
          (new-plist (face-explorer-fix-face-attributes plist)))
      (cond ((eq key :italic)
             (push (if value 'italic 'normal) new-plist)
             (push :slant new-plist))
            ((eq key :bold)
             (push (if value 'bold 'normal) new-plist)
             (push :weight new-plist))
            ((eq key :reverse-video)
             (push value new-plist)
             (push :inverse-video new-plist))
            ((and (eq key :box)
                  (eq value t))
             (push 1 new-plist)
             (push :box new-plist))
            ((eq plist new-plist)
             ;; Use original plist, to minimize consing.
             (setq new-plist orig))
            (t
             (push value new-plist)
             (push key new-plist)))
      new-plist)))


(defun face-explorer-face-attributes-for-future-frames (face)
  "The primitive face attributes of FACE specified for future frames."
  (let ((plist '()))
    (dolist (attribute-pair custom-face-attributes)
      (let ((attr (car attribute-pair)))
        (let ((value (face-attribute face attr t t)))
          (unless (or (null value)
                      (eq value 'unspecified))
            (push value plist)
            (push attr plist)))))
    plist))


(defun face-explorer-not-disjunct-p (list1 list2)
  "Non-nil, if LIST1 and LIST2 contain at least one common element."
  (let ((res nil))
    (while list1
      (when (memq (pop list1) list2)
        (setq res t)
        (setq list1 nil)))
    res))


(defun face-explorer-remove-plist-duplicates (plist)
  "Remove duplicate properties from PLIST, nondestructively.

Entries towards the end of the the list take precedence."
  (and plist
       (let ((attr (nth 0 plist))
             (rest (face-explorer-remove-plist-duplicates (cdr (cdr plist)))))
         (if rest
             (if (plist-member rest (car plist))
                 ;; ATTR already present.
                 rest
               ;; Keep attr. (Optimized to avoid re-consing the entire list.)
               (if (eq rest (cdr (cdr plist)))
                   plist
                 (cons attr (cons (nth 1 plist) rest))))
           plist))))


(defun face-explorer-plist-delete (key plist)
  "Delete KEY from property list PLIST, nondestructively.

Return a new property list."
  (let ((res '()))
    (if (null plist)
        plist
      (if (eq (car plist) key)
          (face-explorer-plist-delete key (cdr (cdr plist)))
        (let ((orig plist)
              (this-key (pop plist))
              (this-value (pop plist))
              (new-plist (face-explorer-plist-delete key plist)))
          (if (eq plist new-plist)
              orig
            (push this-value new-plist)
            (push this-key new-plist)
            new-plist))))))


(defun face-explorer-prop-valid-value (attr value)
  "For face attribute ATTR, return non-nil if VALUE is a valid value.

Currently, this is only implemented for some properties."
  (cond ((memq attr '(:family :foundry :foreground :distant-foreground
                              :background))
         (stringp value))
        ((eq attr :inherit)
         (or (symbolp value)
             (listp value)))
        (t
         t)))


(defun face-explorer-alias-of-face (face)
  "Return FACE, or a face it is aliased to.

Face aliases are defined using `define-obsolete-face-alias'."
  (let (new-face)
    (while (and (setq new-face (get face 'face-alias))
                (facep new-face))
      (setq face new-face)))
  face)


(defun face-explorer-parse-face-prop (face-prop
                                      face-xattributes-function
                                      &optional extra-arguments)
  "Parse FACE-PROP, a face specification used by the `face' text property.

Face specification can be a single face or a (possibly nested)
list of faces and property value pairs.  For
example `(font-lock-waning-face :underline t)'.  Entries earlier
in the list take precedence.

Return (FLAG . PLIST) where FLAG is a non-nil if the face
inherited (directly or indirectly) from the `default' face, and
PLIST is a property list of primitive face attributes.

The returned PLIST contains the face properties needed to render
text the same way as FACE-PROP.  However, it does not include
face properties inherited from the `default' face.

No face property is included twice.  The :inherit face property
is never included, but face properties of the faces it has
inherited from are.

The actual properties of a face are determined by the function
bound to FACE-XATTRIBUTES-FUNCTION.  It is passed a face, t (to
use `face-remapping-alist'), and the arguments in EXTRA-ARGUMENTS
and it should return (FLAG . PLIST) as described above.

Face remappings performed by `face-remapping-alist' are handled."
  (unless (listp face-prop)
    (setq face-prop (list face-prop)))
  (let ((res-xattrs '(nil)))
    (while face-prop
      (let ((face-xplist
             (cond ((keywordp (car face-prop))
                    ;; Once a keyword is found, the rest of the list
                    ;; is treated as a plist.
                    (let ((plist-xplist '(nil . ())))
                      (while face-prop
                        (let ((attr (pop face-prop))
                              (value (pop face-prop)))
                          (setq plist-xplist
                                (face-explorer-join-face-xattributes
                                 (cond ((eq attr :inherit)
                                        ;; `value' is a face or list of faces.
                                        (face-explorer-parse-face-prop
                                         value
                                         face-xattributes-function
                                         extra-arguments))
                                       ((and (keywordp attr)
                                             (face-explorer-prop-valid-value
                                              attr value))
                                        (list nil attr value))
                                       (t
                                        '(nil . ())))
                                 plist-xplist))))
                      ;; `face-prop' empty, so the outer loop will
                      ;; break after this.
                      plist-xplist))
                   ((eq (car face-prop) 'foreground-color)
                    (let ((color (cdr face-prop)))
                      (setq face-prop nil)
                      `(nil . (:foreground ,color))))
                   ((eq (car face-prop) 'background-color)
                    (let ((color (cdr face-prop)))
                      (setq face-prop nil)
                      `(nil . (:background ,color))))
                   ((or (symbolp (car face-prop))
                        (stringp (car face-prop)))
                    (let ((face (pop face-prop)))
                      (when (stringp face)
                        (setq face (intern face)))
                      (setq face (face-explorer-alias-of-face face))
                      (cond ((not (facep face))
                             '(nil . ()))
                            ((eq face 'default)
                             '(t . ()))
                            (t
                             (face-explorer-parse-face-remapping-alist
                              face
                              face-xattributes-function
                              extra-arguments)))))
                   ((listp (car face-prop))
                    (face-explorer-parse-face-prop
                     (pop face-prop)
                     face-xattributes-function
                     extra-arguments))
                   (t
                    (error "Unexpected face specification")))))
        (setq res-xattrs (face-explorer-join-face-xattributes
                          res-xattrs
                          face-xplist))))
    ;; Strip away properties explicitly set to `unspecified'.
    ;;
    ;; For example: `((:foreground unspecified) green-face)'.
    (let ((flag (car res-xattrs))
          (plist (cdr res-xattrs))
          (res-plist '()))
      (while plist
        (let ((attr (pop plist))
              (value (pop plist)))
          (unless (eq value 'unspecified)
            (push attr res-plist)
            (push value res-plist))))
      (cons flag (nreverse res-plist)))))


(defun face-explorer-expand-inherit (plist use-remapping
                                           face-xattributes-function
                                           &optional extra-arguments)
  "Expand :inherit in property list PLIST.

When USE-REMAPPING is non-nil, `face-remapping-alist' is
consulted when expanding inherited faces.

Return (FLAG . PLIST) where FLAG is a non-nil if the face
inherited (directly or indirectly) from the `default' face, and
PLIST is a property list of primitive face attributes.
Properties inherited from the `default' face are not included in
PLIST.

The actual properties of a face are determined by the function
bound to FACE-XATTRIBUTES-FUNCTION.  It is passed a face,
USE-REMAPPING, and the arguments in EXTRA-ARGUMENTS and it should
return (FLAG . PLIST) as described above."
  ;; In a face attribute list, the position of the :inherit property
  ;; does not matter.  (However, in `face' text properties, it does
  ;; matter.)
  (let ((xplist '(nil . ()))
        (inherit (plist-get plist :inherit)))
    ;; Get all inherited properties.
    (when inherit
      (unless (consp inherit)
        (setq inherit  (list inherit)))
      (dolist (face inherit)
        (when (facep face)
          (setq face (face-explorer-alias-of-face face))
          (if (eq face 'default)
              (setq xplist
                    (cons t
                          (cdr xplist)))
            (setq xplist
                  (face-explorer-join-face-xattributes
                   xplist
                   (if use-remapping
                       (face-explorer-parse-face-remapping-alist
                        face
                        face-xattributes-function
                        extra-arguments)
                     (apply face-xattributes-function
                            face
                            use-remapping
                            extra-arguments))))))))
    ;; Override with properties explicitly defined.
    (face-explorer-join-face-xattributes
     (cons nil (face-explorer-plist-delete :inherit plist))
     xplist)))


(defun face-explorer-maybe-set-inherit-from-default-flag (xplist is-default)
  "Set FLAG field in XPLIST to t if IS-DEFAULT is non-nil, nondestructively.

XPLIST is in the form (FLAG . PLIST)."
  (cond ((null is-default)
         xplist)
        ((car xplist)
         xplist)
        (t
         (cons t (cdr xplist)))))


(defvar face-explorer--seen-faces '()
  "Faces seen by `face-explorer-parse-face-remapping-alist'.

This is used to break loops when parsing `face-remapping-alist'.

This variable is typically `let'-bound.")


(defun face-explorer-parse-face-remapping-alist
    (face face-xattributes-function &optional extra-arguments)
  "The primitive face attributes of FACE, including face remappings.

Return (FLAG . PLIST) where FLAG is a non-nil if the face
inherited (directly or indirectly) from the `default' face, and
PLIST is a property list of primitive face attributes.
Properties inherited from the `default' face are not included in
PLIST.

See `face-explorer-parse-face-prop' for information about
FACE-XATTRIBUTES-FUNCTION and EXTRA-ARGUMENTS.

See `face-remapping-alist' for information on face remappings."
  (let ((entry (assq face face-remapping-alist)))
    (if (or (null entry)
            (memq face face-explorer--seen-faces))
        ;; Use the non-aliased value.
        (apply face-xattributes-function
               face
               t
               extra-arguments)
      (let ((face-explorer--seen-faces
             (cons face face-explorer--seen-faces)))
        (face-explorer-parse-face-prop
         (cdr entry)
         face-xattributes-function
         extra-arguments)))))


(defun face-explorer-next-face-property-change (pos &optional limit)
  "Next position after POS where the `face' property change.

Both text properties and overlays are searched.

Search up to LIMIT (defaults to `point-max').

If POS is nil and the first character has a `face' property,
return `point-min'.

If last character contains a face property, return `point-max'."
  (unless limit
    (setq limit (point-max)))
  (if (equal pos limit)
      ;; Last search returned `limit'. There is no more to search for.
      nil
    (if (and (null pos)
             (get-text-property (point-min) 'face))
        ;; `pos' is `nil' and the character at `point-min' contains a
        ;; face property, return `point-min'.
        (point-min)
      (unless pos
        ;; Start from the beginning.
        (setq pos (point-min)))
      (next-char-property-change pos limit))))


(defun face-explorer-overlays-at (pos)
  "The overlays at POS.

In newer Emacs versions, the list is sorted by decreasing priority."
  (condition-case nil
      (with-no-warnings
        (overlays-at pos :sorted))
    (error
     ;; Note: `reverse' is needed to preserve the original order for
     ;; otherwise equal overlays.  In addition, as it creates a new
     ;; list, it is used to ensure that `sort' doesn't destructively
     ;; modify the original list returned by `overlays-at'.
     (let ((seq (reverse (overlays-at pos))))
       (setq seq
	     (sort seq (lambda (a b)
			 (let ((a-prio (or (overlay-get a 'priority)
					   0))
			       (b-prio (or (overlay-get b 'priority)
					   0)))
			   ;; Priorities are typically numbers, but some
			   ;; internal overlays use other, undocumented,
			   ;; values.
			   (if (not (equal a-prio b-prio))
			       (and (numberp a-prio)
				    (numberp b-prio)
				    (< a-prio b-prio))
			     (let ((a-start (overlay-start a))
				   (b-start (overlay-start b)))
			       (if (not (eq a-start b-start))
				   (< a-start b-start)
				 (let ((a-end (overlay-end a))
				       (b-end (overlay-end b)))
				   (if (not (eq a-end b-end))
				       (> a-end b-end)
				     nil)))))))))
       seq))))


(defun face-explorer-face-text-props-at (&optional pos)
  "The active face text properties at POS.

This includes the face text properties from the buffer text as
well from overlays."
  (unless pos
    (setq pos (point)))
  (let ((res (get-text-property pos 'face)))
    (dolist (overlay (face-explorer-overlays-at pos))
      (let ((overlay-text-prop (overlay-get overlay 'face)))
        (when overlay-text-prop
          (if res
              (progn
                (unless (listp res)
                  (setq res (list res)))
                (unless (listp overlay-text-prop)
                  (setq overlay-text-prop (list overlay-text-prop)))
                ;; The face text properties can have any kind of
                ;; structure, so it's hard to remove duplicates here.
                ;; However, `face-explorer-parse-face-prop' handles
                ;; them as they are.
                (setq res (append res overlay-text-prop)))
            (setq res overlay-text-prop)))))
    res))


;; -------------------------------------------------------------------
;; Functions for existing frames.
;;

(defun face-explorer-face-attributes (face &optional frame)
  "The primitive face attributes for FACE in FRAME.

Properties inherited from the `default' face are not included.

FRAME defaults to the selected frame."
  (cdr (face-explorer-face-xattributes face nil frame)))


(defun face-explorer-face-xattributes (face &optional use-remapping frame)
  "The primitive face attribute for FACE.

When USE-REMAPPING is non-nil, `face-remapping-alist' is
consulted when expanding inherited faces.

Face information for FRAME is used, FRAME defaults to the
selected frame.

This function return (FLAG . PLIST) where FLAG is a non-nil if
the face inherited (directly or indirectly) from the `default'
face, and PLIST is a property list of primitive face attributes.
Properties inherited from the `default' face are not included in
PLIST."
  (setq face (face-explorer-alias-of-face face))
  (let ((plist '()))
    ;; Collect the unexpanded attributes for FACE.
    (dolist (attribute-pair custom-face-attributes)
      (let ((attr (car attribute-pair)))
        (let ((value (face-attribute face attr frame)))
          (unless (eq value 'unspecified)
            (push value plist)
            (push attr plist)))))
    ;; Expand the :inherit attribtue.  Note that this will recursively
    ;; call this function to expand :inherit of the inherited faces.
    (let ((xplist (face-explorer-expand-inherit
                   plist
                   use-remapping
                   #'face-explorer-face-xattributes
                   (list frame))))
      (face-explorer-maybe-set-inherit-from-default-flag
       xplist
       (eq face 'default)))))


(defun face-explorer-face-prop-attributes (face-prop &optional frame)
  "The primitive face attributes for FACE-PROP for FRAME.

If FRAME is omitted, the selected frame is used.

FACE-PROP is a face specification, as accepted by the `face' text
property.

This function returns a property list of the primitive face
attributes that would look like FACE-PROP in FRAME.  Properties
inherited from the `default' face are not included."
  (cdr (face-explorer-parse-face-prop
        face-prop
        #'face-explorer-face-xattributes
        (list frame))))


(defun face-explorer-face-attributes-at (&optional pos)
  "The primitive face attributes at POS in the current buffer.

Is POS is omitted, point is used.

Properties inherited from the `default' face are not included.

Face remappings performed by `face-remapping-alist' are handled.

Face information for the selected frame is used."
  (unless pos
    (setq pos (point)))
  (face-explorer-face-prop-attributes
   (face-explorer-face-text-props-at pos)))


;; -------------------------------------------------------------------
;; Fictitious displays
;;
;; The functions in this section can be used to determine how text
;; with face properties would be rendered in fictitious displays.
;;
;; See `face-explorer-match-display-requirements' how to define a
;; fictitious display."

;; Notes on Emacs versions.
;;
;; Prior to Emacs 27, batch frames didn't specify a background mode or
;; color class. As of Emacs 27, they specify `dark' background mode
;; and `mono' color class. The code below ignores this and says that
;; batch frame are use `light' and `color'.


(defun face-explorer-real-frame-p (&optional frame)
  "Non-nil if FRAME is a non-batch frame."
  (or (frame-parameter frame 'window-system)
      (frame-parameter frame 'tty)))


(defun face-explorer-display-number-of-colors (&optional display)
  "Number of colors supported by DISPLAY.

In batch mode, try to estimate number of colors by inspecting the
TERM environment variable.

DISPLAY can be a display name or a frame.  If DISPLAY is omitted
or nil, it defaults to the selected frame's display."
  (let ((colors (display-color-cells display)))
    (if (eq colors 0)
        (let ((term (getenv "TERM")))
          (if (and term
                   (string-match "-256color$" term))
              256
            8))
      colors)))


(defvar face-explorer-number-of-colors (face-explorer-display-number-of-colors)
  "Number of colors of the fictitious display, nil, or t.

This is used to match the `min-colors' display requirement.  When
nil, the match will never succeed.  When t it will always
succeed.")


(defun face-explorer-frame-background-mode (&optional frame)
  "The background mode of FRAME."
  (or (and (face-explorer-real-frame-p frame)
           (frame-parameter frame 'background-mode))
      'light))


(defvar face-explorer-background-mode (face-explorer-frame-background-mode)
  "Background mode of the fictitious display.

This is used to match the `background' display requirement.

Either `light' or `dark'.")


(defun face-explorer-frame-color-class (&optional frame)
  "The color class of FRAME."
  (or (and (face-explorer-real-frame-p frame)
           (frame-parameter frame 'display-type))
      'color))


(defvar face-explorer-color-class (face-explorer-frame-color-class)
  "Color class of the fictitious display.

This is used to match the `color' display requirement.

This can be `color', `grayscale', or `mono'.")


(defun face-explorer-display-window-system-type (&optional display)
  "DISPLAY window system type, corresponding to the `type' display requirement.

The return value is either a symbol or a list of symbols."
  ;; This corresponds to the logic in `face-spec-set-match-display'.
  (if (not (display-graphic-p display))
      'tty
    (let ((ws (framep-on-display display)))
      (cond ((eq ws 'x)
             (let ((res '(x graphic)))
               (when (featurep 'motif)
                 (push 'motif res))
               (when (featurep 'gtk)
                 (push 'gtk res))
               (when (featurep 'x-toolkit)
                 (push 'x-toolkit res))
               (when (and (featurep 'x-toolkit)
                          (not (featurep 'motif))
                          (not (featurep 'gtk)))
                 (push 'lucid res))
               res))
            (t
             `(,ws graphic))))))


(defvar face-explorer-window-system-type
  (face-explorer-display-window-system-type)
  "Window system type of the fictitious display.

This is used to match the `type' display requirement.

This can be a symbol, a list of symbols, or t to indicate that it
match any type.

Examples: `(ns graphic)' and `tty'.")


(defun face-explorer-default-match-supports-function (items)
  "Estimate if fictitious display supports ITEMS.

ITEMS is a property list, see `supports' in `defface' for more
information.

For graphical displays all features are supported.  For
non-graphical displays, features typically supported by tty:s are
supported.

This function is suitable to be used by
`face-explorer-match-supports-function'."
  (if (if (listp face-explorer-window-system-type)
          (memq 'graphic face-explorer-window-system-type)
        (eq 'graphic face-explorer-window-system-type))
      t
    ;; Not graphical. Try to behave as a typical tty.
    (let ((res t))
      (while items
        (let ((key (pop items))
              (value (pop items)))
          (cond ((eq key :underline)
                 (when (listp value)
                   (let ((rest (plist-member value :style)))
                     (unless (eq (nth 1 rest) 'line)
                       (setq res nil)))))
                ((eq key :inverse-video))
                ((and (eq key :weight)
                      (memq value '(nil bold))))
                (t
                 (setq res nil))))
        ;; Break loop.
        (unless res
          (setq items nil)))
      res)))


(defvar face-explorer-match-supports-function
  #'face-explorer-default-match-supports-function
  "The features supported by the fictitious display.

This is used to match the `supports' display requirement.

This can be:

 - A function -- It is called with one argument, a property list
   of items, see `defface' for more information.  The function
   should return non-nil if the fictitious display supports the
   feature.

 - nil -- No features are supported.

 - t -- All features are supported.")


;; Note: Inherited faces only have a ((default :inherit other-face)).
;; If no rule match, make sure the default rule is returned.
(defun face-explorer-match-display-requirements (spec
                                                 &optional no-match-found)
  "Match display requirement SPEC for a fictitious display.

Return a property list of face attributes for SPEC.  If no match
is found NO-MATCH-FOUND is returned.

See `defface' for the format or SPEC.

The fictitious display is described using the following variables:

- `face-explorer-number-of-colors' - The number of colors of the
  display, corresponds to the `min-colors' display requirement.

- `face-explorer-window-system-type' - Window systems features,
  e.g. `(graphic ns)' or `tty'.  Corresponds to the `type'
  display requirement.

- `face-explorer-background-mode' - `light' or `dark'.

- `face-explorer-color-class' - `color', `grayscale', or `mono'.

- `face-explorer-match-supports-function' - Function to call to
  match the `supports' display requirement."
  (let ((res '())
        (default-spec '())
        (found nil)
        (default-found nil))
    (while spec
      (let ((entry (pop spec)))
        (let ((reqs (nth 0 entry))
              (ok t))
          (if (eq reqs 'default)
              (progn
                (setq default-found t)
                (setq default-spec (cdr entry)))
            (if (eq reqs t)
                (setq reqs '()))
            (while (and ok
                        reqs)
              (let ((req (pop reqs)))
                (cond ((eq (nth 0 req) 'class)
                       (setq ok (memq face-explorer-color-class (cdr req))))
                      ((eq (nth 0 req) 'min-colors)
                       (setq ok (cond ((null face-explorer-number-of-colors)
                                       nil)
                                      ((eq face-explorer-number-of-colors t)
                                       t)
                                      (t
                                       (>= face-explorer-number-of-colors
                                           (nth 1 req))))))
                      ((eq (nth 0 req) 'background)
                       (setq ok (memq face-explorer-background-mode
                                      (cdr req))))
                      ((eq (nth 0 req) 'type)
                       (setq ok
                             (or (eq face-explorer-window-system-type t)
                                 (and (consp face-explorer-window-system-type)
                                      (face-explorer-not-disjunct-p
                                       face-explorer-window-system-type
                                       (cdr req)))
                                 (memq face-explorer-window-system-type
                                       (cdr req)))))
                      ((eq (nth 0 req) 'supports)
                       (setq ok
                             (cond
                              ((eq face-explorer-match-supports-function nil)
                               nil)
                              ((eq face-explorer-match-supports-function t)
                               t)
                              (t
                               (funcall face-explorer-match-supports-function
                                        (cdr req))))))
                      (t
                       (setq ok nil)))))
            (when ok
              (setq found t)
              (let ((atts (cdr entry)))
                ;; In older Emacs versions (e.g. Emacs 23.3) ENTRY has
                ;; the form (DISPLAY ATTS) and in newer, like Emacs
                ;; 24.4, (DISPLAY . ATTS).
                ;;
                ;;    (DISPLAY ATTS)
                ;; == (DISPLAY . (ATTS))
                ;; == (DISPLAY . (ATTS . nil))
                (when (and (consp atts)
                           (null (cdr atts)))
                  (setq atts (car atts)))
                (setq res (face-explorer-join-face-attributes
                           (face-explorer-keep-well-formed-attributes atts)
                           default-spec)))
              (setq spec nil))))))
    (if found
        (face-explorer-fix-face-attributes res)
      (if default-found
          (face-explorer-fix-face-attributes default-spec)
        no-match-found))))


(defun face-explorer-expand-inherit-for-fictitious-display
    (plist use-remapping)
  "Expand :inherit in PLIST for a fictitious display.

When USE-REMAPPING is non-nil, `face-remapping-alist' is
consulted when expanding inherited faces.

Return (FLAG . PLIST) where FLAG is a non-nil if the face
inherited (directly or indirectly) from the `default' face, and
PLIST is a property list of primitive face attributes.
Properties inherited from the `default' face are not included in
PLIST.

See `face-explorer-match-display-requirements' how to define a
fictitious display."
  (face-explorer-expand-inherit
   plist
   use-remapping
   #'face-explorer-face-xattributes-for-fictitious-display))


(defun face-explorer-face-attributes-for-fictitious-display (face)
  "The primitive face attributes of FACE for a fictitious display.

The face attributes are taken from the currently active theme
environment, relevant `defface' definitions, and face attributes
set for future frames.  All inheritance relationships have been
resolved.

See `face-explorer-match-display-requirements' how to define a
fictitious display."
  (cdr (face-explorer-face-xattributes-for-fictitious-display face)))


(defun face-explorer-face-xattributes-for-fictitious-display
    (face &optional use-remapping)
  "The primitive face attributes of FACE for a fictitious display.

When USE-REMAPPING is non-nil, `face-remapping-alist' is
consulted when expanding inherited faces.

Return (FLAG . PLIST) where FLAG is a non-nil if the face
inherited (directly or indirectly) from the `default' face, and
PLIST is a property list of primitive face attributes.

The face attributes are taken from the currently active theme
environment, relevant `defface' definitions, and face attributes
set for future frames.  All inheritance relationships have been
resolved.

See `face-explorer-match-display-requirements' how to define a
fictitious display."
  (setq face (face-explorer-alias-of-face face))
  (let ((theme-faces (get face 'theme-face))
        (no-match-found 0)
        (theme-face-applied nil)
        (xattrs '(nil)))
    (if theme-faces
        (dolist (elt (reverse theme-faces))
          (let ((new-plist (face-explorer-match-display-requirements
                            (cadr elt) no-match-found)))
            (unless (eq new-plist no-match-found)
              (setq xattrs
                    (face-explorer-join-face-xattributes
                     (face-explorer-expand-inherit-for-fictitious-display
                      (face-explorer-remove-plist-duplicates
                       new-plist)
                      use-remapping)
                     xattrs))
              (setq theme-face-applied t)))))
    (unless theme-face-applied
      (setq xattrs (face-explorer-join-face-xattributes
                    (face-explorer-expand-inherit-for-fictitious-display
                     (face-explorer-remove-plist-duplicates
                      (face-explorer-match-display-requirements
                       (face-default-spec face)))
                     use-remapping)
                    xattrs)))
    (setq xattrs (face-explorer-join-face-xattributes
                  (face-explorer-expand-inherit-for-fictitious-display
                   (face-explorer-match-display-requirements
                    (get face 'face-override-spec))
                   use-remapping)
                  xattrs))
    (setq xattrs (face-explorer-join-face-xattributes
                  (face-explorer-expand-inherit-for-fictitious-display
                   (face-explorer-face-attributes-for-future-frames face)
                   use-remapping)
                  xattrs))
    (face-explorer-maybe-set-inherit-from-default-flag
     xattrs
     (eq face 'default))))


(defun face-explorer-face-prop-attributes-for-fictitious-display (face-prop)
  "The primitive face attributes for FACE-PROP for a fictitious display.

See `face-explorer-parse-face-prop' for more information.

See `face-explorer-match-display-requirements' how to define a
fictitious display."
  (cdr (face-explorer-parse-face-prop
        face-prop #'face-explorer-face-xattributes-for-fictitious-display)))


(defun face-explorer-face-attributes-for-fictitious-display-at (&optional pos)
  "The primitive face attributes at POS for a fictitious display.

The current buffer is used.  Is POS is omitted, point is used.

Face remappings performed by `face-remapping-alist' are handled.

Face information for a fictitious display is used.  See
`face-explorer-match-display-requirements' how to define a
fictitious display."
  (unless pos
    (setq pos (point)))
  (face-explorer-face-prop-attributes-for-fictitious-display
   (face-explorer-face-text-props-at pos)))


(defmacro face-explorer-with-fictitious-display (&rest body)
  "Bind variables for a fictitious display and evaluate BODY, like `progn'.

The variables are all bound to nil."
  `(let (face-explorer-number-of-colors
         face-explorer-window-system-type
         face-explorer-color-class
         face-explorer-background-mode
         face-explorer-match-supports-function)
     ,@body))


(defun face-explorer-set-fictitious-display-as-frame (&optional frame)
  "Set the fictitious display to match FRAME."
  (setq face-explorer-number-of-colors
        (face-explorer-display-number-of-colors frame))
  (setq face-explorer-window-system-type
        (face-explorer-display-window-system-type frame))
  (setq face-explorer-color-class (face-explorer-frame-color-class frame))
  (setq face-explorer-background-mode
        (face-explorer-frame-background-mode frame))
  (setq face-explorer-match-supports-function
        #'face-explorer-default-match-supports-function))


(defmacro face-explorer-with-fictitious-display-as-frame (frame &rest body)
  "Set up a fictitious display like FRAME and evaluate BODY just like `progn'."
  (declare (indent 1))
  `(face-explorer-with-fictitious-display
    (face-explorer-set-fictitious-display-as-frame ,frame)
    ,@body))


;; -------------------------------------------------------------------
;; Face verifier.
;;


;; ------------------------------
;; Verfier: Check colors.
;;

(defvar face-explorer-base-colors '("black" "red" "green" "yellow"
                                    "blue" "magenta" "cyan" "white")
  "Colors used in a 8 color tty.")


(defun face-explorer-valid-color-p (color color-cells)
  "Non-nil if COLOR is a valid color when display support COLOR-CELLS colors.

COLOR-CELLS is an integer representing number of colors or t
representing infinite number of colors."
  (if (string-match "^color-[0-9]+$" color)
      ;; "color-N" only used by 256 TTY:s.
      (eq color-cells 256)
    (or (eq color-cells t)
        (> color-cells 256)
        (string-match "^#[a-fA-F0-9]+$" color)
        (member color face-explorer-base-colors)
        (and (>= color-cells 16)
             (string-match "^bright\\(.*\\)$" color)
             (member (match-string 1 color) face-explorer-base-colors)))))


(defun face-explorer-verify-valid-colors (face)
  "Verify that FACE use valid colors for various fictitious displays."
  (let ((res '()))
    (dolist (number-of-colors '(8 16 256 t))
      (dolist (background '(light dark))
        (let ((face-explorer-number-of-colors number-of-colors)
              (face-explorer-background-mode background)
              (face-explorer-color-class 'color)
              (face-explorer-window-system-type
               (if (eq number-of-colors t)
                   (face-explorer-display-window-system-type)
                 'tty))
              (face-explorer-match-supports-function
               #'face-explorer-default-match-supports-function))
          (let ((plist (face-explorer-face-attributes-for-fictitious-display
                        face)))
            (dolist (attr '(:foreground :distant-foreground :background))
              (let ((color (plist-get plist attr)))
                (when (and color
                           (not (face-explorer-valid-color-p
                                 color number-of-colors)))
                  (push (format "\
Bad %s color \"%s\" in %s background with %s colors"
                                (substring (symbol-name attr) 1)
                                color
                                background
                                (if (integerp number-of-colors)
                                    number-of-colors
                                  "Infinite"))
                        res))))))))
    (nreverse res)))


;; ------------------------------
;; Verfier: Prefer `graphic' over `(x w32 ns)'.
;;

(defun face-explorer-subset-p (lhs rhs)
  "Non-nil if LHS is a subset of RHS.

Comparison is performed using `eq'."
  (let ((res t))
    (dolist (element lhs)
      (unless (memq element rhs)
        (setq res nil)))
    res))


(defun face-explorer-set-eq (lhs rhs)
  "True if LHS and RHS contain the same element, in any order."
  (and (face-explorer-subset-p lhs rhs)
       (face-explorer-subset-p rhs lhs)))


(defun face-explorer-type-list-all-known-types (spec)
  "Non-nil if SPEC includes a construct like `(type x w32 ns)'."
  (let ((res nil))
    (dolist (entry spec)
      (let ((reqs (nth 0 entry)))
        (unless (memq reqs '(t default))
          (dolist (req reqs)
            (when (eq (nth 0 req) 'type)
              (when (or (face-explorer-set-eq (cdr req) '(x w32 ns))
                        (face-explorer-set-eq (cdr req) '(x w32 ns mac)))
                (setq res t)))))))
    res))


(defun face-explorer-verify-use-graphic (face)
  "Verify FACE:s specification doesn't contain `(type x w32 ns)' constructs."
  (let ((is-bad nil))
    (dolist (elt (get face 'theme-face))
      (when (face-explorer-type-list-all-known-types (cadr elt))
        (setq is-bad t)))
    (when (face-explorer-type-list-all-known-types
           (face-default-spec face))
      (setq is-bad t))
    (when (face-explorer-type-list-all-known-types
           (get face 'face-override-spec))
      (setq is-bad t))
    (if is-bad
        (list "Use `(type graphic)' instead of listing all known types")
      '())))


;; ------------------------------
;; Verfier: Don't inherit from default.
;;

(defun face-explorer-verify-dont-inherit-from-default (face)
  "Verify that FACE doens't explicitly inherit from the `deafult' face."
  (if (eq face 'default)
      '()
    (let ((pair (face-explorer-face-xattributes face)))
      (if (car pair)
          (list "Don't inherit from `default'.")
        '()))))


;; ------------------------------

(defvar face-explorer-verifier-list
  `((?c bad-color face-explorer-verify-valid-colors
        "Face should use colors defined for display types.

Some display types, such as TTY:s have a limited palette of
colors.  By using the available colors, like `red' and `green',
the face doesn't have to rely on Emacs to pick a nearby color.")
    (?d inherit-default face-explorer-verify-dont-inherit-from-default
        ,(concat
          "Face should not explicitly inherit from the `default' face.

A face that explicitly inherit from the default face overrides
all face attributes of faces will less priority.  Concretely, if
a face that inherits from the `default' face in used in a context
were another face specify a background color or strike-through,
this will not be rendered where the face is used.

For example:
    "
          (propertize "This is a "  'font-lock-face (list :background "grey"))
          (propertize "good"        'font-lock-face
                      (list :background "grey" :foreground "blue"))
          (propertize " example"    'font-lock-face (list :background "grey"))
          "
    "
          (propertize "This is a " 'font-lock-face (list :background "grey"))
          (propertize "bad"        'font-lock-face (list :foreground "blue"))
          (propertize " example"   'font-lock-face (list :background "grey"))))
    (?g use-graphic face-explorer-verify-use-graphic
        "Prefer `(type graphic)' over `(type x w32 ns).'

The symbol `graphic' will match all official graphical Emacs
environments.  However, some faces list all available graphical
system in an attempt to do the same.  Unfortunately, this will
not work for unofficial environments such as `mac', nor will it
work in future environments (who knows, Emacs might be ported to
modern phones at some point in time."))
  "List of face verifiers.

Each entry in the list should be in the form (KEY ID FUNC DOC)
where ID is a symbol identifying the rule, FUNC a function to
call to test it, and DOC is a string describing the rule.  FUNC
is called with one argument, the face, and returns a list of
strings each representing an issue that should be warned for.")


;; Note: `bad-color' is disabled by default, since it triggers for too
;; many faces.  Besides, Emacs can map most colors to the available
;; palette, so this might not be a problem in practice.
(defvar face-explorer-disabled-verifiers '(bad-color)
  "List of disabled verifier id:s.")


(defun face-explorer-verify-face (face)
  "Check that FACE is well defiend.

Return a list of (ID MSG)."
  (let ((res '()))
    (dolist (entry face-explorer-verifier-list)
      (let ((id   (nth 1 entry))
            (func (nth 2 entry)))
        (unless (memq id face-explorer-disabled-verifiers)
          (setq res (append res
                            (mapcar `(lambda (str)
                                       (list ',id str))
                                    (funcall func face)))))))
    res))


;; -------------------------------------------------------------------
;; Functions to copy highlighted text.
;;

(defvar face-explorer-fontify-buffer 'auto
  "Control when the function `face-explorer-fontify-buffer' fontifies a buffer.

This is typically buffer local.

When nil, the buffer is never fontified.

When t, the buffer is fontified when Font Lock mode is enabled.

When `auto' , the buffer is fontified when Font Lock mode is
enabled except for special cases.")


(defun face-explorer-fontify-buffer (&optional from to)
  "Ensure buffer from FROM to TO is fontified.

When omitted, `point-min' and `point-max' are used, respectively."
  (unless from
    (setq from (point-min)))
  (unless to
    (setq to (point-max)))
  (when (and font-lock-mode
             (or (eq face-explorer-fontify-buffer t)
                 (and (eq face-explorer-fontify-buffer 'auto)
                      ;; Prevent clearing out face attributes
                      ;; explicitly inserted by functions like
                      ;; `list-faces-display'.  (Font-lock mode is
                      ;; enabled, for some reason, in those buffers.)
                      (not (and (derived-mode-p 'help-mode)
                                (not font-lock-defaults))))))
      (font-lock-fontify-region from to)))


(defun face-explorer-insert-with-primitive-attributes
    (source-buffer &optional from to use-fictitious-display)
  "Insert content of SOURCE-BUFFER but with primitive attributes.

The part between FROM and TO is inserted.  When omitted, TO and
FROM defaults to the beginning and end of the buffer,
respectively.

When USE-FICTITIOUS-DISPLAY is non-nil, face information for a
fictitious display is used.  See
`face-explorer-match-display-requirements' how to define a
fictitious display."
  (with-current-buffer source-buffer
    (unless from
      (setq from (point-min)))
    (unless to
      (setq to (point-max))))
  ;; Ensure that the buffer is fontified before converting it.
  (with-current-buffer source-buffer
    (face-explorer-fontify-buffer from to))
  (while (< from to)
    (let* ((next-pos (or (with-current-buffer source-buffer
                           (face-explorer-next-face-property-change
                            from
                            to))
                         to))
           (plist
            (with-current-buffer source-buffer
              (if use-fictitious-display
                  (face-explorer-face-attributes-for-fictitious-display-at
                   from)
                (face-explorer-face-attributes-at from))))
           (str (with-current-buffer source-buffer
                  (buffer-substring-no-properties
                   from
                   next-pos))))
      (insert (if plist
                  (propertize str 'face plist)
                str))
      (setq from next-pos))))


(defun face-explorer-render-with-primitive-attributes
    (&optional from to use-fictitious-display)
  "Return string that look like OBJECT but with primitive attributes.

The part between FROM and TO is converted.  When omitted, TO and
FROM defaults to the beginning and end of the buffer,
respectively.

When USE-FICTITIOUS-DISPLAY is non-nil, face information for a
fictitious display is used.  See
`face-explorer-match-display-requirements' how to define a
fictitious display."
  (let ((source-buffer (current-buffer)))
    (with-temp-buffer
      (face-explorer-insert-with-primitive-attributes
       source-buffer from to use-fictitious-display)
      (buffer-string))))


(defun face-explorer-copy-properties (source-beg source-end source-object
                                                 dest-start dest-object)
  "Copy `face' text properites and overlays.

Copy from SOURCE-BEG to SOURCE-END in SOURCE-OBJECT to DEST-START
in DEST-OBJECT.

Both text properties and overlays are copied."
  (let ((pos source-beg))
    (while (< pos source-end)
      (let ((value (get-char-property pos 'face source-object))
            (next-pos
             (if (bufferp source-object)
                 (with-current-buffer source-object
                   (next-char-property-change pos source-end))
               (next-property-change pos source-object source-end))))
        (when value
          (put-text-property (+ dest-start (- pos source-beg))
                             (+ dest-start (- next-pos source-beg))
                             'face value dest-object))
        (setq pos next-pos)))))


;; -------------------------------------------------------------------
;; Tools
;;


;; -------------------------------------------------------------------
;; Common code various tools
;;

(defvar face-explorer-update-buffer-function #'ignore
  "Function to call to update buffer according to `face-explorer' settings.")

(defmacro face-explorer-with-saved-window-starts (&rest body)
  "Execute BODY and save the window the start of windows displaying buffer."
  (declare (indent 0) (debug t))
  (let ((starts (make-symbol "generated-starts"))
        (line (make-symbol "generated-line")))
    `(let ((,starts '())
           (,line (count-lines (point-min) (line-beginning-position))))
       ;; Record window starts.
       (dolist (frame (frame-list))
         (dolist (window (window-list frame :ignore-minibuffer))
           (when (eq (current-buffer) (window-buffer window))
             (push (list window
                         (count-lines (point-min)
                                      (window-start window)))
                   ,starts))))
       (prog1
           (progn ,@body)
         ;; Restore window starts.
         (dolist (element ,starts)
           (save-excursion
             (goto-char (point-min))
             (forward-line (nth 1 element))
             (unless (eobp)
               (set-window-start (nth 0 element)
                                 (point))))))
       (goto-char (point-min))
       (forward-line ,line))))


(defun face-explorer-table-format-cell (cell &optional width)
  "Format CELL, an entry in a table.

When WIDTH is nil, the cell is formatted to determine the width
of the column.  Otherwise it is the width of the column.

Return the formatted cell as a string, or an integer
corresponding to the width of the cell."
  (cond ((eq cell :table-line)
         (if width
             (make-string width ?-)
           ""))
        ((and (listp cell)
              (eq (nth 0 cell) :table-cell))
         (funcall (nth 1 cell) width))
        ((stringp cell)
         cell)
        (t
         (format "%S" cell))))


;; Note: This function is internal to the `face-explorer' package, it
;; may change in future versions.
(defun face-explorer-insert-table (entries &optional flush-list)
  "Insert a table of ENTRIES, formatted as FLUSH-LIST.

ENTRIES is a list of lines, where each line is a list of cells.
A cell can be on the form:

- :table-line -- A string of \"-\":s, the width of the column, is returned.

- (:table-cell FUNC) -- FUNC is called with one argument,
  WIDTH (see above) twice:

  - The first time to determine the width of the cell.  This is
    indicated with WIDTH as nil.  The function can either return
    a string or an integer, representing the width of the cell.

  - The second time when the table is generated.  WIDTH is the
    width of the column.  The function can either return a string
    or insert the content of the cell by itself and return the
    number of characters that was inserted.  When called, the
    previous columns, and corresponding padding, have been
    inserted.

- (:table-callback FUNC) -- FUNC is called once with no
  arguments, when the previous cells have been inserted.  This is
  intended for side-effects only.

- A string -- it is returned without modification.

- Any other expression is formatted using `%S'.

FLUSH-LIST is a list where each element represents a column.  If
the element is non-nil, the column is right formatted."
  (let ((max-column-width '()))
    ;; Find the widest column.  This will set `max-column-width' to a
    ;; list of integers, one for each column.
    (dolist (line entries)
      (let ((column-number 0))
        (dolist (cell line)
          (unless (and (consp cell)
                       (eq (car cell) :table-callback))
            (let ((str-or-length (face-explorer-table-format-cell cell)))
              (while (<= (length max-column-width) column-number)
                ;; Warning: Don't use '(0) -- the setcar below will
                ;; modify the list stored in the function definition.
                (setq max-column-width (append max-column-width (list 0))))
              (let ((length (if (stringp str-or-length)
                                (length str-or-length)
                              str-or-length)))
                (setcar (nthcdr column-number max-column-width)
                        (max (nth column-number max-column-width)
                             length)))
              (setq column-number (+ column-number 1)))))))
    ;; Insert the table.
    (dolist (line entries)
      (let ((column-number 0))
        (dolist (cell line)
          (if (and (consp cell)
                   (eq (car cell) :table-callback))
              (funcall (nth 1 cell))
            (unless (eq column-number 0)
              (insert " "))
            (let* ((str-or-length (face-explorer-table-format-cell
                                   cell
                                   (nth column-number max-column-width)))
                   (padding
                    (make-string (- (nth column-number max-column-width)
                                    (if (stringp str-or-length)
                                        (length str-or-length)
                                      str-or-length))
                                 ?\s)))
              ;; Note: Callbacks inserting the string themselves
              ;; can't be flushed to the right here.
              (if (stringp str-or-length)
                  (if (nth column-number flush-list)
                      (insert padding str-or-length)
                    (insert str-or-length padding))
                (insert padding)))
            (setq column-number (+ column-number 1))))
        ;; Clear end of line whitespace.
        (while (and (eq (char-before) ?\s)
                    (not (get-text-property (- (point) 1) 'face)))
          (backward-delete-char 1))
        ;; End of line.
        (insert "\n")))))


(defun face-explorer-table-shift-cells (lines start end count)
  "Shift columns in LINES, from column START to END, COUNT times.

LINES is modified destructively.  Return the modified LINES.

This can be used to create a simple animation, to make it easier
to notice subtle differences in the columns."
  (dolist (line lines)
    (let ((original-values '())
          (col start))
      (while (<= col end)
        (push (nth col line) original-values)
        (setq col (+ col 1)))
      (setq original-values (nreverse original-values))
      (let ((col start))
        (while (<= col end)
          (setcar (nthcdr (+ start (% (+ (- col start) count)
                                      (+ (- end start) 1)))
                          line)
                  (nth (- col start) original-values))
          (setq col (+ col 1))))))
  lines)


(defun face-explorer-common-refresh ()
  "Refresh all `face-explorer' buffers."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (funcall face-explorer-update-buffer-function)))
  (redraw-display))


(defun face-explorer-reset-fictitious-display ()
  "Make the fictitious display like the selected frame."
  (interactive)
  (face-explorer-set-fictitious-display-as-frame)
  (face-explorer-common-refresh))


(defun face-explorer-toggle-background-mode ()
  "Toggle background mode in the `face-explorer' list."
  (interactive)
  (setq face-explorer-background-mode
        (if (eq face-explorer-background-mode 'light)
            'dark
          'light))
  (face-explorer-common-refresh))


(defun face-explorer-toggle-window-system ()
  "Toggle window system between current and `tty'.

On a tty, this does nothing."
  (interactive)
  (setq face-explorer-window-system-type
        (if (eq face-explorer-window-system-type 'tty)
            (setq face-explorer-window-system-type
                  (face-explorer-display-window-system-type))
          'tty))
  (face-explorer-common-refresh))


(defun face-explorer-next-color-class ()
  "Select next color class in the `face-explorer' list.

The color class can be `color', `grayscale', or `mono'."
  (interactive)
  (let* ((current-and-remaining
          (memq face-explorer-color-class '(color grayscale mono)))
         (remaining (cdr-safe current-and-remaining)))
    (setq face-explorer-color-class
          (if remaining
              (car remaining)
            'color)))
  (face-explorer-common-refresh))


(defun face-explorer-set-number-of-colors (number-of-colors)
  "Set number of the fictitious display to NUMBER-OF-COLORS."
  (interactive "nNumber of colors: ")
  (setq face-explorer-number-of-colors number-of-colors)
  (face-explorer-common-refresh))


(defun face-explorer-increase-number-of-colors ()
  "Increase number of colors in the `face-explorer' list.

The numbers are increased to the next of 8, 16, 256, and
t (representing infinite)."
  (interactive)
  (setq face-explorer-number-of-colors
        (cond ((null face-explorer-number-of-colors)
               8)
              ((eq face-explorer-number-of-colors t)
               t)
              ((>= face-explorer-number-of-colors 256)
               t)
              ((>= face-explorer-number-of-colors 16)
               256)
              ((>= face-explorer-number-of-colors 8)
               16)
              (t
               8)))
  (face-explorer-common-refresh))


(defun face-explorer-decrease-number-of-colors ()
  "Increase number of colors in the `face-explorer' list.

The numbers are decreased to 8, 16, and 256."
  (interactive)
  (setq face-explorer-number-of-colors
        (cond ((null face-explorer-number-of-colors)
               nil)
              ((eq face-explorer-number-of-colors t)
               256)
              ((> face-explorer-number-of-colors 256)
               256)
              ((> face-explorer-number-of-colors 16)
               16)
              (t
               8)))
  (face-explorer-common-refresh))


(defvar face-explorer--animation-count 0
  "Current position of the columns with sample text.

See `face-explorer-animate'.")


(defun face-explorer-animate ()
  "Rotate sample columns in the table.

There is no way to programatically check that the sample columns
are equal.  By rotating them makes it easier to spot minor
differences between the columns."
  (interactive)
  (unless (local-variable-p 'face-explorer--animation-count)
    (face-explorer-user-error "Buffer does not contain a table with samples"))
  (setq face-explorer--animation-count (+ face-explorer--animation-count 1))
  (revert-buffer))


;; --------------------
;; Verifiers

(defun face-explorer-verifier-interactive-form ()
  "Read face verifier key, used as interactive form.

Return list with one element, the identifier corresponding to the
verifier key entered by the user."
  (let ((entry (assq (read-char "Enter key: ")
                     face-explorer-verifier-list)))
    (list
     (if entry
         (nth 1 entry)
       (face-explorer-user-error "Unknown verifier rule")))))


(defun face-explorer-verifier (id)
  "The entry in `face-explorer-verifier-list' that corresponds to ID."
  (let ((entry nil)
        (rest face-explorer-verifier-list))
    (while rest
      (let ((candidate (pop rest)))
        (when (eq id (nth 1 candidate))
          (setq entry candidate)
          (setq rest '()))))
    entry))


(defun face-explorer-disable-verifier (id)
  "Enable face verifier ID."
  (interactive (face-explorer-verifier-interactive-form))
  (add-to-list 'face-explorer-disabled-verifiers id)
  (face-explorer-common-refresh))


(defun face-explorer-enable-verifier (id)
  "Disable face verifier ID."
  (interactive (face-explorer-verifier-interactive-form))
  (setq face-explorer-disabled-verifiers
        (delq id face-explorer-disabled-verifiers))
  (face-explorer-common-refresh))


(defun face-explorer-disable-all-verifiers ()
  "Disable all face verifiers."
  (interactive)
  (setq face-explorer-disabled-verifiers
        (mapcar
         (lambda (entry)
           (nth 1 entry))
         face-explorer-verifier-list))
  (face-explorer-common-refresh))


(defun face-explorer-enable-all-verifiers ()
  "Enable all face verifiers."
  (interactive)
  (setq face-explorer-disabled-verifiers '())
  (face-explorer-common-refresh))


(defun face-explorer-describe-verifier (id)
  "Display help text for face verifier ID."
  (interactive (face-explorer-verifier-interactive-form))
  (help-setup-xref (cons #'face-explorer-describe-verifier (list id))
                   (called-interactively-p 'interactive))
  (with-help-window (help-buffer)
    (with-current-buffer standard-output
      (let ((entry (face-explorer-verifier id)))
        (insert (format "Decription of the [%c] verifier:\n\n" (nth 0 entry)))
        (insert (nth 3 entry))))))


;; --------------------


(defun face-explorer-context-colors ()
  "Additional face attributes for foreground and background colors.

The colors are used to display sample text in a natural light or
dark background, as specified by
`face-explorer-light-mode-colors' and
`face-explorer-dark-mode-colors'.  Normally, the colors of the
default face is used.  However, when it doesn't provide different
colors for the background modes, it can use fallback colors."
  (let ((spec (if (eq face-explorer-background-mode 'light)
                  face-explorer-light-mode-colors
                face-explorer-dark-mode-colors))
        (res '()))
    (while spec
      (let ((entry (pop spec)))
        (if (consp entry)
            ;; A cons cell -- use the colors it specifies.
            (progn
              (setq res `(:foreground ,(car entry) :background ,(cdr entry)))
              ;; Break the loop.
              (setq spec nil))
          ;; A face -- if it provides colors for both background
          ;; modes, use it.
          (let ((spec-this-color-mode
                 (face-explorer-face-attributes-for-fictitious-display
                  entry))
                (spec-other-color-mode
                 (let ((face-explorer-background-mode
                        (if (eq face-explorer-background-mode 'light)
                            'dark
                          'light)))
                   (face-explorer-face-attributes-for-fictitious-display
                    entry))))
            (let ((this-fg  (plist-get spec-this-color-mode  :foreground))
                  (this-bg  (plist-get spec-this-color-mode  :background))
                  (other-fg (plist-get spec-other-color-mode :foreground))
                  (other-bg (plist-get spec-other-color-mode :background)))
              (setq res `(:foreground ,this-fg :background ,this-bg))
              (when (and this-fg
                         this-bg
                         (or (not (equal this-fg other-fg))
                             (not (equal this-bg other-bg))))
                ;; Break the loop.
                (setq spec nil)))))))
    res))


(defun face-explorer-add-context-colors (plist)
  "Add background and foreground colors to face attribute PLIST.

Return a new property list.

If PLIST contains the color specification, it is retained."
  (face-explorer-join-face-attributes
   plist
   (face-explorer-context-colors)))


(defun face-explorer-insert-fictitious-display-settings ()
  "Insert human readable version of the display type settings."
  (insert "Current fictitious display:\n\n")
  (insert (format "    Color class: %s\n" face-explorer-color-class))
  (insert (format "    Background mode: %s\n" face-explorer-background-mode))
  (insert (format "    Number of colors: %s\n"
                  (cond ((eq face-explorer-number-of-colors t)
                         "Infinite")
                        ((null face-explorer-number-of-colors)
                         "None")
                        (t
                         face-explorer-number-of-colors))))
  (insert (format "    Window system type: %s\n"
                  face-explorer-window-system-type)))


(defun face-explorer-insert-verifier-settings ()
  "Insert human readable version of verifiers."
  (insert "Face verifiers:\n\n")
  ;; TODO: Sort on key.
  (dolist (verifier face-explorer-verifier-list)
    (insert (format "[%c]: [%s] %s\n"
                    (nth 0 verifier)
                    (if (memq (nth 1 verifier)
                              face-explorer-disabled-verifiers)
                        "Disabled"
                      " Enabled")
                    (car (split-string (nth 3 verifier) "\n")))))
  (insert "\n"))


(defvar face-explorer-attribute-name-alist
  '((:family             . "Family")
    (:foundry            . "Foundry")
    (:width              . "Width")
    (:height             . "Height")
    (:weight             . "Weight")
    (:slant              . "Slant")
    (:foreground         . "Foreground")
    (:distant-foreground . "Distant foreground")
    (:background         . "Background")
    (:underline          . "Underline")
    (:overline           . "Overline")
    (:strike-through     . "Strike-through")
    (:box                . "Box")
    (:inverse-video      . "Inverse")
    (:stipple            . "Stipple")
    (:font               . "Font")
    (:fontset            . "Fontset")
    (:inherit            . "Inherit"))
  "Alist from face attribute to human-readable name.")


(defun face-explorer-insert-plist (plist)
  "Insert PLIST, a face property list, as a table."
  (let ((lines '()))
    ;; Insert entries sorted.
    (dolist (pair face-explorer-attribute-name-alist)
      (let ((rest (plist-member plist (car pair))))
        (when rest
          ;; The %S ensures that strings are printed with quotes.
          (push (list "" (cdr pair) (format "%S" (nth 1 rest))) lines))))
    ;; Ensure all entries are printed, even when unknown.
    (while plist
      (let ((attr (pop plist))
            (value (pop plist)))
        (unless (assq attr face-explorer-attribute-name-alist)
          (push (list "" (format "%s" attr) value) lines))))
    (setq lines (reverse lines))
    (push '("" :table-line :table-line) lines)
    (push '("    " "Property" "Value") lines)
    (face-explorer-insert-table lines '(nil t))))


(defun face-explorer-insert-attributes-and-sample (attrs)
  "Insert human readable version of ATTRS and corresponding sample text."
  (insert "Primitive attributes in the selected frame:\n\n")
  (face-explorer-insert-plist attrs)
  (insert "\n\nSample: ")
  (insert (propertize face-explorer-sample-text 'face attrs) "\n\n")
  (insert "\
Note: This sample should look the same as the native sample.  If
they don't look the same you either view then in a different kind
of frame than the one used to create this buffer, or you have
found a problem with the `face-explorer' package.\n"))


(defun face-explorer-insert-attributes-and-sample-in-context (attrs)
  "Insert human readable version of ATTRS and corresponding sample text.

The sample text is inserted twice, once normally and once with a
background that would correspond to the current fictitious
display."
  (insert "Primitive attributes in the current fictitious display:\n\n")
  (face-explorer-insert-fictitious-display-settings)
  (insert "\n")
  (insert "Primitive attributes:\n\n")
  (face-explorer-insert-plist attrs)
  (insert "\n")
  (insert "Sample           : ")
  (insert (propertize face-explorer-sample-text 'face attrs))
  (insert "\n")
  (insert "Sample in context: ")
  (insert (propertize face-explorer-sample-text
                      'face (face-explorer-add-context-colors attrs)))
  (insert "\n"))


(defun face-explorer-insert-typical-displays (face-text-property)
  "Insert table with samples using FACE-TEXT-PROPERTY for typical displays."
  (insert "Typical fictitious displays:\n\n")
  (let ((lines '(("" "" :table-line :table-line)
                 ("" "" "Light" "Dark"))))
    (dolist (colors '(8 16 256 t))
      (let ((line '("    ")))
        (push (format "%s:" (if (eq colors t) "Graphical" colors))
              line)
        (dolist (background '(light dark))
          (let ((face-explorer-number-of-colors colors)
                (face-explorer-background-mode background)
                ;; Monochrome and grayscale displays are no more.
                (face-explorer-color-class 'color)
                (face-explorer-window-system-type
                 (if (eq colors t)
                     (face-explorer-display-window-system-type)
                   'tty))
                (face-explorer-match-supports-function
                 #'face-explorer-default-match-supports-function))
            (push
             (propertize
              face-explorer-sample-text
              'face
              (face-explorer-add-context-colors
               (face-explorer-face-prop-attributes-for-fictitious-display
                face-text-property)))
             line)))
        (push (reverse line) lines)))
    (face-explorer-insert-table (reverse lines) '(nil t))))


;; -------------------------------------------------------------------
;; Minor mode for showing face-related tooltips.
;;


(defvar face-explorer-tooltip-mode-overlay nil
  "Overlay with `echo-help' property, for Face-Explorer-Tooltip mode.")
(make-variable-buffer-local 'face-explorer-tooltip-mode-overlay)


(defvar face-explorer-tooltip-properties
  '(not fontified)
  "The properties Face-Explorer-Tooltip mode should display.

The variable should be a list of symbols to display.  If the
first symbol is `not', then display all symbols except those in
the rest of the list.  To display all properties, set it
to `(not)'.

Properties aliases, defined in `char-property-alias-alist', are
also displayed (or excluded, if the list starts with `not').")


(defvar face-explorer-tooltip-exclude-self t
  "When non-nil, exclude properties used by the mode itself.")


(defun face-explorer-tooltip-kill-overlay ()
  "Remove the Face-Explorer-Tooltip mode overlay.

This is typically attached to `change-major-mode-hook' to ensure
that changing the major mode doesn't leave stray overlays."
  (when face-explorer-tooltip-mode-overlay
    (delete-overlay face-explorer-tooltip-mode-overlay)
    (setq face-explorer-tooltip-mode-overlay nil)))


;;;###autoload
(define-minor-mode face-explorer-tooltip-mode
  "Minor mode to show tooltips for face-related text properties."
  nil
  nil
  nil
  :group 'face-explorer
  (if (not face-explorer-tooltip-mode)
      (face-explorer-tooltip-kill-overlay)
    ;; Enable (unless already enabled).
    (unless face-explorer-tooltip-mode-overlay
      (let ((overlay (make-overlay (point-min) (point-max)
                                   (current-buffer)
                                   nil   ; Front advance
                                   t)))  ; Read advance
        (overlay-put overlay 'help-echo #'face-explorer-tooltip-format)
        (setq face-explorer-tooltip-mode-overlay overlay)))
    (add-to-list 'change-major-mode-hook
                 #'face-explorer-tooltip-kill-overlay)))


;;;###autoload
(define-global-minor-mode face-explorer-tooltip-global-mode
  face-explorer-tooltip-mode
  (lambda ()
    (face-explorer-tooltip-mode 1))
  :group 'face-explorer)


(defun face-explorer-tooltip-format-plist (properties
                                           plist &optional header indent)
  "Format a tooltip snippet for Face-Explorer-Tooltip mode.

PROPERTIES is a list of symbols to display.  If the first symbol
is `not', then display all symbols except those in the rest of
the list.  PLIST is an property list of properties and values.
HEADER is an additional text line to insert in the output, if any
other output is generated.  INDENT is additional space to insert
to non-header lines."
  (unless indent
    (setq indent ""))
  (let ((res '()))
    (while plist
      (let ((prop (pop plist))
            (value (pop plist)))
        (when (and (not (and face-explorer-tooltip-exclude-self
                             (eq prop 'help-echo)
                             (eq value #'face-explorer-tooltip-format)))
                   (if (and (consp properties)
                            (eq (car properties) 'not))
                       (not (memq prop properties))
                     (memq prop properties)))
            (push (format "%s%s: %S" indent prop value)
                  res))))
    (when (and header
               res)
      (push header res))
    res))


(defun face-explorer-tooltip-format (window object pos)
  "Create a help text for the `face' property at the point.

WINDOW is the window in which the tooltip should be shown in.
OBJECT is the object that triggered the tooltip, typically a
buffer or overlay.  POS is the position of the mouse pointer."
  (when (overlayp object)
    (setq object (overlay-buffer object)))
  (with-current-buffer (if (bufferp object)
                           object
                         (current-buffer))
    (let ((properties '()))
      ;; Find property aliases.
      (dolist (prop face-explorer-tooltip-properties)
        ;; Note: `not' is also passed through here, which shouldn't
        ;; hurt.
        (let ((pair (assq prop char-property-alias-alist)))
          (if pair
              (setq properties (append pair properties))
            (push prop properties))))
      (setq properties (reverse properties))
      ;; Note: `get-char-property' adhere to
      ;; `char-property-alias-alist', which mean that it will return
      ;; the value of, say, `font-lock-face' when asking for `face'.
      ;; In this case we don't want that.
      (let* ((plist (text-properties-at pos object))
             (res (face-explorer-tooltip-format-plist properties plist)))
      (when (bufferp object)
        (dolist (overlay (face-explorer-overlays-at pos))
          (setq res (append (face-explorer-tooltip-format-plist
                             properties
                             (overlay-properties overlay)
                             "OVERLAY:"
                             "  ")
                            res))))
      (and res
           (mapconcat (lambda (x) x) res "\n"))))))


;; -------------------------------------------------------------------
;; Common base mode for major modes of tools
;;

(defvar face-explorer-common-mode-base-map
  (let ((map (make-sparse-keymap)))
    (define-key map "+"
      #'face-explorer-increase-number-of-colors)
    (define-key map "-"
      #'face-explorer-decrease-number-of-colors)
    (define-key map "#" #'face-explorer-set-number-of-colors)
    (define-key map "b" #'face-explorer-toggle-background-mode)
    (define-key map "c" #'face-explorer-next-color-class)
    (define-key map "g" #'face-explorer-toggle-window-system)
    (define-key map "r" #'face-explorer-reset-fictitious-display)
    map)
  "Base keymap for `face-explorer' modes.")


;; Note: `make-composed-keymap' could have been used (with the same
;; end result), but it's not available in older Emacs versions.
(defvar face-explorer-common-mode-map
  `(keymap
    ,face-explorer-common-mode-base-map
    ,special-mode-map)
  "Keymap for `face-explorer-common-mode'.")


(define-derived-mode face-explorer-common-mode special-mode
  "FaceExplorerCommon"
  "Base mode for major modes for `face-explorer' tools."
  (font-lock-mode -1)
  (face-explorer-tooltip-mode 1))


(defun face-explorer-define-verifier-keys (map)
  "Add face verifier keys to MAP."
  (define-key map (kbd "wd") #'face-explorer-disable-verifier)
  (define-key map (kbd "we") #'face-explorer-enable-verifier)
  (define-key map (kbd "wa") #'face-explorer-enable-all-verifiers)
  (define-key map (kbd "wn") #'face-explorer-disable-all-verifiers)
  (define-key map (kbd "wx") #'face-explorer-describe-verifier))


;; -------------------------------------------------------------------
;; Common base mode with a current state and history support.
;;

;; Note: `nil' is a valid state whereas `:initial-state' is not.
(defvar face-explorer-common-state-mode--state :initial-state
  "Current state in `face-explorer-common-state-mode' buffers.")

(defvar face-explorer-common-state-mode--history '()
  "Previous states in `face-explorer-common-state-mode' buffers.")

(defvar face-explorer-common-state-mode--future '()
  "Future states in `face-explorer-common-state-mode' buffers.")


(defun  face-explorer-ensure-in-state-mode ()
  "Check that buffer is in `face-explorer-common-state-mode'."
  (unless (derived-mode-p 'face-explorer-common-state-mode)
    (face-explorer-user-error "Not in `face-explorer-common-state-mode'")))


(defun face-explorer-undo-state ()
  "Go to the previous state."
  (interactive)
  (face-explorer-ensure-in-state-mode)
  (if (null face-explorer-common-state-mode--history)
      (face-explorer-user-error "History empty")
    (unless (eq face-explorer-common-state-mode--state :initial-state)
      (push face-explorer-common-state-mode--state
            face-explorer-common-state-mode--future))
    (setq face-explorer-common-state-mode--state
          (pop face-explorer-common-state-mode--history))
    (revert-buffer)))


(defun face-explorer-redo-state ()
  "Go to the next state."
  (interactive)
  (face-explorer-ensure-in-state-mode)
  (if (null face-explorer-common-state-mode--future)
      (face-explorer-user-error "There are no future states")
    (unless (eq face-explorer-common-state-mode--state :initial-state)
      (push face-explorer-common-state-mode--state
            face-explorer-common-state-mode--history))
    (setq face-explorer-common-state-mode--state
          (pop face-explorer-common-state-mode--future))
    (revert-buffer)))


(defun face-explorer-set-state (state)
  "Set buffer to STATE."
  (face-explorer-ensure-in-state-mode)
  (unless (eq face-explorer-common-state-mode--state :initial-state)
    (push face-explorer-common-state-mode--state
          face-explorer-common-state-mode--history))
  (setq face-explorer-common-state-mode--future '())
  (setq face-explorer-common-state-mode--state state)
  (revert-buffer))


;; Note: `make-composed-keymap' could have been used (with the same
;; end result), but it's not available in older Emacs versions.
(defvar face-explorer-common-state-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map face-explorer-common-mode-map)
    (define-key map (kbd "u") #'face-explorer-undo-state)
    (define-key map (kbd "d") #'face-explorer-redo-state)
    map)
  "Keymap for `face-explorer-common-state-mode'.")


(define-derived-mode face-explorer-common-state-mode face-explorer-common-mode
  "FaceExplorerCommonState"
  "Base more for major modes for `face-explorer' tools with state."
  (make-local-variable 'face-explorer-common-state-mode--state)
  (make-local-variable 'face-explorer-common-state-mode--history)
  (make-local-variable 'face-explorer-common-state-mode--future)
  ;; History is permanent.  State must also be permanent, in order to
  ;; save the previous value when a new command is issued.
  (put 'face-explorer-common-state-mode--state   'permanent-local t)
  (put 'face-explorer-common-state-mode--history 'permanent-local t)
  (put 'face-explorer-common-state-mode--future  'permanent-local t))


;; -------------------------------------------------------------------
;; List faces
;;


;;;###autoload
(defun face-explorer-list-faces ()
  "List all faces, with samples for the selected frame and fictitious display.

Information is given on how a face is defined, for example if the
currently active themes affect the face.

The sample text comes from the variable `face-explorer-sample-text'."
  (interactive)
  (with-current-buffer (get-buffer-create "*FaceExplorerFaces*")
    (face-explorer-list-faces-mode)
    (face-explorer-list-faces-refresh)
    (display-buffer (current-buffer))))


;;;###autoload
(defun face-explorer-describe-face-at-point ()
  "Invoke `face-explorer-describe' for face at point."
  (interactive)
  (let ((face (get-text-property (point) 'read-face-name)))
    (unless face
      (face-explorer-user-error "No face at point"))
    (face-explorer-describe-face face)))


(defun face-explorer-list-faces-refresh ()
  "Update content of `face-explorer' buffer."
  (face-explorer-with-saved-window-starts
    (let ((buffer-read-only nil)
          (context-attrs (face-explorer-context-colors)))
      (erase-buffer)
      (insert "\
List of all faces, with samples in current and fictitious displays.\n\n")
      (face-explorer-insert-fictitious-display-settings)
      (insert "\n\n")
      (face-explorer-insert-verifier-settings)
      (insert "\n\n")
      (insert "+---------- Future frames\n")
      (insert "|+---------- Theme face settings\n")
      (insert "||+---------- Default face settings\n")
      (insert "|||+---------- Override\n")
      (insert "||||+---------- Warnings\n")
      (let ((lines '()))
        ;; Note: `lines' is modified destructively.
        (push (copy-tree '("|||||" "" "Native" "Current" "Fictitious")) lines)
        (push (copy-tree '("|||||" "" :table-line :table-line :table-line))
              lines)
        (dolist (face (sort (face-list) #'string-lessp))
          (let ((flags
                 (concat
                  (if (face-explorer-face-attributes-for-future-frames face)
                      "F"
                    " ")
                  (if (get face 'theme-face)
                      "T"
                    " ")
                  (if (face-default-spec face)
                      "D"
                    " ")
                  (if (get face 'face-override-spec)
                      "O"
                    " ")
                  (if (face-explorer-verify-face face)
                      "W"
                    " ")))
                (attrs
                 (face-explorer-join-face-attributes
                  (face-explorer-face-attributes-for-fictitious-display face)
                  context-attrs)))
            (push (list flags
                        (symbol-name face)
                        (propertize face-explorer-sample-text
                                    'face face)
                        (propertize face-explorer-sample-text
                                    'face
                                    (face-explorer-face-attributes face))
                        (propertize face-explorer-sample-text
                                    'face attrs)
                        ;; Make `read-face-name' (used by, for
                        ;; example, `describe-face') default to the
                        ;; face on the line.
                        ;;
                        ;; It is also used by
                        ;; `face-explorer-describe-face-at-point'.
                        `(:table-callback
                          (lambda ()
                            (save-excursion
                              (skip-chars-backward " ")
                              (add-text-properties
                               (line-beginning-position)
                               (point) '(read-face-name ,face))))))
                  lines)))
        (face-explorer-table-shift-cells
         lines 2 4 face-explorer--animation-count)
        (face-explorer-insert-table (nreverse lines))))))


(defun face-explorer-list-faces-revert-buffer (ignore-auto noconfirm)
  "Update content of the `face-explorer-list-faces' buffer.

This function takes, but ignores, two arguments IGNORE-AUTO and
NOCONFIRM.  This makes it suitable to act as a revert buffer
function, c.f. `revert-buffer-function'."
  (face-explorer-list-faces-refresh))


(defvar face-explorer-list-faces-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map face-explorer-common-mode-map)
    (define-key map (kbd "RET") #'face-explorer-describe-face-at-point)
    (define-key map (kbd "TAB") #'face-explorer-animate)
    (face-explorer-define-verifier-keys map)
    map)
  "Keymap for `face-explorer-list-faces-mode'.")


(define-derived-mode face-explorer-list-faces-mode face-explorer-common-mode
  "FaceExplorerListFaces"
  "Major mode used in `face-explorer-list-faces' buffers."
  (font-lock-mode -1)
  (set (make-local-variable 'face-explorer--animation-count) 0)
  (set (make-local-variable 'truncate-partial-width-windows) t)
  (set (make-local-variable 'revert-buffer-function)
       #'face-explorer-list-faces-revert-buffer)
  (set (make-local-variable 'face-explorer-update-buffer-function)
       #'face-explorer-list-faces-refresh))


;; -------------------------------------------------------------------
;; Describe face
;;

;;;###autoload
(defun face-explorer-describe-face (face)
  "Print information about FACE, including the origin of all attributes."
  (interactive (list (read-face-name
                      "Describe face definitions"
                      (or
                       ;; The face used in a buffer.
                       (face-at-point)
                       ;; The name of a face in a buffer.
                       (let ((symbol-name (thing-at-point 'symbol)))
                         (and symbol-name
                              (let ((symbol (intern-soft symbol-name)))
                                (and (facep symbol)
                                     symbol))))
                       ;; Default to `default'.
                       'default))))
  (with-current-buffer (get-buffer-create "*FaceExplorerDefinition*")
    (face-explorer-describe-face-mode)
    (face-explorer-set-state face)
    (display-buffer (current-buffer))))


(defun face-explorer-describe-face-insert-spec (spec
                                                &optional
                                                indentation)
  "Insert description of a face SPEC.

INDENTATION, when specified, is a string inserted before each line."
  (let ((lead "("))
    (dolist (element spec)
      (insert lead)
      (insert (format "%S" element))
      (setq lead "\n ")
      (when indentation
        (setq lead (concat lead indentation))))
    (insert ")")))


(defun face-explorer-describe-face-insert-theme (theme)
  "Insert description of THEME face specification."
  (let ((lead "("))
    (dolist (element theme)
      (insert lead)
      (insert "(")
      (let* ((name (nth 0 element))
             (indentation (make-string (+ 3 (length (symbol-name name)))
                                       ?\s)))
        (insert (format "%S " name))
        (face-explorer-describe-face-insert-spec
         (nth 1 element)
         indentation))
      (insert ")")
      (setq lead "\n "))
    (insert ")")))


(defun face-explorer-describe-face-next ()
  "Describe next face."
  (interactive)
  (let* ((face-list (sort (face-list) #'string-lessp))
         (remaining (memq face-explorer-common-state-mode--state face-list))
         (next (car-safe (cdr remaining))))
    (face-explorer-set-state (or next
                                 (car face-list)))))


(defun face-explorer-describe-face-previous ()
  "Describe previous face."
  (interactive)
  ;; Note: `string-greaterp' is not defined prior to Emacs 25.1.
  (let* ((face-list (sort (face-list) (lambda (str1 str2)
                                        (string-lessp str2 str1))))
         (remaining (memq face-explorer-common-state-mode--state face-list))
         (previous (car-safe (cdr remaining))))
    (face-explorer-set-state (or previous
                                 (car face-list)))))


(defun face-explorer-describe-face-refresh ()
  "Insert content of `face-explorer-describe-face' buffer."
  (face-explorer-with-saved-window-starts
    (let ((buffer-read-only nil)
          (face face-explorer-common-state-mode--state))
      (erase-buffer)
      (let* ((attributes (mapcar #'car custom-face-attributes))
             (max-length
              (apply #'max (mapcar
                            (lambda (attr) (length (symbol-name attr)))
                            attributes))))
        (insert "Face: " (symbol-name face) "\n\n")
        (insert "Native sample: ")
        (insert (propertize face-explorer-sample-text 'face face))
        (insert "\n\n")
        (let ((doc (face-documentation face)))
          (when doc
            (insert "Documentation:\n")
            (insert doc "\n\n")))
        ;; --------------------
        (insert (make-string 70 ?-) "\n\n")
        (insert "Face definition in the selected frame:\n\n")
        (insert "Attributes:\n\n")
        (let ((plist '()))
          (dolist (attr attributes)
            (let ((value (face-attribute face attr nil)))
              (push value plist)
              (push attr plist)))
          (face-explorer-insert-plist plist))
        (insert "\n")
        (face-explorer-insert-attributes-and-sample
         (face-explorer-face-attributes face))
        ;; --------------------
        (insert "\n" (make-string 70 ?-) "\n\n")
        (condition-case nil
            (face-explorer-insert-attributes-and-sample-in-context
             (face-explorer-face-attributes-for-fictitious-display face))
          (error (insert "\n\nERROR OCCURRED WHILE DEDUCING FACE\n\n")))
        ;; --------------------
        (insert "\n" (make-string 70 ?-) "\n\n")
        (face-explorer-insert-typical-displays face)
        ;; --------------------
        (insert "\n" (make-string 70 ?-) "\n\n")
        (face-explorer-insert-verifier-settings)
        (let ((verify-res (face-explorer-verify-face face)))
          (if verify-res
              (progn
                (insert "Issues found:\n\n")
                (dolist (entry verify-res)
                  (insert "- ")
                  (insert (nth 1 entry))
                  (insert "\n")))
            (insert "No issues found\n")))
        ;; --------------------
        (insert "\n" (make-string 70 ?-) "\n\n")
        (insert "Face source information:\n\n")
        (let ((plist '())
              (any-defined nil))
          (dolist (attr attributes)
            (let ((value (face-attribute face attr t)))
              (unless (eq value 'unspecified)
                (setq any-defined t))
              (push value plist)
              (push attr plist)))
          (when any-defined
            (insert "Face definition in new frames:\n\n")
            (face-explorer-insert-plist plist)))
        (let ((theme-face (get face 'theme-face)))
          (when theme-face
            (insert "Theme definition:\n\n")
            (face-explorer-describe-face-insert-theme theme-face)
            (insert "\n\n")))
        (let ((default-spec (face-default-spec face)))
          (when default-spec
            (insert "Face default specification:\n\n")
            (face-explorer-describe-face-insert-spec default-spec)
            (insert "\n\n")))
        (let ((override-spec (get face 'face-override-spec)))
          (when override-spec
            (insert "Face override specification:\n\n")
            (face-explorer-describe-face-insert-spec
             override-spec)
            (insert "\n")))))))


(defun face-explorer-describe-face-revert-buffer (ignore-auto noconfirm)
  "Update content of the `face-explorer-describe-face' buffer.

This function takes, but ignores, two arguments IGNORE-AUTO and
NOCONFIRM.  This makes it suitable to act as a revert buffer
function, c.f. `revert-buffer-function'."
  (face-explorer-describe-face-refresh))


(defvar face-explorer-describe-face-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map face-explorer-common-state-mode-map)
    (define-key map (kbd "RET") #'face-explorer-describe-face)
    (define-key map "n" #'face-explorer-describe-face-next)
    (define-key map "p" #'face-explorer-describe-face-previous)
    map)
  "Keymap for `face-explorer-describe-face-mode'.")


(define-derived-mode face-explorer-describe-face-mode
  face-explorer-common-state-mode
  "FaceExplorerDescribeFace"
  "Major mode used in `face-explorer-describe-face' buffer."
  (set (make-local-variable 'revert-buffer-function)
       #'face-explorer-describe-face-revert-buffer)
  (set (make-local-variable 'face-explorer-update-buffer-function)
       #'face-explorer-describe-face-refresh))


;; -------------------------------------------------------------------
;; Describe face a specification, as used by the `face' text property
;;

;;;###autoload
(defun face-explorer-describe-face-prop (face-text-property)
  "Print information about FACE-TEXT-PROPERTY.

When called interactively, print information about the `face'
text property at the point."
  (interactive (list (get-text-property (point) 'face)))
  (with-current-buffer (get-buffer-create "*FaceExplorerProperty*")
    (face-explorer-describe-face-prop-mode)
    (face-explorer-set-state face-text-property)
    (display-buffer (current-buffer))))


(defun face-explorer-describe-face-prop-refresh ()
  "Insert content of `face-explorer-describe-face-prop' buffer."
  (face-explorer-with-saved-window-starts
    (let ((buffer-read-only nil)
          (face-text-property
           face-explorer-common-state-mode--state))
      (erase-buffer)
      (insert "`face' text property: ")
      (insert (format "%S\n\n" face-text-property))
      (insert "Native sample: ")
      (insert (propertize face-explorer-sample-text 'face face-text-property)
              "\n")
      ;; --------------------
      (insert "\n" (make-string 70 ?-) "\n\n")
      (face-explorer-insert-attributes-and-sample
       (face-explorer-face-prop-attributes
        face-text-property))
      ;; --------------------
      (insert "\n" (make-string 70 ?-) "\n\n")
      (condition-case nil
          (face-explorer-insert-attributes-and-sample-in-context
           (face-explorer-face-prop-attributes-for-fictitious-display
            face-text-property))
        (error (insert "\n\nERROR OCCURRED WHILE DEDUCING FACE\n\n")))
      ;; --------------------
      (insert "\n" (make-string 70 ?-) "\n\n")
      (face-explorer-insert-typical-displays face-text-property))))


(defun face-explorer-describe-face-prop-revert-buffer (ignore-auto
                                                       noconfirm)
  "Update content of the `face-explorer-describe-face-prop' buffer.

This function takes, but ignores, two arguments IGNORE-AUTO and
NOCONFIRM.  This makes it suitable to act as a revert buffer
function, c.f. `revert-buffer-function'."
  (face-explorer-describe-face-prop-refresh))


(defvar face-explorer-describe-face-prop-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map face-explorer-common-state-mode-map)
    (define-key map (kbd "RET") #'face-explorer-describe-face)
    map)
  "Keymap for `face-explorer-describe-face-prop-mode'.")


(define-derived-mode face-explorer-describe-face-prop-mode
  face-explorer-common-state-mode
  "FaceExplorerDescribeFaceProp"
  "Major mode used in `face-explorer-describe-face-prop' buffer."
  (set (make-local-variable 'revert-buffer-function)
       #'face-explorer-describe-face-prop-revert-buffer)
  (set (make-local-variable 'face-explorer-update-buffer-function)
       #'face-explorer-describe-face-prop-refresh))


;; -------------------------------------------------------------------
;; List display features.
;;

;; It's not possible, programatically, to check if a display supports
;; a feature.  However, using specially constructed faces, it's
;; possible to draw text differently depending on the availability of
;; a feature.
;;
;; To see the difference, run this function in a graphical frame and
;; connect using `emacsclient -nw' from a terminal.  The
;; `*DisplayFeatures*' buffer will look different in the two displays.
;;
;; Unfortunately, it's not possible to print different texts on the
;; same position, which puts some restrictions on the layout.

;;;###autoload
(defun face-explorer-list-display-features ()
  "Show which features the current display supports."
  (interactive)
  (with-current-buffer (get-buffer-create "*FaceExplorerDisplayFeatures*")
    (face-explorer-list-display-features-mode)
    (face-explorer-list-display-features-refresh)
    (display-buffer (current-buffer))))


(defvar face-explorer-display-feature-list
  '((box              "Box"             (:box 1))
    (inverse         "Inverse"         (:inverse-video t))
    (strike-through  "Strike-through"  (:strike-through t))
    (overline        "Overline"        (:overline t))
    (underline       "Underline"       (:underline t))
    (underline-wave  "Underline wave"  (:underline (:style wave)))
    (underline-color "Underline color" (:underline (:color "red")))
    (italic          "Italic"          (:slant italic))
    (bold            "Bold"            (:weight bold))))

(defvar face-explorer-display-requirements-list
  '((color     "Color"            (class color)      "Color"     "Not color")
    (grayscale "Grayscale"        (class grayscale)
               "Grayscale"  "Not grayscale")
    (mono      "Mono"             (class mono)       "Mono"      "Not mono")
    (light     "Light background" (background light) "Light"     "Not light")
    (dark      "Dark background"  (background dark)  "Dark"      "Not dark")
    (ns        "NS  (official macOS)"   (type ns)    "NS"        "Not NS")
    (mac       "Mac (by Y. Mitsuharu)"  (type mac)   "Mac"       "Not Mac")
    (w32       "Windows"          (type w32)         "Windows"   "Not Windows")
    (x         "X11"              (type x)           "X11"       "Not X11")
    (tty       "tty"              (type tty)         "tty"       "Not tty")
    (c8        "Colors"           (min-colors 8)     ">= 8"      "< 8")
    (c16       "Colors"           (min-colors 16)    ">= 16"     "< 16")
    (c256      "Colors"           (min-colors 256)   ">= 256"    "< 256")))


(defun face-explorer-remove-face (face)
  "Remove FACE."
  (put face 'face-override-spec nil)
  (put face 'theme-face nil)
  (put face 'face-defface-spec nil)
  (let ((pair (assq face face-new-frame-defaults)))
    (when pair
      (setq face-new-frame-defaults
            (delq pair face-new-frame-defaults)))))


(defun face-explorer-pad-string (string width)
  "Pad STRING with spaces to the right to make result WIDTH characters wide."
  (concat
   string
   (make-string (- width (length string)) ?\s)))


(defun face-explorer-list-display-features-refresh ()
  "Insert content of `face-explorer-list-display-features' buffer."
  (face-explorer-with-saved-window-starts
    (let ((buffer-read-only nil))
      (erase-buffer)
      (insert "\
This buffer contains text using specially constructed faces that
will look differently depending on available display features.
For example, if you run `emacsclient -nw' from a terminal, this
buffer will look differently than it does in a graphical frame.\n\n")
      (insert "Basic display requirements:\n\n")
      (let ((lines '())
            (max-width-supported 0)
            (max-width-not-supported 0))
        (dolist (entry face-explorer-display-requirements-list)
          (setq max-width-supported (max max-width-supported
                                         (length (nth 3 entry))))
          (setq max-width-not-supported (max max-width-not-supported
                                             (length (nth 4 entry)))))
        (push '("Name" "Supported" "Not supported") lines)
        (push '(:table-line :table-line :table-line) lines)
        (dolist (entry face-explorer-display-requirements-list)
          (let ((face-yes (intern (format "face-explorer-feature-%s-yes"
                                          (nth 0 entry))))
                (face-no  (intern (format "face-explorer-feature-%s-no"
                                          (nth 0 entry)))))
            ;; When the text should be invisible, use the same fore- and
            ;; background.  (Unfortunately, it's not possible to specify
            ;; an "invisible" foreground color.)  Remember that the
            ;; buffer could be visible in different frame with different
            ;; background colors, so one can't simply use the background
            ;; color of the default face as the foreground color of the
            ;; invisible text.
            (face-explorer-remove-face face-yes)
            (custom-declare-face face-yes
                                 `((((,@(nth 2 entry)))
                                    :foreground "black" :background "green")
                                   (t :foreground "green" :background "green"))
                                 "")
            (face-explorer-remove-face face-no)
            (custom-declare-face face-no
                                 `((((,@(nth 2 entry)))
                                    :foreground "red" :background "red")
                                   (t :foreground "black" :background "red"))
                                 "")
            (push (list (nth 1 entry)
                        (propertize (face-explorer-pad-string
                                     (nth 3 entry)
                                     max-width-supported)
                                    'face face-yes)
                        (propertize (face-explorer-pad-string
                                     (nth 4 entry)
                                     max-width-not-supported)
                                    'face face-no))
                  lines)))
        (face-explorer-insert-table (nreverse lines)))
      (insert "\n\n")
      ;; --------------------
      (insert "`supports' display requirements:\n\n")
      (let ((lines '()))
        (push '("Name" "Available" "Not available" "Sample") lines)
        (push '(:table-line :table-line :table-line :table-line) lines)
        (dolist (entry face-explorer-display-feature-list)
          (let ((face-yes (intern (format "face-explorer-feature-%s-yes"
                                          (nth 0 entry))))
                (face-no  (intern (format "face-explorer-feature-%s-no"
                                          (nth 0 entry))))
                (face-sample (intern (format "face-explorer-feature-%s"
                                             (nth 0 entry)))))
            ;; When the text should be invisible, use the same fore- and
            ;; background.  (Unfortunately, it's not possible to specify
            ;; an "invisible" foreground color.)  Remember that the
            ;; buffer could be visible in different frame with different
            ;; background colors, so one can't simply use the background
            ;; color of the default face as the foreground color of the
            ;; invisible text.
            (face-explorer-remove-face face-yes)
            (custom-declare-face face-yes
                                 `((((supports ,@(nth 2 entry)))
                                    :foreground "black" :background "green")
                                   (t :foreground "green" :background "green"))
                                 "")
            (face-explorer-remove-face face-no)
            (custom-declare-face face-no
                                 `((((supports ,@(nth 2 entry)))
                                    :foreground "red" :background "red")
                                   (t :foreground "black" :background "red"))
                                 "")
            (face-explorer-remove-face face-sample)
            (custom-declare-face face-sample
                                 `((((supports ,@(nth 2 entry)))
                                    ,@(nth 2 entry))
                                   (t))
                                 "")
            (push (list (nth 1 entry)
                        (propertize "Available" 'face face-yes)
                        (propertize "Not available" 'face face-no)
                        (propertize face-explorer-sample-text
                                    'face face-sample))
                  lines)))
        (face-explorer-insert-table (nreverse lines))))))


(defun face-explorer-list-display-features-revert-buffer (ignore-auto
                                                          noconfirm)
  "Update content of the `face-explorer-list-display-features' buffer.

This function takes, but ignores, two arguments IGNORE-AUTO and
NOCONFIRM.  This makes it suitable to act as a revert buffer
function, c.f. `revert-buffer-function'."
  (face-explorer-list-display-features-refresh))


(defvar face-explorer-list-display-features-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map face-explorer-common-state-mode-map)
    map)
  "Keymap for `face-explorer-list-display-features-mode'.")


(define-derived-mode face-explorer-list-display-features-mode
  face-explorer-common-state-mode
  "FaceExplorerDescribeFace"
  "Major mode used in `face-explorer-list-display-features' buffer."
  (set (make-local-variable 'revert-buffer-function)
       #'face-explorer-list-display-features-revert-buffer)
  (set (make-local-variable 'face-explorer-update-buffer-function)
       #'face-explorer-list-display-features-refresh))


;; -------------------------------------------------------------------
;; Face spec examples
;;


;;;###autoload
(defun face-explorer-list-face-prop-examples ()
  "List sample text with face spec in various variants."
  (interactive)
  (with-current-buffer (get-buffer-create "*FaceExplorerPropertyExamples*")
    (face-explorer-list-face-prop-examples-mode)
    (face-explorer-list-face-prop-examples-refresh)
    (display-buffer (current-buffer))))


(defvar face-explorer-face-prop-example-list
  '(
    ;; --------------------
    ;; Simple cases
    ;;
    ("empty"
     ()
     ())

    ("symbol"
     face-explorer-example-red-foreground
     (:foreground "red"))

    ("remapped"
     face-explorer-example-red-foreground-remapped
     (:foreground "blue"))

    ("remapped-inh"
     face-explorer-example-inherit-from-remapped-face
     (:foreground "blue" :background "yellow"))

    ("non-existing"
     this-face-does-not-exist
     ())

    ("string"
     "face-explorer-example-red-foreground"
     (:foreground "red"))

    ("remapped-str"
     "face-explorer-example-red-foreground-remapped"
     (:foreground "blue"))

    ("one attr"
     (:foreground "red")
     (:foreground "red"))

    ;; --------------------
    ;; Deprecated forms
    ;;
    ("fgcons"
     (foreground-color . "red")
     (:foreground "red"))

    ("bgcons"
     (background-color . "blue")
     (:background "blue"))

    ("conslist"
     ((foreground-color . "red") (background-color . "blue"))
     (:foreground "red" :background "blue"))

    ;; --------------------
    ;; Two different properties.
    ;;
    ("diff1"
     (:foreground "green" :background "red")
     (:background "red" :foreground "green"))

    ("diff2"
     ((:foreground "green") :background "red")
     (:foreground "green" :background "red"))

    ("diff3"
     ((:foreground "green") (:background "red"))
     (:background "red" :foreground "green"))

    ;; --------------------
    ;; The same property twice.
    ;;

    ("same1"
     (:background "green" :background "red")
     (:background "red"))

    ("same2"
     ((:background "green") :background "red")
     (:background "green"))

    ("same3"
     ((:background "green") (:background "red"))
     (:background "green"))

    ;; --------------------
    ;; The same property -- plist face
    ;;

    ("same1b"
     ((:foreground "green") face-explorer-example-red-foreground)
     (:foreground "green"))

    ("same2b"
     ((:foreground "green") (face-explorer-example-red-foreground))
     (:foreground "green"))

    ;; --------------------
    ;; The same property -- plist face
    ;;
    ("same1c"
     (face-explorer-example-red-foreground :foreground "green")
     (:foreground "red"))

    ("same2c"
     ((face-explorer-example-red-foreground) :foreground "green")
     (:foreground "red"))

    ("same3c"
     ((face-explorer-example-red-foreground) (:foreground "green"))
     (:foreground "red"))

    ("same4c"
     (face-explorer-example-red-foreground (:foreground "green"))
     (:foreground "red"))

    ;; --------------------
    ;; The same property -- more complex.
    ;;
    ("same1d"
     ((:foreground "blue") face-explorer-example-red-foreground
      :foreground "green")
     (:foreground "blue"))

    ("same2d"
     ((:foreground "blue" :foreground "yellow")
      face-explorer-example-red-foreground :foreground "green")
     (:foreground "yellow"))

    ("same3d"
     ((:foreground "blue" :background "green" :foreground "yellow")
      face-explorer-example-red-foreground (:foreground "green"))
     (:background "green" :foreground "yellow"))

    ;; --------------------
    ;; Two faces.
    ;;

    ("twofaces1"
     (face-explorer-example-red-foreground
      face-explorer-example-blue-foreground)
     (:foreground "red"))

    ("twofaces2"
     ((face-explorer-example-red-foreground)
      face-explorer-example-blue-foreground)
     (:foreground "red"))

    ("twofaces3"
     (face-explorer-example-red-foreground
      (face-explorer-example-blue-foreground))
     (:foreground "red"))

    ("twofaces4"
     ((face-explorer-example-red-foreground)
      (face-explorer-example-blue-foreground))
     (:foreground "red"))

    ;; --------------------
    ;; Mix
    ;;
    ;; Apparently, it's possible to (sometimes) mix faces and
    ;; anonymous faces, despite the documentation doesn't cover this
    ;; case.
    ;;
    ;; The examples below demonstrates that if a keyword is found in a
    ;; list, the rest of the list is treated as a property list.

    ("mix1"
     ((face-explorer-example-green-foreground :background "red"))
     (:foreground "green" :background "red"))

    ("mix2"
     ((:background "red" face-explorer-example-green-foreground))
     (:background "red"))

    ;; The faces aren't used.
    ("mix3"
     ((:background "red"
                   face-explorer-example-blue-foreground
                   face-explorer-example-green-foreground))
     (:background "red"))

    ;; The result isn't underlined, if this is seen as a plist,
    ;; `face-explorer-example-green-foreground' is a key and
    ;; `:underline' it's value.
    ("mix4"
     ((:background "red"
                   face-explorer-example-green-foreground
                   :underline t))
     (:background "red"))

    ;; The result is underlined, if this is seen as a plist,
    ;; `face-explorer-example-blue-foreground' is a key and
    ;; `face-explorer-example-green-foreground' it's value.
    ("mix5"
     ((:background "red"
                   face-explorer-example-blue-foreground
                   face-explorer-example-green-foreground
                   :underline t))
     (:background "red" :underline t))

    ;; Placing a face or faces into a list doesn't help.
    ("mix6"
     ((:background "red"
                   (face-explorer-example-green-foreground)
                   :underline t))
     (:background "red"))

    ("mix7"
     ((:background "red"
                   (face-explorer-example-blue-foreground
                    face-explorer-example-green-foreground)
                   :underline t))
     (:background "red"))

    ;; Underlines, since the two faces (in lists) are seen as a
    ;; property value pair.
    ("mix8"
     ((:background "red"
                   (face-explorer-example-blue-foreground)
                   (face-explorer-example-green-foreground)
                   :underline t))
     (:background "red" :underline t))

    ;; --------------------
    ;; This demonstrates that members before the first keyword is seen
    ;; as faces, and members after aren't.

    ("mix2b"
     ((face-explorer-example-underlined
       :background "red"
       face-explorer-example-green-foreground))
     (:underline t :background "red"))

    ("mix3b"
     ((face-explorer-example-underlined
       :background "red"
       face-explorer-example-blue-foreground
       face-explorer-example-green-foreground))
     (:underline t :background "red"))

    ;; --------------------
    ;; Check the data structure is two-level deep or it can have
    ;; arbitrary depth.
    ;;
    ;; Apparently, it can have arbitrary depth...

    ("deep1"
     (((((:background "red")))))
     (:background "red"))

    ("deep2"
     (((((face-explorer-example-red-foreground)))))
     (:foreground "red"))

    ("deep3"
     (((((face-explorer-example-red-foreground :background "blue")))))
     (:background "blue" :foreground "red"))

    ;; --------------------
    ;; Faces with both fore- and background (used to test
    ;; `face-remapping-alist').
    ;;
    ("fgbg1"
     (face-explorer-example-red-blue)
     (:foreground "red" :background "blue"))

    ("fgbg2"
     (face-explorer-example-green-yellow)
     (:foreground "green" :background "yellow"))

    ("fgbg3"
     (face-explorer-example-red-blue
      face-explorer-example-green-yellow)
     (:foreground "red" :background "blue"))

    ;; --------------------
    ;; Inherit.
    ;;
    ;; Apparently, one face attribute that can be specified is
    ;; :inherit.  It is "expanded" when it is discovered, and can
    ;; override previously explicitly specified attributes.

    ;; Same face property.
    ("inh-red1"
     (:inherit face-explorer-example-red-foreground)
     (:foreground "red"))

    ;; In a property list, the last element takes precedence.
    ("inh-red2"
     (:inherit face-explorer-example-red-foreground :foreground "blue")
     (:foreground "blue"))

    ("inh-red3"
     (:foreground "blue" :inherit face-explorer-example-red-foreground)
     (:foreground "red"))

    ;; When split into two property lists, the first take precedence.
    ("inh-red4"
     ((:inherit face-explorer-example-red-foreground) (:foreground "blue"))
     (:foreground "red"))

    ("inh-red5"
     ((:foreground "blue") (:inherit face-explorer-example-red-foreground))
     (:foreground "blue"))

    ;; Different face properties.
    ("inh-underlined2"
     (:inherit face-explorer-example-underlined :foreground "blue")
     (:foreground "blue" :underline t))

    ("inh-underlined3"
     (:foreground "blue" :inherit face-explorer-example-underlined)
     (:underline t :foreground "blue"))

    ("inh-underlined4"
     ((:inherit face-explorer-example-underlined) (:foreground "blue"))
     (:foreground "blue" :underline t))

    ("inh-underlined5"
     ((:foreground "blue") (:inherit face-explorer-example-underlined))
     (:underline t :foreground "blue"))

    ;; --------------------
    ;; Inherit non-existing faces.

    ("inh-non-existing"
     (:inherit this-face-does-not-exist)
     ())

    ("inh-non-existing2"
     face-explorer-example-inh-nonexisting
     ())

    ;; --------------------
    ;; Inherit multiple times
    ;;

    ("inh-mult1"
     (:inherit face-explorer-example-red-foreground
               :inherit face-explorer-example-blue-background)
     (:background "blue" :foreground "red"))

    ("inh-mult2"
     ((:inherit face-explorer-example-red-foreground)
      :inherit face-explorer-example-blue-background)
     (:background "blue" :foreground "red"))

    ("inh-mult3"
     ((:inherit face-explorer-example-red-foreground)
      (:inherit face-explorer-example-blue-background))
     (:background "blue" :foreground "red"))

    ;; --------------------
    ;; Inherit multiple times plain face in between.
    ;;
    ("inh-3mult1"
     ((:inherit face-explorer-example-red-foreground)
      face-explorer-example-underlined
      :inherit face-explorer-example-blue-background)
     (:background "blue" :underline t :foreground "red"))


    ;; --------------------
    ;; Inherit list.
    ;;

    ;; Different properties.
    ("inh-list1"
     (:inherit (face-explorer-example-blue-background
                face-explorer-example-red-foreground))
     (:background "blue" :foreground "red"))

    ;; Same properties.  Faces earlier in the list take precedence.
    ("inh-list2"
     (:inherit (face-explorer-example-blue-foreground
                face-explorer-example-red-foreground))
     (:foreground "blue"))


    ;; --------------------
    ;; Inherit from default -- add face property in default.

    ;; `default' defines everything, the others are ignored.
    ("inh-def1"
     ((:inherit default) (:foreground "green") (:background "red"))
     ())

    ("inh-def2"
     ((:foreground "green") (:inherit default) (:background "red"))
     (:foreground "green"))

    ("inh-def3"
     ((:inherit default :foreground "green") (:background "red"))
     (:foreground "green"))

    ("inh-def4"
     ((:foreground "green" :inherit default) (:background "red"))
     ())

    ("inh-def5"
     (:inherit default :foreground "green" :background "red")
     (:foreground "green" :background "red"))


    ;; --------------------
    ;; Check `unspecified' against face.
    ;;
    ("und-face1"
     (face-explorer-example-red-foreground :foreground unspecified)
     (:foreground "red"))

    ("und-face2"
     ((:foreground unspecified) face-explorer-example-red-foreground)
     (:foreground "red"))

    ("und-face3"
     ((face-explorer-example-red-foreground :foreground unspecified)
      face-explorer-example-blue-foreground)
     (:foreground "red"))

    ("und-face4"
     (face-explorer-example-red-blue
      (:foreground unspecified)
      face-explorer-example-green-foreground)
     (:foreground "red" :background "blue"))

    ;; --------------------
    ;; Check `unspecified' against inherited face.
    ;;

    ("und-inh1"
     (:inherit face-explorer-example-red-blue :foreground unspecified)
     (:foreground "red" :background "blue"))

    ("und-inh2"
     (:foreground unspecified :inherit face-explorer-example-red-blue)
     (:foreground "red" :background "blue"))

    ("und-inh3"
     ((:inherit face-explorer-example-red-blue :foreground unspecified)
      face-explorer-example-green-foreground)
     (:foreground "red" :background "blue"))

    ("und-inh4"
     ((:inherit face-explorer-example-red-blue :foreground "yellow")
      face-explorer-example-green-foreground)
     (:foreground "yellow" :background "blue"))

    ("und-inh5"
     ((:inherit face-explorer-example-red-blue :foreground foo)
      face-explorer-example-green-foreground)
     (:foreground "red" :background "blue"))

    ("und-inh6"
     ((:foreground unspecified :inherit face-explorer-example-red-blue)
      face-explorer-example-green-foreground)
     (:foreground "red" :background "blue"))

    ;; --------------------
    ;; Distant foreground.
    ;;

    ("distant1"
     face-explorer-example-distant-foreground
     (:foreground "blue" :distant-foreground "red"))

    ("distant2"
     (face-explorer-example-distant-foreground
      face-explorer-example-blue-background)
     (:foreground "blue" :distant-foreground "red" :background "blue"))

    ;; --------------------
    ;; Distant foreground.
    ;;
    ("alias1"
     face-explorer-example-aliased-face1
     (:foreground "red"))

    ("alias1-str"
     "face-explorer-example-aliased-face1"
     (:foreground "red"))

    ("alias2"
     face-explorer-example-aliased-face2
     (:foreground "red"))

    ("alias2-str"
     "face-explorer-example-aliased-face2"
     (:foreground "red"))

    )
  "List of example of `face' text properties.

Each element of the list is on the form (NAME PROP REFERENCE),
where NAME is the name of the entry, PROP is a text property, and
REFERENCE is the corresponding as primitive text attributes.")


(defun face-explorer-list-face-prop-examples--define-faces ()
  "Define faces used by `face-explorer-face-prop-example-list'."
  (defface face-explorer-example-red-foreground
    '((t :foreground "red"))
    "Face with red foreground to test face-explorer.")

  (defface face-explorer-example-blue-foreground
    '((t :foreground "blue"))
    "Face with blue foreground to test face-explorer.")

  (defface face-explorer-example-green-foreground
    '((t :foreground "green"))
    "Face with green foreground to test face-explorer.")

  (defface face-explorer-example-red-foreground-remapped
    '((t :foreground "red"))
    "Face with red foreground to test face-explorer.

This is remapped by `face-explorer-list-face-prop-examples' to
`face-explorer-example-blue-foreground'.")

  (defface face-explorer-example-inherit-from-remapped-face
    '((t :background "yellow"
         :inherit face-explorer-example-red-foreground-remapped))
    "Face inheriting from remapped face.")

  ;; --------------------
  (defface face-explorer-example-underlined
    '((t :underline t))
    "Underlined face to test face-explorer.")

  (defface face-explorer-example-red-blue
    '((t :foreground "red" :background "blue"))
    "Test face with both background and foreground.")

  (defface face-explorer-example-green-yellow
    '((t :foreground "green" :background "yellow"))
    "Test face with both background and foreground.")

  (defface face-explorer-example-blue-background
    '((t :background "blue"))
    "Face with blue background to test face-explorer.")

  (defface face-explorer-example-inh-nonexisting
    '((t :inherit this-face-does-not-exist))
    "Face that inherits from a non-existing face.")

  (defface face-explorer-example-distant-foreground
    '((t :foreground "blue" :distant-foreground "red"))
    "Face that with a distant foreground.")

  (define-obsolete-face-alias
    'face-explorer-example-aliased-face1
    'face-explorer-example-red-foreground
    "0.0")

  (define-obsolete-face-alias
    'face-explorer-example-aliased-face2
    'face-explorer-example-aliased-face1
    "0.0")

  nil)


(defmacro face-explorer-example-each-face-prop (macro-var macro-count
                                                          &rest body)
  "Loop over `face-explorer-face-prop-example-list'.

MACRO-VAR is bound to an entry (or a variant of an entry) in the
list.  MACRO-COUNT is bound to an integer, stepped up for each
variant.  BODY is executed a number of times with a variant of
the entries in the list.

Currently, two variant are used: the original entry and a list
containing the entry."
  (declare (indent 2))
  `(progn
     (face-explorer-list-face-prop-examples--define-faces)
     (dolist (,macro-var face-explorer-face-prop-example-list)
       (let ((,macro-count 0))
         ,@body)
       (let ((,macro-count 1))
         (when (nth 1 ,macro-var)
           (setq ,macro-var (list (nth 0 ,macro-var)
                                  (list (nth 1 ,macro-var))
                                  (nth 2 ,macro-var)))
           ,@body)))))


(defun face-explorer-list-face-prop-examples-refresh ()
  "List sample text with face text properties in various variants."
  (interactive)
  (face-explorer-with-saved-window-starts
    (let ((buffer-read-only nil))
      (set (make-local-variable 'truncate-partial-width-windows) t)
      (erase-buffer)
      (let ((lines '()))
        ;; Note: Don't use quote, as the lists are destructively
        ;; modified below.
        (push (list "    " "Name" "Native"
                    "Reference" "CurrentFrame" "Fictitious" "Spec")
              lines)
        (push (list "" :table-line :table-line :table-line
                    :table-line :table-line)
              lines)
        (face-explorer-example-each-face-prop entry count
          (push
           (list ""
                 (if (eq count 0)
                     (nth 0 entry)
                   "")
                 (propertize face-explorer-sample-text 'face
                             (nth 1 entry))
                 (propertize face-explorer-sample-text 'face
                             (nth 2 entry))
                 (propertize face-explorer-sample-text 'face
                             (face-explorer-face-prop-attributes
                              (nth 1 entry)))
                 (face-explorer-with-fictitious-display-as-frame
                     nil
                   (propertize
                    face-explorer-sample-text 'face
                    (face-explorer-face-prop-attributes-for-fictitious-display
                     (nth 1 entry))))
                 (format "%S" (nth 1 entry)))
           lines))
        ;; --------------------
        ;; Rotate columns.
        (face-explorer-table-shift-cells
         lines 2 5  face-explorer--animation-count)
        ;; --------------------
        ;; Insert table.
        (face-explorer-insert-table (nreverse lines))))))


(defun face-explorer-list-face-prop-examples-revert-buffer (ignore-auto
                                                            noconfirm)
  "Update content of the `face-explorer-list-face-prop-examples' buffer.

This function takes, but ignores, two arguments IGNORE-AUTO and
NOCONFIRM.  This makes it suitable to act as a revert buffer
function, c.f. `revert-buffer-function'."
  (face-explorer-list-face-prop-examples-refresh))


(defvar face-explorer-list-face-prop-examples-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map face-explorer-common-state-mode-map)
    (define-key map (kbd "TAB") #'face-explorer-animate)
    map)
  "Keymap for `face-explorer-list-face-prop-examples-mode'.")


(define-derived-mode face-explorer-list-face-prop-examples-mode
  face-explorer-common-state-mode
  "FaceExplorerListFacePropExamples"
  "Major mode used in `face-explorer-list-face-prop-examples' buffer."
  (set (make-local-variable 'face-explorer--animation-count) 0)
  (set (make-local-variable 'revert-buffer-function)
       #'face-explorer-list-face-prop-examples-revert-buffer)
  (set (make-local-variable 'face-remapping-alist)
       '((face-explorer-example-red-foreground-remapped
          . face-explorer-example-blue-foreground))))


;; -------------------------------------------------------------------
;; List overlay examples.
;;

;;;###autoload
(defun face-explorer-list-overlay-examples ()
  "List a number of examples with `face' overlays and text properties."
  (interactive)
  (with-current-buffer (get-buffer-create "*FaceExplorerOverlayExamples*")
    (face-explorer-list-overlay-examples-mode)
    (face-explorer-list-overlay-examples-refresh)
    (display-buffer (current-buffer))))


(defvar face-explorer-add-overlays-scale 1
  "Factor for the START and END values of `face-explorer-add-overlays'.

This is used to make the output more readable.")


(defvar face-explorer-list-overlay-examples--table '()
  "Current table used by `face-explorer-list-overlay-examples'.")


(defun face-explorer-add-overlays (name str &rest overlay-descr-list)
  "Insert table entry named NAME with content STR and add OVERLAY-DESCR-LIST.

Each argument after STR, found in OVERLAY-DESCR-LIST, should be a
list where each element represent one overlay.  Each argument
should be on the form (START END PROP VALUE [PROP VALUE] ...),
where PROP is a property and VALUE the value.  START and END the
positions within the string, scaled by
`face-explorer-add-overlays-scale'.

The table entry is inserted at the beginning of
`face-explorer-list-overlay-examples--table'."
  ;; The following implementation is a bit tricky.  The "As frame" and
  ;; "Fictitious" columns can only be highlited once the "Overlays"
  ;; column has been inserted.  However, when the columns are rotated,
  ;; the "Overlays" column might not be drawn first.  This is solved
  ;; by inserting the two columns without face information.  Once the
  ;; entire table line has been inserted, the face information is
  ;; added to the table entries.
  (push
   (list
    name
    ;; ----------
    ;; The following entries will be animated.
    `(:table-cell
      (lambda (width)
        "Insert string, apply overlays, and record the start position."
        (if (null width)
            ,str
          (setq face-explorer-list-overlay-examples--overlays-start (point))
          (let ((p (point)))
            (insert ,str)
            (dolist (arg ',overlay-descr-list)
              (let ((start (pop arg))
                    (end (pop arg)))
                (let ((overlay
                       (make-overlay
                        (+ p (* face-explorer-add-overlays-scale start))
                        (+ p (* face-explorer-add-overlays-scale end)))))
                  (while arg
                    (let ((prop (pop arg))
                          (value (pop arg)))
                      (overlay-put overlay prop value)))))))
          ;; Return an integer that corresponds to the length of the
          ;; inserted string.
          ,(length str))))
    `(:table-cell
      (lambda (width)
        "Return string and record the start position."
        (when width
          (setq face-explorer-list-overlay-examples--as-frame-start (point)))
        ,str))
    `(:table-cell
      (lambda (width)
        "Return string and record the start position."
        (when width
          (setq face-explorer-list-overlay-examples--fictitious-start (point)))
        ,str))
    ;; ----------
    ;; Interpret the overlay information and set the two other columns
    ;; accordingly.  This is called at the end of each line in the
    ;; table.
    `(:table-callback
      (lambda ()
        (let ((as-frame
               (face-explorer-render-with-primitive-attributes
                face-explorer-list-overlay-examples--overlays-start
                (+ face-explorer-list-overlay-examples--overlays-start
                   ,(length str))
                nil))
              (fictitious
               (face-explorer-render-with-primitive-attributes
                face-explorer-list-overlay-examples--overlays-start
                (+ face-explorer-list-overlay-examples--overlays-start
                   ,(length str))
                t)))
          (face-explorer-copy-properties
           0
           ,(length str)
           as-frame
           face-explorer-list-overlay-examples--as-frame-start
           (current-buffer))
          (face-explorer-copy-properties
           0
           ,(length str)
           fictitious
           face-explorer-list-overlay-examples--fictitious-start
           (current-buffer))))))
   face-explorer-list-overlay-examples--table))


;; NOTE: This doesn't use `face-explorer-insert-table' since that
;; doesn't support for defining overlays.
(defun face-explorer-list-overlay-examples-refresh ()
  "Insert content of the `face-explorer-list-overlay-examples' buffer."
  (face-explorer-with-saved-window-starts
    (let ((buffer-read-only nil)
          (face-explorer-add-overlays-scale 3))
      (erase-buffer)

      (insert "\

This buffer contains a number of examples with overlays,
sometimes mixed with text properties.  It can, for example, be
used to test that packages that convert buffers to other formats
can handle overlays correctly.

The `Overlays' column contains the original text, with overlays
and text properties.

The `As frame' column is drawn using primitive face attributes
matching the selected frame.  (This should match the `Overlays'
column, at least when viewed in the original frame.)

The `Fictit.' column is drawn using primitive face
attributes that should match the current fictitious display.\n")

      (let ((face-explorer-list-overlay-examples--table '()))
        (insert "\n" (make-string 70 ?-) "\n\n")
        (face-explorer-insert-fictitious-display-settings)
        (insert "\n" (make-string 70 ?-) "\n\n")

        (push (list "" "Overlays" "As frame" "Fictit.")
              face-explorer-list-overlay-examples--table)
        (push (list "" :table-line :table-line :table-line)
              face-explorer-list-overlay-examples--table)

        ;; ----------------------------------------
        ;; Overlays only
        ;;

        (face-explorer-add-overlays "none" "AAABBBCCC")

        ;; --------------------
        ;; Nested overlays.  Innermost take precedence.
        (face-explorer-add-overlays "nested-diff"
                                    "AAABBBCCC"
                                    '(0 3 face (:foreground "red"))
                                    '(1 2 face (:background "blue")))
        (face-explorer-add-overlays "nested-same1"
                                    "AAABBBCCC"
                                    '(0 3 face (:foreground "red"))
                                    '(1 2 face (:foreground "blue")))

        (face-explorer-add-overlays "nested-same2"
                                    "AAABBBCCC"
                                    '(1 2 face (:foreground "blue"))
                                    '(0 3 face (:foreground "red")))

        (face-explorer-add-overlays "same-start"
                                    "AAABBBCCC"
                                    '(0 2 face (:foreground "blue"))
                                    '(0 3 face (:foreground "red")))

        (face-explorer-add-overlays "same-end"
                                    "AAABBBCCC"
                                    '(1 3 face (:foreground "blue"))
                                    '(0 3 face (:foreground "red")))
        ;; --------------------
        ;; Overlapping, but not nested.
        ;;
        ;; Apparently, the overlay with the later start position take
        ;; precedence (undocumented).
        (face-explorer-add-overlays "overlapping1"
                                    "AAABBBCCC"
                                    '(0 2 face (:foreground "red"))
                                    '(1 3 face (:foreground "blue")))
        (face-explorer-add-overlays "overlapping2"
                                    "AAABBBCCC"
                                    '(1 3 face (:foreground "red"))
                                    '(0 2 face (:foreground "blue")))

        ;; --------------------
        ;; With priorities.
        (face-explorer-add-overlays "prio-inner"
                                    "AAABBBCCC"
                                    '(0 3
                                        face (:foreground "red")
                                        priority 0)
                                    '(1 2
                                        face (:foreground "blue")
                                        priority 1))

        (face-explorer-add-overlays "prio-outer"
                                    "AAABBBCCC"
                                    '(0 3
                                        face (:foreground "red")
                                        priority 1)
                                    '(1 2
                                        face (:foreground "blue")
                                        priority 0))

        (face-explorer-add-overlays "prio-same"
                                    "AAABBBCCC"
                                    '(0 3
                                        face (:foreground "red")
                                        priority 1)
                                    '(0 3
                                        face (:foreground "blue")
                                        priority 0))
        ;; --------------------
        ;; Property in `char-property-alias-alist'.
        ;;
        ;; Apparently, they apply to overlays as well.  When one overlay
        ;; provide both properties, the original property (`face') takes
        ;; precedence over aliased properties (`my-alias-face').  When
        ;; overlays use different aliases, the normal overlay selection
        ;; system selects the overlay to be used.

        ;; Plain.
        (face-explorer-add-overlays "alias0"
                                    "AAABBBCCC"
                                    '(0 3 my-alias-face (:foreground "blue")))

        ;; Both properties in the same overlay.  Order doesn't matter,
        ;; `face' takes precedence.
        (face-explorer-add-overlays "alias0b"
                                    "AAABBBCCC"
                                    '(0 3
                                        face (:foreground "red")
                                        my-alias-face (:foreground "blue")))
        (face-explorer-add-overlays "alias0c"
                                    "AAABBBCCC"
                                    '(0 3
                                        my-alias-face (:foreground "blue")
                                        face (:foreground "red")))

        ;; Nested, using different overlays.  The innermost overlay take
        ;; precedence.  (See also "unnested-aliasN" below.)
        (face-explorer-add-overlays "alias1"
                                    "AAABBBCCC"
                                    '(0 3 face (:foreground "red"))
                                    '(1 2 my-alias-face (:foreground "blue")))

        (face-explorer-add-overlays "alias2"
                                    "AAABBBCCC"
                                    '(0 3 my-alias-face (:foreground "red"))
                                    '(1 2 face (:foreground "blue")))

        ;; ----------------------------------------
        ;; Mix properties and overlays.
        ;;
        (face-explorer-add-overlays "text-diff"
                                    (propertize "AAABBBCCC"
                                                'face '(:foreground "red"))
                                    '(1 2 face (:background "blue")))

        (face-explorer-add-overlays "text-same"
                                    (propertize "AAABBBCCC"
                                                'face '(:foreground "red"))
                                    '(1 2 face (:foreground "blue")))

        (face-explorer-add-overlays "text-alias1"
                                    (propertize "AAABBBCCC"
                                                'my-alias-face
                                                '(:foreground "red"))
                                    '(1 2 face (:background "blue")))

        (face-explorer-add-overlays "text-alias2"
                                    (propertize "AAABBBCCC"
                                                'face '(:foreground "red"))
                                    '(1 2 my-alias-face (:background "blue")))

        ;; ----------------------------------------
        ;; Only text properties. (Conceptually, this should be in
        ;; `face-explorer-list-face-prop-examples', however, the current
        ;; implementation only supports inserting the `face' attribute.)
        (face-explorer-add-overlays "prop-alias0"
                                    (propertize "AAABBBCCC"
                                                'my-alias-face
                                                '(:foreground "blue")))

        (face-explorer-add-overlays "prop-alias1"
                                    (propertize "AAABBBCCC"
                                                'face
                                                '(:foreground "red")
                                                'my-alias-face
                                                '(:foreground "blue")))

        (face-explorer-add-overlays "prop-alias2"
                                    (propertize "AAABBBCCC"
                                                'my-alias-face
                                                '(:foreground "blue")
                                                'face
                                                '(:foreground "red")))
        ;; --------------------
        ;; Rotate columns.
        (face-explorer-table-shift-cells
         face-explorer-list-overlay-examples--table
         1 3  face-explorer--animation-count)
        ;; --------------------
        ;; Insert table.
        (face-explorer-insert-table
         (reverse face-explorer-list-overlay-examples--table)))

      ;; ----------------------------------------
      (insert "\n" (make-string 70 ?-) "\n\n")
      (insert "\

The following examples are non-deterministic as they have the
same priority and doesn't nest (which is how Emacs sort
overlays).  Effectively, this mean that they can be rendered
differently each time the buffer is redrawn (try pressing 'g' a
couple of times).  However, the important thing is that 1) the
deduced columns should look like the native and 2) when
converting this buffer to other formats, the columns should stay
the same.\n\n")

      ;; WARNING: Sometimes, these comes out as red, sometimes blue --
      ;; in other words, the order really is undefined!!! Fortunately,
      ;; the face-explorer interpretation of them always seems to
      ;; match the original.

      ;; --------------------
      ;; Unnested overlays.

      (let ((face-explorer-list-overlay-examples--table '()))
        (push (list "" "Overlays" "As frame" "Fictit.")
              face-explorer-list-overlay-examples--table)
        (push (list "" :table-line :table-line :table-line)
              face-explorer-list-overlay-examples--table)

        (face-explorer-add-overlays "unnested1"
                                    "AAABBBCCC"
                                    '(0 3 face (:foreground "red"))
                                    '(0 3 face (:foreground "blue")))

        (face-explorer-add-overlays "unnested2"
                                    "AAABBBCCC"
                                    '(0 3 face (:foreground "blue"))
                                    '(0 3 face (:foreground "red")))
        ;; Aliased text properties.
        (face-explorer-add-overlays "unnested-alias1"
                                    "AAABBBCCC"
                                    '(0 3 face (:foreground "red"))
                                    '(0 3 my-alias-face (:foreground "blue")))

        (face-explorer-add-overlays "unnested-alias2"
                                    "AAABBBCCC"
                                    '(0 3 my-alias-face (:foreground "red"))
                                    '(0 3 face (:foreground "blue")))

        ;; --------------------
        ;; Rotate columns.
        (face-explorer-table-shift-cells
         face-explorer-list-overlay-examples--table
         1 3 face-explorer--animation-count)
        ;; --------------------
        ;; Insert table.
        (face-explorer-insert-table
         (reverse face-explorer-list-overlay-examples--table)))
      nil)))


(defvar face-explorer-list-overlay-examples-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map face-explorer-common-mode-map)
    (define-key map (kbd "TAB") #'face-explorer-animate)
    map)
  "Keymap for `face-explorer-list-overlay-examples-mode'.")


(define-derived-mode face-explorer-list-overlay-examples-mode
  face-explorer-common-mode
  "FaceExplorerListOverlayExamples"
  "Major mode used in `face-explorer-list-overlay-examples' buffers."
  (font-lock-mode -1)
  (set (make-local-variable 'face-explorer--animation-count) 0)
  (set (make-local-variable 'char-property-alias-alist)
       '((face my-alias-face)))
  (set (make-local-variable 'truncate-partial-width-windows) t)
  (set (make-local-variable 'revert-buffer-function)
       #'face-explorer-list-overlay-examples-revert-buffer)
  (set (make-local-variable 'face-explorer-update-buffer-function)
       #'face-explorer-list-overlay-examples-refresh))


(defun face-explorer-list-overlay-examples-revert-buffer
    (ignore-auto noconfirm)
  "Update content of the `face-explorer-list-overlay-examples' buffer.

This function takes, but ignores, two arguments IGNORE-AUTO and
NOCONFIRM.  This makes it suitable to act as a revert buffer
function, c.f. `revert-buffer-function'."
  (face-explorer-list-overlay-examples-refresh))


;; -------------------------------------------------------------------
;; Minor mode.
;;

;; Known problems:
;;
;; - The mode line isn't drawn in inverse video, when the background
;;   mode differs from the frame default.
;;
;; - The `fringe' face can't be remapped, since that result in display
;;   problems.
;;
;; - Faces defined or modified don't take effect in buffers where this
;;   mode is enabled.  However, toggling the mode off and on should
;;   correct this.

(defvar face-explorer-simulate-display-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c !") face-explorer-common-mode-base-map)
    map))


;;;###autoload
(define-minor-mode face-explorer-simulate-display-mode
  "Minor mode to simulate another display type.

The following keys are used:
\\{face-explorer-simulate-display-mode-map}"
  nil
  nil
  face-explorer-simulate-display-mode-map
  :group 'face-explorer
  (if face-explorer-simulate-display-mode
      (progn
        (set (make-local-variable 'face-explorer-update-buffer-function)
             #'face-explorer-simulate-display-update)
        (face-explorer-simulate-display-update))
    (kill-local-variable 'face-remapping-alist)
    (kill-local-variable 'face-explorer-update-buffer-function)))


;;;###autoload
(define-global-minor-mode face-explorer-simulate-display-global-mode
  face-explorer-simulate-display-mode
  (lambda ()
    (unless (derived-mode-p 'face-explorer-common-mode)
      (face-explorer-simulate-display-mode 1)))
  :group 'face-explorer)


(defun face-explorer-simulate-display-update ()
  "Update `face-remapping-alist' according to `face-explorer' settings."
  (set (make-local-variable 'face-remapping-alist) '())
  (dolist (face (face-list))
    (let ((plist
           (face-explorer-face-attributes-for-fictitious-display face)))
      ;; When remapping the `fringe' face, the fringe is incorrectly
      ;; rendered.  Sometimes the fringe is rendered in the the
      ;; original face and sometimes in the remapped face, regardless
      ;; of whether the window contains a buffer with a remapped
      ;; `fringe' face.
      (unless (eq face 'fringe)
        (when (eq face 'default)
          ;; Override the `:foreground' and `:background' attributes
          ;; in the `default' face, in case they didn't differ in
          ;; light and dark mode.  This will, at least, give some
          ;; impression of a different color mode.  (If they do differ,
          ;; this function, normally, will return the same attributes
          ;; as already retrieved, making this operation a no-op.)
          (setq plist
                (face-explorer-join-face-attributes
                 (face-explorer-context-colors)
                 plist))
          ;; `default' must be fully specified. If not, an error is
          ;; issued, e.g. when moving between lines, as Emacs tries to
          ;; find the font height.  By ending the replacement list
          ;; with `default', the other attributes are taken from the
          ;; normal definition of `default'.
          (setq plist (append plist '(default))))
        (unless (null plist)
          (push (cons face plist)
                face-remapping-alist))))))


;; -------------------------------------------------------------------
;; The end.
;;

(provide 'face-explorer)

;;; face-explorer.el ends here
