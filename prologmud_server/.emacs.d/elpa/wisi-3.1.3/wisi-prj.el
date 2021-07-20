;;; wisi-prj.el --- project integration -*- lexical-binding:t -*-
;;
;; Copyright (C) 2019 - 2020  Free Software Foundation, Inc.
;;
;; Author: Stephen Leake <stephen_leake@member.fsf.org>
;;
;; This file is part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Usage:
;;
;; See wisi.info (compiled from wisi.texi).

;;; Code:

(require 'cl-lib)
(require 'find-file)
(require 'wisi)

(cl-defstruct wisi-prj
  name     ;; A user-friendly string, used in menus and messages.

  compile-env
  ;; List of strings NAME=VALUE for `compilation-environment'; used
  ;; when running the compiler or makefile. Also prepended to
  ;; `process-environment' when the project file is parsed, or when
  ;; the project file is used by a tool in an external process.

  file-env
  ;; Environment (list of strings NAME=VALUE) set in project file;
  ;; prepended to `process-environment' running tools in an external
  ;; process.

  compiler
  xref
  ;; xref functionality is often provided by the compiler. We allow
  ;; for separate compiler and xref objects, to handle the case where
  ;; the compiler is a cross-compiler for an embedded target, and xref
  ;; is provided by a host compiler.

  (case-exception-files nil)
  ;; List of casing exception files; from `casing' project variable.
  ;;
  ;; New exceptions may be added interactively via
  ;; `wisi-case-create-exception'.  If an exception is defined in
  ;; multiple files, the first occurence is used.
  ;;
  ;; The file format is one word per line, which gives the casing to be
  ;; used for that word in source code.  If the line starts with
  ;; the character *, then the exception will be used for partial
  ;; words that either start at the beginning of a word or after a _
  ;; character, and end either at the end of the word or at a _
  ;; character.  Characters after the first word are ignored, and not
  ;; preserved when the list is written back to the file."

  (case-full-exceptions '())
  ;; Alist of full words that have special casing, built from
  ;; case-exception-files. Indexed by properly cased word; value is t.

  (case-partial-exceptions '())
  ;; Alist of partial words that have special casing, built from
  ;; project casing files list partial word exceptions. Indexed by
  ;; properly cased word; value is t.

  source-path ;; list of absolute directory file names

  file-pred
  ;; Function taking an absolute file name, returns non-nil
  ;; if the file should be included in `project-files'.
  )

(defun wisi-prj-require-prj ()
  "Return current `wisi-prj' object.
Throw an error if current project is not an wisi-prj."
  (let ((prj (project-current)))
    (if (wisi-prj-p prj)
	prj
      (error "current project is not a wisi project."))))

(defun wisi-prj-current-prj ()
  "Return current `wisi-prj' object.
If (project-current) does not return a wisi-prj, return a default prj."
  (let ((prj (project-current)))
    (if (wisi-prj-p prj)
	prj
      (make-wisi-prj :name "default"))))

(defvar wisi-prj-file-extensions (list "prj")
  "List of wisi project file extensions.
Used when searching for project files.")

(defvar wisi-prj--cache nil
  "Alist holding currently parsed project objects.
Indexed by absolute project file name.")

(cl-defgeneric wisi-prj-default (prj)
  "Return a project with default values.
Used to reset a project before refreshing it.")

(cl-defgeneric wisi-prj-parse-one (_project _name _value)
  "If recognized by PROJECT, set NAME, VALUE in PROJECT, return non-nil.
Else return nil."
  nil)

(cl-defgeneric wisi-prj-parse-final (_project _prj-file-name)
  "Do any final processing on PROJECT
after the project file PRJ-FILE-NAME is parsed."
  nil)

(cl-defgeneric wisi-prj-select (project)
  "PROJECT is selected; perform any required actions.")

(cl-defgeneric wisi-prj-deselect (project)
  "PROJECT is deselected; undo any select actions.")

(cl-defgeneric wisi-prj-refresh-cache (prj not-full)
  "Reparse the project file for PRJ, refresh all cached data in PRJ.
If NOT-FULL is non-nil, very slow refresh operations may be skipped.")

(cl-defgeneric wisi-prj-identifier-ignore-case (prj)
  "Return non-nil if case should be ignored when comparing identifiers.")

;; We provide nil defaults for some methods, because some language
;; modes don't have a language-specific compiler (eg java-wisi) or
;; xref process (eg gpr-mode).

(cl-defgeneric wisi-compiler-parse-one (compiler project name value)
  "Set NAME, VALUE in COMPILER, if recognized by COMPILER.
PROJECT is an `wisi-prj' object; COMPILER is `wisi-prj-compiler'.")

(cl-defgeneric wisi-compiler-parse-final (_compiler _project _prj-file-name)
  "Do any compiler-specific processing on COMPILER and PROJECT
after the project file PRJ-FILE-NAME is parsed."
  nil)

(cl-defgeneric wisi-compiler-select-prj (_compiler _project)
  "PROJECT has been selected; do any compiler-specific actions required."
  nil)

(cl-defgeneric wisi-compiler-deselect-prj (_compiler _project)
  "PROJECT has been de-selected; undo any compiler-specific select actions."
  nil)

(cl-defgeneric wisi-compiler-show-prj-path (compiler)
  "Display buffer listing project file search path.")

(cl-defgeneric wisi-compiler-fix-error (compiler source-buffer)
  "Attempt to fix a compilation error, return non-nil if fixed.
Current buffer is compilation buffer; point is at an error message.
SOURCE-BUFFER contains the source code referenced in the error message.")

(cl-defgeneric wisi-xref-parse-one (_xref _project _name _value)
  "If recognized by XREF, set NAME, VALUE in XREF, return non-nil.
Else return nil."
  nil)

(cl-defgeneric wisi-xref-parse-final (_xref _project _prj-file-name)
  "Do any xref-specific processing on XREF and PROJECT
after the project file PRJ-FILE-NAME is parsed."
  nil)

(cl-defgeneric wisi-xref-select-prj (_xref _project)
  "PROJECT has been selected; do any xref-specific actions required."
  nil)

(cl-defgeneric wisi-xref-deselect-prj (_xref _project)
  "PROJECT has been de-selected; undo any xref-specific select actions."
  nil)

(cl-defgeneric wisi-xref-refresh-cache (_xref _project _no-full)
  "Refresh cached information in XREF. If no-full is non-nil,
slow refresh operations may be skipped."
  nil)

(cl-defgeneric wisi-xref-completion-table (xref project)
  "Return a completion table of names defined in PROJECT, for navigating to the declarations.
The table is an alist of (ANNOTATED-SYMBOL . LOC), where:

- ANNOTATED-SYMBOL is the simple name and possibly annotations
such as function arguments, controlling type, containing package,
and line number.

- LOC is the declaration of the name as a list (FILE LINE
COLUMN).")

(cl-defgeneric wisi-xref-completion-delim-regex (xref)
  "Return the value for `completion-pcm--delim-wild-regex' to be used with `wisi-xref-completion-table'.")

(cl-defgeneric wisi-xref-completion-regexp (xref)
  "Return a regular expression matching the result of completing with `wisi-xref-completion-table'.
Group 1 must be the simple symbol; the rest of the item may be annotations.")

(cl-defgeneric wisi-xref-completion-at-point-table (xref project)
  "Return a completion table of names defined in PROJECT, for `completion-at-point'.
The table is a simple list of symbols.")

(cl-defgeneric wisi-xref-definitions (xref project item)
  "Return all definitions (classwide) of ITEM (an xref-item), as a list of xref-items.")

(cl-defgeneric wisi-xref-references (xref project item)
  "Return all references to ITEM (an xref-item), as a list of xref-items.")

(cl-defgeneric wisi-xref-other (project &key identifier filename line column)
  "Return cross reference information.
PROJECT - dispatching object, normally a `wisi-prj' object.
IDENTIFIER - an identifier or operator_symbol
FILENAME - absolute filename containing the identifier
LINE - line number containing the identifier (may be nil)
COLUMN - Emacs column of the start of the identifier (may be nil)
Point is on the start of the identifier.
Returns a list (FILE LINE COLUMN) giving the corresponding location;
FILE is an absolute file name.  If point is at the specification, the
corresponding location is the
body, and vice versa.")

(defvar-local wisi-xref-full-path nil
  "If non-nil, xref functions show full paths in results.")

(defun wisi-goto-source (file line column)
  "Find and select FILE, at LINE and COLUMN.
FILE may be absolute, or on `compilation-search-path'.
LINE, COLUMN are Emacs origin."
  (let ((file-1
	 (if (file-name-absolute-p file) file
	   (ff-get-file-name compilation-search-path file))))
    (if file-1
	(setq file file-1)
      (error "File %s not found; installed library, or set project?" file))
    )

  (push-mark (point) t)

  (let ((buffer (get-file-buffer file)))
    (cond
     ((bufferp buffer)
      ;; use pop-to-buffer, so package other-frame-window works.
      (pop-to-buffer buffer (list #'display-buffer-same-window)))

     ((file-exists-p file)
      (find-file file))

     (t
      (error "'%s' not found" file))))

  ;; move the cursor to the correct position
  (goto-char (point-min))
  (forward-line (1- line))
  (forward-char column))

(defun wisi-show-xref (xref)
  "Display XREF location."
  (let ((marker (xref-location-marker (xref-item-location xref))))
    (push-mark)
    (pop-to-buffer (marker-buffer marker) (list #'display-buffer-same-window))
    (goto-char (marker-position marker))))

(defun wisi-filter-table (table file)
  "If FILE is nil, return TABLE. Otherwise return only items in TABLE with location FILE."
  (cond
   ((null file)
    table)

   (t
    (let (result)
      (dolist (item table)
	(when (string= file (car (cdr item)))
	  (push item result)))
      result))))

(defun wisi-get-identifier (prompt)
  "Get identifier at point, or, if no identifier at point or with user arg, prompt for one.
Single user arg completes on all identifiers in project; double
user arg limits completion to current file."
  ;; Similar to xref--read-identifier, but uses a different completion
  ;; table, because we want a more specific reference.
  (let* ((prj (project-current))
         (def (xref-backend-identifier-at-point prj)))

    (cond
     ((or current-prefix-arg
          (not def))
      (let* ((table (wisi-filter-table (wisi-xref-completion-table (wisi-prj-xref prj) prj)
				       (when (equal '(16) current-prefix-arg) (buffer-file-name))))
	     (completion-pcm--delim-wild-regex (wisi-xref-completion-delim-regex (wisi-prj-xref prj)))
	     (id
	      ;; Since the user decided not to use the identifier at
	      ;; point, don't use it as the default.
              (completing-read prompt table nil nil nil 'xref--read-identifier-history)))
        (if (equal id "")
            (user-error "No identifier provided")

	  ;; The user may have forced exit from completing-read with a
	  ;; string that is not in the table (because gpr-query is out
	  ;; of date, for example).
          (or (and (consp (car table)) ;; alist; return key and value.
		   (assoc id table))
	      id))))
     (t def))))

(defun wisi-goto-spec/body (identifier)
  "Goto declaration or body for IDENTIFIER (default symbol at point).
If no symbol at point, or with prefix arg, prompt for symbol, goto spec."
  (interactive (list (wisi-get-identifier "Goto spec/body of: ")))
  (let ((prj (project-current))
	desired-loc)
    (cond
     ((consp identifier)
      ;; alist element from wisi-xref-completion-table; desired
      ;; location is primary declaration
      (setq desired-loc
	    (xref-make (car identifier)
		       (xref-make-file-location
			(nth 0 (cdr identifier)) ;; file
			(nth 1 (cdr identifier)) ;; line
			(nth 2 (cdr identifier)) ;; column
			))))

     ((stringp identifier)
      ;; from xref-backend-identifier-at-point; desired location is 'other'
      (let ((item (wisi-xref-item identifier prj)))
	(condition-case-unless-debug err
	    (with-slots (summary location) item
	      (let ((eieio-skip-typecheck t))
		(with-slots (file line column) location
		  (let ((target
			 (wisi-xref-other
			  (wisi-prj-xref prj) prj
			  :identifier summary
			  :filename file
			  :line line
			  :column column)))
		    (setq desired-loc
			  (xref-make summary
				     (xref-make-file-location
				      (nth 0 target) ;; file
				      (nth 1 target) ;; line
				      (nth 2 target))) ;; column
			  )))))
	  (user-error ;; from gpr-query; current file might be new to project, so try wisi-names
	   (let ((item (assoc identifier (wisi-names nil t))))
	     (if item
		 (setq desired-loc
		       (xref-make identifier
				  (xref-make-file-location
				   (nth 1 item) ;; file
				   (nth 2 item) ;; line
				   (nth 3 item))))
	       (signal (car err) (cdr err)))))
	  )))

     (t ;; something else
      (error "unknown case in wisi-goto-spec/body")))
    (wisi-show-xref desired-loc)
    ))

(cl-defgeneric wisi-prj-identifier-at-point (_project)
  "Return (IDENT START END) giving the identifier and its bounds at point.
Return nil if no identifier is at point."
  ;; default implementation
  (let ((bounds (bounds-of-thing-at-point 'symbol)))
    (when bounds
      (list (car bounds) (cdr bounds) (buffer-substring-no-properties (car bounds) (cdr bounds))))))

(defun wisi-completion-at-point ()
  "For `completion-at-point-functions'."
  (let ((prj (project-current)))
    (when (wisi-prj-p prj)
      (save-excursion
	(let ((table (wisi-xref-completion-at-point-table (wisi-prj-xref prj) prj))
	      (bounds (wisi-prj-identifier-at-point prj)))
	  (when bounds
	    ;; xref symbol table may be out of date; try dabbrevs
	    (list (nth 0 bounds) (nth 1 bounds) table :exclusive 'no))
	  )))
    ))

(defun wisi-check-current-project (file-name &optional default-prj-function)
  "If FILE-NAME (must be absolute) is found in the current
project source directories, return the current
project. Otherwise, if the current project is a wisi project,
throw an error.  If the current project is not a wisi project,
and DEFAULT-PRJ-FUNCTION is non-nil, use it to return a default
project. Otherwise throw an error."
  (let ((visited-file (file-truename file-name)) ;; file-truename handles symbolic links
        (project (project-current)))
    (if (wisi-prj-p project)
	(let ((found-file (locate-file (file-name-nondirectory visited-file)
				       (wisi-prj-source-path project))))
	  (unless found-file
	    (error "current file not part of current project; wrong project?"))

	  (setq found-file (file-truename found-file))

	  ;; (nth 10 (file-attributes ...)) is the inode; required when hard
	  ;; links are present.
	  (let* ((visited-file-inode (nth 10 (file-attributes visited-file)))
		 (found-file-inode (nth 10 (file-attributes found-file))))
	    (unless (equal visited-file-inode found-file-inode)
              (error "%s (opened) and %s (found in project) are two different files"
		     file-name found-file)))
	  project)

      ;; create a project?
      (if default-prj-function
	  (funcall default-prj-function nil (file-name-directory file-name))
	(error "current project is not a wisi project."))
      )))

(cl-defgeneric wisi-xref-parents (xref project &key identifier filename line column)
  "Displays parent type declarations.
XREF    - dispatching object.
PROJECT - a `wisi-prj' object.
IDENTIFIER - an identifier or operator_symbol
FILENAME - absolute filename containing the identifier
LINE - line number containing the identifier
COLUMN - Emacs column of the start of the identifier

Displays a buffer in compilation-mode giving locations of the parent type declarations.")

(defun wisi-show-declaration-parents ()
  "Display the locations of the parent type declarations of the type identifier around point."
  (interactive)
  (let* ((project (wisi-check-current-project (buffer-file-name)))
	 (id (wisi-prj-identifier-at-point project)))
    (wisi-xref-parents
     (wisi-prj-xref project)
     project
     :identifier (nth 2 id)
     :filename (file-name-nondirectory (buffer-file-name))
     :line (line-number-at-pos)
     :column (save-excursion (goto-char (nth 0 id)) (current-column)))
    ))

(cl-defgeneric wisi-xref-all (xref project &key identifier filename line column local-only append)
  "Displays cross reference information.
XREF    - dispatching object.
PROJECT - a `wisi-prj' object.
IDENTIFIER - an identifier or operator_symbol (a string).
FILENAME - absolute filename containing the identifier
LINE - line number containing the identifier
COLUMN - Emacs column of the start of the identifier
LOCAL-ONLY - if t, show references in FILE only
APPEND - if t, keep previous output in result buffer
Displays a buffer in compilation-mode giving locations where the
identifier is declared or referenced.")

(defun wisi-show-references (&optional append)
  "Show all references of identifier at point.
With prefix, keep previous references in output buffer."
  (interactive "P")
  (let* ((project (wisi-check-current-project (buffer-file-name)))
	 (id (wisi-prj-identifier-at-point project)))
    (wisi-xref-all
     (wisi-prj-xref project)
     project
     :identifier (nth 2 id)
     :filename (file-name-nondirectory (buffer-file-name))
     :line (line-number-at-pos)
     :column (save-excursion (goto-char (nth 0 id)) (current-column))
     :local-only nil
     :append append)
    ))

(defun wisi-show-local-references (&optional append)
  "Show all references of identifier at point occuring in current file.
With prefix, keep previous references in output buffer."
  (interactive "P")
  (let* ((project (wisi-check-current-project (buffer-file-name)))
	 (id (wisi-prj-identifier-at-point project)))
    (wisi-xref-all
     (wisi-prj-xref project)
     project
     :identifier (nth 2 id)
     :filename (file-name-nondirectory (buffer-file-name))
     :line (line-number-at-pos)
     :column (save-excursion (goto-char (nth 0 id)) (current-column))
     :local-only t
     :append append)
    ))

(cl-defgeneric wisi-xref-overriding (xref project &key identifier filename line column)
  "Displays a buffer in compilation-mode giving locations of the overriding declarations.
XREF    - dispatching object.
PROJECT - a `wisi-prj' object.
IDENTIFIER - an identifier or operator_symbol
FILENAME - filename containing the identifier
LINE - line number containing the identifier
COLUMN - Emacs column of the start of the identifier ")

(defun wisi-show-overriding ()
  "Show all overridings of identifier at point."
  (interactive)
  (let* ((project (wisi-check-current-project (buffer-file-name)))
	 (id (wisi-prj-identifier-at-point project)))
    (wisi-xref-overriding
     (wisi-prj-xref project)
     project
     :identifier (nth 2 id)
     :filename (file-name-nondirectory (buffer-file-name))
     :line (line-number-at-pos)
     :column (save-excursion (goto-char (nth 0 id)) (current-column)))
    ))

(cl-defgeneric wisi-xref-overridden (xref project &key identifier filename line column)
  "Returns a list (FILE LINE COLUMN) giving the location of the overridden declaration.
XREF    - dispatching object.
PROJECT - a `wisi-prj' object.
IDENTIFIER - an identifier or operator_symbol
FILENAME - absolute filename containing the identifier
LINE - line number containing the identifier
COLUMN - Emacs column of the start of the identifier")

(defun wisi-show-overridden ()
  "Show the overridden declaration of identifier at point."
  (interactive)
  (let* ((project (wisi-check-current-project (buffer-file-name)))
	 (id (wisi-prj-identifier-at-point project))
	 (target
	  (wisi-xref-overridden
	   (wisi-prj-xref project)
	   project
	   :identifier (nth 2 id)
	   :filename (file-name-nondirectory (buffer-file-name))
	   :line (line-number-at-pos)
	   :column (save-excursion (goto-char (nth 0 id)) (current-column)))))

    (wisi-goto-source (nth 0 target)
		      (nth 1 target)
		      (nth 2 target))
  ))

;;;; wisi-prj specific methods

(cl-defmethod project-roots ((_project wisi-prj))
  ;; Not meaningful
  nil)

(cl-defmethod project-files ((project wisi-prj) &optional dirs)
  (let (result)
    (dolist (dir (or dirs
		     (wisi-prj-source-path project)))
      (mapc
       (lambda (absfile)
	 (when (and (not (string-equal "." (substring absfile -1)))
		    (not (string-equal ".." (substring absfile -2)))
		    (not (file-directory-p absfile))
                    (or (null (wisi-prj-file-pred project))
			(funcall (wisi-prj-file-pred project) absfile)))
	   (push absfile result)))
       (when (file-readable-p dir) ;; GNAT puts non-existing dirs on path.
	 (directory-files dir t))))
    result))

(defun wisi-refresh-prj-cache (not-full)
  "Refresh all cached data in the current project, and re-select it.
With prefix arg, very slow refresh operations may be skipped."
  (interactive "P")
  (let ((prj (project-current)))
    (unless (wisi-prj-p prj)
      (error "current project is not a wisi project"))
    (wisi-prj-refresh-cache prj not-full)
    (wisi-prj-select prj)))

(defvar wisi-prj--current-file nil
  "Current wisi project file (the most recently selected); an
absolute file name.")

(defun wisi-prj-clear-current ()
  "Clear the current project selection; make no project current."
  (interactive)
  (setq wisi-prj--current-file nil))

(defun wisi-prj-show ()
  "Show name of current project."
  (interactive)
  (message
   (cond
    (wisi-prj--current-file
     (wisi-prj-name (cdr (assoc wisi-prj--current-file wisi-prj--cache))))
    (t
     (let ((prj (project-current)))
       (if (wisi-prj-p prj)
	   (wisi-prj-name prj)
	 "not a wisi project"))))))

(cl-defmethod wisi-prj-parse-final (project _prj-file)
  (wisi--case-read-all-exceptions project))

(cl-defmethod wisi-prj-refresh-cache ((project wisi-prj) not-full)
  (when wisi-prj--cache
    (wisi-prj-deselect project)
    (let ((prj-file (car (rassoc project wisi-prj--cache))))
	(setq wisi-prj--cache (delete (cons prj-file project) wisi-prj--cache))
	(setq project (wisi-prj-default project))
	(wisi-prj-parse-file :prj-file prj-file :init-prj project :cache t)
	(wisi-xref-refresh-cache (wisi-prj-xref project) project not-full)
	(wisi-prj-select project))))

(cl-defmethod wisi-prj-select ((project wisi-prj))
  (setq compilation-search-path (wisi-prj-source-path project))

  ;; ‘compilation-environment’ is buffer-local, but the user might
  ;; delete that buffer. So set both global and local.
  (let ((comp-env
	 (append
	  (wisi-prj-compile-env project)
	  (wisi-prj-file-env project)
	  (copy-sequence (wisi-prj-file-env project))))
	(comp-buf (get-buffer "*compilation*")))
    (when (buffer-live-p comp-buf)
      (with-current-buffer comp-buf
	(setq compilation-environment comp-env)))
    (set-default 'compilation-environment comp-env))

  (wisi-compiler-select-prj (wisi-prj-compiler project) project)
  (wisi-xref-select-prj     (wisi-prj-xref project)     project))

(cl-defmethod wisi-prj-deselect ((project wisi-prj))
  (wisi-xref-deselect-prj (wisi-prj-xref project) project)
  (wisi-compiler-deselect-prj (wisi-prj-compiler project) project)
  (setq compilation-environment nil)
  (setq compilation-search-path nil))

(defvar wisi-prj-parse-hook nil
  "Hook run at start of `wisi-prj-parse-file'.")

(defvar wisi-prj-parser-alist (list (cons "prj" #'wisi-prj-parse-file-1))
  "Alist of parsers for project files, indexed by file extension.
Parser is called with two arguments; the project file name and
a project. Parser should update the project with values from the file.")

(cl-defmethod wisi-prj-parse-one (project name value)
  "If NAME is a wisi-prj slot, set it to VALUE, return t.
Else return nil."
  (cond
   ((string= name "casing")
    (cl-pushnew (expand-file-name
                 (substitute-in-file-name value))
                (wisi-prj-case-exception-files project)
		:test #'string-equal)
    t)

   ((string= name "src_dir")
    (cl-pushnew (directory-file-name (expand-file-name (substitute-in-file-name value)))
                (wisi-prj-source-path project)
		:test #'string-equal)
    t)

   ((= ?$ (elt name 0))
    ;; Process env var.
    (setf (wisi-prj-file-env project)
	  (cons (concat (substring name 1) "=" (substitute-in-file-name value))
		(wisi-prj-file-env project)))
    t)

   ))

(defvar-local wisi-prj-parse-undefined-function nil
  "Function called if a project file variable name is not recognized.
Called with three args: PROJECT NAME VALUE.")

(defun wisi-prj-parse-file-1 (prj-file project)
  "Wisi project file parser."
  (with-current-buffer (find-file-noselect prj-file)
    (goto-char (point-min))

    ;; process each line
    (while (not (eobp))

      ;; ignore lines that don't have the format "name=value", put
      ;; 'name', 'value' in match-string.
      (when (looking-at "^\\([^= \n]+\\)=\\(.*\\)")
	(let ((name (match-string 1))
	      (value (match-string 2))
	      result)

	  ;; Both compiler and xref need to see some settings; eg gpr_file, env vars.
	  (when (wisi-compiler-parse-one (wisi-prj-compiler project) project name value)
	    (setq result t))
	  (when (wisi-xref-parse-one (wisi-prj-xref project) project name value)
	    (setq result t))

	  (unless result
	    (setq result (wisi-prj-parse-one project name value)))

	  (when (and (not result)
		     wisi-prj-parse-undefined-function)
	    (funcall wisi-prj-parse-undefined-function project name value))

	   ))

      (forward-line 1)
      )
    ))

(cl-defun wisi-prj-parse-file (&key prj-file init-prj cache)
  "Read project file PRJ-FILE with default values from INIT-PRJ.
PRJ-FILE parser is from `wisi-prj-parser-alist'; if that yields
no parser, no error occurs; the file is just a placeholder.  If
CACHE is non-nil, add the project to `wisi-prj--cache'.  In any
case, return the project."
  (setq prj-file (expand-file-name prj-file))

  (run-hooks 'wisi-prj-parse-hook)

  (let* ((default-directory (file-name-directory prj-file))
	 (parser (cdr (assoc (file-name-extension prj-file) wisi-prj-parser-alist)))
	 (project init-prj)
	 (process-environment (append (wisi-prj-compile-env init-prj) process-environment)))

    (when parser
      ;; If no parser, prj-file is just a placeholder; there is no file to parse.
      ;; For example, sal-android-prj has no project file.
	(funcall parser prj-file project)
	(wisi-prj-parse-final project prj-file)
	(wisi-compiler-parse-final (wisi-prj-compiler project) project prj-file)
	(wisi-xref-parse-final (wisi-prj-xref project) project prj-file))

    (when cache
      ;; Cache the project properties
      (if (assoc prj-file wisi-prj--cache)
	  (setcdr (assoc prj-file wisi-prj--cache) project)
	(push (cons prj-file project) wisi-prj--cache)))

    project))

(defun wisi-prj-show-prj-path ()
  "Show the compiler project file search path."
  (interactive)
  (wisi-compiler-show-prj-path (wisi-prj-compiler (wisi-prj-require-prj))))

(defun wisi-prj-show-src-path ()
  "Show the project source file search path."
  (interactive)
  (if compilation-search-path
      (progn
	(pop-to-buffer (get-buffer-create "*source file search path*"))
	(erase-buffer)
	(dolist (file compilation-search-path)
	  (insert (format "%s\n" file))))
    (message "no source file search path set")
    ))

(defun wisi-fix-compiler-error ()
  "Attempt to fix the current compiler error.
Point must be at the source location referenced in a compiler error.
In `compilation-last-buffer', point must be at the compiler error.
Leave point at fixed code."
  (interactive)
  (let ((source-buffer (current-buffer))
	(line-move-visual nil)); screws up next-line otherwise

    (cond
     ((equal compilation-last-buffer wisi-error-buffer)
      (set-buffer source-buffer)
      (wisi-repair-error))

     (t
      (with-current-buffer compilation-last-buffer
	(let ((comp-buf-pt (point))
	      (success
	       (wisi-compiler-fix-error
		(wisi-prj-compiler (wisi-prj-require-prj))
		source-buffer)))
	  ;; restore compilation buffer point
	  (set-buffer compilation-last-buffer)
	  (goto-char comp-buf-pt)

	  (unless success
	    (error "error not recognized"))
	  )))
      )))

;;;; auto-casing

(defvar-local wisi-auto-case nil
  "Buffer-local value indicating whether to change case while typing.
When non-nil, automatically change case of preceding word while
typing.  Casing of keywords is done according to
`wisi-case-keyword', identifiers according to
`wisi-case-identifier'."
  ;; This is not a defcustom, because it's buffer-local.
  )

(defvar-local wisi-case-keyword 'lower-case
  "Indicates how to adjust the case of `wisi-keywords'.
Value is one of lower-case, upper-case."
  ;; This is not a defcustom, because it's buffer-local
  )

(defvar-local wisi-case-identifier 'mixed-case
  "Buffer-local value indicating how to case language keywords.
Value is one of:

- mixed-case : Mixed_Case
- lower-case : lower_case
- upper-case : UPPER_CASE")

(defvar-local wisi-case-strict t
  "If nil, preserve uppercase chars in identifiers.")

(defvar-local wisi-language-keywords nil
  "List of keywords for auto-case.")

(defvar-local wisi-case-adjust-p-function nil
  "Function taking one argument, the typed char; called from wisi-case-adjust.
Return non-nil if case of symbol at point should be adjusted.
Point is on last char of symbol.")

(defun wisi-case-show-files ()
  "Show casing files list for the current project."
  (interactive)
  (let ((project (project-current)))

    (if (and (wisi-prj-p project)
	     (wisi-prj-case-exception-files project))
	(progn
	  (pop-to-buffer (get-buffer-create "*casing files*"))
	  (erase-buffer)
	  (dolist (file (wisi-prj-case-exception-files project))
	    (insert (format "%s\n" file))))
      (message "no casing files")
      )))

(defun wisi--case-save-exceptions (full-exceptions partial-exceptions file-name)
  "Save FULL-EXCEPTIONS, PARTIAL-EXCEPTIONS to the file FILE-NAME."
  ;; If there is a buffer visiting file-name, it may be out of date
  ;; due to a previous save-exceptions, which will give a user prompt
  ;; about editing a file that has changed on disk. Update the buffer
  (let ((buf (get-file-buffer file-name)))
    (when buf
      (with-current-buffer buf
			(revert-buffer nil t t))))

  (with-temp-file (expand-file-name file-name)
    (mapc (lambda (x) (insert (car x) "\n"))
	  (sort (copy-sequence full-exceptions)
		(lambda(a b) (string< (car a) (car b)))))
    (mapc (lambda (x) (insert "*" (car x) "\n"))
	  (sort (copy-sequence partial-exceptions)
		(lambda(a b) (string< (car a) (car b)))))
    ))

(defun wisi--case-read-exceptions (file-name)
  "Read the content of the casing exception file FILE-NAME.
Return (cons full-exceptions partial-exceptions)."
  (setq file-name (expand-file-name (substitute-in-file-name file-name)))
  (if (file-readable-p file-name)
      (let (full-exceptions partial-exceptions word)
	(with-temp-buffer
	  (insert-file-contents file-name)
	  (while (not (eobp))

	    (setq word (buffer-substring-no-properties
			(point) (save-excursion (skip-syntax-forward "w_") (point))))

	    (if (char-equal (string-to-char word) ?*)
		;; partial word exception
		(progn
		  (setq word (substring word 1))
		  (unless (assoc-string word partial-exceptions t)
		    (push (cons word t) partial-exceptions)))

	      ;; full word exception
	      (unless (assoc-string word full-exceptions t)
		(push (cons word t) full-exceptions)))

	    (forward-line 1))
	  )
	(cons full-exceptions partial-exceptions))

    ;; else file not readable; might be a new project with no
    ;; exceptions yet, so just return empty pair
    (message "'%s' is not a readable file." file-name)
    '(nil . nil)
    ))

(defun wisi--case-merge-exceptions (result new)
  "Merge NEW exeptions into RESULT.
An item in both lists has the RESULT value."
  (dolist (item new)
    (unless (assoc-string (car item) result t)
      (push item result)))
  result)

(defun wisi--case-merge-all-exceptions (exceptions project)
  "Merge EXCEPTIONS into PROJECT case-full-exceptions, case-partial-exceptions."
  (setf (wisi-prj-case-full-exceptions project)
	(wisi--case-merge-exceptions (wisi-prj-case-full-exceptions project)
				     (car exceptions)))
  (setf (wisi-prj-case-partial-exceptions project)
	(wisi--case-merge-exceptions (wisi-prj-case-partial-exceptions project)
				     (cdr exceptions))))

(defun wisi--case-read-all-exceptions (project)
  "Read case exceptions from all files in PROJECT casing files."
  (setf (wisi-prj-case-full-exceptions project) '())
  (setf (wisi-prj-case-partial-exceptions project) '())

  (dolist (file (wisi-prj-case-exception-files project))
    (wisi--case-merge-all-exceptions (wisi--case-read-exceptions file) project)))

(defun wisi--case-add-exception (word exceptions)
  "Add case exception WORD to EXCEPTIONS, replacing current entry, if any."
  (if (assoc-string word exceptions t)
      (setcar (assoc-string word exceptions t) word)
    (push (cons word t) exceptions))
  exceptions)

(defun wisi-case-create-exception (&optional partial)
  "Define a word as an auto-casing exception in the current project.
The word is the active region, or the symbol at point.  If
PARTIAL is non-nil, create a partial word exception.  User is
prompted to choose a file from the project case-exception-files
if it is a list."
  (interactive)
  (let* ((project (wisi-prj-require-prj))
	 (file-name
	 (cond
	   ((< 1 (length (wisi-prj-case-exception-files project)))
	    (completing-read "case exception file: " (wisi-prj-case-exception-files project)
			     nil ;; predicate
			     t   ;; require-match
			     nil ;; initial-input
			     nil ;; hist
			     (car (wisi-prj-case-exception-files project)) ;; default
			     ))

	   ((= 1 (length (wisi-prj-case-exception-files project)))
	    (car (wisi-prj-case-exception-files project)))

	   (t
	    (error "No exception file specified; set `casing' in project file."))
	   ))
	word)

    (if (use-region-p)
	(progn
	  (setq word (buffer-substring-no-properties (region-beginning) (region-end)))
	  (deactivate-mark))
      (save-excursion
	(let ((syntax (if partial "w" "w_")))
	  (skip-syntax-backward syntax)
	  (setq word
		(buffer-substring-no-properties
		 (point)
		 (progn (skip-syntax-forward syntax) (point))
		 )))))

    (let* ((exceptions (wisi--case-read-exceptions file-name))
	   (file-full-exceptions (car exceptions))
	   (file-partial-exceptions (cdr exceptions)))

      (cond
       ((null partial)
	(setf (wisi-prj-case-full-exceptions project)
	      (wisi--case-add-exception word (wisi-prj-case-full-exceptions project)))
	(setq file-full-exceptions (wisi--case-add-exception word file-full-exceptions)))

       (t
	(setf (wisi-prj-case-partial-exceptions project)
	      (wisi--case-add-exception word (wisi-prj-case-partial-exceptions project)))
	(setq file-partial-exceptions (wisi--case-add-exception word file-partial-exceptions)))

       )
      (wisi--case-save-exceptions file-full-exceptions file-partial-exceptions file-name)
      (message "created %s case exception '%s' in file '%s'"
	       (if partial "partial" "full")
	       word
	       file-name)
      )
    ))

(defun wisi-case-create-partial-exception ()
  "Define active region or word at point as a partial word exception.
User is prompted to choose a file from the project
case-exception-files if it is a list."
  (interactive)
  (wisi-case-create-exception t))

(defun wisi-after-keyword-p ()
  "Return non-nil if point is after an element of `wisi-language-keywords'."
  (let ((word (buffer-substring-no-properties
	       (save-excursion (skip-syntax-backward "w_") (point))
	       (point))))
    (member (downcase word) wisi-language-keywords)))

(defvar-local wisi--ret-binding #'wisi-indent-newline-indent)
(defvar-local wisi--lfd-binding #'newline-and-indent)

(defun wisi-case-keyword (beg end)
  (cl-ecase wisi-case-keyword
    (lower-case (downcase-region beg end))
    (upper-case (upcase-region beg end))
    ))

(defun wisi-case-identifier (start end case-strict)
  (cl-ecase wisi-case-identifier
    (mixed-case (wisi-mixed-case start end case-strict))
    (lower-case (downcase-region start end))
    (upper-case (upcase-region start end))
    ))

(defun wisi-mixed-case (start end case-strict)
  "Adjust case of region START END to Mixed_Case."
  (let ((done nil)
	next)
    (if (or case-strict wisi-case-strict)
	(downcase-region start end))
    (goto-char start)
    (while (not done)
      (setq next
	    (or
	     (save-excursion (when (search-forward "_" end t) (point-marker)))
	     (copy-marker (1+ end))))

      ;; upcase first char
      (upcase-region (point) (1+ (point)))

      (goto-char next)
      (if (< (point) end)
	  (setq start (point))
	(setq done t))
      )))

(defun wisi-case-adjust-identifier (&optional force-case)
  "Adjust case of the previous word as an identifier.
Uses `wisi-case-identifier', with exceptions defined in
`wisi-case-full-exceptions', `wisi-case-partial-exceptions'.  If
force-case non-nil (default prefix), treat `wisi-strict-case' as
t."
  (interactive "P")
  (save-excursion
    ;; We don't complain when there is no project; we may be editing a
    ;; random Ada file.
    (let ((prj (wisi-prj-current-prj))
	  (end   (point-marker))
	  (start (progn (skip-syntax-backward "w_") (point)))
	  match
	  next
	  (done nil))

      (if (setq match
		(assoc-string (buffer-substring-no-properties start end)
			      (wisi-prj-case-full-exceptions prj)
			      t ;; case-fold
			      ))
	  ;; full word exception
	  (progn
	    ;; 'save-excursion' puts a marker at 'end'; if we do
	    ;; 'delete-region' first, it moves that marker to 'start',
	    ;; then 'insert' inserts replacement text after the
	    ;; marker, defeating 'save-excursion'. So we do 'insert' first.
	    (insert (car match))
	    (delete-region (point) end))

	;; else apply wisi-case-identifier
	(wisi-case-identifier start end force-case)

	;; apply partial-exceptions
	(goto-char start)
	(while (not done)
	  (setq next
		(or
		 (save-excursion (when (search-forward "_" end t) (point-marker)))
		 (copy-marker (1+ end))))

	  (when (setq match (assoc-string (buffer-substring-no-properties start (1- next))
					  (wisi-prj-case-partial-exceptions prj)
					  t))
	    ;; see comment above at 'full word exception' for why
	    ;; we do insert first.
	    (insert (car match))
	    (delete-region (point) (1- next)))

	  (goto-char next)
	  (if (< (point) end)
	      (setq start (point))
	    (setq done t))
          ))
      )))

(defun wisi-case-adjust-keyword ()
  "Adjust the case of the previous symbol as a keyword."
  (save-excursion
    (let ((end   (point-marker))
	  (start (progn (skip-syntax-backward "w_") (point))))
      (wisi-case-keyword start end)
    )))

(defun wisi-case-adjust (&optional typed-char in-comment)
  "Adjust the case of the symbol before point.
When invoked interactively, TYPED-CHAR must be
`last-command-event', and it must not have been inserted yet.  If
IN-COMMENT is non-nil, adjust case of words in comments and
strings as code, and treat `wisi-case-strict' as t in code."
  (when (not (bobp))
    (when (save-excursion
	    (forward-char -1); back to last character in symbol
	    (and (not (bobp))
		 (eq (char-syntax (char-after)) ?w); it can be capitalized

		 (or in-comment
		     (not (wisi-in-string-or-comment-p)))

		 (or (null wisi-case-adjust-p-function)
		     (funcall wisi-case-adjust-p-function typed-char))
		 ))

      ;; The indentation engine may trigger a reparse on
      ;; non-whitespace changes, but we know we don't need to reparse
      ;; for this change (assuming the user has not abused case
      ;; exceptions!).
      (let ((inhibit-modification-hooks t))
	(cond
	 ;; Some attributes are also keywords, but captialized as
	 ;; attributes. So check for attribute first.
	 ((and
	   (not in-comment)
	   (save-excursion
	     (skip-syntax-backward "w_")
	     (eq (char-before) ?')))
	  (wisi-case-adjust-identifier in-comment))

	 ((and
	   (not in-comment)
	   (not (eq typed-char ?_))
	   (wisi-after-keyword-p))
	  (wisi-case-adjust-keyword))

	 (t (wisi-case-adjust-identifier in-comment))
	 ))
      )))

(defun wisi-case-adjust-at-point (&optional in-comment)
  "If ’wisi-auto-case’ is non-nil, adjust case of symbol at point.
Also move to end of symbol.  With prefix arg, adjust case as code
even if in comment or string; otherwise, capitalize words in
comments and strings.  If ’wisi-auto-case’ is nil, capitalize
current word."
  (interactive "P")
  (cond
   ((or (null wisi-auto-case)
	(and (not in-comment)
	     (wisi-in-string-or-comment-p)))
    (skip-syntax-backward "w_")
    (capitalize-word 1))

   (t
    (when
	(and (not (eobp))
	     ;; We use '(syntax-after (point))' here, not '(char-syntax
	     ;; (char-after))', because the latter does not respect
	     ;; syntax-propertize functions
	     (memq (syntax-class (syntax-after (point))) '(2 3)))
      (skip-syntax-forward "w_"))
    (wisi-case-adjust nil in-comment))
   ))

(defun wisi-case-adjust-region (begin end)
  "Adjust case of all words in region BEGIN END."
  (interactive "r")
  (narrow-to-region begin end)
  (save-excursion
    (goto-char begin)
    (while (not (eobp))
      (forward-comment (point-max))
      (skip-syntax-forward "^w_")
      (skip-syntax-forward "w_")
      (wisi-case-adjust)))
  (widen))

(defun wisi-case-adjust-buffer ()
  "Adjust case of current buffer."
  (interactive)
  (wisi-case-adjust-region (point-min) (point-max)))

(defun wisi-case-adjust-interactive (arg)
  "If `wisi-auto-case' is non-nil, adjust the case of the previous symbol,
and process the character just typed.  To be bound to keys that
should cause auto-casing.  ARG is the prefix the user entered
with \\[universal-argument]."
  (interactive "P")

  ;; Character typed has not been inserted yet.
  (let ((lastk last-command-event)
	(do-adjust nil))
    (cond
     ((null wisi-auto-case))
     (t
      (setq do-adjust t)))

    (cond
     ((eq lastk ?\n)
        (when do-adjust
	  (wisi-case-adjust lastk))
	(funcall wisi--lfd-binding))

     ((memq lastk '(?\r return))
      (when do-adjust
	(wisi-case-adjust lastk))
      (funcall wisi--ret-binding))

     (t
      (when do-adjust
	(wisi-case-adjust lastk))
      (self-insert-command (prefix-numeric-value arg)))
     )))

(defun wisi-case-activate-keys (map)
  "Modify the key bindings for all the keys that should adjust casing."
  (mapc (function
	 (lambda(key)
	   (define-key
	     map
	     (char-to-string key)
	     'wisi-case-adjust-interactive)))
	'( ?_ ?% ?& ?* ?\( ?\) ?- ?= ?+
	      ?| ?\; ?: ?' ?\" ?< ?, ?. ?> ?/ ?\n 32 ?\r ))
  )

;;;; xref backend

(defconst wisi-file-line-col-regexp
  ;; matches Gnu-style file references:
  ;; C:\Projects\GDS\work_dscovr_release\common\1553\gds-mil_std_1553-utf.ads:252:25
  ;; /Projects/GDS/work_dscovr_release/common/1553/gds-mil_std_1553-utf.ads:252:25
  ;; gds-mil_std_1553-utf.ads:252:25 - when wisi-xref-full-path is nil
  "\\(\\(?:.:\\\\\\|/\\)?[^:]*\\):\\([0-9]+\\):\\([0-9]+\\)"
  ;; 1                              2            3
  "Regexp matching <file>:<line>:<column> where <file> is an absolute file name or basename.")

(defun wisi-xref-item (identifier prj)
  "Given IDENTIFIER, return an xref-item, with line, column nil if unknown.
IDENTIFIER is from a user prompt with completion, or from
`xref-backend-identifier-at-point'."
  (let* ((t-prop (get-text-property 0 'xref-identifier identifier))
	 ident file line column)
    (cond
     (t-prop
      ;; IDENTIFIER is from wisi-xref-identifier-at-point.
      (setq ident (substring-no-properties identifier 0 nil))
      (setq file (plist-get t-prop :file))
      (setq line (plist-get t-prop :line))
      (setq column (plist-get t-prop :column))
      )

     ((string-match (wisi-xref-completion-regexp (wisi-prj-xref prj)) identifier)
      ;; IDENTIFIER is from prompt/completion on wisi-xref-completion-table
      (setq ident (match-string 1 identifier))

      (let* ((table (wisi-xref-completion-table (wisi-prj-xref prj) prj))
	     (loc (cdr (assoc identifier table))))

	(setq file (nth 0 loc))
	(setq line (nth 1 loc))
	(setq column (nth 2 loc))
	))

     ((string-match wisi-names-regexp identifier)
      ;; IDENTIFIER is from prompt/completion on wisi-names.
      (setq ident (match-string 1 identifier))
      (setq file (buffer-file-name))
      (when (match-string 2 identifier)
	(setq line (string-to-number (match-string 2 identifier))))
      )

     (t
      ;; IDENTIFIER has no line/column info
      (setq ident identifier)
      (setq file (buffer-file-name)))
     )

    (unless (file-name-absolute-p file)
      (setq file (locate-file file compilation-search-path)))

    (let ((eieio-skip-typecheck t)) ;; allow line, column nil.
      (xref-make ident (xref-make-file-location file line column)))
    ))

(cl-defmethod xref-backend-definitions ((prj wisi-prj) identifier)
  (wisi-xref-definitions (wisi-prj-xref prj) prj (wisi-xref-item identifier prj)))

(cl-defmethod xref-backend-identifier-at-point ((prj wisi-prj))
  (save-excursion
    (let ((id (wisi-prj-identifier-at-point prj)))
      (when id
	(put-text-property
	 0 1
	 'xref-identifier
	 (list ':file (buffer-file-name)
	       ':line (line-number-at-pos)
	       ':column (save-excursion (goto-char (nth 0 id)) (current-column)))
	 (nth 2 id))
	(nth 2 id)))))

(cl-defmethod xref-backend-identifier-completion-table ((prj wisi-prj))
  (wisi-filter-table (wisi-xref-completion-table (wisi-prj-xref prj) prj)
		     (when (equal '(16) current-prefix-arg) (buffer-file-name))))

(cl-defmethod xref-backend-references  ((prj wisi-prj) identifier)
  (wisi-xref-references (wisi-prj-xref prj) prj (wisi-xref-item identifier prj)))

;;;###autoload
(defun wisi-prj-xref-backend ()
  "For `xref-backend-functions'; return the current wisi project."
  ;; We return the project, not the xref object, because the
  ;; wisi-xref-* functions need the project.
  (let ((prj (project-current)))
    (when (wisi-prj-p prj)
      prj)))

;;;; project-find-functions alternatives

(defvar wisi-prj--dominating-alist nil
"Alist of (DOMINATING-FILE . PRJ-FILE-NAME): DOMINATING-FILE is
an absolute filename that can be found by
`wisi-prj-find-dominating-cached' or
`wisi-prj-find-dominating-cached'.  PRJ-FILE-NAME is the wisi
project file for the project for that file.")

;;;###autoload
(defun wisi-prj-select-cache (prj-file init-prj &optional dominating-file)
  "Select project matching PRJ-FILE in `wisi-prj--cache' as current project,
parsing and caching if needed. Also add DOMINATING-FILE (default
current buffer file name) to `wisi-prj--dominating-alist' (for
`wisi-prj-select-dominating'.)"
  (let ((old-prj (project-current)))
    ;; If old-prj is not a wisi-prj, we don't know how to deselect it;
    ;; just ignore that.  If prj-file is the current file, user is
    ;; re-selecting it.
    (when (wisi-prj-p old-prj)
      (wisi-prj-deselect old-prj)))

  (unless (or (memq #'wisi-prj-current-cached project-find-functions)
	      (memq #'wisi-prj-current-cached (default-value 'project-find-functions)))
    (message "wisi-prj-select-cache used without wisi-prj-current-cached in project-find-functions"))

  (setq dominating-file (if dominating-file (expand-file-name dominating-file) (buffer-file-name)))
  (setq prj-file (expand-file-name prj-file))
  (add-to-list 'wisi-prj--dominating-alist (cons dominating-file prj-file))

  (let ((new-prj (cdr (assoc prj-file wisi-prj--cache))))
    (unless new-prj
      (setq new-prj (wisi-prj-parse-file :prj-file prj-file :init-prj init-prj :cache t))
      (unless new-prj
	(error "parsing project file '%s' failed" prj-file)))

    (setq wisi-prj--current-file prj-file)
    (wisi-prj-select new-prj)))

;;;###autoload
(defun wisi-prj-select-dominating (&optional dominating-file)
  "Unless it is already current, select a wisi-prj matching DOMINATING-FILE.
DOMINATING-FILE defaults to the current buffer file name.
Useful before running `compilation-start', to ensure the correct
project is current."
  (when (or dominating-file (buffer-file-name))
    ;; buffer-file-name is nil in *compilation* buffer
    (let ((prj-file (cdr (assoc (or dominating-file (buffer-file-name)) wisi-prj--dominating-alist))))
      (unless (string-equal prj-file wisi-prj--current-file)
	(message "Switching to project file '%s'" prj-file)
	(let ((old-prj (cdr (assoc  wisi-prj--current-file wisi-prj--cache)))
	      (new-prj (cdr (assoc prj-file wisi-prj--cache))))
	  (when (wisi-prj-p old-prj)
	    (wisi-prj-deselect old-prj))
	  (when (wisi-prj-p new-prj)
	    (wisi-prj-select new-prj))
	  (setq wisi-prj--current-file prj-file))))))

;;;###autoload
(defun wisi-prj-current-cached (_dir)
  "For `project-find-functions'; return the current project from `wisi-prj--cache'."
  (cdr (assoc wisi-prj--current-file wisi-prj--cache)))

(defvar wisi-prj--default nil
  "Alist of (PRJ-FILE . INIT-PRJ), for `wisi-prj-parse-current'.
PRJ-FILE is an absolute project file name; INIT-PRJ is the
initial `wisi-prj' object for that project file.")

;;;###autoload
(defun wisi-prj-select-file (prj-file default-prj &optional dominating-file)
  "Set PRJ-FILE as current project, add DEFAULT-PRJ to `wisi-prj--default'.
Also add DOMINATING-FILE (default current buffer file name) to
`wisi-prj--dominating-alist' (for `wisi-prj-select-dominating'.)"
  (unless (or (memq #'wisi-prj-current-parse project-find-functions)
	      (memq #'wisi-prj-current-parse (default-value 'project-find-functions)))
    (message "wisi-prj-select-file used without wisi-prj-current-parse in project-find-functions"))

  (setq dominating-file (if dominating-file (expand-file-name dominating-file) (buffer-file-name)))
  (setq prj-file (expand-file-name prj-file))
  (add-to-list 'wisi-prj--dominating-alist (cons dominating-file prj-file))
  (setq wisi-prj--current-file prj-file)
  (add-to-list 'wisi-prj--default (cons prj-file default-prj)))

;;;###autoload
(defun wisi-prj-current-parse (_dir)
  "For `project-find-functions'; parse the current project file, select and return the project"
  (let ((prj (wisi-prj-parse-file
	      :prj-file wisi-prj--current-file
	      :init-prj (cdr (assoc-string wisi-prj--current-file wisi-prj--default))
	      :cache nil)))
    (wisi-prj-select prj)
    prj))

(defvar wisi-prj--dominating nil
  "List of relative filenames for `wisi-prj-find-dominating-cached'
and `wisi-prj-find-dominating-parse'. Set by `wisi-prj-set-dominating'.")

(defun wisi-prj-reset-cache ()
  "Delete all wisi project cached info."
  (interactive)
  (setq wisi-prj--cache nil)
  (setq wisi-prj--current-file nil)
  (setq wisi-prj--default nil)
  (setq wisi-prj--dominating nil)
  (setq wisi-prj--dominating-alist nil))

;;;###autoload
(defun wisi-prj-cache-dominating (prj-file default-prj &optional dominating-file)
  "Parse prj-file, add to `wisi-prj--cache'.
Also add (DOMINATING-FILE . PRJ-FILE) to `wisi-prj--dominating-alist'.
DOMINATING-FILE defaults to (buffer-file-name). "
  (unless (or (memq #'wisi-prj-find-dominating-cached project-find-functions)
	      (memq #'wisi-prj-find-dominating-cached (default-value 'project-find-functions)))
    (message "wisi-prj-cache-dominating used without wisi-prj-find-dominating-cached in project-find-functions"))

  (setq dominating-file (if dominating-file (expand-file-name dominating-file) (buffer-file-name)))
  (setq prj-file (expand-file-name prj-file))
  (add-to-list 'wisi-prj--dominating (file-name-nondirectory dominating-file))
  (add-to-list 'wisi-prj--dominating-alist (cons dominating-file prj-file))
  (wisi-prj-parse-file :prj-file prj-file :init-prj default-prj :cache t)
  nil)

(defun wisi-prj--find-dominating-file (start-dir)
  "Return the project file matching `wisi-prj--dominating'."
  (let* (dom-file
	 (_dom-dir
	  (locate-dominating-file
	   start-dir
	   (lambda (dir)
	     (let ((names wisi-prj--dominating))
	       (while names
		 (let ((filename (expand-file-name (pop names) dir)))
		   (when (file-exists-p filename)
		     (setq dom-file filename)
		     (setq names nil)))))
	     dom-file))))
    (cdr (assoc-string dom-file wisi-prj--dominating-alist))))

;;;###autoload
(defun wisi-prj-find-dominating-cached (dir)
  "For `project-find-functions'; return the cached project
matching `wisi-prj--dominating' (nil if none). Select it if it is
not the current project."
  (let* ((prj-file (wisi-prj--find-dominating-file dir))
	 (new-prj (cdr (assoc-string prj-file wisi-prj--cache))))
    (when prj-file
      (unless (string= prj-file wisi-prj--current-file)
	(let ((old-prj (cdr (assoc-string wisi-prj--current-file wisi-prj--cache))))
	  (when old-prj (wisi-prj-deselect old-prj))
	  (unless new-prj
	    ;; User may have used `wisi-prj-set-dominating' instead of
	    ;; `wisi-prj-cache-dominating'; parse the project file now.
	    (wisi-prj-parse-file
	     :prj-file prj-file
	     :init-prj (cdr (assoc-string prj-file wisi-prj--default))
	     :cache t))
	  (when new-prj (wisi-prj-select new-prj))))
      new-prj)))

;;;###autoload
(defun wisi-prj-set-dominating (prj-file default-prj &optional dom-file)
  "Add (DOM-FILE . PRJ-FILE) to `wisi-prj--dominating-alist',
and (PRJ-FILE . DEFAULT-PRJ) to `wisi-prj--default'.
DOM-FILE defaults to (buffer-file-name).
For example, call this in the Local Vars of a Makefile to
associate a project with that Makefile."
  (unless (or (memq #'wisi-prj-find-dominating-parse project-find-functions)
	      (memq #'wisi-prj-find-dominating-parse (default-value 'project-find-functions)))
    (message "wisi-prj-cache-dominating used without wisi-prj-find-dominating-parse in project-find-functions"))

  (setq dom-file (if dom-file (expand-file-name dom-file) (buffer-file-name)))
  (setq prj-file (expand-file-name prj-file))
  (add-to-list 'wisi-prj--dominating (file-name-nondirectory dom-file))
  (add-to-list 'wisi-prj--dominating-alist (cons dom-file prj-file))
  (add-to-list 'wisi-prj--default (cons prj-file default-prj))
  nil)

;;;###autoload
(defun wisi-prj-find-dominating-parse (dir)
  "For `project-find-functions'; parse, select, and return the project
file matching `wisi-prj--dominating'."
  (let ((prj-file (wisi-prj--find-dominating-file dir)))
    (when prj-file
      (let ((prj (wisi-prj-parse-file
		  :prj-file prj-file
		  :init-prj (cdr (assoc-string prj-file wisi-prj--default))
		  :cache nil)))
	(wisi-prj-select prj)
	prj))))

;;;###autoload
(defun wisi-prj-dtrt-parse-file (prj-file default-prj dominating-file &optional dir)
  "Depending on wisi-prj function in `project-find-functions',
Do The Right Thing to make PRJ-FILE active and selected; return the project."
  (cond
   ((memq #'wisi-prj-find-dominating-parse project-find-functions)
    (wisi-prj-set-dominating prj-file default-prj dominating-file))

   ((memq #'wisi-prj-find-dominating-cached project-find-functions)
    (wisi-prj-cache-dominating prj-file default-prj dominating-file))

   ((memq #'wisi-prj-current-cached project-find-functions)
    (wisi-prj-select-cache prj-file default-prj dominating-file))

   ((memq #'wisi-prj-current-parse project-find-functions)
    (wisi-prj-select-file prj-file default-prj dominating-file))

   (t
    (user-error "No wisi-prj function in project-find-functions"))
   )
  (project-current nil (or dir default-directory)))

;;;###autoload
(defun wisi-prj-find-function-set-p ()
  "Return non-nil if a wisi-prj function is present in `project-find-functions'."
  (or (memq #'wisi-prj-find-dominating-parse project-find-functions)
      (memq #'wisi-prj-find-dominating-cached project-find-functions)
      (memq #'wisi-prj-current-cached project-find-functions)
      (memq #'wisi-prj-current-parse project-find-functions)))

;;;; project menu

(defun wisi-prj--menu-compute ()
  "Return an easy-menu menu for `wisi-prj-menu--install'.
Menu displays cached wisi projects."
  (let (menu)
    (dolist (item wisi-prj--cache)
      (push
       (vector
	(concat (wisi-prj-name (cdr item))
		(when (equal (car item) wisi-prj--current-file) "  *")) ;; current project
	`(lambda () (interactive)
	   (when wisi-prj--current-file
	     (wisi-prj-deselect (cdr (assoc wisi-prj--current-file wisi-prj--cache))))
	   (setq wisi-prj--current-file ,(car item))
	   (wisi-prj-select ,(cdr item)))
	t)
       menu)
      )
    (nreverse menu)))

(defun wisi-prj-menu-install ()
  "Install the project menu if appropriate, to display cached wisi projects."
  (when
      (or (memq #'wisi-prj-find-dominating-cached project-find-functions)
	  (memq #'wisi-prj-current-cached project-find-functions))

    (let ((menu (wisi-prj--menu-compute)))
      (if menu
	  (define-key-after
	    global-map
	    [menu-bar wisi-prj-select]
	    (easy-menu-binding
	     (easy-menu-create-menu
	      "Wisi Prj Select";; EDE uses "Project" menu
	      menu))
	    (lookup-key global-map [menu-bar tools]))
	;; delete empty menu
	  (define-key-after
	    global-map
	    [menu-bar wisi-prj-select]
	    nil
	    (lookup-key global-map [menu-bar tools]))
	))
    ))

(add-hook 'menu-bar-update-hook 'wisi-prj-menu-install)

(defun wisi-prj-completion-table ()
  "Return list of names of cached projects."
  (mapcar (lambda (item) (wisi-prj-name (cdr item))) wisi-prj--cache))

(defun wisi-prj-delete (name)
  "Delete project NAME (default prompt) from the cached projects."
  (interactive (list (completing-read "project name: " (wisi-prj-completion-table))))
  (let (pair)
    (dolist (item wisi-prj--cache)
      (if (string= name (wisi-prj-name (cdr item)))
	  (setq pair item)))

    (setq wisi-prj--cache (delete pair wisi-prj--cache))

    (setq wisi-prj--dominating-alist
	  (cl-delete-if (lambda (item)
			  (string= (car pair) (cdr item)))
			wisi-prj--dominating-alist))
    ))

(provide 'wisi-prj)
;; end wisi-prj.el
