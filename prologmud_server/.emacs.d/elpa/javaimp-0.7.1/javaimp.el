;;; javaimp.el --- Add and reorder Java import statements in Maven/Gradle projects  -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2019  Free Software Foundation, Inc.

;; Author: Filipp Gunbin <fgunbin@fastmail.fm>
;; Maintainer: Filipp Gunbin <fgunbin@fastmail.fm>
;; Version: 0.7.1
;; Keywords: java, maven, gradle, programming

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Allows to manage Java import statements in Maven/Gradle projects.
;;
;;   Quick start:
;;
;; - customize `javaimp-import-group-alist'
;; - call `javaimp-visit-project', giving it the top-level project
;; directory where pom.xml / build.gradle[.kts] resides
;;
;; Then in a Java buffer visiting a file under that project or one of its
;; submodules call `javaimp-organize-imports' or `javaimp-add-import'.
;;
;; This module does not add all needed imports automatically!  It only helps
;; you to quickly add imports when stepping through compilation errors.
;;
;;   Some details:
;;
;; If Maven/Gradle failed, you can see its output in the buffer named
;; by `javaimp-debug-buf-name' (default is "*javaimp-debug*").
;;
;; Contents of jar files and Maven/Gradle project structures are
;; cached, so usually only the first command should take a
;; considerable amount of time to complete.  If a module's build file
;; or any of its parents' build files (within visited tree) was
;; modified after information was loaded, dependencies are fetched
;; from the build tool again.  If a jar file was changed, its contents
;; are re-read.
;;
;; Currently inner classes are filtered out from completion alternatives.
;; You can always import top-level class and use qualified name.
;;
;;
;;   Example:
;;
;; (require 'javaimp)
;; (add-to-list 'javaimp-import-group-alist
;;   '("\\`\\(my\\.company\\.\\|my\\.company2\\.\\)" . 80))
;; (setq javaimp-additional-source-dirs '("generated-sources/thrift"))
;; (add-hook 'java-mode-hook
;; 	  (lambda ()
;; 	    (local-set-key "\C-ci" 'javaimp-add-import)
;; 	    (local-set-key "\C-co" 'javaimp-organize-imports)))
;; (global-set-key (kbd "C-c j v") 'javaimp-visit-project)
;;

;;; News:

;; v0.7:
;; - Added Gradle support.
;;
;; - Removed javaimp-maven-visit-project in favor of javaimp-visit-project.
;;
;; - Split into multiple files.


;;; Code:

(require 'javaimp-maven)
(require 'javaimp-gradle)



;; User options

(defgroup javaimp ()
  "Add and reorder Java import statements in Maven/Gradle
projects"
  :group 'c)

(defcustom javaimp-import-group-alist '(("\\`java\\." . 10) ("\\`javax\\." . 15))
  "Specifies how to group classes and how to order resulting
groups in the imports list.

Each element should be of the form (CLASSNAME-REGEXP . ORDER)
where CLASSNAME-REGEXP is a regexp matching the fully qualified
class name.  Lowest-order groups are placed earlier.

The order of classes which were not matched is defined by
`javaimp-import-default-order'."
  :group 'javaimp
  :type '(alist :key-type string :value-type integer))

(defcustom javaimp-import-default-order 50
  "Defines the order of classes which were not matched by
`javaimp-import-group-alist'"
  :group 'javaimp
  :type 'integer)

(defcustom javaimp-java-home (getenv "JAVA_HOME")
  "Path to the JDK.  Directory jre/lib underneath this path is
searched for JDK libraries.  By default, it is initialized from
the JAVA_HOME environment variable."
  :group 'javaimp
  :type 'string)

(defcustom javaimp-additional-source-dirs nil
  "List of directories where additional (e.g. generated)
source files reside.

Each directory is a relative path from ${project.build.directory} project
property value.

Typically you would check documentation for a Maven plugin, look
at the parameter's default value there and add it to this list.

E.g. \"${project.build.directory}/generated-sources/<plugin_name>\"
becomes \"generated-sources/<plugin_name>\" (note the absence
of the leading slash.

Custom values set in plugin configuration in pom.xml are not
supported yet."
  :group 'javaimp
  :type '(repeat (string :tag "Relative directory")))

(defcustom javaimp-jar-program "jar"
  "Path to the `jar' program used to read contents of jar files.
Customize it if the program is not on `exec-path'."
  :group 'javaimp
  :type 'string)

(defcustom javaimp-include-current-module-classes t
  "If non-nil, current module's classes are included into
completion alternatives.  `javaimp-add-import' will find all java
files in the current project and add their fully-qualified names
to the completion alternatives list."
  :group 'javaimp
  :type 'boolean)


;; Variables

(defvar javaimp-project-forest nil
  "Visited projects")

(defvar javaimp-cached-jars nil
  "Alist of cached jars.  Each element is of the form (FILE
  . CACHED-JAR).")



;;;###autoload
(defun javaimp-visit-project (dir)
  "Loads a project and its submodules.  DIR should point to a
directory containing pom.xml / build.gradle[.kts].

After being processed by this command, the module tree becomes
known to javaimp and `javaimp-add-import' may be called inside
any module file."
  (interactive "DVisit Maven or Gradle project in directory: ")
  (let* ((exp-dir (expand-file-name (file-name-as-directory dir)))
         build-file
         (tree (cond
                ((file-readable-p (setq build-file (concat exp-dir "pom.xml")))
                 (javaimp--maven-visit build-file))
                ((or (file-readable-p (setq build-file (concat exp-dir "build.gradle")))
                     (file-readable-p (setq build-file (concat exp-dir "build.gradle.kts"))))
                 (javaimp--gradle-visit build-file))
                (t
                 (error "Could not find build file in dir %s" dir)))))
    (when tree
      ;; delete previous tree(s) loaded from this build file, if any
      (setq javaimp-project-forest
	    (seq-remove (lambda (tree)
			  (equal (javaimp-module-file-orig (javaimp-node-contents tree))
			         build-file))
		        javaimp-project-forest))
      (push tree javaimp-project-forest)
      (message "Loaded tree for %s" dir))))


;; Dependency jars

(defun javaimp--update-module-maybe (node)
  (let ((module (javaimp-node-contents node))
	need-update)
    ;; check if deps are initialized
    (unless (javaimp-module-dep-jars module)
      (message "Loading dependencies: %s" (javaimp-module-id module))
      (setq need-update t))
    ;; check if this or any parent build file has changed since we
    ;; loaded the module
    (let ((tmp node))
      (while (and tmp (not need-update))
	(let ((cur (javaimp-node-contents tmp)))
	  (when (> (max (if (file-exists-p (javaimp-module-file cur))
                            (float-time (javaimp--get-file-ts (javaimp-module-file cur)))
                          -1)
                        (if (file-exists-p (javaimp-module-file-orig cur))
                            (float-time (javaimp--get-file-ts (javaimp-module-file-orig cur)))
                          -1))
		   (float-time (javaimp-module-load-ts module)))
	    (message "Reloading dependencies for %s (some build-file changed)"
                     (javaimp-module-id cur))
	    (setq need-update t)))
	(setq tmp (javaimp-node-parent tmp))))
    (when need-update
      (let* ((path (funcall (javaimp-module-dep-jars-path-fetcher module) module))
             (new-dep-jars (javaimp--split-native-path path))
	     (new-load-ts (current-time)))
	(setf (javaimp-module-dep-jars module) new-dep-jars)
	(setf (javaimp-module-load-ts module) new-load-ts)))))

(defun javaimp--get-jar-classes (file)
  (let ((cached (cdr (assoc file javaimp-cached-jars))))
    (cond ((null cached)
	   ;; create, load & put into cache
	   (setq cached
		 (make-javaimp-cached-jar
		  :file file
		  :read-ts (javaimp--get-file-ts file)
		  :classes (javaimp--fetch-jar-classes file)))
	   (push (cons file cached) javaimp-cached-jars))
	  ((> (float-time (javaimp--get-file-ts (javaimp-cached-jar-file cached)))
	      (float-time (javaimp-cached-jar-read-ts cached)))
	   ;; reload
	   (setf (javaimp-cached-jar-classes cached) (javaimp--fetch-jar-classes file))
	   ;; update read-ts
	   (setf (javaimp-cached-jar-read-ts cached) (current-time))))
    ;; return from cached
    (javaimp-cached-jar-classes cached)))

(defun javaimp--fetch-jar-classes (file)
  (message "Reading classes in file: %s" file)
  (with-temp-buffer
    (let ((coding-system-for-read (and (eq system-type 'cygwin) 'utf-8-dos)))
      ;; on cygwin, "jar" is a windows program, so file path needs to be
      ;; converted appropriately.
      (process-file javaimp-jar-program nil t nil
		    ;; `jar' accepts commands/options as a single string
		    "tf" (javaimp-cygpath-convert-maybe file 'windows))
      (goto-char (point-min))
      (while (search-forward "/" nil t)
	(replace-match "."))
      (goto-char (point-min))
      (let (result)
	(while (re-search-forward "\\(^[[:alnum:]._]+\\)\\.class$" nil t)
	  (push (match-string 1) result))
	result))))


;; Tree search routines

(defun javaimp--find-node (predicate)
  (catch 'found
    (dolist (tree javaimp-project-forest)
      (javaimp--find-node-in-tree-1 tree predicate))))

(defun javaimp--select-nodes (predicate)
  (apply #'seq-concatenate 'list
	 (mapcar (lambda (tree)
		   (javaimp--select-nodes-from-tree tree predicate))
		 javaimp-project-forest)))

(defun javaimp--find-node-in-tree (tree predicate)
  (catch 'found
    (javaimp--find-node-in-tree-1 tree predicate)))

(defun javaimp--find-node-in-tree-1 (tree predicate)
  (when tree
    (if (funcall predicate (javaimp-node-contents tree))
	(throw 'found tree))
    (dolist (child (javaimp-node-children tree))
      (javaimp--find-node-in-tree-1 child predicate))))

(defun javaimp--select-nodes-from-tree (tree predicate)
  (when tree
    (append (if (funcall predicate (javaimp-node-contents tree))
		(list tree))
	    (apply #'seq-concatenate 'list
		   (mapcar (lambda (child)
			     (javaimp--select-nodes-from-tree child predicate))
			   (javaimp-node-children tree))))))


;; Some API functions

;; do not expose tree structure, return only modules

(defun javaimp-find-module (predicate)
  (let ((node (javaimp--find-node predicate)))
    (and node
	 (javaimp-node-contents node))))

(defun javaimp-select-modules (predicate)
  (mapcar #'javaimp-node-contents
	  (javaimp--select-nodes predicate)))


;;; Adding imports

;; TODO narrow alternatives by class local name

;;;###autoload
(defun javaimp-add-import (classname)
  "Imports classname in the current file.  Interactively,
asks for a class to import, adds import statement and calls
`javaimp-organize-imports'.  Import statements are not
duplicated.  Completion alternatives are constructed based on
this module's dependencies' classes, JDK classes and top-level
classes in the current module."
  (interactive
   (let* ((file (expand-file-name (or buffer-file-name
				      (error "Buffer is not visiting a file!"))))
	  (node (or (javaimp--find-node
		     (lambda (m)
                       (seq-some (lambda (dir)
                                   (string-prefix-p dir file))
                                 (javaimp-module-source-dirs m))))
		    (error "Cannot find module by file: %s" file))))
     (javaimp--update-module-maybe node)
     (let ((module (javaimp-node-contents node)))
       (list (completing-read
	      "Import: "
	      (append
	       ;; we're not caching full list of classes coming from module
	       ;; dependencies because jars may change and we need to reload
	       ;; them
	       (let ((jars (append (javaimp-module-dep-jars module)
				   (javaimp--get-jdk-jars))))
		 (apply #'seq-concatenate 'list
			(mapcar #'javaimp--get-jar-classes jars)))
	       (and javaimp-include-current-module-classes
		    (javaimp--get-module-classes module)))
	      nil t nil nil (symbol-name (symbol-at-point)))))))
  (barf-if-buffer-read-only)
  (javaimp-organize-imports (cons classname 'ordinary)))

(defun javaimp--get-module-classes (module)
  "Returns list of top-level classes in current module"
  (append
   ;; source dirs
   (seq-mapcat (lambda (dir)
                 (and (file-accessible-directory-p dir)
	              (javaimp--get-directory-classes dir nil)))
               (javaimp-module-source-dirs module))
   ;; additional source dirs
   (seq-mapcat (lambda (rel-dir)
                 (let ((dir (concat (javaimp-module-build-dir module)
                                    (file-name-as-directory rel-dir))))
	           (and (file-accessible-directory-p dir)
	                (javaimp--get-directory-classes dir nil))))
               javaimp-additional-source-dirs)))

(defun javaimp--get-directory-classes (dir prefix)
  (append
   ;; .java files in current directory
   (mapcar (lambda (file)
	     (concat prefix (file-name-sans-extension (car file))))
	   (seq-filter (lambda (file) (null (cadr file))) ;only files
		       (directory-files-and-attributes dir nil "\\.java\\'" t)))
   ;; descend into subdirectories
   (apply #'seq-concatenate 'list
	  (mapcar (lambda (subdir)
		    (let ((name (car subdir)))
		      (javaimp--get-directory-classes
		       (concat dir (file-name-as-directory name)) (concat prefix name "."))))
		  (seq-filter (lambda (file)
				(and (eq (cadr file) t) ;only directories
				     (null (member (car file) '("." "..")))))
			      (directory-files-and-attributes dir nil nil t))))))


;; Organizing imports

;;;###autoload
(defun javaimp-organize-imports (&rest new-imports)
  "Groups import statements according to the value of
`javaimp-import-group-alist' (which see) and prints resulting
groups leaving one blank line between groups.

If the file already contains some import statements, this command
rewrites them, starting with the same place.  Else, if the the
file contains package directive, this command inserts one blank
line below and then imports.  Otherwise, imports are inserted at
the beginning of buffer.

Classes within a single group are ordered in a lexicographic
order.  Imports not matched by any regexp in `javaimp-import-group-alist'
are assigned a default order defined by
`javaimp-import-default-order'.

NEW-IMPORTS is a list of additional imports; each element should
be of the form (CLASS . TYPE), where CLASS is a string and TYPE
is `ordinary' or `static'.  Interactively, NEW-IMPORTS is nil."
  (interactive)
  (barf-if-buffer-read-only)
  (save-excursion
    (goto-char (point-min))
    (let* ((old-data (javaimp--parse-imports))
	   (first (car old-data))
	   (last (cadr old-data))
	   (all-imports (append new-imports (cddr old-data))))
      (if all-imports
	  (progn
	    ;; delete old imports, if any
	    (if first
		(progn
		  (goto-char last)
		  (forward-line)
		  (delete-region first (point))))
	    (javaimp--prepare-for-insertion first)
	    (setq all-imports
		  (cl-delete-duplicates
                   all-imports
                   :test (lambda (first second)
                           (equal (car first) (car second)))))
	    ;; assign order
	    (let ((with-order
		   (mapcar
		    (lambda (import)
		      (let ((order (or (assoc-default (car import)
						      javaimp-import-group-alist
						      'string-match)
				       javaimp-import-default-order)))
			(cons import order)))
		    all-imports)))
	      (setq with-order
		    (sort with-order
			  (lambda (first second)
			    ;; sort by order, name
			    (if (= (cdr first) (cdr second))
				(string< (caar first) (caar second))
			      (< (cdr first) (cdr second))))))
	      (javaimp--insert-imports with-order)))
        (message "Nothing to organize!")))))

(defun javaimp--parse-imports ()
  "Returns (FIRST LAST . IMPORTS)"
  (let (first last imports)
    (while (re-search-forward "^\\s-*import\\s-+\\(static\\s-+\\)?\\([._[:word:]]+\\)" nil t)
      (let ((type (if (match-string 1) 'static 'ordinary))
	    (class (match-string 2)))
	(push (cons class type) imports))
      (setq last (line-beginning-position))
      (or first (setq first last)))
    (cons first (cons last imports))))

(defun javaimp--prepare-for-insertion (start)
  (cond (start
	 ;; if there were any imports, we start inserting at the same place
	 (goto-char start))
	((re-search-forward "^\\s-*package\\s-" nil t)
	 ;; if there's a package directive, insert one blank line below and
	 ;; leave point after it
	 (end-of-line)
	 (if (eobp)
	     (insert ?\n)
	   (forward-line))
	 ;; then insert one blank line and we're done
	 (insert ?\n))
	(t
	 ;; otherwise, just go to bob
	 (goto-char (point-min)))))

(defun javaimp--insert-imports (imports)
  (let ((static (seq-filter (lambda (elt)
			      (eq (cdar elt) 'static))
			    imports))
	(ordinary (seq-filter (lambda (elt)
				(eq (cdar elt) 'ordinary))
			      imports)))
    (javaimp--insert-import-group "import static %s;" static)
    (and static ordinary (insert ?\n))
    (javaimp--insert-import-group "import %s;" ordinary)))

(defun javaimp--insert-import-group (pattern imports)
  (let (last-order)
    (dolist (import imports)
      ;; if adjacent imports have different order value, insert a newline
      ;; between them
      (let ((order (cdr import)))
	(and last-order
	     (/= order last-order)
	     (insert ?\n))
	(insert (format pattern (caar import)) ?\n)
	(setq last-order order)))))

(defun javaimp-reset (arg)
  "Forget loaded trees state.  With prefix arg, also reset jars
cache."
  (interactive "P")
  (setq javaimp-project-forest nil)
  (when arg
    (setq javaimp-cached-jars nil)))

(provide 'javaimp)

;;; javaimp.el ends here
