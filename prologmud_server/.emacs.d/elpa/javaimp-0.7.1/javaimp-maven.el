;;; javaimp-maven.el --- javaimp maven support  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Free Software Foundation, Inc.

;; Author: Filipp Gunbin <fgunbin@fastmail.fm>
;; Maintainer: Filipp Gunbin <fgunbin@fastmail.fm>

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


;;; Code:

(require 'javaimp-util)

(defcustom javaimp-mvn-program "mvn"
  "Path to the `mvn' program.  Customize it if the program is not
on `exec-path'."
  :group 'javaimp
  :type 'string)


(defun javaimp--maven-visit (file)
  "Calls `mvn help:effective-pom' on FILE,
reads project structure from the output and records which files
belong to which modules and other module information"
  (message "Visiting Maven POM file %s..." file)
  (let* ((xml-tree (javaimp--call-build-tool javaimp-mvn-program
                                             #'javaimp--maven-effective-pom-handler
                                             "-f" (javaimp-cygpath-convert-maybe file)
                                             "help:effective-pom"))
	     (projects (javaimp--maven-projects-from-xml xml-tree))
	     (modules (mapcar (lambda (proj-elt)
                            (javaimp--maven-module-from-xml proj-elt file))
                          projects))
	     ;; first module is always root
	     (tree (javaimp--build-tree (car modules) nil modules)))
    (when tree
      ;; Set files in a separate step after building the tree because "real"
      ;; parent of a child (given by <parent>) does not necessary contains the
      ;; child in its <modules>.  This is rare, but happens.
      (javaimp--maven-fill-modules-files file tree)
      ;; check that no :file slot is empty
      (let ((modules-without-files
	         (mapcar #'javaimp-node-contents
		             (javaimp--select-nodes-from-tree
		              tree (lambda (m)
			                 (null (javaimp-module-file m)))))))
	    (if modules-without-files
	        (error "Cannot find file for module(s): %s"
		           (mapconcat #'javaimp-module-id modules-without-files ", "))))
      tree)))

(defun javaimp--maven-effective-pom-handler ()
  (let ((start
	     (save-excursion
	       (progn
	         (goto-char (point-min))
	         (re-search-forward "<\\?xml\\|<projects?")
	         (match-beginning 0))))
	    (end
	     (save-excursion
	       (progn
	         (goto-char (point-min))
	         (re-search-forward "<\\(projects?\\)")
	         ;; corresponding close tag is the end of parse region
	         (search-forward (concat "</" (match-string 1) ">"))
	         (match-end 0)))))
    (xml-parse-region start end)))

(defun javaimp--maven-projects-from-xml (tree)
  "Analyzes result of `mvn help:effective-pom' and returns list
of <project> elements"
  (let ((project (assq 'project tree))
	    (projects (assq 'projects tree)))
    (cond (project
	       (list project))
	      (projects
	       (javaimp--xml-children projects 'project))
	      (t
	       (error "Neither <project> nor <projects> was found in pom")))))

(defun javaimp--maven-module-from-xml (elt file-orig)
  (let ((build-elt (javaimp--xml-child 'build elt)))
    (make-javaimp-module
     :id (javaimp--maven-id-from-xml elt)
     :parent-id (javaimp--maven-id-from-xml (javaimp--xml-child 'parent elt))
     ;; <project> element does not contain pom file path, so we set this slot
     ;; later, see javaimp--maven-fill-modules-files
     :file nil
     :file-orig file-orig
     ;; jar/war supported
     :final-name (let ((packaging (or (javaimp--xml-first-child
		                               (javaimp--xml-child 'packaging elt))
                                      "jar")))
                   (when (member packaging '("jar" "war"))
                     (concat (javaimp--xml-first-child
                              (javaimp--xml-child 'finalName build-elt))
                             "." packaging)))
     :source-dirs (list (file-name-as-directory
		                 (javaimp-cygpath-convert-maybe
		                  (javaimp--xml-first-child
		                   (javaimp--xml-child 'sourceDirectory build-elt))))
                        (file-name-as-directory
		                 (javaimp-cygpath-convert-maybe
			              (javaimp--xml-first-child
			               (javaimp--xml-child 'testSourceDirectory build-elt)))))
     :build-dir (file-name-as-directory
		         (javaimp-cygpath-convert-maybe
		          (javaimp--xml-first-child (javaimp--xml-child 'directory build-elt))))
     :dep-jars nil          ; dep-jars is initialized lazily on demand
     :load-ts (current-time)
     :dep-jars-path-fetcher #'javaimp--maven-fetch-dep-jars-path)))

(defun javaimp--maven-id-from-xml (elt)
  (make-javaimp-id
   :group (javaimp--xml-first-child (javaimp--xml-child 'groupId elt))
   :artifact (javaimp--xml-first-child (javaimp--xml-child 'artifactId elt))
   :version (javaimp--xml-first-child (javaimp--xml-child 'version elt))))

(defun javaimp--maven-fill-modules-files (file tree)
  ;; Reads module id from FILE, looks up corresponding module in TREE, sets its
  ;; :file slot, then recurses for each submodule.  A submodule file path is
  ;; constructed by appending relative path taken from <module> to FILE's
  ;; directory.
  (let* ((xml-tree (with-temp-buffer
		             (insert-file-contents file)
		             (xml-parse-region (point-min) (point-max))))
	     (project-elt (assq 'project xml-tree))
	     (this-id (javaimp--maven-id-from-xml project-elt))
	     ;; seems that the only mandatory component in tested ids is artifact, while
	     ;; group and version may be inherited and thus not presented in pom.xml
	     (id-pred (if (or (null (javaimp-id-group this-id))
			              (null (javaimp-id-version this-id)))
		              (progn
			            (message "File %s contains incomplete id, will check artifact only" file)
			            (lambda (tested-id)
			              (equal (javaimp-id-artifact this-id)
				                 (javaimp-id-artifact tested-id))))
		            (lambda (tested-id)
		              (equal this-id tested-id))))
	     (module
	      (javaimp-node-contents
	       (or (javaimp--find-node-in-tree
		        tree (lambda (m)
		               (funcall id-pred (javaimp-module-id m))))
	           (error "Cannot find module for id %s (taken from file %s)" this-id file)))))
    (setf (javaimp-module-file module) file)
    (let ((rel-paths
	       (mapcar #'javaimp--xml-first-child
		           (javaimp--xml-children (javaimp--xml-child 'modules project-elt) 'module))))
      (dolist (rel-path rel-paths)
	    (javaimp--maven-fill-modules-files (concat (file-name-directory file)
						                           (file-name-as-directory rel-path)
						                           "pom.xml")
					                       tree)))))

(defun javaimp--maven-fetch-dep-jars-path (module)
  (javaimp--call-build-tool javaimp-mvn-program
                            (lambda ()
                              (goto-char (point-min))
                              (search-forward "Dependencies classpath:")
                              (forward-line 1)
                              (thing-at-point 'line))
                            ;; always invoke for this module's pom.ml
                            "-f" (javaimp-cygpath-convert-maybe
                                  (javaimp-module-file module))
                            "dependency:build-classpath"))

(provide 'javaimp-maven)
