;;; load-dir.el --- Load all Emacs Lisp files in a given directory

;; Copyright (C) 2011, 2017 Free Software Foundation, Inc

;; Authors: Teodor Zlatanov <tzz@lifelogs.com>,
;;          Ben Key <bkey76@gmail.com>
;; With-Help-From: Evans Winner <ego111@gmail.com>,
;;          PJ Weisberg <pj@irregularexpressions.net>
;; Maintainer: Teodor Zlatanov <tzz@lifelogs.com>
;; Version: 0.0.5
;; Keywords: lisp, files, convenience
;; Package-Requires: ((cl-lib "0.5"))

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

;; This package provides a way to load all Emacs Lisp snippets (they
;; don't have to be libraries) in a directory on startup or when Emacs is
;; already running.  It won't reload snippets unless the user requests
;; it, so for instance adding a lambda to a hook is usually safe.
;;
;; You can specify ~/.emacs.d/load.d, a single directory, or a list of
;; directories.  The file search can be recursive.  See the
;; customizable variable `load-dirs' for details.
;;
;; The intent of ~/.emacs.d/load.d is to give package installers like
;; el-get.el (see https://github.com/dimitri/el-get) and other tools a
;; way to easily bootstrap themselves without necessarily modifying
;; your .emacs or custom files directly.

;;; Code:

(require 'cl-lib)

(defgroup load-dir nil
  "Automatically load all Emacs Lisp files in given directories."
  :group 'initialization)

(defcustom load-dir-debug t
  "Debugging messages toggle, default to t."
  :group 'load-dir
  :type 'boolean)

(defcustom load-dir-recursive nil
  "Whether subdirectories should be loaded too."
  :group 'load-dir
  :type 'boolean)

(defcustom load-dir-ignore-errors nil
  "Whether errors in the loaded files should be ignored."
  :group 'load-dir
  :type 'boolean)

(defcustom load-dir-ignored '("\\.dir-locals")
  "This list of regular expressions tells load-dir to ignore some filenames.
The match is a substring check against the whole filename."
  :group 'load-dir
  :tag "Ignore these regexps while loading a directory"
  :type '(repeat :tag "Filename regexp" string))

(defcustom load-dirs nil
  "This variable allows you to define which directories should be loaded.

If nil, no directories are loaded.  This is the default behavior.
If t, only files in ~/.emacs.d/load.d will be loaded.
If a single directory name, only files in that directory will be loaded.
If a list of directory names, all files found in all the
directories will be loaded."
  :group 'load-dir
  :tag "What directories to load"
  :type '(choice (const :tag "Load all from ~/.emacs.d/load.d" t)
                 (const :tag "Don't load anything" nil)
                 directory
                 (repeat :tag "Directories" directory)))

;;;###autoload
(defun load-dirs ()
  "Load all Emacs Lisp files in `load-dirs'.
Will not load a file twice (use `load-dir-reload' for that).
Recurses into subdirectories if `load-dir-recursive' is t."
  (interactive)
  ;; avoid the case where users inadvertently set `load-dirs' to a string
  (mapc 'load-dir-one (cond
                       ((eq load-dirs t)
                        (list (expand-file-name "~/.emacs.d/load.d")))
                       ((stringp load-dirs)
                        (list load-dirs))
                       (t load-dirs))))

(defvar load-dir-loaded nil
  "List of already loaded files.")

;;;###autoload
(defun load-dirs-reload ()
  "Load all Emacs Lisp files in `load-dirs'.
Clears the list of loaded files and just calls `load-dir-load'."
  (interactive)
  (setq load-dir-loaded nil)
  (load-dirs))

(defun load-dir-one (dir)
  "Load all Emacs Lisp files in DIR.
Recurses into subdirectories if `load-dir-recursive' is t."
  (load-dir-debug "Loading Emacs Lisp code from %s" dir)
  (let ((suffixes (get-load-suffixes)))
    (dolist (full (and (file-exists-p dir)
                       (file-directory-p dir)
                       (directory-files dir t)))
      (when (and (not (file-directory-p full))
                 (member (file-name-extension full t) suffixes))
        (let ((f (file-name-sans-extension full)))
          (cond
           ((member f load-dir-loaded)
            (load-dir-debug "Skipping %s, it's already loaded." f))
           ((cl-some (lambda (regexp) (string-match-p regexp full)) load-dir-ignored)
            (load-dir-debug "Ignoring %s as per `load-dir-ignored'." full))
           (t
            (if load-dir-ignore-errors
                (with-demoted-errors (load f))
              (load f))
            (add-to-list 'load-dir-loaded f))))))

    (when load-dir-recursive
      (dolist (f (directory-files dir t directory-files-no-dot-files-regexp))
        (when (file-directory-p f)
          (load-dir-one f))))))

(defun load-dir-debug (&rest args)
  "Print a debug message like `message' if `load-dir-debug' is set."
  (when load-dir-debug
    (apply 'message args)))

;;;###autoload
(add-hook 'after-init-hook 'load-dirs)

;;;; ChangeLog:

;; 2017-03-20  Ted Zlatanov  <tzz@lifelogs.com>
;; 
;; 	* packages/load-dir/load-dir.el: require just cl-lib 0.5
;; 
;; 2017-03-20  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* load-dir.el: Add `Maintainer:`.
;; 
;; 2017-03-20  Ted Zlatanov  <tzz@lifelogs.com>
;; 
;; 	load-dir: no need for let f
;; 
;; 2017-03-20  Ted Zlatanov  <tzz@lifelogs.com>
;; 
;; 	load-dir: fix CL and free variable warnings
;; 
;; 2017-03-17  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* load-dir/load-dir.el (load-dir-one): Don't set global `f`.
;; 
;; 2017-03-14  Ted Zlatanov  <tzz@lifelogs.com>
;; 
;; 	load-dir: allow ignoring some files
;; 
;; 	* packages/load-dir/load-dir.el: Update copyright.
;; 	(load-dir-ignored): New defcustom to ignore regexps; by default ignores
;; 	.dir-locals.
;; 	(load-dir-one): Use it.
;; 
;; 2011-08-28  Chong Yidong  <cyd@stupidchicken.com>
;; 
;; 	Bump load-dir version number to 0.0.3.
;; 
;; 2011-08-28  PJ Weisberg	 <pjweisberg@gmail.com>
;; 
;; 	* packages/load-dir/load-dir.el (load-dir-one): Avoid infinite
;; 	recursion.
;; 
;; 2011-07-08  Chong Yidong  <cyd@stupidchicken.com>
;; 
;; 	epoch-view.el, load-dir.el: Capitalize package description string.
;; 
;; 2011-07-01  Chong Yidong  <cyd@stupidchicken.com>
;; 
;; 	Give every package its own directory in packages/ including single-file
;; 	packages.
;; 


(provide 'load-dir)
;;; load-dir.el ends here
