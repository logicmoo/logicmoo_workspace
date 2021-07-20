;;; organize-imports-java.el --- Automatically organize imports in Java code  -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2021  Shen, Jen-Chieh
;; Created date 2018-04-16 13:12:01

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: Automatically organize imports in Java code.
;; Keyword: organize imports java handy eclipse
;; Version: 0.3.0
;; Package-Requires: ((emacs "25.1") (f "0.20.0") (s "1.12.0") (dash "2.14.1") (ht "2.2"))
;; URL: https://github.com/jcs-elpa/organize-imports-java

;; This file is NOT part of GNU Emacs.

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
;;
;; Automatically organize imports in Java code.
;;
;; (@* "TODO" )
;; * Performance is terrible when loading all the jar files to path.
;;   Hopefully I can find out a way to get around this issue.
;;

;;; Code:

(require 'cl-lib)
(require 'f)
(require 's)
(require 'ht)
(require 'subr-x)

(defgroup organize-imports-java nil
  "Organize imports java extension"
  :prefix "organize-imports-java-"
  :group 'editing
  :link '(url-link :tag "Repository" "https://github.com/jcs-elpa/organize-imports-java"))

(defconst organize-imports-java--home-dir (file-name-directory load-file-name)
  "`organize-imports-java' home directory.")

(defcustom organize-imports-java-java-sdk-path
  (expand-file-name "sdk" organize-imports-java--home-dir)
  "Java SDK Path."
  :type 'string
  :group 'organize-imports-java)

(defcustom organize-imports-java-inc-keyword "::SDK_PATH::"
  "Java SDK Path."
  :type 'string
  :group 'organize-imports-java)

(defcustom organize-imports-java-lib-inc-file "oij.config"
  "Java library include config file."
  :type 'string
  :group 'organize-imports-java)

(defcustom organize-imports-java-oij-dir "./.oij/"
  "Directory that stores all oij cache files."
  :type 'string
  :group 'organize-imports-java)

(defcustom organize-imports-java-path-jar-lib-cache-file
  "paths-cache.oij"
  "File generate store all the jar/lib Java paths."
  :type 'string
  :group 'organize-imports-java)

(defcustom organize-imports-java-path-local-source-cache-file
  "paths-cache-local.oij"
  "File generate store all the local source Java paths."
  :type 'string
  :group 'organize-imports-java)

(defcustom organize-imports-java-font-lock-type-faces
  '("font-lock-type-face" "tree-sitter-hl-face:type")
  "List of type font face that current Jave mode applied to use."
  :type 'list
  :group 'organize-imports-java)

(defcustom organize-imports-java-unsearch-class-type
  '("Boolean" "boolean"
    "Byte" "byte"
    "Character" "char"
    "Double" "double"
    "Float" "float"
    "Integer" "integer" "int"
    "Long" "long"
    "String" "string"
    "Short" "short"
    "Void" "void")
  "Class types that do not need to imports any library path."
  :type 'list
  :group 'organize-imports-java)

(defcustom organize-imports-java-source-dir-name "src"
  "Source directory in the project, default is 'src'."
  :type 'string
  :group 'organize-imports-java)

(defcustom organize-imports-java-non-class-list
  '("Callable"
    "Runnable")
  "List that are no need to import like interface, etc."
  :type 'list
  :group 'organize-imports-java)

(defvar organize-imports-java--path-buffer-jar-lib '()
  "All the available java paths store here.")

(defvar organize-imports-java--path-buffer-local-source '()
  "All the available local source java paths store here.")

(defvar organize-imports-java--serach-regexp
  "[a-zA-Z0-9/_-]*/[A-Z][a-zA-Z0-9$_-]*\\.class"
  "Regular Expression to search for java path.")

(defvar organize-imports-java-non-src-list
  '("document"
    "internal"
    "package-info"
    "sun")
  "List of non Java source keywords.")

(defvar organize-imports-java--priority-list
  '("java[.]"
    "javax[.]")
  "Priority list of the insertion.  Insertion will be base on this list order.")

(defvar organize-imports-java--same-class-name-list '()
  "Paths will store temporary, use to check if multiple class exists in the environment.")

(defvar organize-imports-java--default-oij-config
  (expand-file-name "default/oij.config" organize-imports-java--home-dir)
  "Default oij.config file path.")

(defvar organize-imports-java--project-oij (ht-create)
  "List of project to oij configuraion.

This is use to detect changes from oij configuraion file.")

;;; Util

(defmacro organize-imports-java--no-log-apply (&rest body)
  "Execute BODY without write it to message buffer."
  (declare (indent 0) (debug t))
  `(let ((message-log-max nil)) (progn ,@body)))

(defun organize-imports-java--log (fmt &rest args)
  "Debug message like function `message' with same argument FMT and ARGS."
  (organize-imports-java--no-log-apply (apply 'message fmt args)))

(defun organize-imports-java--project-dir ()
  "Get the current project directory."
  (cdr (project-current)))

(defun organize-imports-java--get-alphabet-id (c)
  "Get the alphabet id.
C : character to find the alphabet id."
  (cond ((string= c "A") (progn 0))
        ((string= c "B") (progn 1))
        ((string= c "C") (progn 2))
        ((string= c "D") (progn 3))
        ((string= c "E") (progn 4))
        ((string= c "F") (progn 5))
        ((string= c "G") (progn 6))
        ((string= c "H") (progn 7))
        ((string= c "I") (progn 8))
        ((string= c "J") (progn 9))
        ((string= c "K") (progn 10))
        ((string= c "L") (progn 11))
        ((string= c "M") (progn 12))
        ((string= c "N") (progn 13))
        ((string= c "O") (progn 14))
        ((string= c "P") (progn 15))
        ((string= c "Q") (progn 16))
        ((string= c "R") (progn 17))
        ((string= c "S") (progn 18))
        ((string= c "T") (progn 19))
        ((string= c "U") (progn 20))
        ((string= c "V") (progn 21))
        ((string= c "W") (progn 22))
        ((string= c "X") (progn 23))
        ((string= c "Y") (progn 24))
        ((string= c "Z") (progn 25))
        (t (progn -1))))

(defun organize-imports-java--is-contain-list-string (in-list in-str)
  "Check if IN-STR contain in any string in the IN-LIST."
  (cl-some #'(lambda (lb-sub-str) (string-match-p (regexp-quote lb-sub-str) in-str)) in-list))

(defun organize-imports-java--contain-string (in-sub-str in-str)
  "Check if string IN-SUB-STR is a substring of another string IN-STR."
  (string-match-p (regexp-quote in-sub-str) in-str))

(defun organize-imports-java--is-in-list-string (in-list str)
  "Check if a string (STR) in the string list (IN-LIST)."
  (cl-some #'(lambda (lb-sub-str) (string= lb-sub-str str)) in-list))

(defun organize-imports-java--string-match-position (in-list in-str)
  "Return position if IN-STR is string match in the list IN-LIST.
When not found return nil."
  (let ((index 0) (found-position nil))
    (dolist (str-item in-list)
      (when (string-match-p str-item in-str) (setq found-position index))
      (setq index (1+ index)))
    ;; Return position.
    found-position))

(defun organize-imports-java--current-line-empty-p ()
  "Current line empty, but accept spaces/tabs in there.  (not absolute)."
  (save-excursion (beginning-of-line) (looking-at "[[:space:]\t]*$")))

(defun organize-imports-java--keep-one-line-between ()
  "Keep one line between the two line of code.
If you want to keep more than one line use
`organize-imports-java-keep-n-line-between' instead."
  (if (organize-imports-java--current-line-empty-p)
      (progn
        (forward-line 1)
        ;; Kill empty line until there is one line.
        (while (organize-imports-java--current-line-empty-p)
          (organize-imports-java--kill-whole-line)))
    ;; Make sure have one empty line between.
    (insert "\n")))

(defun organize-imports-java--get-string-from-file (path)
  "Return PATH's file content."
  (if (file-exists-p path)
      (with-temp-buffer (insert-file-contents path) (buffer-string))
    ""))

(defun organize-imports-java--erase-file (in-filename)
  "Erase IN-FILENAME relative to project root."
  (write-region "" nil (concat (organize-imports-java--project-dir) in-filename) nil))

;;; Parse INI

(defun organize-imports-java--parse-ini (file-path)
  "Parse a .ini file with FILE-PATH."
  (let ((tmp-ini (organize-imports-java--get-string-from-file file-path))
        (tmp-ini-list '()) (tmp-pair-list nil)
        (tmp-keyword "") (tmp-value "")
        (count 0))
    (setq tmp-ini (split-string tmp-ini "\n"))

    (dolist (tmp-line tmp-ini)
      ;; check not comment.
      (unless (string-match-p "#" tmp-line)
        ;; Split it.
        (setq tmp-pair-list (split-string tmp-line "="))

        ;; Assign to temporary variables.
        (setq tmp-keyword (nth 0 tmp-pair-list))
        (setq tmp-value (nth 1 tmp-pair-list))

        ;; Check empty value.
        (when (and (not (string-empty-p tmp-keyword)) tmp-value)
          (let ((tmp-list '()))
            (push tmp-keyword tmp-list)
            (setq tmp-ini-list (append tmp-ini-list tmp-list)))
          (let ((tmp-list '()))
            (push tmp-value tmp-list)
            (setq tmp-ini-list (append tmp-ini-list tmp-list)))))
      (setq count (1+ count)))

    ;; return list.
    tmp-ini-list))

(defun organize-imports-java--get-properties (ini-list in-key)
  "Get properties data.  Search by key and return value.
INI-LIST : ini list.  Please use this with/after using
`organize-imports-java--parse-ini' function.
IN-KEY : key to search for value."
  (let ((tmp-index 0) (tmp-key "") (tmp-value "") (returns-value ""))
    (while (< tmp-index (length ini-list))
      ;; Get the key and data value.
      (setq tmp-key (nth tmp-index ini-list))
      (setq tmp-value (nth (1+ tmp-index) ini-list))

      ;; Find the match.
      (when (string= tmp-key in-key)
        ;; return data value.
        (setq returns-value tmp-value))

      ;; Search for next key word.
      (setq tmp-index (+ tmp-index 2)))

    ;; Found nothing, return empty string.
    returns-value))

;;; Project

(defun organize-imports-java--oij-content ()
  "Return oij content in MD5 format."
  (let ((new-content (organize-imports-java--get-string-from-file
                      (organize-imports-java--config-path))))
    (when new-content (md5 new-content))))

(defun organize-imports-java--record-project-ht ()
  "Record current project to oij content once."
  (let* ((key (organize-imports-java--project-dir))
         (old-content (ht-get organize-imports-java--project-oij key))
         (new-content (organize-imports-java--oij-content)))
    (unless (equal old-content new-content)
      (organize-imports-java-reload-paths)  ; Refresh cache paths.
      (ht-set organize-imports-java--project-oij key new-content))))

;;; Core

(defun organize-imports-java--oij-dir (in-filename)
  "Return full file path for oij cache file IN-FILENAME."
  (concat organize-imports-java-oij-dir in-filename))

(defun organize-imports-java--config-path ()
  "Return oij configuration file path."
  (concat (organize-imports-java--project-dir) organize-imports-java-lib-inc-file))

(defun organize-imports-java--re-seq (regexp string)
  "Get a list of all REGEXP match in a STRING."
  (save-match-data
    (let ((case-fold-search t) (pos 0) matches)
      (while (string-match regexp string pos)
        (push (match-string 0 string) matches)
        (setq pos (match-end 0)))
      matches)))

(defun organize-imports-java-get-local-source ()
  "Get the all the local source file path as a list."
  (let* ((src-file-path-list '())
         (project-source-dir (concat (organize-imports-java--project-dir)
                                     organize-imports-java-source-dir-name "/")))
    (setq src-file-path-list (f--files project-source-dir
                                       (and (string= (f-ext it) "java")
                                            (not (string-match-p "#" (f-filename it))))
                                       t))

    (let ((index 0) (current-dirname "") (last-dirname ""))
      (dolist (src-file-path src-file-path-list)
        ;; Get the current dirname.
        (setq current-dirname (f-dirname src-file-path))

        (unless (string= current-dirname last-dirname)
          ;; Update last dirname.
          (setq last-dirname current-dirname)
          (organize-imports-java--log "Searching local source under %s..." current-dirname))

        ;; Remove the source file path, only left the package file path.
        (setf (nth index src-file-path-list)
              (s-replace project-source-dir "" src-file-path))

        ;; Convert '/' to '.'.
        (setf (nth index src-file-path-list)
              (s-replace "/" "." (nth index src-file-path-list)))

        ;; Lastly, remove `.java' extension.
        (setf (nth index src-file-path-list)
              (s-replace ".java" "" (nth index src-file-path-list)))

        (setq index (1+ index))))

    src-file-path-list))

(defun organize-imports-java-unzip-lib ()
  "Decode it `.jar' binary to readable data strucutre."
  (let ((tmp-lib-inc-file (organize-imports-java--config-path))
        (tmp-lib-list '())
        ;; Value read from the .ini/.properties file.
        (tmp-lib-path "")
        ;; Buffer read depends on one of the `tmp-lib-path'.
        (tmp-lib-buffer nil)
        ;; After search using regular expression and add all the paths to
        ;; the list/array.
        (tmp-class-list '())
        ;; index through the lib/jar paths list.
        (tmp-index 0)
        ;; length of the lib/jar paths list.
        (tmp-lib-list-length -1)
        ;; First character of the path readed from .ini file.
        (first-char-from-path nil)
        ;; Final return path list.
        (all-lib-path-list '()))
    (when (file-exists-p tmp-lib-inc-file)
      ;; Read the ini file, in order to get all the target lib/jar files.
      (organize-imports-java--log "Reading %s..." tmp-lib-inc-file)

      (setq tmp-lib-list (organize-imports-java--parse-ini tmp-lib-inc-file))
      (organize-imports-java--log "Done reading %s..." tmp-lib-inc-file)

      ;; Get the length of the library list
      (setq tmp-lib-list-length (length tmp-lib-list))

      (while (< tmp-index tmp-lib-list-length)
        ;; Get the value of the path.
        (setq tmp-lib-path (nth (1+ tmp-index) tmp-lib-list))

        ;; Get the first character of the path.
        (setq first-char-from-path (substring tmp-lib-path 0 1))

        (cond
         ;; If the first character is not '.', then we use
         ;; absolute path instead of version control relative path.
         ((string= first-char-from-path ".")
          ;; Modefied path to version control path.
          (setq tmp-lib-path (concat (organize-imports-java--project-dir)
                                     tmp-lib-path)))
         ;; Swap #SDK_PATH# to valid Java SDK path, if contain.
         ((organize-imports-java--contain-string organize-imports-java-inc-keyword
                                                 tmp-lib-path)
          (setq tmp-lib-path (s-replace organize-imports-java-inc-keyword
                                        organize-imports-java-java-sdk-path
                                        tmp-lib-path))))

        (organize-imports-java--log "Loading %s..." tmp-lib-path)

        ;; Read the jar/lib to temporary buffer.
        (setq tmp-lib-buffer (organize-imports-java--get-string-from-file tmp-lib-path))

        ;; Get all the library path strings by using
        ;; regular expression.
        (setq tmp-class-list (organize-imports-java--re-seq
                              organize-imports-java--serach-regexp
                              tmp-lib-buffer))

        (organize-imports-java--log "Done loading lib path from %s." tmp-lib-path)

        ;; Add the paths to the list.
        (push tmp-class-list all-lib-path-list)

        ;; Add up index.
        (setq tmp-index (+ tmp-index 2))))
    all-lib-path-list))

;;;###autoload
(defun organize-imports-java-erase-cache-file ()
  "Clean all the buffer in the cache files."
  (interactive)
  (let ((cache-jar (organize-imports-java--oij-dir organize-imports-java-path-jar-lib-cache-file))
        (cache-local (organize-imports-java--oij-dir organize-imports-java-path-local-source-cache-file)))
    (organize-imports-java--erase-file cache-jar)
    (organize-imports-java--erase-file cache-local)))

;;;###autoload
(defun organize-imports-java-reload-paths ()
  "Reload the Java include paths and local source path once."
  (interactive)
  ;; Write Java path to according cache file, both `jar/lib' and `local-source' cache.
  (organize-imports-java-reload-jar-lib-paths)
  (organize-imports-java-reload-local-source-paths))

(defun organize-imports-java--create-default-oij-config ()
  "Create default oij configuration file."
  (let ((config-fp (organize-imports-java--config-path)))
    (unless (file-exists-p config-fp)
      (if (y-or-n-p (format "Missing the %s in project root directory, create one? "
                            organize-imports-java-lib-inc-file))
          (progn
            (write-region
             (organize-imports-java--get-string-from-file organize-imports-java--default-oij-config)
             nil config-fp)
            (message "[INFO] Created default oij configuration file: %s" config-fp))
        (user-error "[WARNING] Include jar path file missing: %s" config-fp)))))

;;;###autoload
(defun organize-imports-java-reload-jar-lib-paths ()
  "Reload external Java paths.
For .jar files."
  (interactive)
  ;; Import all libs/jars.
  (setq organize-imports-java--path-buffer-jar-lib
        (organize-imports-java-unzip-lib))

  ;; Flatten it.
  (setq organize-imports-java--path-buffer-jar-lib
        (-flatten organize-imports-java--path-buffer-jar-lib))

  ;; Remove duplicates value from list.
  (setq organize-imports-java--path-buffer-jar-lib
        (delete-dups organize-imports-java--path-buffer-jar-lib))

  (let ((cache-file (organize-imports-java--oij-dir organize-imports-java-path-jar-lib-cache-file)))
    (organize-imports-java--erase-file cache-file)
    (organize-imports-java--load-path-and-write-cache organize-imports-java--path-buffer-jar-lib
                                                      cache-file)))

;;;###autoload
(defun organize-imports-java-reload-local-source-paths ()
  "Reload internal Java paths.
Usually Java files under project root 'src' directory."
  (interactive)
  ;; Import local source files. File that isn't a jar/lib file.
  (setq organize-imports-java--path-buffer-local-source
        (organize-imports-java-get-local-source))

  ;; Flatten it.
  (setq organize-imports-java--path-buffer-local-source
        (-flatten organize-imports-java--path-buffer-local-source))

  ;; Remove duplicates value from list.
  (setq organize-imports-java--path-buffer-local-source
        (delete-dups organize-imports-java--path-buffer-local-source))

  (let ((cache-file (organize-imports-java--oij-dir organize-imports-java-path-local-source-cache-file)))
    (organize-imports-java--erase-file cache-file)
    (organize-imports-java--load-path-and-write-cache organize-imports-java--path-buffer-local-source
                                                      cache-file)))

(defun organize-imports-java--load-path-and-write-cache (path-list in-filename)
  "Load the path and write the cache file.
PATH-LIST : content paths we will write to IN-FILENAME.
IN-FILENAME : name of the cache file."
  (let ((first-char-from-path "") (tmp-write-to-file-content-buffer ""))
    ;; Write into file so we don't need to do it every times.
    (dolist (tmp-path path-list)
      ;; Get the first character of the path.
      (setq first-char-from-path (substring tmp-path 0 1))

      (when (and (not (equal (upcase first-char-from-path) first-char-from-path))
                 (not (organize-imports-java--is-contain-list-string organize-imports-java-non-src-list
                                                                     tmp-path))
                 (not (organize-imports-java--is-contain-list-string organize-imports-java-non-class-list
                                                                     tmp-path))
                 (not (organize-imports-java--is-digit-string first-char-from-path))
                 (not (string= first-char-from-path "-"))
                 (not (string= first-char-from-path ".")))
        ;; Swap `/' to `.'.
        (setq tmp-path (s-replace "/" "." tmp-path))

        ;; Swap `$' to `.'.
        (setq tmp-path (s-replace "$" "." tmp-path))

        ;; Remove `.class'.
        (setq tmp-path (s-replace ".class" "" tmp-path))

        (let ((class-name "") (split-path-list '()))
          (setq split-path-list (split-string tmp-path "\\."))

          ;; Get the class name.
          (setq class-name (nth (1- (length split-path-list)) split-path-list))

          (unless (numberp (read class-name))
            ;; Add line break at the end.
            (setq tmp-path (concat tmp-path "\n"))

            ;; add to file content buffer.
            (setq tmp-write-to-file-content-buffer (concat tmp-path tmp-write-to-file-content-buffer))))))

    ;; Write to file all at once.
    (write-region tmp-write-to-file-content-buffer
                  nil
                  (concat (organize-imports-java--project-dir) in-filename)
                  t)))

(defun organize-imports-java-get-faces (pos)
  "Get the font faces at POS."
  (-flatten
   (remq nil
         (list
          (get-char-property pos 'read-face-name)
          (get-char-property pos 'face)
          (plist-get (text-properties-at pos) 'face)))))

(defun organize-imports-java-get-current-point-face ()
  "Get current point's type face as string."
  (organize-imports-java-get-faces (point)))

(defun organize-imports-java--is-current-point-face (in-face)
  "Check if current face the same face as IN-FACE.
Returns, True if is the same as pass in face name string.
False, is not the same as pass in face name string.
IN-FACE : input face name as string."
  (let ((faces (organize-imports-java-get-current-point-face)))
    (if (listp faces)
        (cl-position in-face faces :test 'string=)
      (string= in-face faces))))

(defun organize-imports-java--current-point-face-list-p (face-name-list)
  "Is the current face name same as one of the pass in string in the list?
FACE-NAME-LIST : list of face name in string."
  (cl-some #'(lambda (face-name) (organize-imports-java--is-current-point-face face-name)) face-name-list))

(defun organize-imports-java--get-type-face-keywords-by-face-name (face-name-list)
  "Get all the type keywords in current buffer.
FACE-NAME-LIST : face name to search."
  (let ((tmp-keyword-list '()))
    (save-excursion
      ;; Goto the end of the buffer.
      (with-no-warnings (end-of-buffer))
      (while (< (point-min) (point))
        (backward-word 1)
        (when (organize-imports-java--current-point-face-list-p face-name-list)
          (push (thing-at-point 'word) tmp-keyword-list))))
    ;; Remove duplicate
    (setq tmp-keyword-list (delete-dups tmp-keyword-list))
    tmp-keyword-list))

(defun organize-imports-java--insert-import-lib (tmp-one-path)
  "Insert the import code line here.  Also design it here.
Argument TMP-ONE-PATH Temporary passing in path, use to insert import string/code."
  (insert "import ")
  (insert tmp-one-path)
  (insert ";\n"))

(defun organize-imports-java--kill-whole-line ()
  "Deletes a line, but does not put it in the `kill-ring'."
  (let ((kill-ring))
    (if (use-region-p)
        (delete-region (region-beginning) (region-end))
      (move-beginning-of-line 1)
      (kill-line 1))))

;;;###autoload
(defun organize-imports-java-clear-all-imports ()
  "Clear all imports in the current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-max))
    (while (< (point-min) (point))
      (beginning-of-line)
      (when (string= (thing-at-point 'word) "import")
        (organize-imports-java--kill-whole-line))
      (forward-line -1))))

(defun organize-imports-java--is-digit-string (c)
  "Check if C is a digit."
  (string-match-p "\^[0-9]'" c))

(defun organize-imports-java--same-class-ask (type)
  "Ask the user which path should you import?
TYPE : path string will be store at."
  (interactive
   (list (completing-read
          "Choose select class: " organize-imports-java--same-class-name-list)))
  ;; Just return the asked result/answer input.
  type)

(defun organize-imports-java--get-paths-from-cache (in-cache)
  "Return the list of path from IN-CHACHE.
IN-CACHE : cache file name relative to project root folder."
  (let ((tmp-cache (concat
                    (organize-imports-java--project-dir)
                    (organize-imports-java--oij-dir in-cache)))
        ;; Read file to buffer.
        tmp-path-buffer
        ;; Split `tmp-path-buffer', from the file.
        tmp-path-list)
    ;; If the file does not exists, load the Java path once.
    ;; Get this plugin ready to use.
    (unless (file-exists-p tmp-cache)
      (cond ((string= in-cache organize-imports-java-path-jar-lib-cache-file)
             (organize-imports-java-reload-jar-lib-paths))
            ((string= in-cache organize-imports-java-path-local-source-cache-file)
             (organize-imports-java-reload-local-source-paths))
            (t
             ;; Reload all the cache file as default. Even though this
             ;; should not happens.
             (organize-imports-java-reload-paths))))

    ;; Read file to buffer.
    (setq tmp-path-buffer (organize-imports-java--get-string-from-file tmp-cache))

    (when (stringp tmp-path-buffer)
      ;; Make the path buffer back to list.
      ;;
      ;; Why I use the word 'back'? Because when we make our
      ;; list, we made it from one chunk of buffer/string.
      ;; And now we split the string back to list again.
      (setq tmp-path-list (split-string tmp-path-buffer "\n")))

    ;; Return all the path from list.
    tmp-path-list))

(defun organize-imports-java--insert-paths (in-paths)
  "Insert the IN-PATHS."
  (let (;; Loop the data once and split into alphabetic order.
        ;; List-Len = (Alphabet Id - 1).
        ;; 0 = A, 1 = B, 2 = C, etc.
        (alphabet-list-first '())
        ;; List will do the final insert.
        (insert-path-list '()))
    ;; Reset alphabetic list.
    (setq alphabet-list-first '())

    ;; Make 26 list for all A-Z alphabet letters.
    (let ((count 0))
      (while (< count 26)
        (push '() alphabet-list-first)
        (setq count (1+ count))))

    ;; Store data/paths to alphabetic order depends on the class.
    ;; So we can use alphabet id instead of loop through the whole list.
    ;;
    ;; Saved a tone of performance.
    (dolist (tmp-path in-paths)
      (let ((tmp-split-path-list '())
            ;; Usually the class name data.
            (tmp-last-element "") (first-char-from-path nil)
            (alphabet-id -1) (alphabet-list nil))

        ;; split the string into list
        (setq tmp-split-path-list (split-string tmp-path "\\."))

        ;; the last element is usually the class name.
        (setq tmp-last-element (nth (1- (length tmp-split-path-list)) tmp-split-path-list))

        (when (and (not (string-empty-p tmp-last-element))
                   ;; Exclude current buffer file name because own file
                   ;; cannot be inserted. Is illegal in Java programming.
                   (not (string= tmp-last-element
                                 ;; Filename without extension.
                                 (file-name-sans-extension (file-name-nondirectory buffer-file-name)))))
          ;; Get the first character from class name, in order
          ;; to sort in alphabetic order.
          (setq first-char-from-path (substring tmp-last-element 0 1))

          ;; get the alphabet id, which is the same as array id.
          (setq alphabet-id (organize-imports-java--get-alphabet-id first-char-from-path))

          (unless (= alphabet-id -1)
            ;; get the current alphabet list.
            (setf alphabet-list (nth alphabet-id alphabet-list-first))

            ;; First push it class name.
            (push tmp-last-element alphabet-list)
            ;; Then push it full path.
            (push tmp-path alphabet-list)

            ;; set to the two dimensional array.
            (setf (nth alphabet-id alphabet-list-first) alphabet-list)))))

    ;; ------------------------------------------------------------------------------
    ;; Start searching class name.
    ;;
    ;; Loop inside loop, we loop through the whole list of typename in current
    ;; buffer we attempt to do 'organize-imports' job. And inside this loop, we
    ;; loop through the alphabet list.
    ;;   -> First Layer Loop: typename in current buffer.
    ;;   -> Second Layer Loop: alphabet list.
    ;; Compare the `typename' in current buffer and the list of sorted `typename'
    ;; will do the job.
    (let ((tmp-same-class-name-list-length -1)
          ;; Get all the `class-name'/`font-lock-type' from current buffer.
          (type-name-list (organize-imports-java--get-type-face-keywords-by-face-name
                           organize-imports-java-font-lock-type-faces)))
      (dolist (tmp-type-class-keyword type-name-list)
        ;; Exclude the general data type. (String, Integer, etc.)
        (unless (organize-imports-java--is-in-list-string
                 organize-imports-java-unsearch-class-type tmp-type-class-keyword)
          (let (;; Choose one list from `alphabet-list-first', depends on alphabet id.
                (alphabet-list nil) (alphabet-id -1) (tmp-class-name-first-char ""))

            ;; Get the first character from the class
            ;; name keyword.
            (setq tmp-class-name-first-char (substring tmp-type-class-keyword 0 1))

            ;; Get alphabet id.
            (setq alphabet-id (organize-imports-java--get-alphabet-id tmp-class-name-first-char))

            ;; Get the alphabet list depends on `alphabet id'
            ;; what we just get above a line of code.
            (setq alphabet-list (nth alphabet-id alphabet-list-first))

            ;; ------------------------------------------------------------------------------
            ;; We loop through the alphabet list to find if there are
            ;; any matching classes?

            (let ((alphabet-list-index 0)
                  (alphabet-list-length (length alphabet-list)))
              (while (< alphabet-list-index alphabet-list-length)
                (let ((tmp-class-name "") (tmp-full-path ""))
                  ;; Get class name from the sorted alphabet list.
                  (setq tmp-class-name (nth (1+ alphabet-list-index) alphabet-list))

                  ;; Compare the keyword and class name stored.
                  (when (string= tmp-type-class-keyword tmp-class-name)
                    ;; Get full path, after we make sure that this path
                    ;; is use inside the current buffer.
                    (setq tmp-full-path (nth alphabet-list-index alphabet-list))

                    ;; add full path to check same class name list.
                    (push tmp-full-path organize-imports-java--same-class-name-list)))

                ;; Plus two cuz data structure look like this.
                ;;   => Class name
                ;;   => Full path
                (setq alphabet-list-index (+ alphabet-list-index 2)))))

          ;; ------------------------------------------------------------------------------
          ;; After we found the full path we need, we check to see if we
          ;; need to prompt to ask user if there are more than two result.

          ;; Clean duplicates item, just to lower down some performance.
          ;; And make the logic clearer.
          (setq organize-imports-java--same-class-name-list (delete-dups organize-imports-java--same-class-name-list))

          ;; Get the length of the check same class list.
          (setq tmp-same-class-name-list-length (length organize-imports-java--same-class-name-list))

          ;; to the final insert path list.
          ;; IMPORTANT: Here is where you actually add the inserted path
          (cond ((= tmp-same-class-name-list-length 1)
                 ;; Is exactly 1 result. Just add that to the final pre-insert list.
                 (push (nth 0 organize-imports-java--same-class-name-list) insert-path-list))
                ((>= tmp-same-class-name-list-length 2)
                 ;; Is is more than 2 results. Meaning we need the user to
                 ;; select which class to import!
                 (call-interactively 'organize-imports-java--same-class-ask)))

          ;; Clean the same class paths for next loop.
          (setq organize-imports-java--same-class-name-list '()))))

    ;; ------------------------------------------------------------------------------
    ;; Before insertion job, we do stuff in order to prepare to do insertion job.

    ;; Remove duplicate for pre insert list.
    (setq insert-path-list (delete-dups insert-path-list))

    ;; Sort in alphabetic order.
    (setq insert-path-list (sort insert-path-list 'string<))

    (let ((priority-length (length organize-imports-java--priority-list))
          (priority-index-list '())
          ;; Represent the list inside `priority-index-list'.
          ;; Is two dimensional array.
          (priority-list nil)
          ;; Store path that aren't in the priority list.
          (else-path-list '()))

      ;; Make list of prority list's length.
      (let ((count 0))
        (while (< count priority-length)
          (push '() priority-index-list)
          (setq count (1+ count))))

      ;; Sort list with priority list.
      (dolist (insert-path insert-path-list)
        (let ((position-in-priority-list -1))  ; Position is also the index/id.
          ;; NOTE: if `cl-position' not found will return nil.
          (setq position-in-priority-list
                (organize-imports-java--string-match-position
                 organize-imports-java--priority-list
                 insert-path))

          (if (null position-in-priority-list)
              (push insert-path else-path-list)
            ;; Get the priority list from the `priority-index-list'.
            (setf priority-list (nth position-in-priority-list
                                     priority-index-list))

            ;; Add it to prority index list by the priority/id/position.
            (push insert-path priority-list)

            ;; apply to the two dimensional array.
            (setf (nth position-in-priority-list priority-index-list)
                  priority-list))))

      ;; ▾▾▾▾▾▾▾▾▾▾ Reverse all the list. ▾▾▾▾▾▾▾▾▾▾▾▾▾▾▾▾▾▾▾▾▾▾▾▾▾▾▾▾
      ;; STUDY: Other way to do this, is instead of use `push'. Use some
      ;; other function instead.

      ;; Reverse all the priority list in `priority-index-list'.
      ;; And again, `priority-index-list' is two dimensional array.
      (let ((index 0))
        (while (< index priority-length)
          (setf (nth index priority-index-list) (reverse (nth index priority-index-list)))
          (setq index (1+ index))))

      ;; Reverse other path list too.
      (setq else-path-list (reverse else-path-list))
      ;; ▴▴▴▴▴▴▴▴▴▴ Reverse all the list. ▴▴▴▴▴▴▴▴▴▴▴▴▴▴▴▴▴▴▴▴▴▴▴▴▴▴▴▴

      ;; First set to the priority list, don't forget to flatten the two
      ;; dimensional array.
      (setq insert-path-list (-flatten priority-index-list))
      ;; Then append the rest of the path listt.
      (setq insert-path-list (append insert-path-list else-path-list)))

    ;; ------------------------------------------------------------------------------
    ;; Start the real insertion job here.
    (save-excursion
      ;; Check package keyword exists.
      (goto-char (point-min))

      ;; Make it under `package' line. Otherwise, will just
      ;; insert at the very top of the file.
      (when (string= (thing-at-point 'word) "package")
        (end-of-line)
        (insert "\n"))

      ;; Insert all import path line.
      (let ((tmp-split-path-list '())
            (tmp-first-element "") (tmp-record-first-element ""))
        (dolist (tmp-in-path insert-path-list)
          ;; split the path into list by using `.' delimiter.
          (setq tmp-split-path-list (split-string tmp-in-path "\\."))

          ;; the first element is always the class name.
          (setq tmp-first-element (nth 0 tmp-split-path-list))

          (unless (string= tmp-first-element tmp-record-first-element)
            (insert "\n")
            ;; record it down.
            (setq tmp-record-first-element tmp-first-element))

          (organize-imports-java--insert-import-lib tmp-in-path)))

      ;; keep one line.
      (organize-imports-java--keep-one-line-between))))

;;;###autoload
(defun organize-imports-java-do-imports ()
  "Do the functionalitiies of how organize imports work."
  (interactive)
  (if (not (organize-imports-java--project-dir))
      (user-error "[WARNING] Can't organize imports without project root")
    ;; Ensure oij cache directory exists.
    (ignore-errors
      (make-directory
       (concat (organize-imports-java--project-dir) organize-imports-java-oij-dir)))
    ;; Make sure oij configuration file exists before organize imports task.
    (organize-imports-java--create-default-oij-config)
    (organize-imports-java--record-project-ht)
    (save-window-excursion
      (save-excursion
        ;; Path have all the classpath from local and external cache.
        (let* (;; Add the local source path from local source cache.
               (local-paths (organize-imports-java--get-paths-from-cache
                             organize-imports-java-path-local-source-cache-file))
               ;; Get the external source path from external source cache.
               (jar-lib-paths (organize-imports-java--get-paths-from-cache
                               organize-imports-java-path-jar-lib-cache-file))
               (all-lib-paths (append local-paths jar-lib-paths)))
          (unless (= 0 (length all-lib-paths))
            ;; Clear all imports before insert new imports.
            (organize-imports-java-clear-all-imports)
            ;; Do insert.
            (organize-imports-java--insert-paths all-lib-paths)))))))

(provide 'organize-imports-java)
;;; organize-imports-java.el ends here
