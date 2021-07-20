;;; auto-overlays.el --- Automatic regexp-delimited overlays    -*- lexical-binding: t; -*-

;; Copyright (C) 2005-2020  Free Software Foundation, Inc

;; Version: 0.10.10
;; Author: Toby Cubitt <toby-predictive@dr-qubit.org>
;; Maintainer: Toby Cubitt <toby-predictive@dr-qubit.org>
;; Keywords: extensions
;; URL: http://www.dr-qubit.org/tags/computing-code-emacs.html
;; Repository: http://www.dr-qubit.org/git/predictive.git
;; Package-Requires: ((cl-lib "0.5"))

;; This file is part of the Emacs.
;;
;; This file is free software: you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with this program.  If not, see <http://www.gnu.org/licenses/>.


;;; Code:

(eval-when-compile (require 'cl-lib))

(defvar auto-overlay-regexps nil)
(make-variable-buffer-local 'auto-overlay-regexps)
(defvar auto-overlay-load-hook nil)
(defvar auto-overlay-unload-hook nil)


;; (defvar auto-overlay-list nil)
;; (make-variable-buffer-local 'auto-overlay-list)
(defvar auto-o-pending-updates nil)
(make-variable-buffer-local 'auto-o-pending-updates)
(defvar auto-o-pending-suicides nil)
(make-variable-buffer-local 'auto-o-pending-suicides)
(defvar auto-o-pending-pre-suicide nil)
(make-variable-buffer-local 'auto-o-pending-pre-suicide)
(defvar auto-o-pending-post-suicide nil)
(make-variable-buffer-local 'auto-o-pending-post-suicide)
(defvar auto-o-pending-post-update nil)
(make-variable-buffer-local 'auto-o-pending-post-update)





;;; ============================================================
;;;              Replacements for CL functions

(defun auto-o-assq-position (key alist)
  "Find the first association of KEY in ALIST.
Return the index of the matching item, or nil of not found.
Comparison is done with 'eq."
  (let (el (i 0))
    (catch 'found
      (while (setq el (nth i alist))
	(when (eq key (car el)) (throw 'found i))
	(setq i (1+ i))
	nil))))



(defun auto-o-position (item list)
  "Find the first occurrence of ITEM in LIST.
Return the index of the matching item, or nil of not found.
Comparison is done with `equal'."
  (let (el (i 0))
    (catch 'found
      (while (setq el (nth i list))
	(when (equal item el) (throw 'found i))
	(setq i (1+ i))
	nil))))



(defun auto-o-sublist (list start &optional end)
  "Return the sub-list of LIST from START to END.
If END is omitted, it defaults to the length of the list
If START or END is negative, it counts from the end."
  (let (len)
    ;; sort out arguments
    (if end
	(when (< end 0) (setq end (+ end (setq len (length list)))))
      (setq end (or len (setq len (length list)))))
    (when (< start 0)
      (setq start (+ start (or len (length list)))))

    ;; construct sub-list
    (let (res)
      (while (< start end)
	(push (nth start list) res)
	(setq start (1+ start)))
      (nreverse res))))


(defmacro auto-o-adjoin (item list)
  "Cons ITEM onto front of LIST if it's not already there.
Comparison is done with `eq'."
  `(if (memq ,item ,list) ,list (setf ,list (cons ,item ,list))))


(defun auto-o--plist-get (list prop)
  ;; Return property PROP from LIST, which can be a "half"-plist, i.e. a
  ;; list containing both plist keys and entries, and other elements, of the
  ;; form: (elt1 elt2 ... :key1 val1 elt3 ... :key2 val2 ...)
  (while (and list (not (eq prop (car list))))
    (setq list (cdr list)))
  (cadr list))


(defun auto-o--plist-delete (list &rest keys)
  ;; Destructively delete KEYS from PLIST
  (while (memq (car list) keys)
    (setq list (cddr list)))
  (let ((el list))
    (while el
      (when (memq (cadr el) keys)
	(setcdr el (cdddr el)))
      (setq el (cdr el))))
  list)


(defun auto-o--plist-tail (list)
  ;; Return part of LIST coming after initial plist-like ":key val" elements.
  ;; Works for "half"-plists that have their plist part at the beginning:
  ;; (:key1 val1 :key2 val2 ... elt1 elt2 ...) -> (elt1 elt2 ...)
  (while (and (symbolp (car list))
	      (= ?: (aref (symbol-name (car list)) 0)))
    (setq list (cddr list)))
  list)




;;;========================================================
;;;               Regexp definition functions

;; --- set ---

(defun auto-o-get-set (set-id)
  ;; Return the list of regexp definitions for regexp set SET-ID.
  (cddr (assq set-id auto-overlay-regexps)))


(defun auto-o-create-set (set-id)
  ;; Add blank entry for a new regexp set SET-ID to `auto-overlay-regexps'.
  (push (list set-id nil) auto-overlay-regexps))


(defun auto-o-delete-set (set-id)
  ;; Delete SET-ID entry from `auto-overlay-regexps'.
  (setq auto-overlay-regexps
	(assq-delete-all set-id auto-overlay-regexps)))



(defun auto-o-get-full-buffer-list (set-id)
  ;; Return the list of buffers and associated properties for set SET-ID.
  (nth 1 (assq set-id auto-overlay-regexps)))


(defun auto-o-get-buffer-list (set-id)
  ;; Return list of buffers using set SET-ID.
  (mapcar #'car (auto-o-get-full-buffer-list set-id)))


(defun auto-o-add-to-buffer-list (set-id buffer)
  ;; Add BUFFER to the list of buffers using regexp set SET-ID.
  (let ((set (assq set-id auto-overlay-regexps)))
    (and set
	 (null (assq buffer (cadr set)))
	 (setcar (cdr set) (cons (cons buffer nil) (cadr set))))))


(defun auto-o-delete-from-buffer-list (set-id buffer)
  ;; Remove BUFFER from the list of buffers using regexp set SET-ID.
  (let ((set (assq set-id auto-overlay-regexps)))
    (and set
	 (setcar (cdr set) (assq-delete-all buffer (cadr set))))))



(defun auto-o-enabled-p (set-id &optional buffer)
  ;; Return non-nil if regexp set identified by SET-ID is enabled in BUFFER.
  (let ((buff (or buffer (current-buffer))))
    (cdr (assq buff (auto-o-get-full-buffer-list set-id)))))


(defun auto-o-enable-set (set-id &optional buffer)
  ;; Set enabled flag for BUFFER in regexp set SET-ID.
  (let ((buff (or buffer (current-buffer))))
    (setcdr (assq buff (auto-o-get-full-buffer-list set-id)) t)))


(defun auto-o-disable-set (set-id &optional buffer)
  ;; Unset enabled flag for BUFFER in regexp set SET-ID.
  (let ((buff (or buffer (current-buffer))))
    (setcdr (assq buff (auto-o-get-full-buffer-list set-id)) nil)))



(defun auto-o-save-filename (set-id)
  ;; Return the default filename to save overlays in
  (concat "auto-overlays-"
	   (replace-regexp-in-string
	    "\\." "-" (file-name-nondirectory (or (buffer-file-name)
						  (buffer-name))))
	   "-" (symbol-name set-id)))



;; --- definition ---

(defun auto-o-get-definition (set-id definition-id)
  ;; Return definition identified by SET-ID and DEFINITION-ID
  (cdr (assq definition-id (auto-o-get-set set-id))))


(defun auto-o-append-definition (set-id entry)
  ;; Append regexp ENTRY to SET-ID's regexps.
  (nconc (auto-o-get-set set-id) (list entry)))


(defun auto-o-prepend-definition (set-id entry)
  ;; Prepend regexp ENTRY to SET-ID's regexps.
  (setcdr (cdr (assq set-id auto-overlay-regexps))
	  (nconc (list entry) (auto-o-get-set set-id))))


(defun auto-o-insert-definition (set-id pos entry)
  ;; Insert regexp ENTRY in SET-ID's regexps at POS.
  (setcdr (nthcdr (1- pos) (auto-o-get-set set-id))
	  (nconc (list entry) (nthcdr pos (auto-o-get-set set-id)))))


(defun auto-o-definition-class (set-id definition-id)
  ;; Return class corresponding to SET-ID and DEFINITION-ID.
  (nth 1 (assq definition-id (auto-o-get-set set-id))))


(defun auto-o-definition-complex-class-p (set-id definition-id)
  ;; Return non-nil if regexp corresponding to SET-ID and DEFINITION-ID
  ;; requires separate start and end regexps
  (get (auto-o-definition-class set-id definition-id)
	'auto-overlay-complex-class))

;; (defun auto-o-regexp-compound-class-p (set-id definition-id)
;;   ;; Return non-nil if regexp corresponding to SET-ID and DEFINITION-ID
;;   ;; contains a list of regexp entries rather than a single entry.
;;   (let ((entry (cadr (auto-o-get-regexp set-id definition-id))))
;;     (and (listp entry)
;; 	 (or (symbolp (cdr entry))
;; 	     (and (listp (cdr entry)) (symbolp (cadr entry)))))))



;; --- regexp ---

(defun auto-o-get-regexp (set-id definition-id regexp-id)
  ;; Return regexp entry identified by SET-ID, DEFINITION-ID and REGEXP-ID.
  (cdr (assq regexp-id (auto-o-get-definition set-id definition-id))))


(defun auto-o-regexp-regexp (set-id definition-id regexp-id)
  ;; Return regexp corresponsing to SET-ID, DEFINITION-ID and REGEXP-ID.
  (let ((regexp (car (auto-o-get-regexp set-id definition-id regexp-id))))
     (if (atom regexp) regexp (car regexp))))


(defun auto-o-regexp-group (set-id definition-id regexp-id &optional n)
  ;; Return regexp group for SET-ID, DEFINITION-ID and REGEXP-ID. If N is
  ;; supplied, return the Nth regexp group entry if specified, nil otherwise.
  (unless n (setq n 0))
  (let ((regexp (car (auto-o-get-regexp set-id definition-id regexp-id))))
    (if (= n 0)
	(cond
	 ((atom regexp) 0)
	 ((atom (cdr regexp)) (cdr regexp))
	 (t (cadr regexp)))
      (when (and (consp regexp) (listp (cdr regexp)))
	(nth n (cdr regexp))))))


(defun auto-o-regexp-props (set-id definition-id regexp-id)
  ;; Return properties of regexp corresponding to SET-ID, DEFINITION-ID and
  ;; REGEXP-ID.
  (auto-o--plist-tail (cdr (auto-o-get-regexp set-id definition-id regexp-id))))


(defun auto-o-regexp-key-value (set-id definition-id regexp-id key)
  ;; Return value for KEY in regexp corresponding to SET-ID, DEFINITON-ID and
  ;; REGEXP-ID.
  (auto-o--plist-get (auto-o-get-regexp set-id definition-id regexp-id) key))


(defun auto-o-regexp-edge (set-id definition-id regexp-id)
  ;; Return edge ('start or 'end) of regexp corresponding to SET-ID,
  ;; DEFINITION-ID and REGEXP-ID
  (auto-o-regexp-key-value set-id definition-id regexp-id :edge))


(defun auto-o-regexp-match-exclusive (set-id definition-id regexp-id)
  ;; Return :exclusive property of regexp corresponding to SET-ID,
  ;; DEFINITION-ID and REGEXP-ID
  (auto-o-regexp-key-value set-id definition-id regexp-id :exclusive))


(defun auto-o-regexp-parse-hook-function (set-id definition-id regexp-id)
  ;; Return parse hook function of regexp corresponding to SET-ID,
  ;; DEFINITION-ID and REGEXP-ID.
  (auto-o-regexp-key-value set-id definition-id regexp-id :parse-function))


(defun auto-o-regexp-suicide-hook-function (set-id definition-id regexp-id)
  ;; Return suicide hook function of regexp corresponding to SET-ID,
  ;; DEFINITION-ID and REGEXP-ID.
  (auto-o-regexp-key-value set-id definition-id regexp-id :suicide-function))


(defun auto-o-regexp-match-hook-function (set-id definition-id regexp-id)
  ;; Return match hook function of regexp corresponding to SET-ID,
  ;; DEFINITION-ID and REGEXP-ID.
  (auto-o-regexp-key-value set-id definition-id regexp-id :match-function))



;; --- match overlays ---

(defun auto-o-class (o-match)
  ;; Return class of match overlay O-MATCH.
  (auto-o-definition-class (overlay-get o-match 'set-id)
			    (overlay-get o-match 'definition-id)))

(defun auto-o-regexp (o-match)
  ;; Return match overlay O-MATCH's regexp.
  (auto-o-regexp-regexp (overlay-get o-match 'set-id)
			(overlay-get o-match 'definition-id)
			(overlay-get o-match 'regexp-id)))

(defun auto-o-group (o-match &optional n)
  ;; Return match overlay O-MATCH's regexp group.
  ;; If N in supplied, return the Nth regexp group entry if that is specified,
  ;; nil otherwise.
  (auto-o-regexp-group (overlay-get o-match 'set-id)
		       (overlay-get o-match 'definition-id)
		       (overlay-get o-match 'regexp-id)
		       n))


(defun auto-o-regexp-match (o-match &optional n)
  ;; Return string matching O-MATCH's regexp.
  ;; If N is supplied, return string matching the group specified by
  ;; the N'th group entry if that is specified, nil otherwise.
  (let ((str (buffer-substring-no-properties
	      (overlay-start o-match)
	      (overlay-end o-match))))
    (if (null n) str
      (let ((g (auto-o-group o-match n)))
	(when (and g (string-match (auto-o-regexp o-match) str))
	  (match-string g str))))))

(defun auto-o-key-value (o-match key)
  ;; Return value of regexp KEY for match overlay O-MATCH.
  (auto-o-regexp-key-value (overlay-get o-match 'set-id)
			   (overlay-get o-match 'definition-id)
			   (overlay-get o-match 'regexp-id)
			   key))


(defun auto-o-edge (o-match)
  ;; Return edge ('start or 'end) of match overlay O-MATCH
  (auto-o-regexp-edge (overlay-get o-match 'set-id)
		      (overlay-get o-match 'definition-id)
		      (overlay-get o-match 'regexp-id)))


(defun auto-o-props (o-match)
  ;; Return properties associated with match overlay O-MATCH.
  (auto-o-regexp-props (overlay-get o-match 'set-id)
		       (overlay-get o-match 'definition-id)
		       (overlay-get o-match 'regexp-id)))


(defun auto-o-match-exclusive (o-match)
  ;; Return :exclusive property of match overlay O-MATCH
  (auto-o-regexp-match-exclusive (overlay-get o-match 'set-id)
				 (overlay-get o-match 'definition-id)
				 (overlay-get o-match 'regexp-id)))


;; (defun auto-o-compound-class-p (o-match)
;;   ;; Return non-nil if O-MATCH's regexp class is a compound class
;;   ;; (can just check for 'regexp-id property instead of checking regexp
;;   ;; definitions, since this is always set for such match overlays)
;;   (overlay-get o-match 'regexp-id))


(defun auto-o-complex-class-p (o-match)
  ;; Return non-nil if O-MATCH's regexp class is a compound class
  (get (auto-o-class o-match) 'auto-overlay-complex-class))


;; (defun auto-o-priority (o-match)
;;   ;; Return the priority of match overlay O-MATCH
;;   (overlay-get o-match 'priority))


(defun auto-o-priority-< (a b)
  ;; Return t iff the priority of A (overlay, number or nil) is smaller than
  ;; that of B (overlay, number or nil).
  (when (overlayp a) (setq a (overlay-get a 'priority)))
  (when (overlayp b) (setq b (overlay-get b 'priority)))
  (or (and (null a) b)
      (and a b (< a b))))


(defun auto-o-rank (o-match)
  ;; Return the rank of match overlay O-MATCH
  (auto-o-assq-position
    (overlay-get o-match 'regexp-id)
    (cddr (assq (overlay-get o-match 'definition-id)
		(auto-o-get-set (overlay-get o-match 'set-id))))))


(defun auto-o-parse-hook-function (o-match)
  ;; Return parse hook function for match overlay O-MATCH
  (auto-o-regexp-parse-hook-function
   (overlay-get o-match 'set-id)
   (overlay-get o-match 'definition-id)
   (overlay-get o-match 'regexp-id)))


(defun auto-o-suicide-hook-function (o-match)
  ;; Return suicide hook function for match overlay O-MATCH
  (auto-o-regexp-suicide-hook-function
   (overlay-get o-match 'set-id)
   (overlay-get o-match 'definition-id)
   (overlay-get o-match 'regexp-id)))


(defun auto-o-match-hook-function (o-match)
  ;; Return match hook function for match overlay O-MATCH
  (auto-o-regexp-match-hook-function (overlay-get o-match 'set-id)
				     (overlay-get o-match 'definition-id)
				     (overlay-get o-match 'regexp-id)))


(defun auto-o-call-parse-function (o-match)
  ;; Call appropriate parse function for match overlay O-MATCH.
  (prog1
      (funcall (get (auto-o-class o-match) 'auto-overlay-parse-function)
	       o-match)
    (let ((h (auto-o-parse-hook-function o-match)))
      (when h (funcall h o-match)))))


(defun auto-o-call-suicide-function (o-match)
  ;; Call appropriate suicide function for match overlay O-MATCH.
  (prog1
      (funcall (get (auto-o-class o-match) 'auto-overlay-suicide-function)
	       o-match)
    (let ((h (auto-o-suicide-hook-function o-match)))
      (when h (funcall h o-match)))))


(defun auto-o-call-match-function (o-match)
  ;; Return match function for match overlay O-MATCH, if any.
  (let ((m (get (auto-o-class o-match) 'auto-overlay-match-function)))
    (when m
      (prog1
	  (funcall m o-match)
	(let ((h (auto-o-match-hook-function o-match)))
	  (when h (funcall h o-match)))))))



;; --- overlays ---

(defun auto-o-edge-matched-p (overlay edge)
  ;; test if EDGE of OVERLAY is matched
  (overlay-get overlay edge))


(defun auto-o-start-matched-p (overlay)
  ;; test if OVERLAY is start-matched
  (overlay-get overlay 'start))


(defun auto-o-end-matched-p (overlay)
  ;; test if OVERLAY is end-matched
  (overlay-get overlay 'end))



;;; ===============================================================
;;;                    Compatibility aliases

(unless (fboundp 'line-number-at-pos)
  (defun line-number-at-pos (&optional pos)
    "Return (narrowed) buffer line number at position POS.
\(Defaults to the point.\)"
    (unless pos (setq pos (point)))
    ;; note: need to add 1 if at beginning of line
    (+ (count-lines (point-min) pos)
       (if (save-excursion (goto-char pos) (bolp)) 1 0))))

(unless (fboundp 'replace-regexp-in-string)
  (defun replace-regexp-in-string (regexp rep string)
    "Return a new string with all matches for REGEXP in STRING replaced
with REP."
    (let ((str string))
      (while (string-match regexp str)
	(setq str (replace-match rep nil nil str)))
      str)))



;;; ===============================================================
;;;                Public overlay query functions

(defun auto-overlay-< (a b)
  "Return non-nil iff overlay A comes before overlay B in buffer."
  (and (eq (overlay-buffer a) (overlay-buffer b))
       (or (< (overlay-start a) (overlay-start b))
	   (and (= (overlay-start a) (overlay-start b))
		(> (overlay-end a) (overlay-end b))))))


;;;###autoload
(cl-defun auto-overlays-in (start end &rest prop-tests
				  &key within inactive all-overlays &allow-other-keys)
  "Return auto overlays overlapping region between START and END.

If keyword argument :within is non-nil, only overlays entirely
within START and END are returned.

If keyword argument :inactive is non-nil, both active and
inactive overlays are returned (usually inactive ones are
ignored).

If keyword argument :all-overlays is non-nil, all overlays are
returned, not just auto-overlays.

Any remaining arguments (which *must* come after any keyword
arguments) specify property tests, each of which should be a list
with one of the following forms:

  PROPERTY

  (FUNCTION PROPERTY)

  (FUNCTION PROPERTY VALUE)

  (FUNCTION (PROPERTY1 PROPERTY2 ...) (VALUE1 VALUE2 ...))

where PROPERTY indicates an overlay property name (a symbol), and
VALUE indicates an arbitrary value or lisp expression.

For each overlay between START and END, first the values
corresponding to the property names are retrieved from the
overlay, then FUNCTION is called with the properties values
followed by the other values as its arguments. The test is
satisfied if the result is non-nil, otherwise it fails.

A PROPERTY symbol on its own tests whether that property has a
non-null value, equivalent to (identity PROPERTY).

Tests are evaluated in order, but only up to the first failure.
Only overlays that satisfy all property tests are returned."

  ;; remove any keyword arguments from PROP-TESTS
  (setq prop-tests
	(auto-o--plist-delete prop-tests :within :inactive :all-overlays))
  ;; exclude inactive overlays unless told not to
  (unless inactive (push '(null inactive) prop-tests))
  ;; exclude non-auto-overlays unless told not to
  (unless all-overlays (push '(identity auto-overlay) prop-tests))

  ;; FIXME: Is updating just START and END enough to trigger all updates?
  (auto-overlay-trigger-update start)
  (unless (= start end) (auto-overlay-trigger-update end))

  (let (overlay-list function prop-list value-list)
    ;; check properties of each overlay in region
    (dolist (o (overlays-in start end))
      (catch 'failed
	;; check overlay is entirely within region
	(when (and within
		   (or (< (overlay-start o) start)
		       (> (overlay-end o) end)))
	  (throw 'failed nil))

	;; check if properties match
	;; Note: The whole thing would be neater with something like (apply
	;;       #'and (map ...)) but `and' is a special form, not a function
	(dolist (test prop-tests)
	  ;; single property
	  (if (symbolp test)
	      (when (null (overlay-get o test)) (throw 'failed nil))
	    ;; propery test
	    (setq function (nth 0 test))
	    (unless (listp (setq prop-list (nth 1 test)))
	      (setq prop-list (list prop-list)))
	    (setq value-list nil)
	    (unless (or (< (length test) 3)
			(and (setq value-list (nth 2 test))
			     (listp value-list)))            ; nil isn't list
	      (setq value-list (list value-list)))
	    ;; apply the test
	    (unless (apply function
			   (append (mapcar (lambda (p) (overlay-get o p))
					   prop-list)
				   value-list))
	      (throw 'failed nil))))

	;; add overlay to result list if its properties matched
	(push o overlay-list)))

    ;; return result list
    (nreverse overlay-list)))



;;;###autoload
(cl-defun auto-overlays-at-point (&optional point
				  &rest prop-tests
				  &key inactive all-overlays &allow-other-keys)
  ;; FIXME: Maybe we shouldn't use `&key' here: it just costs extra work at
  ;; run-time to extract that info, then extra code below to silence the byte
  ;; compiler warnings because we then "ignore" that data.
  "Return overlays overlapping POINT, defaulting to the point.

If keyword argument :inactive is non-nil, both active and
inactive overlays are returned (usually inactive ones are
ignored).

If keyword argument :all-overlays is non-nil, all overlays are
returned, not just auto-overlays.

Any remaining arguments (which *must* come after any keyword
arguments) specify property tests, each of which should be a list
with one of the following forms:

  (FUNCTION PROPERTY)

  (FUNCTION PROPERTY VALUE)

  (FUNCTION (PROPERTY1 PROPERTY2 ...) (VALUE1 VALUE2 ...))

where PROPERTY indicates an overlay property name (a symbol), and
VALUE indicates an arbitrary value or lisp expression.

For each overlay between START and END, first the values
corresponding to the property names are retrieved from the
overlay, then FUNCTION is called with the properties values
followed by the other values as its arguments. The test is
satisfied if the result is non-nil, otherwise it fails. Tests are
evaluated in order, but only up to the first failure. Only
overlays that satisfy all property tests are returned."
  (ignore all-overlays inactive)
  (when (null point) (setq point (point)))
  (auto-overlay-trigger-update point)

  (let (overlay-list)
    ;; get overlays overlapping POINT and zero-length overlays at POINT
    (setq overlay-list
	  (apply #'auto-overlays-in point point prop-tests))
    ;; get overlays that end at POINT
    (dolist (o (apply #'auto-overlays-in (1- point) point prop-tests))
      (when (and (< (overlay-start o) point)
		 (= (overlay-end o) point))
	(push o overlay-list)))
    ;; get overlays that start at POINT
    (dolist (o (apply #'auto-overlays-in point (1+ point) prop-tests))
      (when (and (> (overlay-end o) point)
		 (= (overlay-start o) point))
	(push o overlay-list)))
    overlay-list))



;;;###autoload
(cl-defun auto-overlay-highest-priority-at-point ( &optional point
					           &rest prop-tests
						   &key inactive all-overlays
						   &allow-other-keys)
  ;; FIXME: Maybe we shouldn't use `&key' here: it just costs extra work at
  ;; run-time to extract that info, then extra code below to silence the byte
  ;; compiler warnings because we then "ignore" that data.
  "Return highest priority overlay at POINT, defaulting to the point.

If two overlays have the same priority, the innermost one takes
precedence (i.e. the one that begins later, or if they begin at
the same point the one that ends earlier).

The remaining arguments are as for `auto-overlays-at' (which see)."
  (ignore all-overlays inactive)
  (unless point (setq point (point)))

  ;; get all overlays at point with a non-nil SYMBOL property
  (let* ((overlay-list (apply #'auto-overlays-at-point point prop-tests))
	 (overlay (pop overlay-list))
	 p p1)

    ;; find the highest priority, innermost overlay
    (dolist (o1 overlay-list)
      (setq p (overlay-get overlay 'priority))
      (setq p1 (overlay-get o1 'priority))
      (when (or (and (null p) p1)
		(and p p1 (> p1 p))
		(and (equal p1 p)
		     (or (> (overlay-start o1) (overlay-start overlay))
			 (and (= (overlay-start o1) (overlay-start overlay))
			      (< (overlay-end o1) (overlay-end o1))))))
	(setq overlay o1)))

    ;; return the overlay
    overlay))



;;;###autoload
(defun auto-overlay-local-binding (symbol &optional point only-overlay)
  "Return \"overlay local \" binding of SYMBOL at POINT,
or the current local binding if there is no overlay binding. If
there is no overlay binding and SYMBOL is not bound, return
nil. POINT defaults to the point.

If ONLY-OVERLAY is non-nil, only overlay bindings are
returned. If none exists at POINT, nil is returned

An \"overlay local\" binding is created by giving an overlay a
non-nil value for a property named SYMBOL. If more than one
overlay at POINT has a non-nil SYMBOL property, the value from
the highest priority overlay is returned.

See `auto-overlay-highest-priority-at-point' for a definition of
\"highest priority\"."

  (let ((overlay (auto-overlay-highest-priority-at-point
		  point `(identity ,symbol))))
    (if overlay
	(overlay-get overlay symbol)
      (and (not only-overlay) (boundp symbol) (symbol-value symbol)))))



;;; =========================================================
;;;          Public auto-overlay definition functions

;;;###autoload
(defun auto-overlay-load-set (set-id definitions &optional noparse)
  "Load the set of auto-overlay DEFINITIONS
into the set identified by SET-ID the current buffer.

DEFINITIONS should be a list of the form:

  (DEFINITION1 DEFINITION2 ... )

The DEFINITION's should be lists of the form:

  (CLASS @optional :id DEFINITION-ID @rest REGEXP1 REGEXP2 ... )

CLASS is a symbol specifying the auto-overlay class. The standard
classes are 'word, 'line, 'self, 'flat and 'nested. The :id
property is optional. It should be a symbol that can be used to
uniquely identify DEFINITION (see
`auto-overlay-unload-definition').

The REGEXP's should be lists of the form:

  (RGXP &optional :edge EDGE :id REGEXP-ID
        &rest PROPERTY1 PROPERTY2 ... )

RGXP is either a single regular expression (a string), or a cons
cell of the form (RGXP . GROUP) where RGXP is a regular
expression and GROUP is an integer specifying which group in the
regular expression forms the delimiter for the auto-overlay. The
rest of the PROPERTY entries should be cons cells of the
form (NAME . VALUE) where NAME is an overlay property name (a
symbol) and VALUE is its value.

The :edge and :id properties are optional. EDGE should be one of
the symbols 'start or 'end. If it is not specified, :edge is
assumed to be 'start. ID property is a symbol that can be used to
uniquely identify REGEXP (see `auto-overlay-unload-regexp')."

  ;; load new set of definitions, and collect definition-id's
  (let ((definition-ids
	  (mapcar (lambda (def)
		    (auto-overlay-load-definition set-id def nil 'no-parse))
		  definitions)))
    ;; reparse buffer for new definitions
    (when (and (not noparse) (auto-o-enabled-p set-id))
      (auto-o-parse-buffer set-id))
    definition-ids))


;;;###autoload
(defun auto-overlay-load-definition (set-id definition &optional pos noparse)
  "Load DEFINITION into the set of auto-overlay definitions SET-ID
in the current buffer. If SET-ID does not exist, it is created.

If POS is nil, DEFINITION is added at the end of the list of
auto-overlay definitions. If it is t, it is added at the
beginning. If it is an integer, it is added at that position in
the list. The position in the list makes no difference to the
behaviour of the auto-overlays. But it can make a difference to
the speed and efficiency. In general, higher-priority and
exclusive DEFINITIONS should appear earlier in the list.

Returns a unique id for the loaded definition, which can be used
to unload it later using `auto-overlay-unload-definition' (which
see).


DEFINITION should be a list of the form:

  (CLASS @optional :id DEFINITION-ID @rest REGEXP1 REGEXP2 ... )

CLASS is a symbol specifying the auto-overlay class. The standard
classes are `word', `line', `self', `flat' and `nested'. The :id
property is optional. It should be a symbol that uniquely
identifies the DEFINITION within SET-ID (see
`auto-overlay-unload-definition').

REGEXP should be a list of the form:

  (RGXP &optional :edge EDGE :id REGEXP-ID
        &rest PROPERTY1 PROPERTY2 ... )

RGXP is either a single regular expression (a string), or a cons
cell of the form (RGXP . GROUP) where RGXP is a regular
expression and GROUP is an integer specifying which group in the
regular expression forms the delimiter for the auto-overlay. The
rest of the PROPERTY entries should be cons cells of the
form (NAME . VALUE) where NAME is an overlay property name (a
symbol) and VALUE is its value.

The EDGE and ID properties are optional. EDGE should be one of
the symbols `start' or `end'. If it is not specified, :edge is
assumed to be `start'. ID should be a symbol that uniquely
identifies REGEXP within DEFINITION (see
`auto-overlay-unload-regexp')."

  (let ((regexps (auto-o-get-set set-id))
	(class (car definition))
	definition-id)
    ;; if SET-ID doesn't exist in regexp list, create empty set
    (when (null regexps)
      (auto-o-create-set set-id)
      (auto-o-add-to-buffer-list set-id (current-buffer))
      (setq regexps (auto-o-get-set set-id)))

    (let (n)
      (if (null (setq n (auto-o-position :id definition)))
	  ;; if DEFINITION-ID is not specified, create a unique numeric
	  ;; DEFINITION-ID
	  (setq definition-id
		(1+ (apply #'max -1
			   (mapcar (lambda (elt)
				     (if (integerp (car elt))
					 (car elt) -1))
				   regexps))))
	;; if DEFINITION-ID is specified, check it's unique
	(setq definition-id (nth (1+ n) definition))
	(setq definition (append (auto-o-sublist definition 0 n)
				 (auto-o-sublist definition (+ n 2))))
	(when (assq definition-id regexps)
	  (error "Definition ID \"%s\" is not unique"
		 (symbol-name definition-id)))))

    ;; load new definition
    (cond
     ;; adding first entry or at start
     ((or (eq pos t) (= (length regexps) 0)
	  (and (integerp pos) (<= pos 0)))
      (auto-o-prepend-definition set-id (list definition-id class)))
     ;; adding at end
     ((or (null pos) (and (integerp pos) (>= pos (length regexps))))
      (auto-o-append-definition set-id (list definition-id class)))
     ;; adding at POS
     ((integerp pos)
      (auto-o-insert-definition set-id pos (list definition-id class))))
    ;; load new regexps
    (dolist (regexp (auto-o--plist-tail (cdr definition)))
      (auto-overlay-load-regexp set-id definition-id regexp nil 'no-parse))

    ;; re-parse buffer for new definitions
    (when (and (auto-o-enabled-p set-id) (not noparse))
      (auto-o-parse-buffer set-id definition-id))
    definition-id))  ; return new definition ID



;;;###autoload
(defun auto-overlay-load-regexp (set-id definition-id regexp &optional pos noparse)
  "Load REGEXP into the auto-overlay definition identified by
DEFINITION-ID in the regexp list named SET-ID in the current
buffer.

If POS is nil, REGEXP is added at the end of the definition. If
it is t, it is added at the beginning. If it is an integer, it is
added at that position.


REGEXP should be a list of the form:

  (RGXP &optional :edge EDGE :id REGEXP-ID
        &rest PROPERTY1 PROPERTY2 ... )

RGXP is either a single regular expression (a string), or a cons
cell of the form (RGXP . GROUP) where RGXP is a regular
expression and GROUP is an integer specifying which group in the
regular expression forms the delimiter for the auto-overlay. The
rest of the PROPERTY entries should be cons cells of the
form (NAME . VALUE) where NAME is an overlay property name (a
symbol) and VALUE is its value.

The :edge and :id properties are optional. EDGE should be one of
the symbols `start' or `end'. If it is not specified, :edge is
assumed to be `start'. ID property is a symbol that can be used to
uniquely identify REGEXP (see `auto-overlay-unload-regexp')."

  (let ((defs (assq definition-id (auto-o-get-set set-id)))
	regexp-id)
    (when (null defs)
      (error "Definition \"%s\" not found in auto-overlay regexp set %s"
	     (symbol-name definition-id) (symbol-name set-id)))

    ;; generate/check regexp id
    (if (setq regexp-id (auto-o--plist-get regexp :id))
	(if (assq regexp-id defs)
	    (error "Regexp ID \"%s\" is not unique" (symbol-name regexp-id))
	  (setq regexp (auto-o--plist-delete regexp :id)))
      ;; if no id is specified, create a unique numeric ID
      (setq regexp-id
	    (1+ (apply #'max -1
		       (mapcar (lambda (elt)
				 (if (integerp (car elt)) (car elt) -1))
			       (cddr defs))))))
    ;; construct new regexp definition
    (setq regexp (cons regexp-id regexp))

    (cond
     ;; adding at end
     ((or (null pos) (and (integerp pos) (>= pos (length (cddr defs)))))
      (nconc defs (list regexp)))
     ;; adding at start
     ((or (eq pos t) (and (integerp pos) (<= pos 0)))
      (setcdr (cdr defs) (nconc (list regexp) (cddr defs))))
     ;; adding at POS
     ((integerp pos)
      (setcdr (nthcdr (1- pos) (cddr defs))
	      (nconc (list regexp) (nthcdr pos (cddr defs))))))

    ;; re-parse buffer for new regexp
    (when (and (auto-o-enabled-p set-id) (not noparse))
      (auto-o-parse-buffer set-id definition-id))

    regexp-id))  ; return new ID



(defun auto-overlay-unload-set (set-id)
  "Unload the entire regexp set SET-ID from the current buffer."

  ;; disable regexp set to delete overlays, then delete regexp set from
  ;; current buffer
  (when (auto-o-enabled-p set-id)
    (auto-overlay-stop set-id))
  (auto-o-delete-from-buffer-list set-id (current-buffer))
  (auto-o-delete-set set-id))



(defun auto-overlay-unload-definition (set-id definition-id)
  "Unload auto-overlay definition DEFINITION-ID in set SET-ID
from the current buffer. Returns the deleted definition."

  (save-current-buffer
    ;; call suicide function for corresponding overlays in all buffers in
    ;; which the set is enabled
    (dolist (buff (auto-o-get-buffer-list set-id))
      (set-buffer buff)
      (when (auto-o-enabled-p set-id)
	(mapc (lambda (o)
		(let ((line (line-number-at-pos (overlay-start o))))
		  (auto-o-suicide o 'force)
		  (auto-o-schedule-update line nil nil set-id)))
	      (auto-overlays-in (point-min) (point-max) :all-overlays t
				'(identity auto-overlay-match)
				`(eq set-id ,set-id)
				`(eq definition-id ,definition-id)))))
    ;; delete definition
    (let ((olddef (assq definition-id (auto-o-get-set set-id)))
	   def-id class regexps)
      ;; safe to delete by side effect here because definition is guaranteed
      ;; not to be the first element of the list (the first two elements of a
      ;; regexp set are always the set-id and the buffer list)
      (assq-delete-all definition-id (assq set-id auto-overlay-regexps))

      ;; massage deleted definition into form suitable for
      ;; `auto-overlay-load-definition'
      (setq def-id (nth 0 olddef)
	    class (nth 1 olddef)
	    regexps (nthcdr 2 olddef))
      (setq olddef (list class :id def-id))

      (dolist (rgxp regexps)
	(nconc olddef
	       (list
		(nconc (list (nth 1 rgxp) :id (nth 0 rgxp))
		       (nthcdr 2 rgxp)))))
      olddef)))  ; return deleted definition



(defun auto-overlay-unload-regexp (set-id definition-id regexp-id)
  "Unload the regexp identified by REGEXP-ID from auto-overlay
definition DEFINITION-ID in set SET-ID of the current buffer.
Returns the deleted regexp."

  (save-current-buffer
    ;; call suicide function for corresponding overlays in all buffers in
    ;; which the set is enabled
    (dolist (buff (auto-o-get-buffer-list set-id))
      (set-buffer buff)
      (when (auto-o-enabled-p set-id)
	(mapc (lambda (o)
		(let ((line (line-number-at-pos (overlay-start o))))
		  (auto-o-suicide o 'force)
		  (auto-o-schedule-update line nil nil set-id)))
	      (auto-overlays-in (point-min) (point-max) :all-overlays t
				'(identity auto-overlay-match)
				`(eq set-id ,set-id)
				`(eq definition-id ,definition-id)
				`(eq regexp-id ,regexp-id)))))
    ;; delete regexp entry
    (let* ((def (cdr (assq definition-id (auto-o-get-set set-id))))
	   (oldregexp (assq regexp-id def))
	   id edge regexp props)
      ;; can safely delete by side effect here because the regexp definition
      ;; is guaranteed not to be the first element of the list (the first two
      ;; elements of a definition are always the :id and class)
      (assq-delete-all regexp-id def)

      ;; massage deleted definition into form suitable for
      ;; `auto-overlay-load-definition'
      (setq id (nth 0 oldregexp)
	    edge (nth 1 oldregexp)
	    regexp (nth 2 oldregexp)
	    props (nthcdr 3 oldregexp))
      (setq oldregexp (append (list regexp :edge edge :id id) props))
      oldregexp))  ; return deleted regexp
)



;;;###autoload
(defun auto-overlay-share-regexp-set (set-id from-buffer &optional to-buffer)
  "Make TO-BUFFER share the regexp set identified by SET-ID with FROM-BUFFER.
Any changes to that regexp set in either buffer will be reflected in the
other. TO-BUFFER defaults to the current buffer."

  (unless to-buffer (setq to-buffer (current-buffer)))
  (let (regexps)
    ;; get regexp set from FROM-BUFFER
    (with-current-buffer from-buffer
      (setq regexps (assq set-id auto-overlay-regexps))
      ;; delete any existing set with same ID, and add regexp set to TO-BUFFER
      (set-buffer to-buffer)
      (setq auto-overlay-regexps
	    (assq-delete-all set-id auto-overlay-regexps))
      (push regexps auto-overlay-regexps)
      ;; add TO-BUFFER to list of buffers using regexp set SET-ID
      (auto-o-add-to-buffer-list set-id to-buffer)
      )))



(defun auto-overlay-start (set-id &optional buffer overlay-file no-regexp-check)
  "Activate the set of auto-overlay regexps identified by SET-ID
in BUFFER, or the current buffer if none is specified.

If optional argument OVERLAY-FILE is nil, try to load the
overlays from the default save file if it exists. If OVERLAY-FILE
is a string, it specifies the location of the file (if only a
directory is given, it will look for the default filename in that
directory). Anything else will cause the save file to be ignored,
and the buffer will be reparsed from scratch, as if the save file
did not exist.

If the overlays are being loaded from a save file, but optional
argument NO-REGEXP-CHECK is non-nil, the file of saved overlays
will be used, but no check will be made to ensure regexp
definitions are the same as when the overlays were saved."

  (save-current-buffer
    (when buffer (set-buffer buffer))
    ;; run initialisation hooks
    (run-hooks 'auto-overlay-load-hook)
    ;; add hook to run all the various functions scheduled be run after a
    ;; buffer modification
    (add-hook 'after-change-functions #'auto-o-run-after-change-functions
	      nil t)
    ;; add hook to schedule an update after a buffer modification
    (add-hook 'after-change-functions #'auto-o-schedule-update nil t)
    ;; add hook to simulate missing `delete-in-front-hooks' and
    ;; `delete-behind-hooks' overlay properties
    (add-hook 'after-change-functions
	      #'auto-o-schedule-delete-in-front-or-behind-suicide
	      nil t)

    (unless (auto-o-enabled-p set-id)
      ;; set enabled flag for regexp set, and make sure buffer is in buffer list
      ;; for the regexp set
      (auto-o-enable-set set-id)
      ;; try to load overlays from file
      (unless (and (or (null overlay-file) (stringp overlay-file))
		   (auto-overlay-load-overlays set-id nil overlay-file
					       no-regexp-check))
	;; if loading from file was unsuccessful, search for new auto overlays
	(auto-o-parse-buffer set-id)))
    ))



(defun auto-overlay-stop (set-id &optional buffer save-file leave-overlays)
  "Clear all auto-overlays in the set identified by SET-ID
from BUFFER, or the current buffer if none is specified.

If SAVE-FILE is non-nil and the buffer is associated with a file,
save the overlays to a file to speed up loading if the same set
of regexp definitions is enabled again. If SAVE-FILE is a string,
it specifies the location of the file to save to (if it only
specifies a directory, the default filename is used). Anything
else will cause the overlays to be saved to the default file name
in the current directory.

If LEAVE-OVERLAYS is non-nil, don't bother deleting the overlays
from the buffer \(this is generally a bad idea, unless the buffer
is about to be killed in which case it speeds things up a bit\)."

  (save-current-buffer
    (when buffer (set-buffer buffer))
    ;; disable overlay set
    (auto-o-disable-set set-id)

    ;; if SAVE-FILE is non-nil and buffer is associated with a file, save
    ;; overlays to file
    (when save-file
      (unless (stringp save-file) (setq save-file nil))
      (auto-overlay-save-overlays set-id nil save-file))

    ;; delete overlays unless told not to bother
    (unless leave-overlays
      (mapc #'delete-overlay
      	    (auto-overlays-in
      	     (point-min) (point-max) :all-overlays t :inactive t
	     (list (lambda (overlay match) (or overlay match))
		   '(auto-overlay auto-overlay-match))
	     `(eq set-id ,set-id))))

    ;; if there are no more active auto-overlay definitions...
    (unless (catch 'enabled
	      (dolist (set auto-overlay-regexps)
		(when (auto-o-enabled-p (car set))
		  (throw 'enabled t)))
	      nil)
      ;; run clear hooks
      (run-hooks 'auto-overlay-unload-hook)
      ;; reset variables
      (remove-hook 'after-change-functions #'auto-o-schedule-update t)
      (remove-hook 'after-change-functions
		   #'auto-o-run-after-change-functions t)
      (setq auto-o-pending-suicides nil
	    auto-o-pending-updates nil
	    auto-o-pending-post-suicide nil))))



(defun auto-overlay-save-overlays (set-id &optional buffer file)
  "Save overlays in set SET-ID in BUFFER to FILE.
Defaults to the current buffer.

If FILE is nil or a directory, and if the buffer is associated
with a file, the filename is constructed from the buffer's file
name and SET-ID. The directory is created if necessary. If the
buffer is not associated with a file and FILE doesn't specify a
filename, an error occurs.

The overlays can be loaded again later using
`auto-overlay-load-overlays'."

  (save-current-buffer
    (when buffer (set-buffer buffer))

    ;; construct filename
    (let ((path (or (and file (file-name-directory file)) ""))
	  (filename (or (and file (file-name-nondirectory file)) "")))
      ;; use default filename if none supplied
      (when (string= filename "")
	(if (buffer-file-name)
	    (setq filename (auto-o-save-filename set-id))
	  (error "Can't save overlays to default filename when buffer isn't\
 visiting a file")))
      ;; create directory if it doesn't exist
      (make-directory path t)
      ;; construct full path to file, since that's all we need from now on
      (setq file (concat path filename)))

    ;; create temporary buffer
    (let ((buff (generate-new-buffer " *auto-overlay-save*"))
	  overlay-list)
      ;; write md5 digests to first two lines
      (prin1 (md5 (current-buffer)) buff)
      (terpri buff)
      (prin1 (md5 (prin1-to-string (auto-o-get-set set-id))) buff)
      (terpri buff)

      ;; get sorted list of all match overlays in set SET-ID
      (setq overlay-list
	    (sort (auto-overlays-in (point-min) (point-max)
				    :all-overlays t
				    'auto-overlay-match
				    `(eq set-id ,set-id))
		  #'auto-overlay-<))

      ;; write overlay data to temporary buffer
      (mapc (lambda (o)
	      (prin1 (list (overlay-get o 'definition-id)
			   (overlay-get o 'regexp-id)
			   (overlay-start o)
			   (overlay-end o)
			   (marker-position (overlay-get o 'delim-start))
			   (marker-position (overlay-get o 'delim-end)))
		     buff)
	      (terpri buff))
	    overlay-list)

      ;; save the buffer and kill it
      (with-current-buffer buff (write-file file))
      (kill-buffer buff))
    ))



;;;###autoload
(defun auto-overlay-load-overlays (set-id &optional buffer
					  file no-regexp-check)
  "Load overlays for BUFFER from FILE.
Returns t if successful, nil otherwise.
Defaults to the current buffer.

If FILE is null, or is a string that only specifies a directory,
the filename is constructed from the buffer's file name and
SET-ID. If the buffer is not associated with a file and FILE
doesn't specify a full filename, an error occurs.

The FILE should be generated by `auto-overlay-save-overlays'. By
default, the buffer contents and regexp definitions for SET-ID
will be checked to make sure neither have changed since the
overlays were saved. If they don't match, the saved overlay data
will not be loaded, and the function will return nil.

If NO-REGEXP-CHECK is non-nil, the check for matching regexp
definitions will be skipped; the saved overlays will be loaded
even if different regexp definitions were active when the
overlays were saved."

  (save-current-buffer
    (when buffer (set-buffer buffer))

    ;; construct filename
    (let ((path     (or (and file (file-name-directory file))    ""))
	  (filename (or (and file (file-name-nondirectory file)) "")))
      ;; use default filename if none supplied
      ;; FIXME: should we throw error if buffer not associated with file?
      (when (string= filename "")
	(setq filename (auto-o-save-filename set-id)))
      ;; construct full path to file, since that's all we need from now on
      (setq file (concat path filename)))


    ;; return nil if file does not exist
    (when (file-exists-p file)
      (let ((buff (find-file-noselect file t))
	    md5-buff md5-regexp data o-match o-new lines
	    (i 0))

	;; read md5 digests from first two lines of FILE
	(with-current-buffer buff (goto-char (point-min)))
	(setq md5-buff (read buff))
	(setq md5-regexp (read buff))


	;; if saved buffer md5 sum doesn't match buffer contents, or if saved
	;; regexp md5 sum doesn't match regexp definitions and checking is not
	;; overridden, return nil
	(if (not (and (string= md5-buff (md5 (current-buffer)))
		      (or no-regexp-check
			  (string= md5-regexp
				   (md5 (prin1-to-string
					 (auto-o-get-set set-id)))))))
	    (progn (kill-buffer buff) nil)

	  ;; count number of overlays, for progress message
	  (with-current-buffer buff
	    (setq lines (count-lines (point) (point-max))))

	  ;; read overlay data from FILE until we reach the end
	  (message "Loading auto-overlays...(1 of %d)" lines)
	  (while (condition-case nil (setq data (read buff)) ('end-of-file))
	    ;; create a match overlay corresponding to the data
	    (setq o-match (auto-o-make-match
			   set-id (nth 0 data) (nth 1 data) (nth 2 data)
			   (nth 3 data) (nth 4 data) (nth 5 data)))
	    ;; call the appropriate parse function, unless match overlay is
	    ;; within a higher priority exclusive overlay
	    (unless (auto-o-within-exclusive-p
		     (overlay-get o-match 'delim-start)
		     (overlay-get o-match 'delim-end)
		     (cdr (assq 'priority (auto-o-props o-match))))
	      (setq o-new (auto-o-call-parse-function o-match))
	      (unless (listp o-new) (setq o-new (list o-new)))
	      ;; give any new overlays some basic properties
	      (mapc (lambda (o)
		      (overlay-put o 'auto-overlay t)
		      (overlay-put o 'set-id set-id)
		      (overlay-put o 'definition-id
				   (overlay-get o-match 'definition-id))
		      (overlay-put o 'regexp-id
				   (overlay-get o-match 'regexp-id)))
		    o-new)
	      ;; run match function if there is one
	      (auto-o-call-match-function o-match))
	    ;; display progress message
	    (setq i (1+ i))
	    (when (= 0 (mod i 10))
	      (message "Loading auto-overlays...(%d of %d)" i lines)))

	  (kill-buffer buff)
	  t)))))  ; return t to indicate successful loading)



;;;=============================================================
;;;               Update and change-hook functions

(defun auto-o-run-after-change-functions (beg end len)
  ;; Assigned to the `after-change-functions' hook.  Run all the various
  ;; functions that should run after a change to the buffer, in the correct
  ;; order.

  ;; ignore changes that aren't either insertions or deletions
  (when (and (not undo-in-progress)
	     (or (and (/= beg end) (=  len 0))    ; insertion
		 (and (=  beg end) (/= len 0))))  ; deletion
    ;; repeat until all the pending functions have been cleared (it may be
    ;; necessary to run multiple times since the pending functions may
    ;; themselves cause more functions to be added to the pending lists)
    (while (or auto-o-pending-pre-suicide auto-o-pending-suicides
	       auto-o-pending-post-suicide auto-o-pending-updates
	       auto-o-pending-post-update)
      ;; run pending pre-suicide functions
      (when auto-o-pending-pre-suicide
	(mapc (lambda (f) (apply (car f) (cdr f))) ;Just #'apply in Emacs≥24.3
	      auto-o-pending-pre-suicide)
	(setq auto-o-pending-pre-suicide nil))
      ;; run pending suicides
      (when auto-o-pending-suicides
	(mapc #'auto-o-suicide auto-o-pending-suicides)
	(setq auto-o-pending-suicides nil))
      ;; run pending post-suicide functions
      (when auto-o-pending-post-suicide
	(mapc (lambda (f) (apply (car f) (cdr f))) ;Just #'apply in Emacs≥24.3
	      auto-o-pending-post-suicide)
	(setq auto-o-pending-post-suicide nil))
      ;; run updates
      (when auto-o-pending-updates
	(mapc (lambda (l) (auto-overlay-update (car l) (cdr l)))
	      auto-o-pending-updates)
	(setq auto-o-pending-updates nil))
      ;; run pending post-update functions
      (when auto-o-pending-post-update
	(mapc (lambda (f) (apply (car f) (cdr f))) ;Just #'apply in Emacs≥24.3
	      auto-o-pending-post-update)
	(setq auto-o-pending-post-update nil))
      ))

  ;; ;; FIXME: horrible hack to delete all marker update entries in latest
  ;; ;;        `buffer-undo-list' change group, since undoing these can badly
  ;; ;;        mess up the overlays
  ;; (while (and (consp (car buffer-undo-list))
  ;; 	      (markerp (caar buffer-undo-list)))
  ;;   (setq buffer-undo-list (cdr buffer-undo-list)))
  ;; (let ((p buffer-undo-list))
  ;;   (while (cadr p)
  ;;     (if (and (consp (cadr p)) (markerp (car (cadr p))))
  ;; 	  (setcdr p (cddr p))
  ;; 	(setq p (cdr p)))))
  )



(defun auto-o-schedule-update (start &optional end _unused _set-id)
  ;; Schedule `auto-overlay-update' of lines between positions START and END
  ;; (including lines containing START and END). If END is not supplied,
  ;; schedule update for just line containing START. The update will be run by
  ;; `auto-o-run-after-change-functions' after buffer modification is
  ;; complete. This function is assigned to `after-change-functions'.
  ;; FIXME: Optionally allow argument to restrict to SET-ID?
  (save-restriction
    (widen)   ; need to widen, since goto-line goes to absolute line
    (setq start (line-number-at-pos start))
    (setq end (if end (line-number-at-pos end) start))

    (let ((pending auto-o-pending-updates))
      (cond
       ;; if pending list is empty, just add new entry to the list
       ((null pending)
	(setq auto-o-pending-updates (list (cons start end))))

       ;; if start of the new entry is before start of the first entry in
       ;; pending list, add new entry to front of the list
       ((<= start (caar pending))
	(setq auto-o-pending-updates (nconc (list (cons start end)) pending))
	(setq pending auto-o-pending-updates))

       ;; otherwise...
       (t
	;; search for entry in pending list that new one should come after
	;; Note: we do an O(n) linear search here, as opposed to the O(log n)
	;; we would get were we to store the entries in a binary tree. But the
	;; pending list is unlikely to ever be all that long, so the
	;; optimisation almost certainly isn't worth the effort.
	(catch 'found
	  (while (cdr pending)
	    (when (<= start (car (cadr pending))) (throw 'found t))
	    (setq pending (cdr pending))))
	;; if start of new entry is before end of entry it should come after,
	;; merge it with that entry
	(if (<= start (1+ (cdar pending)))
	    (when (> end (cdar pending)) (setcdr (car pending) end))
	  ;; otherwise, insert new entry after it
	  (setcdr pending (nconc (list (cons start end)) (cdr pending)))
	  (setq pending (cdr pending)))
	))

      ;; merge new entry with successive entries until end of merged entry is
      ;; before start of next entry (see above note about O(n) vs. O(log n))
      (while (and (cdr pending)
		  (>= (1+ (cdar pending)) (car (cadr pending))))
	(setcdr (car pending) (max (cdar pending) (cdr (cadr pending))))
	(setcdr pending (cddr pending)))
      )))



(defun auto-o-schedule-delete-in-front-or-behind-suicide (start end len)
  ;; Schedule `auto-o-suicide' for any overlay that has had characters deleted
  ;; in front or behind it, to simulate missing `delete-in-front-hooks' and
  ;; `delete-behind-hooks' overlay properties
  (unless (= len 0)
    (dolist (o (auto-overlays-at-point nil :all-overlays t
				       '(identity auto-overlay-match)))
      (when (or (= (overlay-end o) start) (= (overlay-start o) end))
	(auto-o-adjoin o auto-o-pending-suicides)))))



(defun auto-o-schedule-suicide (o-self &optional modified &rest _unused)
  ;; Schedule `auto-o-suicide' to run after buffer modification is
  ;; complete. It will be run by `auto-o-run-after-change-functions'. Assigned
  ;; to overlay modification and insert in-front/behind hooks.
  (unless modified (auto-o-adjoin o-self auto-o-pending-suicides)))



(defun auto-overlay-update (&optional start-line end-line set-ids definition-ids)
  ;; Parse lines from line number START to line number END. If only START is
  ;; supplied, just parse that line. If neither are supplied, parse line
  ;; containing the point. If one or both of SET-ID and DEFINITION-ID are
  ;; specified, only look for matches for specified regexp definitions.

  ;; FIXME: Switch to using buffer positions, not line numbers!

  ;; sort our arguments
  (cond
   ((null set-ids) (setq set-ids (mapcar #'car auto-overlay-regexps)))
   ((atom set-ids) (setq set-ids (list set-ids))))
  (unless (listp definition-ids) (setq definition-ids (list definition-ids)))

  (save-restriction
    (widen)
    (let (definition-id regexp-id regexp group priority
	   o-match o-overlap o-new beg end)
      (unless start-line (setq start-line (line-number-at-pos)))
      (save-excursion
	(save-match-data
	  ;; (goto-line start-line) without messing around with mark and messages
	  ;; Note: this is a bug in simple.el; there clearly can be a need for
	  ;;       non-interactive calls to goto-line from Lisp code, and
	  ;;       there's no warning about doing this. Yet goto-line *always*
	  ;;       calls push-mark, which usually *shouldn't* be invoked by
	  ;;       Lisp programs, as its docstring warns.
	  (goto-char 1)
	  (if (eq selective-display t)
	      (re-search-forward "[\n\C-m]" nil 'end (1- start-line))
	    (forward-line (1- start-line)))

	  (dotimes (_ (if end-line (1+ (- end-line start-line)) 1))

	    ;; check each enabled set of overlays, or just the specified set
	    (dolist (set-id set-ids)
	      (when (auto-o-enabled-p set-id)

		;; check each definition in regexp set, or just the specified one
		(dolist (def (auto-o-get-set set-id))
		  (setq definition-id (pop def))
		  (when (or (null definition-ids)
			    (memq definition-id definition-ids))
		    (pop def)

		    ;; check all regexps for current definition
		    (dotimes (rank (length def))
		      (setq regexp-id (car (nth rank def)))

		      ;; extract regexp properties from current entry
		      (setq regexp (auto-o-regexp-regexp
				    set-id definition-id regexp-id)
			    group  (auto-o-regexp-group
				    set-id definition-id regexp-id)
			    priority (cdr (assq 'priority
						(auto-o-regexp-props
						 set-id definition-id regexp-id))))


		      ;; look for matches in current line, ensuring case *is*
		      ;; significant
		      (forward-line 0)
		      (while (let ((case-fold-search nil))
			       (re-search-forward regexp (line-end-position) t))
			;; sanity check regexp definition against match
			(unless (and (setq beg (match-beginning group))
			             (setq end (match-end group)))
			  (error "Match for regexp \"%s\" has no group %d"
				 regexp group))

			(cond
			 ;; ignore match if it already has a match overlay
			 ((setq o-match
				(auto-o-matched-p (match-beginning 0) (match-end 0)
						  set-id definition-id regexp-id
						  priority))
			  (move-marker (overlay-get o-match 'delim-start) beg)
			  (move-marker (overlay-get o-match 'delim-end) end))


			 ;; if existing match overlay corresponding to same entry
			 ;; and edge but different subentry overlaps new match...
			 ((setq o-overlap
				(auto-o-overlapping-match
				 (match-beginning group) (match-end group)
				 set-id definition-id regexp-id
				 (auto-o-regexp-edge set-id definition-id regexp-id)))
			  ;; if new match takes precedence, replace existing one
			  ;; with new one, otherwise ignore new match
			  (when (or (auto-o-priority-<
				     (overlay-get o-overlap 'priority)
				     priority)
				    (< rank (auto-o-rank o-overlap)))
			    (delete-overlay o-overlap)
			    (setq o-match (auto-o-make-match
					   set-id definition-id regexp-id
					   (match-beginning 0) (match-end 0)
					   beg end))
			    (when (overlay-get o-overlap 'parent)
			      (auto-o-match-overlay
			       (overlay-get o-overlap 'parent)
			       o-match))
			    ;; run match function if there is one
			    (auto-o-call-match-function o-match)))

			 ;; if match is within a higher priority exclusive
			 ;; overlay, create match overlay but don't parse it
			 ((auto-o-within-exclusive-p (match-beginning group)
						     (match-end group)
						     priority)
			  (auto-o-make-match set-id definition-id regexp-id
					     (match-beginning 0) (match-end 0)
					     beg end))

			 ;; if we're going to parse the new match...
			 (t
			  ;; create a match overlay for it
			  (setq o-match (auto-o-make-match
					 set-id definition-id regexp-id
					 (match-beginning 0) (match-end 0)
					 beg end))
			  ;; call the appropriate parse function
			  (setq o-new (auto-o-call-parse-function
				       o-match))
			  (unless (listp o-new) (setq o-new (list o-new)))
			  ;; give any new overlays some basic properties
			  (mapc (lambda (o)
				  (overlay-put o 'auto-overlay t)
				  (overlay-put o 'set-id set-id)
				  (overlay-put o 'definition-id definition-id)
				  (overlay-put o 'regexp-id regexp-id))
				o-new)
			  ;; run match function if there is one
			  (auto-o-call-match-function o-match)))


			;; go to character one beyond the start of the match, to
			;; make sure we don't miss the next match (if we find the
			;; same one again, it will just be ignored)
			(goto-char (+ (match-beginning 0) 1))))))
		(forward-line 1))
	      )))
	))))


(defun auto-overlay-trigger-update (&optional point)
  "Trigger auto-overlay update at POINT, defaulting to the point."
  ;; FIXME: Inserting and deleting to trigger change hooks is hacky. But some
  ;;        updates are triggered by overlay change hooks (e.g. self overlay
  ;;        cascades), and auto-overlays doesn't know about these. Would need
  ;;        to refactor how class-specific updates are performed.
  (unless point (setq point (point)))
  (let ((inhibit-read-only t))
    (with-silent-modifications
      (save-excursion
	(goto-char point)
	(insert " ")
	(delete-char -1)))))


(defun auto-o-parse-buffer (&optional set-id definition-id buffer)
  ;; Parse whole BUFFER for auto-overlays, restricted to SET-ID and
  ;; DEFINITION-ID if specified.
  (save-excursion
    (with-current-buffer (or buffer (current-buffer))
      (let ((lines (count-lines (point-min) (point-max))))
	(goto-char (point-min))
	(message "Scanning for auto-overlays...(line 1 of %d)"
		 lines)
	(dotimes (i lines)
	  (when (= 9 (mod i 10))
	    (message
	     "Scanning for auto-overlays...(line %d of %d)"
	     (+ i 1) lines))
	  (auto-overlay-update nil nil set-id definition-id)
	  (forward-line 1))
	(message "Scanning for auto-overlays...done")))))


(defun auto-o-suicide (o-self &optional force)
  ;; This function is assigned to all match overlay modification hooks, and
  ;; calls the appropriate suicide function for match overlay O-SELF.
  ;; If FORCE is non-nil, O-SELF is deleted irrespective of whether its
  ;; overlay still matches.

  ;; have to widen temporarily
  (save-restriction
    (widen)
;;     ;; this condition is here to avoid a weird Emacs bug(?) where the
;;     ;; modification-hooks seem to be called occasionally for overlays that
;;     ;; have already been deleted
;;     (when (overlay-buffer o-self)
      ;; if match overlay no longer matches the text it covers...
      (unless (and (not force)
		   (overlay-buffer o-self)
		   (save-excursion
		     (goto-char (overlay-start o-self))
		     (looking-at (auto-o-regexp o-self)))
		   (= (match-end 0) (overlay-end o-self)))

	;; if we have a parent overlay...
	(let ((o-parent (overlay-get o-self 'parent))
	      o-other)
	  (when o-parent
	    ;; if our regexp class is a compound class...
	    (when (auto-o-complex-class-p o-self)
	      (setq o-other
		    (overlay-get o-parent (if (eq (auto-o-edge o-self) 'start)
					      'start 'end)))
	      ;; if parent's properties have been set by us, remove them
	      (when (or (null o-other)
			(>= (auto-o-rank o-self)
			    (auto-o-rank o-other)))
		(dolist (p (auto-o-props o-self))
		  (overlay-put o-parent (car p) nil))))
	    ;; call appropriate suicide function
	    (auto-o-call-suicide-function o-self)))

	;; schedule an update (necessary since if match regexp contains
	;; "context", we may be comitting suicide only for the match overlay
	;; to be recreated in a slightly different place)
	(auto-o-schedule-update (overlay-start o-self))
	;; delete ourselves
	(delete-overlay o-self));)
    ))




(defun auto-o-update-exclusive (set-id beg end old-priority new-priority)
  ;; If priority has increased, delete all overlays between BEG end END that
  ;; have priority lower than NEW-PRIORITY. If priority has decreased, re-parse
  ;; all matches with priority lower than OLD-PRIORITY.

  (let (overlay-list)
    (cond
     ;; if priority has increased...
     ((and new-priority
	   (or (null old-priority) (> new-priority old-priority)))
      ;; find overlays entirely within BEG and END that are both start and end
      ;; matched and have priority lower than NEW-PRIORITY
      (setq overlay-list
	    (auto-overlays-in
	     beg end :within t
	     `(eq set-id ,set-id)
	     '(identity start)
	     (list (lambda (definition-id start end)
		     (or (null (auto-o-definition-complex-class-p
				set-id definition-id))
			 (and start end)))
		   '(definition-id start end))
	     (list (lambda (pri new) (or (null pri) (< pri new)))
		   'priority new-priority)))
      ;; mark overlays in list as inactive (more efficient than calling
      ;; suicide functions or deleting the overlays, and leaves them intact in
      ;; case the exclusivity of the region is later reduced - see below)
      (dolist (o overlay-list) (overlay-put o 'inactive t))

      ;; find match overlays between BEG and END that have priority lower then
      ;; NEW-PRIORITY but still have an active parent overlay
      (setq overlay-list
	    (auto-overlays-in
	     beg end :all-overlays t
	     '(identity auto-overlay-match)
	     `(eq set-id ,set-id)
	     ;; note: parentless overlays are possible if a suicide is in
	     ;; progress, so need to check overlay has a parent first
	     '(identity parent)
	     (list (lambda (parent) (not (overlay-get parent 'inactive)))
		   'parent)
	     (list (lambda (set-id definition-id regexp-id new-pri)
		     (let ((pri (cdr (assq
				      'priority
				      (auto-o-regexp-props
				       set-id definition-id regexp-id)))))
		       (or (null pri) (< pri new-pri))))
		   '(set-id definition-id regexp-id)
		   (list new-priority))))
      ;; call appropriate suicide function for each match overlay in list
      (dolist (o overlay-list) (auto-o-call-suicide-function o)))


     ;; if priority has decreased...
     ((and old-priority
	   (or (null new-priority) (< new-priority old-priority)))
      ;; find inactive overlays entirely within BEG and END that have priority
      ;; higher or equal to NEW-PRIORITY
      (setq overlay-list
	    (auto-overlays-in
	     beg end :within t :inactive t
	     `(eq set-id ,set-id)
	     '(identity inactive)
	     (list (lambda (pri new) (or (null new) (>= pri new)))
		   'priority new-priority)))
      ;; mark overlays in list as active again
      (dolist (o overlay-list) (overlay-put o 'inactive nil))

      ;; find match overlays between BEG and END that have priority higher or
      ;; equal to NEW-PRIORITY but no parent overlay
      (setq overlay-list
	    (auto-overlays-in
	     beg end :all-overlays t
	     '(identity auto-overlay-match)
	     `(eq set-id ,set-id)
	     '(null parent)
	     (list (lambda (set-id definition-id regexp-id new-pri)
		     (let ((pri (cdr (assq
				      'priority
				      (auto-o-regexp-props
				       set-id definition-id regexp-id)))))
		       (or (null new-pri) (>= pri new-pri))))
		   '(set-id definition-id regexp-id)
		   (list new-priority))))
      ;; call appropriate parse function for each match overlay in list
      (dolist (o-match overlay-list)
	(when (not (auto-o-within-exclusive-p o-match))
	  (let ((o-new (auto-o-call-parse-function o-match)))
	    ;; give any new overlays the basic properties and add them to
	    ;; `auto-overlay-list'
	    (unless (listp o-new) (setq o-new (list o-new)))
	    (mapc (lambda (o)
		    (overlay-put o 'auto-overlay t)
		    (overlay-put o 'set-id set-id)
		    (overlay-put o 'definition-id
				 (overlay-get o-match 'definition-id))
		    (overlay-put o 'regexp-id
				 (overlay-get o-match 'regexp-id)))
		  o-new)))))
     )))




(defun auto-o-make-match (set-id definition-id regexp-id start end
			      &optional delim-start delim-end)
  ;; Create a new match overlay and give it the appropriate properties.
  (let ((o-match (make-overlay start end nil 'front-advance nil)))
    (overlay-put o-match 'auto-overlay-match t)
    (overlay-put o-match 'set-id set-id)
    (overlay-put o-match 'definition-id definition-id)
    (overlay-put o-match 'regexp-id regexp-id)
    (overlay-put o-match 'delim-start
		 (set-marker (make-marker)
			     (if delim-start delim-start start)))
    (set-marker-insertion-type (overlay-get o-match 'delim-start) nil)
    (overlay-put o-match 'delim-end
		 (set-marker (make-marker)
			     (if delim-end delim-end end)))
    (set-marker-insertion-type (overlay-get o-match 'delim-end) t)
    (set-marker-insertion-type (overlay-get o-match 'delim-start) t)
    (set-marker-insertion-type (overlay-get o-match 'delim-end) nil)
    (overlay-put o-match 'modification-hooks '(auto-o-schedule-suicide))
    (overlay-put o-match 'insert-in-front-hooks '(auto-o-schedule-suicide))
    (overlay-put o-match 'insert-behind-hooks '(auto-o-schedule-suicide))
    ;; return the new match overlay
    o-match))




(defun auto-o-match-overlay (overlay start &optional end
				     no-props no-parse protect-match)
  "Match start and end of OVERLAY with START and END match overlays.
If START or END are numbers or markers, move that edge to the
buffer location specified by the number or marker and make it
unmatched.  If START or END are non-nil but neither of the above,
make that edge unmatched.  If START or END are null, don't change
that edge. However, if END is null, and START is an `end' overlay,
match end of OVERLAY rather than start.

If NO-PARSE is non-nil, block re-parsing due to exclusive overlay
changes. If NO-PROPS is non-nil, block updating of overlay's
properties. If PROTECT-MATCH is non-nil, don't modify any match
overlays associated with OVERLAY (i.e. don't modify their `parent'
properties)."

  (let ((old-start (overlay-start overlay))
	(old-end (overlay-end overlay))
	(old-o-start (overlay-get overlay 'start))
	(old-o-end (overlay-get overlay 'end))
	(old-exclusive (overlay-get overlay 'exclusive))
	(old-priority (overlay-get overlay 'priority)))

    ;; if END is null, we're not unmatching, and START is an end overlay,
    ;; match end of overlay instead of start (Note: assumes we're matching an
    ;; overlay class with 'start and 'end regexps)
    (when (and (null end) (overlayp start) (eq (auto-o-edge start) 'end))
      (setq end start)
      (setq start nil))


    ;; move overlay to new location
    (move-overlay overlay
		  (cond
		   ((overlayp start) (overlay-get start 'delim-end))
		   ((number-or-marker-p start) start)
		   (start (point-min))
		   (t (overlay-start overlay)))
		  (cond
		   ((overlayp end) (overlay-get end 'delim-start))
		   ((number-or-marker-p end) end)
		   (end (point-max))
		   (t (overlay-end overlay))))

    ;; if changing start match...
    (when start
      ;; sort out parent property of old start match
      (when (and old-o-start (not (eq old-o-start end)) (null protect-match))
	(overlay-put old-o-start 'parent nil))
      ;; if unmatching start, set start property to nil
      (if (not (overlayp start))
	  (overlay-put overlay 'start nil)
	;; if matching start, set start property to new start match
	(overlay-put overlay 'start start)
	(overlay-put start 'parent overlay)))

    ;; if changing end match...
    (when end
      ;; sort out parent property of old end match
      (when (and old-o-end (not (eq old-o-end start)) (null protect-match))
	(overlay-put old-o-end 'parent nil))
      ;; if unmatching end, set end property to nil
      (if (not (overlayp end))
	  (overlay-put overlay 'end nil)
	;; if matching end, set end property to new end match
	(overlay-put overlay 'end end)
	(overlay-put end 'parent overlay)))


    ;; unless it's blocked, update properties if new match takes precedence
    ;; (Note: this sometimes sets the overlay's properties to the ones it
    ;; already had, but it hardly seems worth checking for that)
    (unless no-props
      ;; when start was previously matched and is being changed, remove
      ;; properties due to old start match
      ;; Note: no need to check if properties were really set by start match,
      ;; since if not they will be recreated below
      (when (and start old-o-start)
	(dolist (p (auto-o-props old-o-start))
	  (overlay-put overlay (car p) nil)))
      ;; when end was previously matched and is being changed, remove
      ;; properties due to old end match (see note above)
      (when (and end old-o-end)
	(dolist (p (auto-o-props old-o-end))
	  (overlay-put overlay (car p) nil)))
      ;; sort out properties due to new matches
      (let (props)
	(cond
	 ;; if start has been unmatched, use properties of end match
	 ((not (auto-o-start-matched-p overlay))
	  (setq props (auto-o-props (overlay-get overlay 'end))))
	 ;; if end has been unmatched, use properties of start match
	 ((not (auto-o-end-matched-p overlay))
	  (setq props (auto-o-props (overlay-get overlay 'start))))
	 (t  ;; otherwise, use properties of whichever match takes precedence
	  (let ((o-start (overlay-get overlay 'start))
		(o-end (overlay-get overlay 'end)))
	    (setq props (auto-o-props (if (<= (auto-o-rank o-start)
		                              (auto-o-rank o-end))
		                          o-start
		                        o-end))))))
	;; bundle properties inside a list if not already, then update them
	(when (symbolp (car props)) (setq props (list props)))
	(dolist (p props) (overlay-put overlay (car p) (cdr p)))))


    ;; unless it's blocked or overlay is inactive, check if anything needs
    ;; reparsing due to exclusive overlay changes
    (unless (or no-parse (overlay-get overlay 'inactive))
      (let ((set-id (overlay-get overlay 'set-id))
	    (start (overlay-start overlay))
	    (end (overlay-end overlay))
	    (exclusive (overlay-get overlay 'exclusive))
	    (priority (overlay-get overlay 'priority)))
	(cond

	;; if overlay wasn't and still isn't exclusive, do nothing
	 ((and (null exclusive) (null old-exclusive)))

	 ;; if overlay has become exclusive, delete lower priority overlays
	 ;; within it
	 ((and (null old-exclusive) exclusive)
	  (auto-o-update-exclusive set-id start end nil priority))

	 ;; if overlay was exclusive but no longer is, re-parse region it
	 ;; used to cover
	 ((and old-exclusive (null exclusive))
	  (auto-o-update-exclusive set-id old-start old-end old-priority nil))

	 ;; if overlay was and is exclusive, and has been moved to a
	 ;; completely different location re-parse old location and delete
	 ;; lower priority overlays within new location
	 ((or (< end old-start) (> start old-start))
	  (auto-o-update-exclusive set-id start end old-priority nil)
	  (auto-o-update-exclusive set-id start end nil priority))

	 ;; if overlay was and is exclusive, and overlaps its old location...
	 (t
	  ;; if priority has changed, re-parse/delete in overlap region
	  (when (/= old-priority priority)
	    (auto-o-update-exclusive set-id
				     (max start old-start) (min end old-end)
				     old-priority priority))
	  (cond
	   ;; if overlay was exclusive and start has shrunk, re-parse
	   ;; uncovered region
	   ((and (> start old-start) old-exclusive)
	    (auto-o-update-exclusive set-id old-start start old-priority nil))
	   ;; if overlay is exclusive and has grown, delete lower priority
	   ;; overlays in newly covered region
	   ((and (< start old-start) exclusive)
	    (auto-o-update-exclusive set-id start old-start nil priority)))
	  (cond
	   ;; if overlay was exclusive and end has shrunk, re-parse
	   ((and (< end old-end) old-exclusive)
	    (auto-o-update-exclusive set-id end old-end old-priority nil))
	    ;; if overlay is exclusive and has grown, delete lower priority
	   ((and (> end old-end) exclusive)
	    (auto-o-update-exclusive set-id old-end end nil priority))))
	 )))
    ))




(defun auto-o-delete-overlay (overlay &optional no-parse protect-match)
  "Delete OVERLAY from buffer.

If PROTECT-MATCH is non-nil, don't modify any match overlays
associated with OVERLAY (i.e. leave their `parent' properties
alone). If NO-PARSE is non-nil, block re-parsing due to exclusive
overlay changes."

  (let ((start (overlay-start overlay))
	(end (overlay-end overlay))
	o-match)
    ;; delete overlay from buffer and `auto-overlay-list'
    (delete-overlay overlay)
    (unless (setq o-match (overlay-get overlay 'start))
      (setq o-match (overlay-get overlay 'end)))
;;    (auto-o-delete-from-overlay-list overlay)

    ;; unless blocked, if overlay's exclusive flag was set, re-parse region it
    ;; covered
    (when (and (null no-parse) (overlay-get overlay 'exclusive))
      (auto-o-update-exclusive (overlay-get overlay 'set-id) start end
			       (overlay-get overlay 'priority) nil))

    ;; Note: it's vital that the match overlays' parent properties are only
    ;; set to nil *after* `auto-update-exclusive' is run: if the overlay
    ;; overlapped one of its match overlays, the newly parentless match
    ;; overlay would be re-parsed by `auto-update-exclusive', which would
    ;; re-create the parent overlay that's just been deleted!

    ;; unmatch match overlays
    (unless protect-match
      (when (setq o-match (overlay-get overlay 'start))
	(overlay-put o-match 'parent nil))
      (when (setq o-match (overlay-get overlay 'end))
	(overlay-put o-match 'parent nil)))
    ))




(defun auto-o-matched-p (beg end set-id definition-id regexp-id &optional priority)
  ;; Determine if characters between BEG end END are already matched by a
  ;; match overlay corresponding to SET-ID, DEFINITION-ID and REGEXP-ID, or to
  ;; a higher-priority match-exclusive match overlay.
  (let (o-match)
    (catch 'match
      (mapc (lambda (o)
	      (when (and (or (and (eq (overlay-get o 'definition-id) definition-id)
				  (eq (overlay-get o 'regexp-id) regexp-id))
			     (and (auto-o-match-exclusive o)
				  (not (auto-o-priority-<
					(overlay-get o 'priority) priority))))
			 (= (overlay-start o) beg)
			 (= (overlay-end o) end))
		(setq o-match o)
		(throw 'match t)))
	    (auto-overlays-in beg end :all-overlays t
			      '(identity auto-overlay-match)
			      `(eq set-id ,set-id))))
    o-match))




(defun auto-o-within-exclusive-p (match &optional end priority)
  ;; If MATCH is an overlay, determine if it is within a higher priority
  ;; exclusive overlay. If MATCH is a number or marker, determine whether
  ;; region between MATCH and END is within an exclusive overlay with higher
  ;; priority than PRIORITY.

  (when (null end)
    (setq end (overlay-get match 'delim-end))
    (setq priority (overlay-get match 'priority))
    (setq match (overlay-get match 'delim-start)))

  ;; look for higher priority exclusive overlays
  (auto-overlays-in
   match end
   '(identity exclusive)
   (list (lambda (p q) (and p (or (null q) (> p q))))
	 'priority priority)))




(defun auto-o-overlapping-match (beg end set-id definition-id regexp-id edge)
  ;; Returns any match overlay corresponding to same SET-ID, DEFINITION-ID and
  ;; EDGE but different REGEXP-ID whose delimiter overlaps region from BEG to
  ;; END. (Only returns first one it finds; which is returned if more than one
  ;; exists is undefined.)
  (catch 'match
    (mapc (lambda (o)
	    (when (and (eq (overlay-get o 'definition-id) definition-id)
		       (not (eq (overlay-get o 'regexp-id) regexp-id))
		       (eq (auto-o-edge o) edge)
		       ;; check delimiter (not just o) overlaps BEG to END
		       (< (overlay-get o 'delim-start) end)
		       (> (overlay-get o 'delim-end) beg))
	      (throw 'match o)))
	  (auto-overlays-in beg end :all-overlays t
			    '(identity auto-overlay-match)
			    `(eq set-id ,set-id)))
    nil))



(provide 'auto-overlays)

(require 'auto-overlay-word)
(require 'auto-overlay-line)
(require 'auto-overlay-self)
(require 'auto-overlay-nested)
(require 'auto-overlay-flat)


;;; auto-overlays.el ends here
