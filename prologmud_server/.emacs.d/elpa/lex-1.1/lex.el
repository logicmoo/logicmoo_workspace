;;; lex.el --- Lexical analyser construction

;; Copyright (C) 2008,2013  Free Software Foundation, Inc.

;; Author: Stefan Monnier <monnier@iro.umontreal.ca>
;; Keywords:

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

;; Format of regexps is the same as used for `rx' and `sregex'.
;; Additions:
;; - (ere RE) specify regexps using the ERE syntax.
;; - (inter REs...) (aka `&') make a regexp that only matches
;;   if all its branches match.  E.g. (inter (ere ".*a.*") (ere ".*b.*"))
;;   match any string that contain both an "a" and a "b", in any order.
;; - (case-fold REs...) and (case-sensitive REs...) make a regexp that
;;   is case sensitive or not, regardless of case-fold-search.

;; Input format of lexers:
;;
;; ALIST of the form ((RE . VAL) ...)

;; Format of compiled DFA lexers:
;;
;; nil                     ; The trivial lexer that fails
;; (CHAR . LEXER)
;; (table . CHAR-TABLE)
;; (stop VAL . LEXER)      ; Match the empty string at point or LEXER.
;; (check (PREDICATE . ARG) SUCCESS-LEXER . FAILURE-LEXER)

;; Intermediate NFA nodes may additionally look like:
;; (or LEXERs...)
;; (orelse LEXERs...)
;; (and LEXERs...)
;; (join CONT . EXIT)
;; Note: we call those things "NFA"s but they're not really NFAs.

;;; Bugs:

;; - `inter' doesn't work right.  Matching `join' to the corresponding `and'
;;   is done incorrectly in some cases.
;; - since `negate' uses intersections, it doesn't work right either.
;; - "(\<)*" leads to a DFA that gets stuck in a cycle.

;;; Todo:

;; - dfa "no-fail" simplifier
;; - dfa minimization
;; - dfa compaction (different representation)
;; - submatches
;; - backrefs?
;; - search rather than just match
;; - extensions:
;;   - repeated submatches
;;   - negation
;;   - lookbehind and lookahead
;;   - match(&search?) backward
;;   - agrep

;;; Notes



;; Search
;; ------

;; To turn a match into a search, the basic idea is to use ".*RE" to get
;; a search-DFA as opposed to the match-DFA generated from "RE".

;; Search in Plan9's regexp library is done as follows: match ".*RE" until
;; reaching the first match and then continue with only "RE".  The first
;; ".*RE" match corresponds to a search success for the leftmost shortest
;; match.  If we want the longest match, we need to continue.  But if we
;; continue with ".*RE" then we have no idea when to stop, so we should only
;; continue with "RE".
;; Downside: we may still match things after the "leftmost longest" match,
;; but hopefully will stop soon after.  I.e. we may look at chars past the
;; end of the leftmost longest match, but hopefully not too many.

;; Alternatives:
;; - Like emacs/src/regexp.c, we can just start a match at every buffer
;;   position.  Advantage: no need for submatch info in order to find
;;   (match-beginning 0), no need for a separate search-DFA.
;;   Downsize: O(N^2) rather than O(N).  But it's no worse than what we live
;;   with for decades in src/regexp.c.
;;
;; - After the shortest-search, stop the search and do a longest-match
;;   starting at position (match-beginning 0).  The good thing is that we
;;   will not look at any char further than needed.  Also we don't need to
;;   figure out how to switch from ".*RE" to "RE" in the middle of the search.
;;   The downside is that we end up looking twice at the chars common to the
;;   shortest and longest matches.  Also this doesn't work: the shortest
;;   match may not be the leftmost match, so we can't just start the match
;;   at (match-beginning 0).
;;
;; - Generate a specialized search&match-DFA which encodes the job done by
;;   Plan9's regexp library.  I.e. do a specialized merge on
;;   (or LEXER (anything . LEXER)) where whenever we get a `stop' we don't
;;   merge any more.  After matching such a lexer, we still have to figure
;;   which of the matches we had is the leftmost longest match, of course.
;;   Actually, it's not that easy: the tail of a `stop' in the match-DFA can
;;   only match things whose (match-beginning 0) may be the same as the one
;;   of the `stop', whereas we also want to accept longer matches that start
;;   before (match-beginning 0).  So we want to keep merging on the tail of
;;   `stop' nodes, but only "partially" (whatever that means).

;; - Better yet, do what TRE does: after the shortest-search, use the
;;   submatch data to figure out the NFA states (corresponding to the
;;   current search-DFA state) which are only reachable from later starting
;;   positions than (match-beginning 0), remove them and figure out from
;;   that the match-DFA state to which to switch.  Problem is: there might
;;   not be any such state in the match-DFA.
;;
;; - In the end I do a mix of the last 2: .*?RE
;;   This uses the `orelse' merge operator, which contrary to `or' only
;;   matches the righthand side when the lefthand side fails to match.
;;   It turns out to be fairly simple to implement, and is optimal.
;;
;; Lookahead
;; ---------

;; I suspect that the (?=<RE>) lookahead can be encoded using something like
;; `andalso'.  Of course, it can also trivially be encoded as a predicate,
;; but then we get an O(N^2) complexity.

;; Merging operators.
;; ------------------

;; The NFA merging operators (or, and, orelse) seem to work fine on their own,
;; but I'm not convinced they always DTRT when combined.  It's not even
;; clear that the NFA->DFA conversion terminates in all such cases.

;; Intersection
;; ------------

;; Implementing the `inter' regexp operator turns out to be more difficult
;; than it seemed.  The problem is basically in the `join'.  Each `and' has
;; to have its own matching `join', but preserving this invariant is
;; tricky.  Among other things, we cannot flatten nested `and's like we do
;; for `or's and `orelse's.

;; Submatch info
;; -------------

;; Keeping track of submatch info with a DFA is tricky business and can slow
;; down the matcher or make it use algorithmically more memory
;; (e.g. O(textsize)).  Here are some approaches:

;; - Reproduce what an NFA matcher would do: when compiling the DFA, keep
;;   track of the NFA nodes corresponding to each DFA node, and for every
;;   transition, check the mapping between "incoming NFA nodes" and
;;   "outgoing NFA nodes" to maintain the list of submatch-info (one element
;;   per NFA node).

;; - Keep a log of the states traversed during matching, so at the end it
;;   can be used to reproduce the parse tree or submatch info, based on
;;   auxiliary tables constructed during the DFA construction.

;; - Some submatch info can be maintained cheaply: basically a submatch
;;   position can be represented by a single global variable in the case
;;   where we have the following property: every ε transition in the NFA
;;   which corresponds to this submatch point has the following property:
;;   no other ε transition for this same submatch can be traversed between
;;   the text position where this transition is traversed and the position
;;   where the target NFA subgraph fails to match.

;;

;;; Code:

(eval-when-compile (require 'cl-lib))

(defun copy-char-table (ct1)
  (let* ((subtype (char-table-subtype ct1))
         (ct2 (make-char-table subtype)))
    (map-char-table (lambda (c v) (set-char-table-range ct2 c v)) ct1)
    (dotimes (i (or (get subtype 'char-table-extra-slots) 0))
      (set-char-table-extra-slot ct2 i (char-table-extra-slot ct1 i)))
    ct2))

(defun lex--char-table->alist (ct)
  (let ((res ()))
    (map-char-table (lambda (k v)
                      (push (cons (if (consp k)
                                      ;; If k is a cons cell, we have to
                                      ;; copy it because map-char-table
                                      ;; reuses it.
                                      (cons (car k) (cdr k))
                                    ;; Otherwise, create a trivial cons-cell
                                    ;; so we have fewer cases to handle.
                                    (cons k k))
                                  v)
                            res))
                    ct)
    res))

(defun lex--merge-into (op al1 al2 ct)
  (cl-assert (memq op '(and or orelse)))
  ;; We assume that map-char-table calls its function with increasing
  ;; `key' arguments.
  (while (and al1 al2)
    (let ((k1 (caar al1)) (k2 (caar al2)))
      (cond
       ;; Perfect overlap.
       ((equal k1 k2)
        (set-char-table-range ct k1
                              (lex--merge op (cdr (pop al1)) (cdr (pop al2)))))
       ;; k1 strictly greater than k2.
       ((and (consp k1) (consp k2) (> (car k1) (cdr k2)))
        (let ((v (cdr (pop al1))))
          (if (not (eq op 'and)) (set-char-table-range ct k1 v))))
       ;; k2 strictly greater than k1.
       ((and (consp k1) (consp k2) (> (car k2) (cdr k1)))
        (let ((v (cdr (pop al2))))
          (if (not (eq op 'and)) (set-char-table-range ct k2 v))))
       ;; There's partial overlap.
       ((and (consp k1) (consp k2) (> (cdr k1) (cdr k2)))
        (if (not (eq op 'and))
            (set-char-table-range ct (cons (1+ (cdr k2)) (cdr k1)) (cdar al1)))
        (setcdr k1 (cdr k2)))
       ((and (consp k1) (consp k2) (< (cdr k1) (cdr k2)))
        (if (not (eq op 'and))
            (set-char-table-range ct (cons (1+ (cdr k1)) (cdr k2)) (cdar al2)))
        (setcdr k2 (cdr k1)))
       ;; Now the tails are equal.
       ((and (consp k1) (consp k2) (> (car k1) (car k2)))
        (set-char-table-range ct k1 (lex--merge op (cdr (pop al1)) (cdar al2)))
        (setcdr k2 (1- (car k1))))
       ((and (consp k1) (consp k2) (< (car k1) (car k2)))
        (set-char-table-range ct k2 (lex--merge op (cdar al1) (cdr (pop al2))))
        (setcdr k1 (1- (car k2))))
       (t (cl-assert nil)))))
  (if (not (eq op 'and))
      (dolist (x (or al1 al2))
        (set-char-table-range ct (car x) (cdr x))))
  ct)

(defvar lex--states)
(defvar lex--memoize)

(defun lex--set-eq (l1 l2)
  (let ((len (length l2)))
    (setq l2 (copy-sequence l2))
    (while (consp l1)
      (cl-assert (= len (length l2)))
      (unless (> len
                 (setq len (length (setq l2 (delq (pop l1) l2)))))
        (setq l1 t)))
    (not l1)))

(define-hash-table-test 'lex--set-eq 'lex--set-eq
  (lambda (l)
    (let ((hash 0))
      (while l
        (let ((x (pop l)))
          (if (memq x l) (progn (debug) nil)
            (setq hash (+ hash (sxhash x))))))
      hash)))
      

(defun lex--flatten-state (state)
  (cl-assert (memq (car state) '(and or orelse)))
  (let ((op (car state))
        (todo (cdr state))
        (done (list state))
        (res nil))
    (while todo
      (setq state (pop todo))
      (cond
       ((null state) (if (eq op 'and) (setq res nil todo nil)))
       ((memq state done) nil)
       ((eq (car-safe state) op)
        (push state done)
        (setq todo (append (cdr state) todo)))
       (t (unless (memq state res) (push state res)))))
    (cons op (nreverse res))))

(defun lex--merge-2 (op lex1 lex2)
  (cl-assert (memq op '(and or orelse)))
  ;; The order between lex1 and lex2 matters: preference is given to lex1.
  (cond
   ;; `lex1' and `lex2' might actually be the same when we use this code to
   ;; cancel out the `and' and the `join' from lex--merge-and-join.
   ;; ((eq lex1 lex2) (debug) lex1)  ;CHECK: ruled out by `lex--flatten-state'?
   ;; ((equal lex1 lex2) lex1)             ;Stack overflow :-(

   ;; Handle the 2 possible nil cases.
   ;; CHECK: ruled out by `lex--flatten-state'?
   ((null lex1) (debug) (if (eq op 'and) nil lex2))
   ((null lex2) (debug) (if (eq op 'and) nil lex1))

   ;; Do the predicate cases before the `stop' because the stop should
   ;; always come after the checks.
   ;; TODO: add optimizations for pairs of `checks' which are redundant,
   ;; or mutually exclusive, ... although we can also do it in lex-optimize.
   ((and (eq (car lex1) 'check) (eq (car lex2) 'check)
         (equal (nth 1 lex1) (nth 1 lex2))) ; Same predicate.
    (cl-list* 'check (nth 1 lex1)
              (lex--merge op (nth 2 lex1) (nth 2 lex2))
              (lex--merge op (nthcdr 3 lex1) (nthcdr 3 lex2))))
   ((eq (car lex1) 'check)
    (cl-list* 'check (nth 1 lex1)
              (lex--merge op (nth 2 lex1) lex2)
              (lex--merge op (nthcdr 3 lex1) lex2)))
   ((eq (car lex2) 'check)
    (cl-list* 'check (nth 1 lex2)
              (lex--merge op lex1 (nth 2 lex2))
              (lex--merge op lex1 (nthcdr 3 lex2))))

   ;; Joins have the form (join CONT . EXIT) where EXIT is a lexer
   ;; corresponding to the rest of the regexp after the `and' sub-regexp.
   ;; All the joins corresponding to the same `and' have the same EXIT.
   ;; CONT is a lexer that contains another join inside, it corresponds to
   ;; the decision to not yet leave the `and'.
   ((and (eq (car lex1) 'join) (eq (car lex2) 'join))
    (cl-assert (eq (cddr lex1) (cddr lex2))) ;Check they're the same join.
    (let ((in (lex--merge op (cadr lex1) (cadr lex2))))
      (if (eq op 'and)
          ;; Eliminate the join once it was all merged.
          ;; FIXME: This arbitrarily chooses `or' instead of `orelse',
          ;; and it arbitrarily gives CONT precedence over EXIT.
          (lex--merge 'or in (cddr lex1))
        `(join ,in ,@(cddr lex1)))))
   ;; If one the two lex's is a join but the other not, the other must
   ;; contain a corresponding join somewhere inside.
   ((eq (car lex1) 'join)
    (let ((next (lex--merge op (nth 1 lex1) lex2)))
      ;; lex1 is a valid exit point but lex2 isn't.
      (if (eq op 'and)
          next
        ;; FIXME: lex1 is implicitly an `or(else)' between (cadr lex1) and
        ;; (cddr lex1).  Here we construct an `or(else)' between `next' and
        ;; (cddr lex1).  I.e. we lose the `op' and we do not preserve the
        ;; ordering between lex2 and (cddr lex1).
        `(join ,next ,@(cddr lex1)))))
   ((eq (car lex2) 'join)
    (let ((next (lex--merge op lex1 (nth 1 lex2))))
      (if (eq op 'and) next `(join ,next ,@(cddr lex2)))))

   ;; The three `stop' cases.
   ((and (eq (car lex1) 'stop) (eq (car lex2) 'stop))
    ;; Here is where we give precedence to `lex1'.
    (if (eq op 'orelse) lex1
      (cl-list* 'stop (cadr lex1) (lex--merge op (cddr lex1) (cddr lex2)))))
   ((eq (car lex1) 'stop)
    (let ((next (lex--merge op (cddr lex1) lex2)))
      (pcase op
        (`or     (cl-list* 'stop (cadr lex1) next))
        (`orelse lex1)
        ;; CHECK: We should have hit a `join' before reaching a `stop'.
        (`and    (debug) next)
        (_       (error "lex.el: got %S but expected one of or/and/orelse"
                        op)))))
   ((eq (car lex2) 'stop)
    (let ((next (lex--merge op lex1 (cddr lex2))))
      ;; For `orelse', we want here to delay the `stop' until the point
      ;; where we know that lex1 doesn't match.  Sadly, I don't know how to
      ;; do it.
      (pcase op
        ;; FIXME: One thing we can do is to mark the value attached to the
        ;; `stop' so as to indicate that an earlier match may finish later.
        ;; This way, if the match is not `earlystop', we know it's one of
        ;; the leftmost ones, and maybe the search loop can avoid some work
        ;; when determining which is the leftmost longest match.
        (`orelse (cl-list* 'stop `(earlystop ,(cadr lex2)) next))
        ((or `or `orelse) (cl-list* 'stop (cadr lex2) next))
        ;; CHECK: We should have hit a `join' before reaching a `stop'.
        (`and    (debug) next)
        (_       (error "lex.el: got %S but expected one of or/and/orelse"
                        op)))))

   ;; The most general case.
   ((and (eq (car lex1) 'table) (eq (car lex2) 'table))
    (let ((al1 (lex--char-table->alist (cdr lex1)))
          (al2 (lex--char-table->alist (cdr lex2)))
          (ct (make-char-table 'lexer)))
      (lex--merge-into op al1 al2 ct)
      (cons 'table ct)))

   ((and (characterp (car lex1)) (characterp (car lex2))
         (eq (car lex1) (car lex2)))
    (cons (car lex1) (lex--merge op (cdr lex1) (cdr lex2))))
   ((and (characterp (car lex1)) (characterp (car lex2)))
    (unless (eq op 'and)
      (let ((ct (make-char-table 'lexer)))
        (aset ct (car lex1) (cdr lex1))
        (aset ct (car lex2) (cdr lex2))
        (cons 'table ct))))
   ((and (characterp (car lex1)) (eq (car lex2) 'table))
    (let ((next (lex--merge op (cdr lex1) (aref (cdr lex2) (car lex1)))))
      (if (eq op 'and)
          (if next (cons (car lex1) next))
        (let ((ct (copy-sequence (cdr lex2))))
          (aset ct (car lex1) next)
          (cons 'table ct)))))
   ((and (eq (car lex1) 'table) (characterp (car lex2)))
    (let ((next (lex--merge op (aref (cdr lex1) (car lex2)) (cdr lex2))))
      (if (eq op 'and)
          (if next (cons (car lex2) next))
        (let ((ct (copy-sequence (cdr lex1))))
          (aset ct (car lex2) next)
          (cons 'table ct)))))

   ((or (memq (car lex1) '(or orelse and))  ;state
        (memq (car lex2) '(or orelse and))) ;state
    ;; `state' nodes are nodes whose content is not known yet, so we
    ;; have to delay the merge via the memoization table.
    ;; `or' and `and' nodes should only happen when the other `op' is being
    ;; performed, in which case we can't do the merge either before lex1
    ;; and lex2 have both been merged.
    (lex--merge op lex1 lex2))
   (t (cl-assert nil))))

(defun lex--merge-now (&rest state)
  (cl-assert (memq (car state) '(and or orelse)))
  ;; Re-flatten, in case one of the sub-states was changed.
  (setq state (lex--flatten-state state))
  (if (<= (length state) 2)
      (if (eq (car state) 'and)
          ;; Need to strip out the `join's.
          (lex--merge-and-join (cadr state))
        (cadr state))
    (let ((op (pop state))
          (res (pop state)))
      (dolist (lex state)
        ;; CHECK: we fold the lexers  using left-associativity.
        ;; For `orelse', that means that `earlystop' never accumulates,
        ;; whereas if we folded in a right-associative way, we could get
        ;; some (earlystop (earlystop (earlystop V))).  Not sure which one's
        ;; preferable, so let's stick with what we have for now.
        (setq res (lex--merge-2 op res lex)))
      res)))

(defun lex--merge-and-join (lex)
  (lex--merge-2 'and lex lex))
    

(defun lex--merge (&rest state)
  (cl-assert (memq (car state) '(and or orelse)))
  (setq state (lex--flatten-state state))
  (if (and (<= (length state) 2)
           (not (eq (car state) 'and)))
      (cadr state)
    (or (gethash state lex--memoize)
        (progn
          ;; (debug)
          (cl-assert (memq (car state) '(and or orelse)))
          (push state lex--states)
          ;; The `state' node will be later on modified via setcar/setcdr,
          ;; se be careful to use a copy of it for the key.
          (puthash (cons (car state) (cdr state)) state lex--memoize)
          state))))

(defun lex--compile-category (category)
  (if (and (integerp category) (< category 128))
      category
    (if (symbolp category)
        (if (= 1 (length (symbol-name category)))
            (aref (symbol-name category) 0)
          (require 'rx)
          (defvar rx-categories)
          (cdr (assq category rx-categories))))))

(defun lex--compile-syntax (&rest syntaxes)
  (mapcar (lambda (x)
            (if (and (integerp x) (< x 32)) x
              (if (symbolp x)
                  (setq x (if (= 1 (length (symbol-name x)))
                              (symbol-name x)
                            (require 'rx)
                            (defvar rx-syntax)
                            (cdr (assq x rx-syntax)))))
              (if (characterp x) (setq x (string x)))
              (car (string-to-syntax x))))
          syntaxes))

(defconst lex--char-classes
  `((alnum alpha digit)
    (alpha word (?a . ?z) (?A . ?Z))
    (blank ?\s ?\t)
    (cntrl (?\0 . ?\C-_))
    (digit (?0 . ?9))
    ;; Include all multibyte chars, plus all the bytes except 128-159.
    (graph (?! . ?~) multibyte (#x3fffa0 . #x3fffff))
    ;; src/regexp.c handles case-folding inconsistently: lower and upper
    ;; match both lower- and uppercase ascii chars, but lower also matches
    ;; uppercase non-ascii chars whereas upper does not match lowercase
    ;; nonascii chars.  Here I simply ignore case-fold for [:lower:] and
    ;; [:upper:] because it's simpler and doesn't seem worse.
    (lower (check (lex--match-lower)))
    (upper (check (lex--match-upper)))
    (print graph ?\s)
    (punct (check (not (lex--match-syntax . ,(lex--compile-syntax "w"))))
           (?! . ?/) (?: . ?@) (?\[ . ?`) (?\{ . ?~))
    (space (check (lex--match-syntax . ,(lex--compile-syntax " "))))
    (xdigit digit (?a . ?f) (?A . ?F))
    (ascii (?\0 . ?\177))
    (nonascii (?\200 . #x3fffff))
    (unibyte ascii (#x3fff00 . #x3fffff))
    (multibyte (#x100 . #x3ffeff))
    (word (check (lex--match-syntax . ,(lex--compile-syntax "w"))))
    ;; `rx' alternative names.
    (numeric digit)
    (num digit)
    (control cntrl)
    (hex-digit xdigit)
    (hex xdigit)
    (graphic graph)
    (printing print)
    (alphanumeric alnum)
    (letter alpha)
    (alphabetic alpha)
    (lower-case lower)
    (upper-case upper)
    (punctuation punct)
    (whitespace space)
    (white space))
  "Definition of char classes.
Each element has the form (CLASS . DEFINITION) where definition
is a list of elements that can be either CHAR or (CHAR . CHAR),
or CLASS (another char class) or (check (PREDICATE . ARG))
or (check (not (PREDICATE . ARG))).")

(defvar lex--char-equiv-table nil
  "Equiv-case table to use to compile case-insensitive regexps.")

(defun lex--char-equiv (char)
  (when lex--char-equiv-table
    (let ((chars ())
          (tmp char))
      (while (and (setq tmp (aref lex--char-equiv-table tmp))
                  (not (eq tmp char)))
        (push tmp chars))
      (if chars (cons char chars)))))
    
;; For convenience we use lex itself to tokenize charset strings, so we
;; define it in another file.
(autoload 'lex--parse-charset "lex-parse-re")

(defun lex--nfa (re state)
  (cl-assert state)                   ;If `state' is nil we can't match anyway.
  (cond
   ((characterp re)
    (let ((chars (lex--char-equiv re)))
      (if (null chars)
          (cons re state)
        (let ((ct (make-char-table 'lexer)))
          (dolist (char chars) (aset ct char state))
          (cons 'table ct)))))
   ((stringp re)
    (if (null lex--char-equiv-table)
        ;; (Very) minor optimization.
        (nconc (mapcar 'identity re) state)
      (lex--nfa `(seq ,@(mapcar 'identity re)) state)))
   (t
    (pcase (or (car-safe re) re)
      ((or `: `seq `sequence
           ;; Hack!
           `group)
       (dolist (elem (reverse (cdr re)))
         (setq state (lex--nfa elem state)))
       state)
      ((or `char `in `not-char)
       (let ((chars (cdr re))
             (checks nil)
             (fail nil)
             (char nil)  ;The char seen, or nil if none, or t if more than one.
             (ct (make-char-table 'lexer)))
         (when (or (eq 'not (car chars)) (eq 'not-char (car re)))
           (setq chars (cdr chars))
           (set-char-table-range ct t state)
           (setq fail state)
           (setq state nil))
         (while chars
           (let ((range (pop chars)))
             (cond
              ((stringp range)
               (setq chars (append (cdr (lex--parse-charset range)) chars)))
              ((symbolp range)
               (setq range (or (cdr (assq range lex--char-classes))
                               (error "Uknown char class `%s'" range)))
               (setq chars (append range chars)))
              ((and (consp range) (eq 'check (car range)))
               (push (cadr range) checks))
              (t
               (setq char (if (or char (not (characterp range))
                                  (and lex--char-equiv-table
                                       (lex--char-equiv range)))
                              t range))
               ;; Set the range, first, regardless of case-folding.  This is
               ;; important because case-tables like to be set with few
               ;; large ranges rather than many small ones, as is done in
               ;; the case-fold loop.
               (set-char-table-range ct range state)
               (when (and lex--char-equiv-table
                          ;; Avoid looping over all characters.
                          (not (equal range '(#x100 . #x3ffeff))))
                 ;; Add all the case-equiv chars.
                 (let ((i (if (consp range) (car range) range))
                       (max (if (consp range) (cdr range) range))
                       char)
                   (while (<= i max)
                     (setq char i)
                     (while (and (setq char (aref lex--char-equiv-table char))
                                 (not (eq char i)))
                       (aset ct char state))
                     (setq i (1+ i)))))))))

         (let ((res (if (or (eq char t) fail)
                        (cons 'table ct)
                      (if char (cons char state)))))
           (if (and (not fail) checks)
               (setq state (lex--nfa 'anything state)))
           (dolist (check checks)
             (setq res
                   (if fail
                       ;; We do an `and' of the negation of the check and res.
                       (if (eq (car-safe check) 'not)
                           (list 'check (cadr check) res)
                         (cl-list* 'check check nil res))
                     ;; An `or' of the check and res.
                     (if (eq (car-safe check) 'not)
                         (list 'check (cadr check) res state)
                       (cl-list* 'check check state res)))))
           res)))

      ((or `union `or `| `orelse)
       (let ((newstate
              (cons (if (eq (car re) 'orelse) 'orelse 'or)
                    (mapcar (lambda (re) (lex--nfa re state)) (cdr re)))))
         (push newstate lex--states)
         newstate))

      ((or `inter `intersection `&)
       (if (<= (length re) 2)
           ;; Avoid constructing degenerate `and' nodes.
           (lex--nfa (cadr re) state)
         ;; Just using `and' is not enough because we have to enforce that the
         ;; sub-regexps (rather than the whole regexp) match the same string.
         ;; So we need to mark the juncture point.
         (let* ((join `(join nil ,@state))
                (newstate
                 `(and ,@(mapcar (lambda (re) (lex--nfa re join)) (cdr re)))))
           (push newstate lex--states)
           newstate)))

      ((or `0+ `zero-or-more `* `*\?)
       (let ((newstate (list 'state)))
         (let ((lexer (lex--nfa (cons 'seq (cdr re)) newstate)))
           (setcdr newstate (if (memq (car re) '(*\?))
                                (list state lexer)
                              (list lexer state))))
         (setcar newstate (if (memq (car re) '(*\?)) 'orelse 'or))
         (push newstate lex--states)
         newstate))
      
      ((or `string-end `eos `eot `buffer-end `eob)
       `(check (lex--match-eobp) ,state))
      ((or `string-start `bos `bot `buffer-start `bob)
       `(check (lex--match-bobp) ,state))
      ((or `line-end `eol) `(check (lex--match-eolp) ,state))
      ((or `line-start `bol) `(check (lex--match-bolp) ,state))
      ((or `word-start `bow) `(check (lex--match-bowp) ,state))
      ((or `word-end `eow) `(check (lex--match-eowp) ,state))
      (`symbol-start `(check (lex--match-bosp) ,state))
      (`symbol-end `(check (lex--match-eosp) ,state))
      (`not-word-boundary `(check (lex--match-not-word-boundary) ,state))
      (`word-boundary `(check (lex--match-not-word-boundary) nil . ,state))
      (`syntax `(check (lex--match-syntax
                        . ,(apply 'lex--compile-syntax (cdr re)))
                       ,(lex--nfa 'anything state)))
      (`not-syntax `(check (lex--match-syntax
                            . ,(apply 'lex--compile-syntax (cdr re)))
                           nil . ,(lex--nfa 'anything state)))
      (`category `(check (lex--match-category
                          . ,(lex--compile-category (cadr re)))
                         ,(lex--nfa 'anything state)))
      (`not-category `(check (lex--match-category
                              . ,(lex--compile-category (cadr re)))
                             nil . ,(lex--nfa 'anything state)))
      
      ;; `rx' accepts char-classes directly as regexps.  Let's reluctantly
      ;; do the same.
      ((or `digit `numeric `num `control `cntrl `hex-digit `hex `xdigit `blank
           `graphic `graph `printing `print `alphanumeric `alnum `letter
           `alphabetic `alpha `ascii `nonascii `lower `lower-case `upper
           `upper-case `punctuation `punct `space `whitespace `white)
       (lex--nfa `(char ,re) state))

      (`case-sensitive
       (let ((lex--char-equiv-table nil))
         (lex--nfa `(seq ,@(cdr re)) state)))

      (`case-fold
       (let ((lex--char-equiv-table
              (case-table-get-table (current-case-table) 'eqv)))
         (lex--nfa `(seq ,@(cdr re)) state)))

      ((or `point
           ;; Sub groups!
           `submatch `group `backref
           ;; Greediness control
           `minimal-match `maximal-match)
       (error "`%s' Not implemented" (or (car-safe re) re)))

      ((or `not-newline `nonl `dot) (lex--nfa '(char not ?\n) state))
      (`anything (lex--nfa '(char not) state))
      ((or `word `wordchar) (lex--nfa '(syntax w) state))
      (`not-wordchar (lex--nfa '(not-syntax w) state))

      (`any
       ;; `rx' uses it for (char ...) sets, and sregex uses it for `dot'.
       (lex--nfa (if (consp re) (cons 'char (cdr re)) '(char not ?\n)) state))

      (`negate
       ;; We could define negation directly on regexps, but it's easier to
       ;; do it on NFAs since those have fewer cases to deal with.
       (let ((posnfa
              ;; Trow away the mergable states generated while computing the
              ;; posnfa, since it's only an intermediate datastructure.
              (let (lex--states)
                (lex--nfa `(seq ,@(cdr re)) '(stop negate)))))
         (lex-negate posnfa state)))

      (`not
       ;; The `not' as used in `rx' should be deprecated so we can make it
       ;; an alias for `negate', whose semantics is different.  E.g.
       ;; (negate (char ...)) matches the empty string and 2-char strings.
       (setq re (cadr re))
       (pcase (or (car-safe re) re)
         (`word-boundary
          (message "`not' deprecated: use not-word-boundary")
          (lex--nfa 'not-word-boundary state))
         ((or `any `in `char)
          (message "`not' deprecated: use (%s not ...)" (or (car-safe re) re))
          (lex--nfa (cl-list* (car re) 'not (cdr re)) state))
         ((or `category `syntax)
          (message "`not' deprecated: use not-%s" (car re))
          (lex--nfa (cons (intern (format "not-%s" (car re))) (cdr re)) state))
         (elem (error "lex.el: unexpected argument `%S' to `not'." elem))))

      (`and
       ;; `rx' defined `and' as `sequence', but we may want to define it
       ;; as intersection instead.
       (error "`and' is deprecated, use `seq', `:', or `sequence' instead"))
         
      ((or `1+ `one-or-more `+ `+\?)
       (lex--nfa `(seq (seq ,@(cdr re))
                       (,(if (memq (car re) '(+\?)) '*\? '0+) ,@(cdr re)))
                 state))
      ((or `opt `zero-or-one `optional `\?)
       (lex--nfa `(or (seq ,@(cdr re)) "") state))
      (`\?\?
       (lex--nfa `(orelse "" (seq ,@(cdr re))) state))
      ((or `repeat `** `=)
       (let ((min (nth 1 re))
             (max (nth 2 re))
             (res (nthcdr 3 re)))
         (unless res
           (setq res (list max)) (setq max min))
         (lex--nfa `(seq ,@(append (make-list (or min 0)
                                              (if (eq (length res) 1)
                                                  (car res)
                                                (cons 'seq res)))
                                   (if (null max)
                                       `((0+ ,@res))
                                     (make-list (- max (or min 0))
                                                `(opt ,@res)))))
                   state)))
      (`>= (lex--nfa `(repeat ,(nth 1 re) nil ,@(nthcdr 2 re)) state))

      ((or `bre `re `ere)
       (lex--nfa (lex-parse-re (nth 1 re) (car re)) state))
      (elem (error "lex.el: unknown RE element %S" elem))))))

(defun lex--negate-inftail (state howmany)
  ;; We hashcons the infinite tails and store them in the memoize table.
  ;; This is an abuse, but saves us from passing it around as an
  ;; extra argument.
  (let ((inftail-1+ (gethash state lex--memoize)))
    (unless inftail-1+
      ;; Precompute the final infinitely repeating tail.
      (setq inftail-1+ `(table . ,(make-char-table 'lexer)))
      (set-char-table-range (cdr inftail-1+) t `(or ,state ,inftail-1+))
      (push (aref (cdr inftail-1+) 0) lex--states)
      (puthash state inftail-1+ lex--memoize))
    (pcase howmany
      (`1+ inftail-1+)
      (`0+ (aref (cdr inftail-1+) 0))
      (_ (error "lex.el: howmany is `%S' instead of one of 1+/0+" howmany)))))

(defun lex--negate-now (nfa state)
  (pcase (car nfa)
    (`nil (lex--negate-inftail state '0+))
    (`check
     `(check ,(nth 1 nfa) ,(lex--negate-memo (nth 2 nfa) state)
             ,@(lex--negate-memo (nthcdr 3 nfa) state)))
    (`stop
     (if (cddr nfa)
         ;; This is valid but should normally not happen.
         (lex--negate-now `(or (stop ,(cadr nfa)) ,(cddr nfa)) state)
       (lex--negate-inftail state '1+)))

    ((or `or `orelse)
     (let ((join `(join nil . ,state)))
       `(and ,@(mapcar (lambda (nfa) (lex--negate-memo nfa join)) (cdr nfa)))))

    (`and
     `(or ,@(mapcar (lambda (nfa) (lex--negate-memo nfa state)) (cdr nfa))))

    (`join
     ;; The join says: either exit the `and' because we matched all branches,
     ;; or keep matching further.  Negation makes the synchrony between
     ;; `and' branches irrelevant, so we can consider it as an `or(else)'.
     (if (cadr nfa)
         ;; This is valid but should normally not happen.
         (lex--negate-now `(or ,(cadr nfa) ,(cddr nfa)) state)
       (lex-negate (cddr nfa) state)))
    (_
     (let ((ct (make-char-table 'lexer)))
       ;; Get inftail-0+ from the hashtable.
       (set-char-table-range ct t (lex--negate-inftail state '0+))
       (if (characterp (car nfa))
           (aset ct (car nfa) (lex--negate-memo (cdr nfa) state))
         (cl-assert (eq 'table (car nfa)))
         (map-char-table (lambda (range nfa)
                           (set-char-table-range ct range
                                                 (lex--negate-memo nfa state)))
                         (cdr nfa)))
       `(or ,state (table ,@ct))))))

(defun lex--negate-memo (nfa state)
  ;; Make sure our `inftail' abuse of the hastable doesn't break anything.
  (cl-assert (not (eq nfa state)))
  (or (gethash nfa lex--memoize)
      (let ((newstate (cons 'state nil)))
        (puthash nfa newstate lex--memoize)
        (let ((res (lex--negate-now nfa state)))
          (when (memq (car res) '(or and orelse))
            (push newstate lex--states))
          (if (null res)
              (setq res '(?a))
            (setcar newstate (car res))
            (setcdr newstate (cdr res))
            newstate)))))
              
(defun lex-negate (nfa state)
  "Concatenate the negation of NFA with STATE.
Returns a new NFA."
  (let ((lex--memoize (make-hash-table :test 'eq)))
    (lex--negate-memo nfa state)))

(defun lex--dfa-wrapper (f)
  (let* ((lex--states ())
         (res (funcall f))
         (postponed ())
         (lex--memoize (make-hash-table :test 'lex--set-eq))
         (states-dfa (make-hash-table :test 'eq)))

    (while lex--states
      (dolist (state (prog1 lex--states (setq lex--states nil)))
        (let ((merged (apply 'lex--merge-now state)))
          (if (memq (car merged) '(and or orelse))
              ;; The merge could not be performed for some reason:
              ;; let's re-schedule it.
              (push state postponed)
            (puthash state merged states-dfa))))

      (unless lex--states
        ;; If states-dfa is empty it means we haven't made any progress,
        ;; so we're stuck in an infinite loop.  Hopefully this cannot happen?
        (cl-assert (not (zerop (hash-table-count states-dfa))))
        (maphash (lambda (k v)
                   (unless v
                     ;; With `intersection', lex--merge may end up returning
                     ;; nil if the intersection is empty, so `v' can be
                     ;; nil here.  In since `k' is necessarily a cons cell,
                     ;; we can't turn it into nil, so we turn it into
                     ;; a more costly lexer that also fails for all inputs.
                     (setq v '(?a)))
                   (setcar k (car v))
                   (setcdr k (cdr v)))
                 states-dfa)
        (clrhash states-dfa)
        (setq lex--states postponed)
        (setq postponed nil)))

    res))

(defun lex-compile (alist)
  (lex--dfa-wrapper
   (lambda ()
     (let* ((lex--char-equiv-table
             (if case-fold-search
                 (case-table-get-table (current-case-table) 'eqv)))
            (newstate
             `(or
               ,@(mapcar (lambda (x) (lex--nfa (car x) (list 'stop (cdr x))))
                         alist))))
       (push newstate lex--states)
       newstate))))

(defun lex-search-dfa (match-dfa)
  ;; This constructs a search-DFA whose last match should be the leftmost
  ;; longest match.
  (lex--dfa-wrapper
   (lambda ()
     (lex--nfa '(*\? (char not)) match-dfa))))
     

(defun lex--terminate-if (new old)
  (cond
   ((eq new t) t)
   ((eq old t) t)
   (t (while new (let ((x (pop new))) (if (not (memq x old)) (push x old))))
      old)))

(defun lex--optimize-1 (lexer)
  (let ((terminate nil))
    (cons
     (pcase (car lexer)
       (`table
        (let ((ct (cdr lexer))
              (char nil))
          ;; Optimize each entry.
          (map-char-table
           (lambda (range v)
             (let ((cell (lex--optimize v)))
               (setq terminate (lex--terminate-if (cdr cell) terminate))
               (set-char-table-range ct range (car cell))))
           ct)
          ;; Optimize the internal representation of the table.
          (optimize-char-table (cdr lexer) 'eq)
          ;; Eliminate the table if possible.
          (map-char-table
           (lambda (range v)
             (setq char
                   (if (and (characterp range) (null char))
                       range t)))
           ct)
          (pcase char
            (`nil nil)
            (`t lexer)
            (_ (setcar lexer 'char) (setcdr lexer (aref ct char)) lexer))))
       (`stop
        (let ((cell (lex--optimize (cddr lexer))))
          (setq terminate t)
          (setf (cddr lexer) (car cell)))
        lexer)
       (`check
        (let* ((test (nth 1 lexer))
               (cellf (lex--optimize (nthcdr 3 lexer)))
               (fail (setf (nthcdr 3 lexer) (car cellf)))
               (cells (lex--optimize (nth 2 lexer)))
               (succ (setf (nth 2 lexer) (car cells))))
          (setq terminate (lex--terminate-if (cdr cellf) terminate))
          (setq terminate (lex--terminate-if (cdr cells) terminate))
          ;; TODO: the check-optimizations below only work on consecutive
          ;; pairs of checks.  We need to be more agressive and make sure
          ;; the optimized DFA never does twice the same test at the same
          ;; position.  Most importantly: don't do the same test in
          ;; a tight loop as in "(^\<)*".
          (when (eq 'check (car succ))
            (cond
             ((equal test (nth 1 succ)) ;Same successful test.
              (setf (nth 2 lexer) (setq succ (nth 2 succ))))
             ;; TODO: we can add rules such as bobp -> eolp,
             ;; bosp -> bowp, (syntax X) -> (syntax Y X), ...
             ))
          (when (eq 'check (car fail))
            (cond
             ((equal test (nth 1 fail)) ;Same failing test.
              (setf (nthcdr 3 lexer) (setq fail (nthcdr 3 succ))))
             ;; TODO: we can add rules such as !eolp -> !bobp,
             ;; !bowp -> !bosp, !(syntax Y X) -> !(syntax X), ...
             ))
          (if (or succ fail) lexer)))
       (_
        (cl-assert (characterp (car lexer)))
        (let ((cell (lex--optimize (cdr lexer))))
          (setq terminate (lex--terminate-if (cdr cell) terminate))
          (if (setf (cdr lexer) (car cell))
              lexer))))
     (if (consp terminate)
         (delq lexer terminate)
       terminate))))

(defun lex--optimize (lexer)
  (when lexer
    ;; The lex--memoize cache maps lexer states to (LEXER . TERMINATE) where
    ;; TERMINATE is either t to say that LEXER can terminate or a list of
    ;; lexers which means that LEXER terminates only if one of the lexers in
    ;; the list terminates.
    (let ((cache (gethash lexer lex--memoize)))
      (if cache
          ;; Optimize (char C) to nil.
          (if (and (characterp (caar cache)) (null (cdar cache))) nil cache)
        ;; Store a value indicating that we're in the process of computing it,
        ;; so when we encounter a loop, we don't recurse indefinitely.
        ;; Not knowing any better, we start by stating the tautology that
        ;; `lexer' terminates if and only if `lexer' terminates.
        (let ((cell (cons lexer (list lexer))))
          (puthash lexer cell lex--memoize)
          (let ((res (lex--optimize-1 lexer)))
            (if (and (car res) (cdr res))
                res
              (setcar lexer ?a)
              (setcdr lexer nil)
              (puthash lexer '(nil) lex--memoize)
              nil)))))))

(defun lex-optimize (lexer)
  (let ((lex--memoize (make-hash-table :test 'eq)))
    (prog1 (car (lex--optimize lexer))
      (message "Visited %d states" (hash-table-count lex--memoize)))))

(defmacro lex-case (object posvar &rest cases)
  (declare (indent 2))
  (let* ((i -1)
         (alist (mapcar (lambda (case) (cons (car case) (cl-incf i))) cases))
         (lex (lex-compile alist))
         (tmpsym (make-symbol "tmp")))
    (setq i -1)
    `(let ((,tmpsym (lex-match-string ',lex ,object ,posvar)))
       (pcase (car ,tmpsym)
         ,@(mapcar (lambda (case)
                     `(,(cl-incf i)
                       (set-match-data
                        (list ,posvar (setq ,posvar (cadr ,tmpsym))))
                       ,@(cdr case)))
                   cases)))))

;;; Matching engine

(defun lex--match-bobp (arg pos &optional string)
  (= pos (if string 0 (point-min))))

(defun lex--match-eobp (arg pos &optional string)
  (= pos (if string (length string) (point-max))))

(defun lex--match-bolp (arg pos &optional string)
  (if string (or (= pos 0) (eq ?\n (aref string (1- pos))))
    (memq (char-before pos) '(nil ?\n))))

(defun lex--match-eolp (arg pos &optional string)
  (if string (or (= pos (length string)) (eq ?\n (aref string pos)))
    (memq (char-after pos) '(nil ?\n))))

(defun lex--match-bowp (arg pos &optional string)
  (and (not (if string (and (> pos 0)
                            (eq ?w (char-syntax (aref string (1- pos)))))
              (and (> pos (point-min)) (eq 2 (car (syntax-after (1- pos)))))))
       (if string (and (< pos (length string))
                       (eq ?w (char-syntax (aref string pos))))
         (eq 2 (car (syntax-after pos))))))

(defun lex--match-eowp (arg pos &optional string)
  (and (if string (and (> pos 0)
                       (eq ?w (char-syntax (aref string (1- pos)))))
         (and (> pos (point-min)) (eq 2 (car (syntax-after (1- pos))))))
       (not (if string (and (< pos (length string))
                            (eq ?w (char-syntax (aref string pos))))
              (eq 2 (car (syntax-after pos)))))))

(defun lex--match-bosp (arg pos &optional string)
  (and (not (if string
                (and (> pos 0)
                     (memq (char-syntax (aref string (1- pos))) '(?w ?_)))
              (and (> pos (point-min))
                   (memq (car (syntax-after (1- pos))) '(2 3)))))
       (if string (and (< pos (length string))
                       (memq (char-syntax (aref string pos)) '(?w ?_)))
         (memq (car (syntax-after pos)) '(2 3)))))

(defun lex--match-eosp (arg pos &optional string)
  (and (if string (and (> pos 0)
                       (memq (char-syntax (aref string (1- pos))) '(?w ?_)))
         (and (> pos (point-min)) (memq (car (syntax-after (1- pos))) '(2 3))))
       (not (if string (and (< pos (length string))
                            (memq (char-syntax (aref string pos)) '(?w ?_)))
              (memq (car (syntax-after pos)) '(2 3))))))

(defun lex--match-not-word-boundary (arg pos &optional string)
  (eq (if string (and (> pos 0)
                      (eq ?w (char-syntax (aref string (1- pos)))))
        (and (> pos (point-min)) (eq 2 (car (syntax-after (1- pos))))))
      (if string (and (< pos (length string))
                      (eq ?w (char-syntax (aref string pos))))
        (eq 2 (car (syntax-after pos))))))

(defun lex--match-upper (arg pos &optional string)
  (when (< pos (if string (length string) (point-max)))
    (let ((char (if string (aref string pos) (char-after pos))))
      (not (eq (downcase char) char)))))

(defun lex--match-lower (arg pos &optional string)
  (when (< pos (if string (length string) (point-max)))
    (let ((char (if string (aref string pos) (char-after pos))))
      (not (eq (upcase char) char)))))


(defun lex--match-category (category pos &optional string)
  (when (< pos (if string (length string) (point-max)))
    (aref (char-category-set (if string (aref string pos)
                               (char-after pos)))
          category)))

(defun lex--match-syntax (syntaxes pos &optional string)
  (when (< pos (if string (length string) (point-max)))
    (memq (car (if string (aref (syntax-table) (aref string pos))
                 (syntax-after pos)))
          syntaxes)))


(defun lex-match-string (lex string &optional start stop)
  "Match LEX against STRING between START and STOP.
Return a triplet (VALUE ENDPOS . LEXER) where VALUE is the
value of returned by the lexer for the match found (or nil), ENDPOS
is the end position of the match found (or nil), and LEXER is the
state of the engine at STOP, which can be passed back to
`lex-match-string' to continue the match elsewhere."
  ;; FIXME: Move this to C.
  (unless start (setq start 0))
  (unless stop  (setq stop (length string)))
  (let ((match (list nil nil))
        (lastlex lex))
    (while
        (progn
          (while (eq (car lex) 'check)
            (setq lex (if (funcall (car (nth 1 lex)) (cdr (nth 1 lex))
                                   start string)
                          (nth 2 lex) (nthcdr 3 lex))))
          (when (eq (car lex) 'stop)
            ;; Don't stop yet, we're looking for the longest match.
            (setq match (list (cadr lex) start))
            (message "Found match: %s" match)
            (setq lex (cddr lex)))
          (cl-assert (not (eq (car lex) 'stop)))
          (and lex (< start stop)))
      (let ((c (aref string start)))
        (setq start (1+ start))
        (setq lex (cond
                   ((eq (car lex) 'table) (aref (cdr lex) c))
                   ((integerp (car lex)) (if (eq c (car lex)) (cdr lex)))))
        (setq lastlex lex)))
    (message "Final search pos considered: %s" start)
    ;; The difference between `lex' and `lastlex' is basically that `lex'
    ;; may depend on data after `stop' (if there was an `end-of-file' or
    ;; `word-boundary' or basically any `check').  So let's return `lastlex'
    ;; so it can be correctly used to continue the match with a different
    ;; content than what's after `stop'.
    (nconc match lastlex)))
        
(defun lex-match-string-first (lex string &optional start stop)
  "Match LEX against STRING between START and STOP.
Return a triplet (VALUE ENDPOS . LEXER) where VALUE is the
value of returned by the lexer for the match found (or nil), ENDPOS
is the end position of the match found (or nil), and LEXER is the
state of the engine at STOP, which can be passed back to
`lex-match-string' to continue the match elsewhere."
  ;; FIXME: Move this to C.
  (unless start (setq start 0))
  (unless stop  (setq stop (length string)))
  (let ((match (list nil nil))
        (lastlex lex))
    (catch 'found
      (while
          (progn
            (while (eq (car lex) 'check)
              (setq lex (if (funcall (car (nth 1 lex)) (cdr (nth 1 lex))
                                     start string)
                            (nth 2 lex) (nthcdr 3 lex))))
            (when (eq (car lex) 'stop)
              (throw 'found (cl-list* (cadr lex) start (cddr lex))))
            (cl-assert (not (eq (car lex) 'stop)))
            (and (not match) lex (< start stop)))
        (let ((c (aref string start)))
          (setq start (1+ start))
          (setq lex (cond
                     ((eq (car lex) 'table) (aref (cdr lex) c))
                     ((integerp (car lex)) (if (eq c (car lex)) (cdr lex)))))
          (setq lastlex lex)))
      ;; The difference between `lex' and `lastlex' is basically that `lex'
      ;; may depend on data after `stop' (if there was an `end-of-file' or
      ;; `word-boundary' or basically any `check').  So let's return `lastlex'
      ;; so it can be correctly used to continue the match with a different
      ;; content than what's after `stop'.
      (cl-list* nil start lastlex))))

(defun lex-match-buffer (lex &optional stop)
  "Match LEX against buffer between point and STOP.
Return a triplet (VALUE ENDPOS . LEXER) where VALUE is the
value of returned by the lexer for the match found (or nil), ENDPOS
is the end position of the match found (or nil), and LEXER is the
state of the engine at STOP, which can be passed back to
continue the match elsewhere."
  ;; FIXME: Move this to C.
  (unless stop  (setq stop (point-max)))
  (let ((start (point))
        (match (list nil nil))
        (lastlex lex))
    (while
        (progn
          (while (eq (car lex) 'check)
            (setq lex (if (funcall (car (nth 1 lex)) (cdr (nth 1 lex))
                                   start)
                          (nth 2 lex) (nthcdr 3 lex))))
          (when (eq (car lex) 'stop)
            ;; Don't stop yet, we're looking for the longest match.
            (setq match (list (cadr lex) start))
            (message "Found match: %s" match)
            (setq lex (cddr lex)))
          (cl-assert (not (eq (car lex) 'stop)))
          (and lex (< start stop)))
      (let ((c (char-after start)))
        (setq start (1+ start))
        (setq lex (cond
                   ((eq (car lex) 'table) (aref (cdr lex) c))
                   ((integerp (car lex)) (if (eq c (car lex)) (cdr lex)))))
        (setq lastlex lex)))
    (message "Final search pos considered: %s" start)
    ;; The difference between `lex' and `lastlex' is basically that `lex'
    ;; may depend on data after `stop' (if there was an `end-of-file' or
    ;; `word-boundary' or basically any `check').  So let's return `lastlex'
    ;; so it can be correctly used to continue the match with a different
    ;; content than what's after `stop'.
    (nconc match lastlex)))

(provide 'lex)
;;; lex.el ends here
