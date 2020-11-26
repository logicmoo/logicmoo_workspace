;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               repl.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Implements a prolog REPL and loader.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2010-01-16 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2010 - 2010
;;;;    
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU General Public License
;;;;    as published by the Free Software Foundation; either version
;;;;    2 of the License, or (at your option) any later version.
;;;;    
;;;;    This program is distributed in the hope that it will be
;;;;    useful, but WITHOUT ANY WARRANTY; without even the implied
;;;;    warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;;;    PURPOSE.  See the GNU General Public License for more details.
;;;;    
;;;;    You should have received a copy of the GNU General Public
;;;;    License along with this program; if not, write to the Free
;;;;    Software Foundation, Inc., 59 Temple Place, Suite 330,
;;;;    Boston, MA 02111-1307 USA
;;;;**************************************************************************


(defun eval-prolog (c)
  "
Evaluates a code_tail.
"
  (let ((*package* (find-package "MICRO-PROLOG-II")))
   (setq *top-of-frozen-goals-stack* +bottom-of-frozen-goals-stack+
         *top-of-global-stack* +bottom-of-global-stack+
         *top-of-local-stack* +bottom-of-local-stack+
         *top-of-trail* +bottom-of-trail+
         *specific-cut-point* 0
         *current-continuation* nil
         *cl* 0
         *backtracking-local-register* 0
         *backtracking-global-register* +bottom-of-global-stack+
         *awakened-goals* nil
         *not-done* t) 
   (push_cont)                          ; initial continuation
   (push_E (nloc c))                    ; local env. for the query
   (push_G (nglob c))                   ; global env. for the query
   (setq *current-continuation* (cdr c)
         *cl* *top-of-local-stack*)     ; current continuation
   (maj_L (nloc c))                     ; ends local block
   (maj_G (nglob c))                    ; ends global block
   (catch 'debord (forward))))




(defun read-prolog (&optional (stream *standard-input*))
  "
Read a clause (same as the $ reader macro).
"
  (let* ((*standard-input* stream)
         (*package* (find-package "MICRO-PROLOG-II"))
         (c (read_code_cl)))
    (add_cl (pred (head c)) c 'def)     ; always in the var subset
    (if (largs (head c)) 
        (let ((b (nature (car (largs (head c))))))
          (if (eq b 'def)               ; first arg is a variable
              (mapc                     ; add c to all the subsets
               (lambda (x) (add_cl (pred (head c)) c x))
               '(atom empty list fonct))
              (add_cl (pred (head c)) c b))))
    c))





(defun load-prolog (file)
  "
Load a prolog file.
The prolog clauses must not be prefixed by the $ character.
% is the comment character.
"
  (with-open-file (input file)
    (catch 'quit
      (loop
         :while (loop
                   :for peek = (peek-char t input nil nil)
                   :while (and peek (char= #\% peek))
                   :do (read-line input)
                   :finally (return peek))
         :do (handler-case
             (let ((*readtable* *prolog-readtable*))
               (read-prolog input))
           #+clisp
           (SYSTEM::SIMPLE-END-OF-FILE (err)
             (let ((args (SIMPLE-CONDITION-FORMAT-ARGUMENTS err)))
               (unless (and (listp args)
                            (eq 'read (first args)))
                 (terpri)
                 (princ "End of file while reading a clause.")
                 (terpri))
               (return nil)))
           (end-of-file ()
             (terpri)
             (princ "End of file while reading a clause.")
             (terpri)
             (return nil)))))))


(defun repl-prolog (&optional (*query-io* *query-io*))
  "
Implement a prolog REPL: reads a code_tail and evaluates it.
"
  (catch 'quit
    (loop
       (restart-case
           (loop
              :while (progn
                       (terpri *query-io*)
                       (princ "?- " *query-io*)
                       (finish-output *query-io*)
                       (loop
                          :for peek = (peek-char t *query-io* nil nil)
                          :while (and peek (char= #\% peek))
                          :do (read-line *query-io*)
                          :finally (return peek)))
              :do (handler-case
                      (let ((*readtable* *prolog-readtable*)
                            (*package* (find-package "MICRO-PROLOG-II"))
                            (*standard-input* *query-io*))
                        (let ((form (read_code_tail)))
                          ;; (print form)
                          (eval-prolog form))) 
                    (end-of-file (err)
                      (terpri *query-io*)
                      (princ "End of file while reading a clause." *query-io*)
                      (terpri *query-io*)
                      (finish-output *query-io*)
                      (return nil))))
         (continue () :report "Return to the REPL"))))
  (terpri *query-io*)
  (princ "Bye." *query-io*)
  (terpri *query-io*)
  (values))





(defun get-string (argument &optional (error-message "expected a string"))
  (handler-case
      (map 'string (function code-char)
           (undo_l (ultimate argument *current-environment* *current-goal*)))
    (error (err)
      ;; (print err) (princ err)
      (terpri)
      (princ error-message)
      (terpri)
      (return-from get-string nil))))


(define-prolog-function |load| (x)
  "
load/1 +Pathname
"
  (let ((path (or (get-string x "load/1 expected a string containing a pathname.")
                  (return-from |load| 'fail))))
    (load-prolog path)))


(define-prolog-function |lisp| (text)
  "
lisp/1 +String
Reads one form from the String and evaluates it.
"
  (let* ((src (or (get-string text "lisp/1 expected a string containing a lisp form.")
                  (return-from |lisp| 'fail)))
         (values (multiple-value-list (eval (read-from-string src)))))
    (format t "~& --> ~{~S~^ ;~%     ~}~%" values)
    (first values)))


(define-prolog-function |quit| ()
  "
load/0
"
  (throw 'quit nil))



(define-prolog-function |help| (&optional name)
  (if name
      (let ((name (or (get-string name "help/1 expected a string containing a name.")
                      (return-from |help| 'fail))))
        (princ (documentation (intern name) 'function)))
      (dolist (item (sort (copy-list *ob-micro-log*) (function string<)))
        (princ item) (princ "/") (princ (get item 'evaluable)) (terpri))))
