;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               loader.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Loader file for the microPrologII
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
;;;;    warranty of MERCHANTABILITY or FITNESS FOR +max-of-trail+ PARTICULAR
;;;;    PURPOSE.  See the GNU General Public License for more details.
;;;;    
;;;;    You should have received a copy of the GNU General Public
;;;;    License along with this program; if not, write to the Free
;;;;    Software Foundation, Inc., 59 Temple Place, Suite 330,
;;;;    Boston, MA 02111-1307 USA
;;;;**************************************************************************

(load "packages.lisp")
(in-package "MICRO-PROLOG-II")
(let ((files '("boot.lisp" "clecteur.lisp" "lecteur.lisp"
               "blocs.lisp" "cutili.lisp" "unify.lisp" "cunify.lisp" 
               "pred.lisp" "cpred.lisp" "resol.lisp"
               
               "repl.lisp")))
  (dolist (file files) (load file))
  #- (and)
  (dolist (file files) (load (compile-file file))))

(defun run () (repl-prolog))


;;; (load"loader") (muprolog2:run)
;;; load("init.pl"), load("benchmark.pl").

;;;; THE END ;;;;

