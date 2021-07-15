;;; psgml-nofill.el --- ???  -*- lexical-binding:t -*-

;; Copyright (C) 2016  Free Software Foundation, Inc.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(require 'psgml-parse)
(require 'psgml-edit)
(eval-when-compile (require 'cl-lib))

;; psgml-parse.el

(defun sgml-parse-set-appflag (flagsym)
  (cl-loop for name = (sgml-parse-name)
        while name
        for et = (sgml-lookup-eltype name)
        for flag-value = t
        do
        (when (looking-at "@")
          (forward-char 1)
          (let ((attr (sgml-check-name)))
            (sgml-check-delim "VI")
            (let ((val (sgml-parse-literal)))
              (setq flag-value (cons attr val)))))
        (setf (sgml-eltype-appdata et flagsym) flag-value)
        (message "Defining element %s as %s %S" name flagsym flag-value)
        (sgml-skip-cs)))

;; psgml-edit.el

(defun sgml-element-fillable (element)
  (and (sgml-element-mixed element)
       (let ((nofill (sgml-element-appdata element 'nofill)))
         (if (or (eq nofill t) (not (consp nofill)))
             t
           (let ((attr (car nofill))
                 (val  (cdr nofill)))
             (let ((attval (sgml-element-attval element attr)))
               (not (equal val attval))))))))
