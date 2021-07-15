;;; jupiter-nop.el --- Jupiter no operation  -*- lexical-binding:t -*-
;;
;; Copyright (C) 2009, 2014, 2016 Free Software Foundation, Inc.
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: jupiter, operation, nop
;; X-RCS: $Id:$
;;
;; This file is part of Rudel.
;;
;; Rudel is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; Rudel is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Rudel. If not, see <http://www.gnu.org/licenses>.


;;; Commentary:
;;
;; Class `jupiter-nop' implements a no-operation for the Jupiter
;; algorithm.


;;; History:
;;
;; 0.1 - Initial revision.


;;; Code:
;;

(require 'eieio)

(require 'jupiter-operation)


;;; Class jupiter-nop
;;

(defclass jupiter-nop (jupiter-operation)
  ()
  "Operation, which does not change anything.")

(cl-defmethod rudel-apply ((_this jupiter-nop) _object)
  "Applying THIS does not change OBJECT.")

(cl-defmethod jupiter-transform ((_this jupiter-nop) other)
  "Transforming OTHER with THIS simply returns OTHER."
  other)

(provide 'jupiter-nop)
;;; jupiter-nop.el ends here
