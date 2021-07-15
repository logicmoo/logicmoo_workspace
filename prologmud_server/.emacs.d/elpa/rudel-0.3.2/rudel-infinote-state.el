;;; rudel-infinote-state.el --- Base classes for infinote states  -*- lexical-binding:t -*-
;;
;; Copyright (C) 2009, 2010, 2014, 2016 Free Software Foundation, Inc.
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: rudel, infinotes, states
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
;; This file contains the `rudel-infinote-state' class, which is the
;; base class for state classes in the infinote backend.


;;; History:
;;
;; 0.1 - Initial version


;;; Code:
;;

(require 'eieio)

(require 'rudel-state-machine)

(require 'rudel-infinote-util)


;;; Class rudel-infinote-state
;;

;; TODO server connections should use the same state class
(defclass rudel-infinote-state (rudel-state)
  ((connection :initarg :connection
	       :type    rudel-infinote-client-connection
	       :documentation
	       ""))
  "Base class for infinote state classes.")

(cl-defmethod rudel-enter ((_this rudel-infinote-state))
  ""
  nil)

(cl-defmethod rudel-leave ((_this rudel-infinote-state))
  "")

(cl-defmethod rudel-accept ((_this rudel-infinote-state) _xml)
  ""
  nil)

(provide 'rudel-infinote-state)
;;; rudel-infinote-state.el ends here
