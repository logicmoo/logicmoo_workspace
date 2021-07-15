;;; rudel-protocol.el --- Interface implemented by Rudel protocol backends  -*- lexical-binding:t -*-
;;
;; Copyright (C) 2009, 2014, 2016 Free Software Foundation, Inc.
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: Rudel, backend, protocol
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
;; This file contains the interface for Rudel protocol backends.  This
;; interface consists of the following functions:
;;
;; + joining sessions
;;   + `rudel-ask-join-info'
;;   + `rudel-join'
;; + hosting sessions
;;   + `rudel-ask-host-info'
;;   + `rudel-host'
;; + factory functions
;;   + `rudel-make-document'


;;; History:
;;
;; 0.1 - Initial version


;;; Code:
;;

(require 'cl-generic)
(require 'eieio)

(require 'rudel-backend)


;;; Class rudel-protocol-backend
;;

(defclass rudel-protocol-backend (rudel-backend)
  ()
  "Interface implemented by protocol backends."
  :abstract t)

(cl-defgeneric rudel-ask-connect-info ((this rudel-protocol-backend)
				    &optional info)
  "Retrieve information for joining a session from user.
When INFO is non-nil, augment INFO to produce new list.
Return a property list that contains the collected information.")

(cl-defgeneric rudel-connect ((this rudel-protocol-backend) transport
			   info info-callback
			   &optional progress-callback)
  "Create a new connection through TRANSPORT according to the data in INFO.
TRANSPORT has to be an object of a class derived from `rudel-transport'.
INFO has to be a property list.
INFO-CALLBACK has to be a function of two arguments which will be
bound to THIS and INFO. When called, INFO-CALLBACK should return
a modified version of the INFO argument in which no information
is missing.
When non-nil, PROGRESS-CALLBACK has to be a function that may be
called repeatedly while the connection is established.

Implementations can rely on the fact that the property :session
in INFO contains the `rudel-session' object to which the new
connection will be associated.")

(cl-defgeneric rudel-ask-host-info ((this rudel-protocol-backend)
				 &optional info)
  "Retrieve information for hosting a session from user.
When INFO is non-nil, augment INFO to produce new list.
Return a property list that contains the collected information.")

(cl-defgeneric rudel-host ((this rudel-protocol-backend) backend
			info)
  "Create a new session according to the property list INFO.
BACKEND has to be an object of a class derived from
`rudel-transport-backend' and has to have the listen
capability.
The created session object is returned.")

(cl-defgeneric rudel-make-document ((this rudel-protocol-backend)
				 name session)
  "Create a new document object named NAME for SESSION.")

(provide 'rudel-protocol)
;;; rudel-protocol.el ends here
