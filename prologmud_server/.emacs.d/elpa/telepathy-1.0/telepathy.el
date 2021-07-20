;;; telepathy.el --- Access Telepathy from Emacs

;; Copyright (C) 2013 Nicolas Petton
;;
;; Author: Nicolas Petton <petton.nicolas@gmail.com>
;; Keywords: telepathy tools
;; Package-Version: 1.0
;; Package-Commit: 211d785b02a29ddc254422fdcc3db45262582f8c
;; Package: telepathy
;; Version: 0.1

;; Telepathy.el is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; Telepathy is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;

;;; Code:


(require 'dbus)

(defvar telepathy-account-manager-iface
  "org.freedesktop.Telepathy.AccountManager")

(defvar telepathy-account-manager-path
  "/org/freedesktop/Telepathy/AccountManager")

(defvar telepathy-account-iface
  "org.freedesktop.Telepathy.Account")

(defvar telepathy-simple-presence-iface
  "org.freedesktop.Telepathy.Connection.Interface.SimplePresence")

(defun telepathy-path-to-iface (path)
  (substring (replace-regexp-in-string "/" "." path) 1))

(defun telepathy-get-valid-accounts ()
  "Get a list of valid telepathy accounts D-Bus path"
  (dbus-get-property 
   :session 
   telepathy-account-manager-iface
   telepathy-account-manager-path
   telepathy-account-manager-iface
   "ValidAccounts"))

(defun telepathy-get-account-presence (account-path)
  "Get the telepathy presence of the ACCOUNT-PATH D-Bus object path.
   See http://telepathy.freedesktop.org/spec/Connection_Interface_Simple_Presence.html#Mapping:Simple_Status_Spec_Map"
  (dbus-get-property
   :session
   telepathy-account-manager-iface
   account-path
   telepathy-account-iface
   "CurrentPresence"))

(defun telepathy-get-account-connection (account-path)
  "Get the telepathy connection path of the ACCOUNT-PATH D-Bus object path"
  (dbus-get-property
   :session
   telepathy-account-manager-iface
   account-path
   telepathy-account-iface
   "Connection"))

(defun telepathy-get-valid-accounts-presence ()
  "Get all valid accounts presence"
  (mapcar (lambda (account-path) 
	    (telepathy-get-account-presence account-path))
	  (telepathy-get-valid-accounts)))

(defun telepathy-get-account-statuses (account-path)
  "Get the Statuses property for ACCOUNT-PATH"
  (let* ((connection-path (telepathy-get-account-connection account-path))
	 (connection-iface (telepathy-path-to-iface connection-path)))
    (dbus-get-property
       :session
       connection-iface
       connection-path
       telepathy-simple-presence-iface
       "Statuses")))

(defun telepathy-set-account-presence (account-path status status-message)
  "STATUS is the string identifier of the desired status. Possible status identifiers are defined in the Statuses property.
   See http://telepathy.freedesktop.org/spec/Connection_Interface_Simple_Presence.html#Property:Statuses"
  (let* ((connection-path (telepathy-get-account-connection account-path))
	 (connection-iface (telepathy-path-to-iface connection-path)))
    (unless (or (string-equal connection-path "/") ;; invalid account
		(= (car (telepathy-get-account-presence account-path)) 0)) ;; offline
      (dbus-call-method
       :session
       connection-iface
       connection-path
       telepathy-simple-presence-iface
       "SetPresence"
       status
       status-message))))

(defun telepathy-set-valid-accounts-presence (status message)
  (let ((accounts (telepathy-get-valid-accounts)))
    (dolist (account-path accounts)
      (telepathy-set-account-presence account-path status message))))

(provide 'telepathy)

;;; telepathy.el ends here
