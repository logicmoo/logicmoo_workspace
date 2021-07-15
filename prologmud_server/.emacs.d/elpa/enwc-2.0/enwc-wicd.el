;;; enwc-wicd.el --- The Wicd backend to ENWC

;; Copyright (C) 2012-2017 Free Software Foundation, Inc.

;; Author: Ian Dunn <dunni@gnu.org>
;; Keywords: external, network, wicd, manager, nm
;; Version: 2.0
;; Homepage: https://savannah.nongnu.org/p/enwc

;; This file is part of GNU Emacs

;; GNU Emacs is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;;; Commentary:

;; Wicd is one of the default supported back-ends for ENWC.

;;; Code:

(require 'enwc-backend)
(require 'enwc)
(require 'dbus)

(defgroup enwc-wicd nil
  "*Wicd variables for ENWC."
  :prefix "enwc-wicd-"
  :group 'enwc)

(defcustom enwc-wicd-dbus-service "org.wicd.daemon"
  "The wicd D-Bus service identifier."
  :group 'enwc-wicd
  :type 'string)

(defcustom enwc-wicd-dbus-wireless-path "/org/wicd/daemon/wireless"
  "The wicd wireless D-Bus path."
  :group 'enwc-wicd
  :type 'string)

(defcustom enwc-wicd-dbus-wireless-interface "org.wicd.daemon.wireless"
  "The wicd wireless D-Bus interface."
  :group 'enwc-wicd
  :type 'string)

(defcustom enwc-wicd-dbus-wired-path "/org/wicd/daemon/wired"
  "The wicd wired D-Bus path."
  :group 'enwc-wicd
  :type 'string)

(defcustom enwc-wicd-dbus-wired-interface "org.wicd.daemon.wired"
  "The wicd wired D-Bus interface."
  :group 'enwc-wicd
  :type 'string)

(defvar enwc-wicd-details-list
  '("essid" "bssid" "quality" "encryption" "channel")
  "The list of the desired details to be obtained from each network.")

(defvar enwc-wicd-current-ap ""
  "Current access point.
UNUSED")

(defvar enwc-wicd-current-nw-id -1
  "Id of the current network.
UNUSED")

(defvar enwc-wicd-end-scan-signal nil
  "D-Bus signal object for the \"SendEndScanSignal\" signal.")

(defvar enwc-wicd-status-changed-signal nil
  "D-Bus signal objects for the \"StatusChanged\" signal.")

(defun enwc-wicd-dbus-wireless-call-method (method &rest args)
  "Calls D-Bus method METHOD with arguments ARGS within
the wicd wireless interface."
  (apply 'dbus-call-method :system
         enwc-wicd-dbus-service
         enwc-wicd-dbus-wireless-path
         enwc-wicd-dbus-wireless-interface
         method
         :timeout 25000
         args))

(defun enwc-wicd-dbus-wired-call-method (method &rest args)
  "Calls D-Bus method METHOD with arguments ARGS within
the wicd wired interface."
  (apply 'dbus-call-method :system
         enwc-wicd-dbus-service
         enwc-wicd-dbus-wired-path
         enwc-wicd-dbus-wired-interface
         method
         :timeout 25000
         args))

;;;;;;;;;;
;; Scan ;;
;;;;;;;;;;

(defun enwc-wicd-scan ()
  "Wicd scan function."
  (enwc-wicd-dbus-wireless-call-method "Scan"))

;;;;;;;;;;;;;;;;;;
;; Get networks ;;
;;;;;;;;;;;;;;;;;;

(defun enwc-wicd-get-networks (&optional wired)
  (if wired
      (enwc-wicd-get-wired-profiles)
    (enwc-wicd-get-wireless-networks)))

(defun enwc-wicd-get-wireless-networks ()
  "Wicd get networks function.
Just returns a number sequence."
  (number-sequence 0 (1- (enwc-wicd-dbus-wireless-call-method "GetNumberOfNetworks"))))

(defun enwc-wicd-get-wired-profiles ()
  "Get the list of wired network profiles."
  (enwc-wicd-dbus-wired-call-method "GetWiredProfileList"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Get network properties ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar enwc-wicd-prop-values nil)
(defvar enwc-wicd-prop-num 0)
(defvar enwc-wicd-prop-timeout 3)

(defun enwc-wicd-nw-prop-handler (prop &rest args)
  "The handler for `enwc-wicd-get-wireless-network-property'.
This receives the value of network property PROP,
and appends the value to `enwc-wicd-prop-values'."
  (push (cons prop (car args)) enwc-wicd-prop-values)
  (setq enwc-wicd-prop-num (1+ enwc-wicd-prop-num)))

(defun enwc-wicd-prop-to-prop (prop)
  "Convert a Wicd network property to an ENWC network property."
  (cond
   ((equal prop "essid") 'essid)
   ((equal prop "bssid") 'bssid)
   ((equal prop "quality") 'strength)
   ((equal prop "encryption") 'encrypt)
   ((equal prop "mode") 'mode)
   ((equal prop "channel") 'channel)))

(defun enwc-wicd-get-wireless-network-property (id prop)
  "Wicd get wireless network property function.
This calls the D-Bus method on Wicd to get the property PROP
from wireless network with id ID."
  (dbus-call-method-asynchronously :system
                                   enwc-wicd-dbus-service
                                   enwc-wicd-dbus-wireless-path
                                   enwc-wicd-dbus-wireless-interface
                                   "GetWirelessProperty"
                                   `(lambda (x) (enwc-wicd-nw-prop-handler ,prop x))
                                   :timeout 1000
                                   :int32 id
                                   :string prop))

(defun enwc-wicd-build-prop-list (prop-list det-list)
  (mapcar
   (lambda (det)
     (let* ((cur-prop (assoc det prop-list))
            (act-det (enwc-wicd-prop-to-prop det))
            (act-prop (when cur-prop (cdr cur-prop))))
       (when (or (string-equal det "essid")
                 (string-equal det "channel"))
         (setq act-prop (string-to-number cur-prop)))
       (cons act-det act-prop)))
   det-list))

(defun enwc-wicd-get-wireless-nw-props (id)
  "Get the network properties of a network.
This function returns an associative list of properties
for the network with id ID.
For a list of properties, see `enwc-wicd-details-list'."
  (setq enwc-wicd-prop-values nil)
  (setq enwc-wicd-prop-num 0)
  (dolist (x enwc-wicd-details-list)
    (enwc-wicd-get-wireless-network-property id x))

  (with-timeout (enwc-wicd-prop-timeout)
    (while (< enwc-wicd-prop-num (length enwc-wicd-details-list))
      (read-event nil nil 0.001)))

  (if (assoc "encryption" enwc-wicd-prop-values)
      (let ((enc-type (enwc-wicd-get-encryption-type id)))
        (setcdr (assoc "encryption" enwc-wicd-prop-values)
                (or enc-type "Unsecured")))
    (setq enwc-wicd-prop-values
          (cons (cons "encryption" "Unsecured")
                enwc-wicd-prop-values)))

  (enwc-wicd-build-prop-list enwc-wicd-prop-values enwc-wicd-details-list))

(defalias 'enwc-wicd-get-wireless-network-props 'enwc-wicd-get-wireless-nw-props)

(defun enwc-wicd-get-encryption-type (id)
  "Wicd get encryption type function.
This calls the D-Bus method on Wicd to get the encryption_method
property from wireless network with id ID."
  (enwc-wicd-dbus-wireless-call-method "GetWirelessProperty"
                                       id "encryption_method"))

(defun enwc-wicd-get-wired-nw-prop (id det)
  "Get property DET from the wired network with id ID."
  (enwc-wicd-dbus-wired-call-method "GetWiredProperty" id det))

(defun enwc-wicd-get-wired-nw-props (id)
  ;; TODO Do wicd wired profiles have names?
  (ignore id)
  `((name . "Wired Profile")))

(defun enwc-wicd-get-network-props (id &optional wired)
  (if wired
      (enwc-wicd-get-wired-nw-props id)
    (enwc-wicd-get-wireless-network-props id)))

;;;;;;;;;;;;;;;;;;;;;;;
;; Connect Functions ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defun enwc-wicd-connect (id &optional wired)
  "Wicd connect function.
This calls the D-Bus method on Wicd to connect to a
wired or wireless network with id ID."
  (if wired
      (enwc-wicd-wired-connect id)
    (enwc-wicd-dbus-wireless-call-method "ConnectWireless" id)))

(defun enwc-wicd-wireless-connect (id)
  "Wicd connect function.
This calls the D-Bus method on Wicd to connect to a
wireless network with id ID."
  (enwc-wicd-dbus-wireless-call-method "ConnectWireless" id))

(defun enwc-wicd-wired-connect (id)
  "Connect to the wired network with profile id ID."
  (let* ((profs (enwc-wicd-get-wired-profiles))
         (prf (nth id profs)))
    (enwc-wicd-dbus-wired-call-method "ReadWiredNetworkProfile" prf)
    (enwc-wicd-dbus-wired-call-method "ConnectWired")))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Disconnect functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun enwc-wicd-disconnect (&optional wired)
  (if wired
      (enwc-wicd-wired-disconnect)
    (enwc-wicd-wireless-disconnect)))

(defun enwc-wicd-wireless-disconnect ()
  "Wicd disconnect function."
  (enwc-wicd-dbus-wireless-call-method "DisconnectWireless"))

(defun enwc-wicd-wired-disconnect ()
  "Disconnect from the wired connection."
  (enwc-wicd-dbus-wired-call-method "DisconnectWired"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Get current network id ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun enwc-wicd-get-current-nw-id (wired-p)
  "Wicd get current network id function.
The current network id is updated upon connect,
so this jut returns the tracked network id."
  (let ((ap (enwc-wicd-dbus-wireless-call-method "GetCurrentNetworkID")))
    (cond
     (wired-p 'wired)
     ((< ap 0) nil)
     (t ap))))

;;;;;;;;;;;;;;;;;;;;;;
;; Check Connecting ;;
;;;;;;;;;;;;;;;;;;;;;;

(defun enwc-wicd-check-connecting ()
  "Return non-nil if currently connecting to a network."
  (enwc-wicd-dbus-wireless-call-method "CheckIfWirelessConnecting"))

;;;;;;;;;;;;;
;; Is Wired
;;;;;;;;;;;;;

(defun enwc-wicd-is-wired ()
  "Return non-nil if connected to a wired network."
  (not (not (enwc-wicd-dbus-wired-call-method "GetWiredIP"))))

;;;;;;;;;;;;;;;;;;;;;;
;; Get Profile Info ;;
;;;;;;;;;;;;;;;;;;;;;;

;; (defun enwc-wicd-get-profile-ent (id ent wired)
;;   "Get profile entry ENT from the network with id ID.
;; WIRED is set to indicate whether this is a wired network.
;; This function is a wrapper around the *-get-(wired|wireless)-nw-prop
;; functions, allowing for a single function that checks for wired."
;;   (if wired
;;       (enwc-wicd-get-wired-nw-prop id ent)
;;     (enwc-wicd-dbus-wireless-call-method "GetWirelessProperty" id ent)))

;; (defun enwc-wicd-get-profile-info (id &optional wired)
;;   "Get the profile for profile ID.
;; WIRED is set to indicate whether this is a wired network."
;;   (let ((dns-list (enwc-wicd-get-dns id wired))
;;         (sec-info (enwc-wicd-get-profile-sec-info id wired)))
;;     `((addr . ,(enwc-wicd-get-ip-addr id wired))
;;       (netmask . ,(enwc-wicd-get-netmask id wired))
;;       (gateway . ,(enwc-wicd-get-gateway id wired))
;;       (dns1 . ,(nth 0 dns-list))
;;       (dns2 . ,(nth 1 dns-list))
;;       ,@sec-info)))

;; (defun enwc-wicd-get-profile-sec-info (id &optional wired)
;;   "Get the security info for profile with id ID.
;; WIRED is set to indicate whether this is a wired network."
;;   (remq nil
;;    (mapcar
;;     (lambda (ent)
;;       (let ((info (enwc-wicd-get-profile-ent wired id (symbol-name (car ent)))))
;;         (if info
;;             (cons (car ent) info)
;;           nil)))
;;     enwc-supplicant-alist)))

;; (defun enwc-wicd-get-ip-addr (id wired)
;;   "Get the IP Address from the network with id ID.
;; Wired is set to indicate whether this is a wired network."
;;   (or (enwc-wicd-get-profile-ent id "ip" wired) ""))

;; (defun enwc-wicd-get-netmask (id wired)
;;   "Get the Netmask from the network with id ID.
;; WIRED is set to indicate whether this is a wired network."
;;   (or (enwc-wicd-get-profile-ent id "netmask" wired) ""))

;; (defun enwc-wicd-get-gateway (id wired)
;;   "Get the Gateway from the network with id ID.
;; WIRED is set to indicate whether this is a wired network."
;;   (or (enwc-wicd-get-profile-ent id "gateway" wired) ""))

;; (defun enwc-wicd-get-dns (id wired)
;;   "Get the list of DNS servers from the network with id ID.
;; WIRED is set to indicate whether this is a wired network."
;;   (list (or (enwc-wicd-get-profile-ent id "dns1" wired) "")
;;         (or (enwc-wicd-get-profile-ent id "dns2" wired) "")
;;         (or (enwc-wicd-get-profile-ent id "dns3" wired) "")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Save Network Settings ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun enwc-wicd-set-nw-prop (wired id prop val)
  "Set the network property PROP of the network with id ID
to VAL.
WIRED indicates whether this is a wired network."
  (if wired
      (enwc-wicd-dbus-wired-call-method "SetWiredProperty"
                                        id prop val)
    (enwc-wicd-dbus-wireless-call-method "SetWirelessProperty"
                                         id prop val)))

(defun enwc-wicd-save-nw-profile (wired id)
  "Save the network profile with for the network with id ID.
WIRED indicates whether this is a wired network."
  (if wired
      (enwc-wicd-dbus-wired-call-method "SaveWiredNetworkProfile" id)
    (enwc-wicd-dbus-wireless-call-method "SaveWirelessNetworkProfile" id)))

(defun enwc-wicd--phase-to-string (phase)
  "Convert a list of phase settings PHASE into a string.

The format of this string is \"ENT0=VAL0 ENT1=VAL1 ... ENTN=VALN\",
for each entry in PHASE."
  (mapconcat
   (lambda (ent)
     (format "%s=%s" (car ent) (cdr ent)))
   phase " "))

(defun enwc-wicd-save-nw-settings (id settings &optional wired)
  "Set the settings for network ID to SETTINGS and save the profile.
The association list SETTINGS contains the settings for the network.
WIRED indicates whether or not ID is a wired connection."
  (dolist (setting settings)
    (let (key ent)
      ;; There are four special cases in here:
      ;;  1. IP Address, which has a different key.
      ;;  2. Encryption type, which has a different key and value.
      ;;  3,4. Phases, both of which need to have their results concatenated.
      (pcase (car setting)
        ('addr (setq key "ip"
                     ent (cdr setting)))
        ('sec-type (setq key "enctype"
                         ent (if (cdr setting)
                                 (symbol-name (cdr setting))
                               "None")))
        ('phase1 (setq key "phase1"
                       ent (enwc-wicd--phase-to-string (cdr setting))))
        ('phase2 (setq key "phase2"
                       ent (enwc-wicd--phase-to-string (cdr setting))))
        (_ (setq key (symbol-name (car setting))
                 ent (or (cdr setting) ""))))
      (enwc-wicd-set-nw-prop wired id key ent)))

  (enwc-wicd-save-nw-profile wired id))

(defun enwc-wicd-wireless-prop-changed (state info)
  "Callback for when the network status changes."
  (when state
    (if (eq state 0)
        (setq enwc-wicd-current-ap ""
              enwc-wicd-current-nw-id nil)
      (setq enwc-wicd-current-ap (car (cadr info))
            enwc-wicd-current-nw-id (and info
                                         (nthcdr 3 info)
                                         (caar (nthcdr 3 info))
                                         (string-to-number (caar (nthcdr 3 info))))))))


; ;;;;;;;;;;;;;;;;; ;
; ;; Load/Unload ;; ;
; ;;;;;;;;;;;;;;;;; ;

(defun enwc-wicd-load ()
  "Load the Wicd backend."
  ;; Thanks to Michael Albinus for pointing out this signal.
  (setq enwc-wicd-end-scan-signal
        (dbus-register-signal :system
                              enwc-wicd-dbus-service
                              enwc-wicd-dbus-wireless-path
                              enwc-wicd-dbus-wireless-interface
                              "SendEndScanSignal"
                              'enwc-process-scan))

  (setq enwc-wicd-status-changed-signal
        (dbus-register-signal :system
                              enwc-wicd-dbus-service
                              "/org/wicd/daemon"
                              enwc-wicd-dbus-service
                              "StatusChanged"
                              'enwc-wicd-wireless-prop-changed)))

(defun enwc-wicd-unload ()
  "Unload the Wicd back-end.

Unregister all of the D-Bus signals set up during load."
  (dbus-unregister-object enwc-wicd-end-scan-signal)
  (dbus-unregister-object enwc-wicd-status-changed-signal))

(defun enwc-wicd-can-load-p ()
  (dbus-ping :system enwc-wicd-dbus-service))

(enwc-register-backend
 (make-enwc-backend
  :key 'enwc
  :can-load-p #'enwc-wicd-can-load-p
  :load #'enwc-wicd-load
  :unload #'enwc-wicd-unload
  :network-ids #'enwc-wicd-get-networks
  :scan #'enwc-wicd-scan
  :connect #'enwc-wicd-connect
  :disconnect #'enwc-wicd-disconnect
  :current-nw-id #'enwc-wicd-get-current-nw-id
  :is-connecting-p #'enwc-wicd-check-connecting
  :nw-props #'enwc-wicd-get-network-props
  :is-wired-p #'enwc-wicd-is-wired))

(provide 'enwc-wicd)

;;; enwc-wicd.el ends here.
