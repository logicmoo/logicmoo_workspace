;;; enwc-nm.el - The NetworkManager backend to ENWC

;; Copyright (C) 2012-2017 Free Software Foundation, Inc.

;; Author: Ian Dunn <dunni@gnu.org>
;; Keywords: external, network, wicd, manager, nm
;; Version: 2.0
;; Homepage: https://savannah.nongnu.org/p/enwc

;; This file is part of GNU Emacs.

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
;;
;;   This requires NetworkManager >= 0.9.6
;;

(require 'enwc-backend)
(require 'enwc)
(require 'dbus)
(require 'subr-x)

(defgroup enwc-nm nil
  "*NetworkManager variables for ENWC"
  :prefix "enwc-nm-"
  :group 'enwc)

(defcustom enwc-nm-dbus-service "org.freedesktop.NetworkManager"
  "NetworkManager D-Bus service."
  :group 'enwc-nm
  :type 'string)

(defcustom enwc-nm-dbus-path "/org/freedesktop/NetworkManager"
  "The default D-Bus path for NetworkManager."
  :group 'enwc-nm
  :type 'string)

(defcustom enwc-nm-dbus-settings-path "/org/freedesktop/NetworkManager/Settings"
  "The settings D-Bus path for NetworkManager."
  :group 'enwc-nm
  :type 'string)

(defcustom enwc-nm-dbus-interface "org.freedesktop.NetworkManager"
  "The default D-Bus interface for NetworkManager."
  :group 'enwc-nm
  :type 'string)

(defcustom enwc-nm-dbus-wireless-interface "org.freedesktop.NetworkManager.Device.Wireless"
  "The wireless D-Bus interface for NetworkManager."
  :group 'enwc-nm
  :type 'string)

(defcustom enwc-nm-dbus-wired-interface "org.freedesktop.NetworkManager.Device.Wired"
  "The wired D-Bus interface for NetworkManager."
  :group 'enwc-nm
  :type 'string)

(defcustom enwc-nm-dbus-device-interface "org.freedesktop.NetworkManager.Device"
  "The device D-Bus interface for NetworkManager."
  :group 'enwc-nm
  :type 'string)

(defcustom enwc-nm-dbus-accesspoint-interface "org.freedesktop.NetworkManager.AccessPoint"
  "The access point D-Bus interface for NetworkManager."
  :group 'enwc-nm
  :type 'string)

(defcustom enwc-nm-dbus-settings-interface "org.freedesktop.NetworkManager.Settings"
  "The settings D-Bus interface for NetworkManager."
  :group 'enwc-nm
  :type 'string)

(defcustom enwc-nm-dbus-connections-interface "org.freedesktop.NetworkManager.Settings.Connection"
  "The connections D-Bus interface for NetworkManager."
  :group 'enwc-nm
  :type 'string)

(defcustom enwc-nm-dbus-active-interface "org.freedesktop.NetworkManager.Connection.Active"
  "The active connection D-Bus interface for NetworkManager."
  :group 'enwc-nm
  :type 'string)

;;TODO: Make this customizable.
(defconst enwc-nm-dbus-settings-groups
  '(("802-11-wireless-security" . ("name"
                                   "key-mgmt"
                                   "wep-tx-keyidx"
                                   "auth-alg"
                                   "proto"
                                   "pairwise"
                                   "group"
                                   "leap-username"
                                   "wep-key0"
                                   "wep-key1"
                                   "wep-key2"
                                   "wep-key3"
                                   "wep-key-type"
                                   "psk"
                                   "leap-password"))
    ("ipv4" . ("addresses"
               "dns"))))

(defvar enwc-nm-wired-dev nil
  "The wired device object path.")

(defvar enwc-nm-wireless-dev nil
  "The wireless device object path.")

(defvar enwc-nm-active-ap nil
  "The active access point object path.")

(defvar enwc-nm-connecting-p nil
  "Whether or not NetworkManager is connecting.")

(defvar enwc-nm-wired-p nil
  "Whether or not NetworkManager is wired.")

(defvar enwc-nm-edit-info nil
  "The information for the network connection being edited.")

;; D-Bus Signals:

(defvar enwc-nm-access-point-added-signal nil
  "D-Bus signal object for the \"AccessPointAdded\" signal.")

(defvar enwc-nm-access-point-removed-signal nil
  "D-Bus signal object for the \"AccessPointRemoved\" signal.")

(defvar enwc-nm-properties-changed-signal nil
  "D-Bus signal object for the \"PropertiesChanged\" signal.")

(defvar enwc-nm-wired-state-changed-signal nil
  "D-Bus signal object for the \"StateChanged\" signal for the wired device.")

(defvar enwc-nm-state-changed-signal nil
  "D-Bus signal object for the \"StateChanged\" signal for the default path.")

(defun enwc-nm-dbus-call-method (method &optional path interface &rest args)
  (unless path (setq path enwc-nm-dbus-path))
  (unless interface (setq interface enwc-nm-dbus-interface))
  (apply 'dbus-call-method :system
         enwc-nm-dbus-service
         path
         interface
         method
         :timeout 25000
         args))

(defun enwc-nm-dbus-default-call-method (method &rest args)
  (apply 'enwc-nm-dbus-call-method method nil nil args))

(defun enwc-nm-dbus-settings-call-method (method &rest args)
  (apply 'enwc-nm-dbus-call-method
         method
         enwc-nm-dbus-settings-path
         enwc-nm-dbus-settings-interface
         args))

(defun enwc-nm-dbus-wireless-call-method (method &rest args)
  (apply 'enwc-nm-dbus-call-method
         method
         enwc-nm-wireless-dev
         enwc-nm-dbus-wireless-interface
         args))

(defun enwc-nm-dbus-wired-call-method (method &rest args)
  (apply 'enwc-nm-dbus-call-method
         method
         enwc-nm-wired-dev
         enwc-nm-dbus-wired-interface
         args))

(defun enwc-nm-get-settings (conn)
  "Get the connection settings from CONN."
  (enwc-nm-dbus-call-method "GetSettings" conn
                            enwc-nm-dbus-connections-interface))

(defun enwc-nm-list-connections ()
  "List the connections."
  (enwc-nm-dbus-settings-call-method "ListConnections"))

;; Default
(defun enwc-nm-get-device-by-name (name)
  (enwc-nm-dbus-default-call-method "GetDeviceByIpIface" :string name))

(defun enwc-nm--ap-to-conn (nw)
  "Get the connection that corresponds to NW."
  (let ((ap-ssid (dbus-byte-array-to-string
                  (enwc-nm-get-wireless-network-property nw "Ssid")))
        (profile-table (make-hash-table :test #'equal)))
    ;; Create a hash table of connections, indexed by ssid
    ;; TODO: Store this somewhere else
    (dolist (conn (enwc-nm-list-connections))
      (let ((settings (enwc-nm-get-settings conn)))
        (map-put profile-table
                 (dbus-byte-array-to-string (enwc-nm-get-dbus-dict-entry
                                             "802-11-wireless/ssid"
                                             settings))
                 conn)))
    (map-elt profile-table ap-ssid)))

(defun enwc-nm-connection-p (conn)
  "Return non-nil if CONN is a connection object."
  (and conn
       (stringp conn)
       (string-match "^/org/freedesktop/NetworkManager/Settings/[0-9]+$" conn)))

(defun enwc-nm--profile-wired-p (conn)
  "Return non-nil if CONN is a wired profile."
  (let ((props (enwc-nm-get-settings conn)))
    (string= (enwc-nm-get-dbus-dict-entry "connection/type" props)
             "802-3-ethernet")))

;;;;;;;;;;;;;;;;;;
;; Get networks ;;
;;;;;;;;;;;;;;;;;;

(defun enwc-nm-get-networks (&optional wired)
  (if wired
      (enwc-nm-get-wired-profiles)
    (enwc-nm-get-wireless-networks)))

(defun enwc-nm-get-wireless-networks ()
  "The NetworkManager get networks function.
This returns a list of D-Bus paths to the access points."
  (enwc-nm-dbus-wireless-call-method "GetAccessPoints"))

(defun enwc-nm-get-wired-profiles ()
  (let ((profs-list (enwc-nm-list-connections)))
    (cl-remove-if-not #'enwc-nm--profile-wired-p profs-list)))

;;;;;;;;;;;;;
;; Connect ;;
;;;;;;;;;;;;;

(defun enwc-nm-connect (nw &optional wired)
  "The NetworkManager connect function.
This gets the connection path from NW, and connects to it."
  (if wired
      (enwc-nm-wired-connect nw)
    (enwc-nm-wireless-connect nw)))

(defun enwc-nm-wireless-connect (nw)
  (when-let ((conn (enwc-nm--ap-to-conn nw)))
    (enwc-nm-dbus-default-call-method "ActivateConnection"
                                      :object-path conn
                                      :object-path enwc-nm-wireless-dev
                                      :object-path conn)))

(defun enwc-nm-wired-connect (nw)
  (enwc-nm-dbus-default-call-method "ActivateConnection"
                                    :object-path nw
                                    :object-path enwc-nm-wired-dev
                                    :object-path nw))

;;;;;;;;;;;;;;;;
;; Disconnect ;;
;;;;;;;;;;;;;;;;

(defun enwc-nm-disconnect (&optional wired)
  (if wired
      (enwc-nm-wired-disconnect)
    (enwc-nm-disconnect-wireless)))

;; Device
(defun enwc-nm-disconnect-wireless ()
  (enwc-nm-dbus-call-method "Disconnect"
                            enwc-nm-wireless-dev
                            enwc-nm-dbus-device-interface))

(defun enwc-nm-wired-disconnect ()
  (enwc-nm-dbus-call-method "Disconnect"
                            enwc-nm-wired-dev
                            enwc-nm-dbus-device-interface))

;;;;;;;;;;
;; Scan ;;
;;;;;;;;;;

(defun enwc-nm-scan (&optional wired)
  "The NetworkManager scan function."
  (let ((dev (if wired
                 enwc-nm-wired-dev
               enwc-nm-wireless-dev))
        (interface (if wired
                       enwc-nm-dbus-wired-interface
                     enwc-nm-dbus-wireless-interface)))
    (enwc-nm-dbus-call-method "RequestScan"
                              dev interface
                              '(:array :signature "{sv}"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Get network properties ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun enwc-nm-get-wireless-network-property (nw prop)
  "The NetworkManager get wireless network property function.
This runs like normal, using element ID of `enwc-access-points'
 to determine the access point path, then obtains the property
PROP from that access point.  It also sets the channel from the
 frequency if necessary."
  (dbus-get-property :system
                     enwc-nm-dbus-service
                     nw
                     enwc-nm-dbus-accesspoint-interface
                     prop))

(defun enwc-nm--freq-to-channel (freq)
  "Convert a frequency FREQ into a channel."
  (1+ (/ (- freq 2412) 5)))

(defun enwc-nm-get-wireless-nw-props (nw)
  "Get the network properties for the network NW."
  (let ((props (dbus-get-all-properties :system
                                        enwc-nm-dbus-service
                                        nw
                                        enwc-nm-dbus-accesspoint-interface)))

    `((essid    . ,(dbus-byte-array-to-string (cdr (assoc "Ssid" props))))
      (bssid    . ,(cdr (assoc "HwAddress" props)))
      (strength . ,(cdr (assoc "Strength" props)))
      (encrypt  . ,(or (enwc-nm-get-encryption-type nw) "Unsecured"))
      (channel  . ,(enwc-nm--freq-to-channel
                    (cdr (assoc "Frequency" props)))))))

(defun enwc-nm-get-encryption-type (nw)
  "The NetworkManager get encryption type function.
This gets the WPA flags and RSN flags from access point in NW.
If both are 0, then it returns WEP, otherwise WPA."
  (let ((wpa-flags (enwc-nm-get-wireless-network-property nw "WpaFlags"))
        (rsn-flags (enwc-nm-get-wireless-network-property nw "RsnFlags")))
    (if (and (= wpa-flags 0) (= rsn-flags 0))
        "WEP"
      "WPA")))

(defun enwc-nm-get-wired-nw-props (nw)
  (let ((settings (enwc-nm-get-settings nw)))
    `((name . ,(enwc-nm-get-dbus-dict-entry "connection/id" settings)))))

(defun enwc-nm-get-nw-props (nw &optional wired-p)
  (if wired-p
      (enwc-nm-get-wired-nw-props nw)
    (enwc-nm-get-wireless-nw-props nw)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Get Current network id ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun enwc-nm-wireless-prop-changed (props)
  "Called when network properties are changed.
PROPS is a list of updated properties."
  (let ((ap (assoc "ActiveAccessPoint" props)))
    (when ap
      (setq enwc-nm-active-ap (car (cadr ap))))))

(defun enwc-nm-get-current-nw-id (wired)
  "The NetworkManager get current network id function.
This simply checks for the active access point."
  (cond
   (wired 'wired)
   ((string= enwc-nm-active-ap "/") nil)
   (t enwc-nm-active-ap)))

(defun enwc-nm-prop-changed (state)
  "Called when NetworkManager's state is changed.
STATE is the new state

If STATE is 40, then NetworkManager is connecting to a new AP."
  (setq enwc-nm-connecting-p (eq state 40)))

;;;;;;;;;;;;;;;;;;;;;;
;; Check Connecting ;;
;;;;;;;;;;;;;;;;;;;;;;

(defun enwc-nm-check-connecting ()
  "The NetworkManager check connecting function."
  enwc-nm-connecting-p)

(defun enwc-nm-dev-prop-changed (new-state old-state reason)
  (setq enwc-nm-wired-p (eq new-state 100)))

(defun enwc-nm-is-wired ()
  enwc-nm-wired-p)

;;;;;;;;;;;;;;;;;;;;;;
;; Profile Handling ;;
;;;;;;;;;;;;;;;;;;;;;;

(defun enwc-nm-gen-uuid ()
  "Generate a UUID."
  (random t)
  (apply 'format
         "%04x%04x-%04x-%04x-%04x-%04x%04x%04x"
         (mapcar (lambda (x)
                   (random 65535))
                 (number-sequence 0 7))))

(defmacro enwc-nm--hex-substring (str st ed)
  "Get a standard integer from hex string STR starting at ST and ending st ED"
  `(string-to-number (substring ,str ,st ,ed) 16))

(defun enwc-nm-convert-addr (addr)
  "Convert an address ADDR from an integer in network byte order to a string."
  (if (and addr (integerp addr))
      (let* ((hex-addr (format "%08x" addr))
             (subs (mapcar
                    (lambda (n)
                      (enwc-nm--hex-substring hex-addr n (+ n 2)))
                    (number-sequence 6 0 -2))))
        (apply 'format "%i.%i.%i.%i" subs))
    ""))

(defun enwc-nm-addr-back (addr)
  "Convert an IP address ADDR in dots notation to an integer."
  (cl-check-type addr string)
  (let* ((bytes (split-string addr "\\."))
         (byte-string (mapcar
                       (lambda (n) (lsh (string-to-number (nth n bytes))
                                   (* 8 n)))
                       (number-sequence 0 3))))
    (apply 'logior byte-string)))

;; These next two come from libnm-util/nm-utils.c in NM's source.

;; (defun enwc-nm-netmask-to-prefix (netmask)
;;   "Convert a netmask to a CIDR prefix.
;; NETMASK is an ip address in network byte order."
;;   (if (and netmask (integerp netmask))
;;       (progn
;;         (setq netmask (enwc--htonl netmask))
;;         (while (cl-evenp netmask)
;;           (setq netmask (lsh netmask -1)))
;;         (floor (log (1+ netmask) 2)))
;;     0))

;; (defun enwc-nm-prefix-to-netmask (prefix)
;;   "Convert a CIDR prefix to a netmask.
;; PREFIX is an integer <= 32."
;;   (if (and prefix (integerp prefix))
;;       (progn
;;         (setq prefix (min prefix 32))
;;         (enwc--htonl (lsh (1- (expt 2 prefix)) (- 32 prefix))))
;;     0))

;;;;;;;;;;;;;;;;;;;;;;
;; D-Bus Conversion ;;
;;;;;;;;;;;;;;;;;;;;;;

(defun enwc-nm-get-dbus-dict-entry (entry dict)
  "Get an entry ENTRY from D-Bus dictionary DICT.

ENTRY is in the form LVL1/LVL2/.../LVLN, where each LVLi is a string
representing another layer in the dictionary."
  (cl-check-type entry string)
  (cl-check-type dict list)
  (let ((ent-strs (split-string entry "/"))
        (cur-ent dict)
        cur-str)
    (while ent-strs
      (setq cur-str (pop ent-strs))
      (setq cur-ent (assoc cur-str cur-ent))
      (when cur-ent
        (setq cur-ent (cadr cur-ent))))
    (when cur-ent (car cur-ent))))

(defun enwc-nm--recurse-dbus-entry (dict value entries)
  "Look in DICT for ENTRIES, and set the final one to VALUE."
  (if (not entries)
      (setcar dict value)
    (let* ((cur-str (car entries))
           (cur-ent (assoc cur-str dict)))
      (unless cur-ent
        (if (equal dict '(nil))
            (setcar dict `(,cur-str (nil)))
          (nconc dict `((,cur-str (nil)))))
        (setq cur-ent (assoc cur-str dict)))
      (enwc-nm--recurse-dbus-entry (cadr cur-ent) value (cdr entries)))))

(defun enwc-nm-set-dbus-dict-entry (entry dict value)
  "Set entry ENTRY in D-Bus dictionary DICT to VALUE."
  (cl-check-type entry string)
  (cl-check-type dict list)
  (enwc-nm--recurse-dbus-entry dict value (split-string entry "/")))

(defun enwc-nm-convert-dict-list (dict-ent settings)
  "Convert a D-Bus dictionary entry DICT-ENT from SETTINGS to an alist."
  (mapcar
   (lambda (ent)
     (cons
      (intern (car ent))
      (cl-caadr ent)))
   (cadr (assoc dict-ent settings))))

(defun enwc-nm-get-sec-info (settings)
  "Get security information from SETTINGS."
  (append
   (enwc-nm-convert-dict-list "802-11-wireless-security" settings)
   (enwc-nm-convert-dict-list "802-1x" settings)))

(defun enwc-nm-pair-to-dbus-dict-ent (pair)
  "Convert PAIR into a D-Bus dictionary entry."
  (let ((str (car pair))
        (var (cdr pair)))
    `(:dict-entry (:string ,str :variant ,var))))

(defun enwc-nm-alist-to-dbus-dict (alist)
  "Convert ALIST into a D-Bus dictionary."
  (let (dict)
    (append
     '(:array)
     (dolist (pr alist dict)
       (setq dict (apply 'list dict (enwc-nm-pair-to-dbus-dict-ent pr)))))))

(defun enwc-nm-process-profile-info (settings prof-info)
  (dolist (ent prof-info settings)
    ;; Find the corresponding entry in settings, and set it to the new value.
    ;; Check 802-11-wireless-security, then 802-1x, then ipv4.
    (let ((ent-list '("802-11-wireless-security"
                      "ipv4"))
          cur-ent)
      (while ent-list
        (setq cur-ent (pop ent-list))
        (when (enwc-nm-set-dbus-dict-entry (concat cur-ent "/"
                                                   (symbol-name (car ent)))
                                           settings
                                           (cdr ent))
          (setq ent-list nil))))))

(defun enwc-nm-alist-to-dbus-str-str-var-map-map (alist)
  (let (ret)
    (dolist (ent alist ret)
      (push `(:string (car ent) ,(enwc-nm-alist-to-dbus-dict (cadr ent))) ret)
      (push :dict-entry ret))))

;; (defun enwc-nm-get-profile-info (ap &optional wired)
;;   "Get the profile info for access point AP."
;;   (let ((conn (enwc-nm--ap-to-conn ap))
;;         settings)
;;     (when conn
;;       (setq settings (enwc-nm-get-settings conn)))
;;     (when settings
;;       (let* ((adr-info (caar (enwc-nm-get-dbus-dict-entry "ipv4/addresses" settings)))
;;              (ip-addr (enwc-nm-convert-addr (nth 0 adr-info)))
;;              (netmask (enwc-nm-convert-addr (enwc-nm-prefix-to-netmask (nth 1 adr-info))))
;;              (gateway (enwc-nm-convert-addr (nth 2 adr-info)))
;;              (dns-list (mapcar 'enwc-nm-convert-addr
;;                                (car (enwc-nm-get-dbus-dict-entry "ipv4/dns"
;;                                                                  settings))))
;;              (sec-info (enwc-nm-get-sec-info settings)))
;;         `((addr . ,ip-addr)
;;           (netmask . ,netmask)
;;           (gateway . ,gateway)
;;           (dns1    . ,(nth 0 dns-list))
;;           (dns2    . ,(nth 1 dns-list))
;;           ,@sec-info)))))

(defun enwc-nm-finalize-settings (settings)
  "Set up all of the D-BUS types of a settings list SETTINGS.
This will place all of the necessary markers in the list, such as :array,
:dict-entry, etc."
  `(:array ,@(enwc-nm-alist-to-dbus-str-str-var-map-map settings)))

(defun enwc-nm-setup-settings (conn settings wired)
  "Set up NetworkManager settings.
Get the current network properties of network CONN
and uses the information in the association list SETTINGS
to put it in the form that NetworkManager will recognize."
  (let (conn-settings)
    (if (enwc-nm-connection-p conn)
        (setq conn-settings (enwc-nm-get-settings conn))
      (message "Not a connection")
      (print conn)
      ;;TODO: ssid will be invalid for wired connections.
      ;; This would actually throw an error in that case.
      (let ((ssid (dbus-byte-array-to-string (enwc-nm-get-wireless-network-property
                                              conn "Ssid")))
            (type (if wired "802-3-ethernet" "802-11-wireless")))
        (setq conn-settings `(("connection"
                               (("id" (,(concat ssid " settings")))
                                ("uuid" (,(enwc-nm-gen-uuid)))
                                ("autoconnect" (nil))
                                ("type" (,type))))))))
    (setq conn-settings (enwc-nm-process-profile-info conn-settings settings))
    (pp conn-settings)
    (print conn-settings)
    (enwc-nm-finalize-settings conn-settings)))

(defun enwc-nm-save-nw-settings (ap settings wired)
  "Save network AP with settings SETTINGS."
  (let ((conn (enwc-nm--ap-to-conn ap)))
    (print conn)
    (if conn
        (enwc-nm-dbus-call-method "Update"
                                  conn
                                  enwc-nm-dbus-connections-interface
                                  (enwc-nm-setup-settings conn settings wired))
      (enwc-nm-dbus-call-method "AddConnection"
                                enwc-nm-dbus-settings-path
                                enwc-nm-dbus-settings-interface
                                (enwc-nm-setup-settings ap settings wired)))))


                                        ; ;;;;;;;;;;;;;;;;; ;
                                        ; ;; Load/Unload ;; ;
                                        ; ;;;;;;;;;;;;;;;;; ;


(defun enwc-nm-load ()
  "Setup the NetworkManager back-end."
  ;;TODO: Add way of changing these two after load.
  (setq enwc-nm-wired-dev (enwc-nm-get-device-by-name enwc-wired-device)
        enwc-nm-wireless-dev (enwc-nm-get-device-by-name enwc-wireless-device))

  (setq enwc-nm-access-point-added-signal
        (dbus-register-signal :system
                              enwc-nm-dbus-service
                              enwc-nm-wireless-dev
                              enwc-nm-dbus-wireless-interface
                              "AccessPointAdded"
                              'enwc-process-scan))

  (setq enwc-nm-access-point-removed-signal
        (dbus-register-signal :system
                              enwc-nm-dbus-service
                              enwc-nm-wireless-dev
                              enwc-nm-dbus-wireless-interface
                              "AccessPointRemoved"
                              'enwc-process-scan))

  (setq enwc-nm-active-ap
        (let ((cur-net (dbus-get-property :system
                                          enwc-nm-dbus-service
                                          enwc-nm-wireless-dev
                                          enwc-nm-dbus-wireless-interface
                                          "ActiveAccessPoint")))
          (if (string= cur-net "/")
              "/"
            cur-net)))

  (setq enwc-nm-properties-changed-signal
        (dbus-register-signal :system
                              enwc-nm-dbus-service
                              enwc-nm-wireless-dev
                              enwc-nm-dbus-wireless-interface
                              "PropertiesChanged"
                              'enwc-nm-wireless-prop-changed))

  (setq enwc-nm-connecting-p
        (let ((state (dbus-get-property :system
                                        enwc-nm-dbus-service
                                        enwc-nm-dbus-path
                                        enwc-nm-dbus-interface
                                        "State")))
          (eq state 40)))

  (setq enwc-nm-wired-state-changed-signal
        (dbus-register-signal :system
                              enwc-nm-dbus-service
                              enwc-nm-wired-dev
                              enwc-nm-dbus-device-interface
                              "StateChanged"
                              'enwc-nm-dev-prop-changed))

  (setq enwc-nm-wired-p
        (let ((state (dbus-get-property :system
                                        enwc-nm-dbus-service
                                        enwc-nm-wired-dev
                                        enwc-nm-dbus-device-interface
                                        "State")))
          (eq state 100)))
  (setq enwc-nm-state-changed-signal
        (dbus-register-signal :system
                              enwc-nm-dbus-service
                              enwc-nm-dbus-path
                              enwc-nm-dbus-interface
                              "StateChanged"
                              'enwc-nm-prop-changed)))

(defun enwc-nm-unload ()
  "Unload the NetworkManager back-end.

Unregister all of the D-Bus signals set up during load."
  (dbus-unregister-object enwc-nm-access-point-added-signal)
  (dbus-unregister-object enwc-nm-access-point-removed-signal)
  (dbus-unregister-object enwc-nm-properties-changed-signal)
  (dbus-unregister-object enwc-nm-wired-state-changed-signal)
  (dbus-unregister-object enwc-nm-state-changed-signal))

(defun enwc-nm-can-load-p ()
  ;; (dbus-ping :system enwc-nm-dbus-service)
  ;; NetworkManager doesn't allow Ping, so we have to use an alternative.
  (dbus-introspect :system enwc-nm-dbus-service enwc-nm-dbus-path))

(enwc-register-backend
 (make-enwc-backend
  :key 'nm
  :can-load-p #'enwc-nm-can-load-p
  :load #'enwc-nm-load
  :unload #'enwc-nm-unload
  :network-ids #'enwc-nm-get-networks
  :scan #'enwc-nm-scan
  :connect #'enwc-nm-connect
  :disconnect #'enwc-nm-disconnect
  :current-nw-id #'enwc-nm-get-current-nw-id
  :is-connecting-p #'enwc-nm-check-connecting
  :nw-props #'enwc-nm-get-nw-props
  :is-wired-p #'enwc-nm-is-wired))

(provide 'enwc-nm)

;;; enwc-nm.el ends here
