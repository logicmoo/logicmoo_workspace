;; enwc-cm.el --- The ConnMan back-end to ENWC.

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

;; ConnMan support is still experimental.

;;; Code:

(require 'enwc)

(defgroup enwc-cm nil
  "*ConnMan variables for ENWC"
  :prefix "enwc-cm-"
  :group 'enwc)

(defcustom enwc-cm-dbus-service "net.connman"
  "ConnMan D-Bus service."
  :group 'enwc-cm
  :type 'string)

(defcustom enwc-cm-dbus-manager-interface "net.connman.Manager"
  "ConnMan D-Bus Manager interface."
  :group 'enwc-cm
  :type 'string)

(defcustom enwc-cm-dbus-service-interface "net.connman.Service"
  "ConnMan D-Bus Service interface."
  :group 'enwc-cm
  :type 'string)

(defcustom enwc-cm-dbus-technology-interface "net.connman.Technology"
  "ConnMan D-Bus Technology Interface"
  :group 'enwc-cm
  :type 'string)

(defcustom enwc-cm-dbus-technology-path "/net/connman/technology/"
  "ConnMan D-Bus Technology Path"
  :group 'enwc-cm
  :type 'string)

(defcustom enwc-cm-dbus-service-path "/net/connman/service"
  "ConnMan D-Bus Service Path"
  :group 'enwc-cm
  :type 'string)

(defcustom enwc-cm-dbus-agent-service "org.gnu.enwc.agent"
  "The service for the ConnMan Agent for ENWC to use."
  :group 'enwc-cm
  :type 'string)

(defcustom enwc-cm-dbus-agent-path "/org/gnu/enwc/agent"
  "The path to the Agent for ENWC to use for ConnMan security."
  :group 'enwc-cm
  :type 'string)

(defcustom enwc-cm-dbus-agent-interface "org.gnu.enwc.agent"
  "The interface for the ConnMan Agent for ENWC to use."
  :group 'enwc-cm
  :type 'string)

(defvar enwc-cm-details-list
  '("Name" "Ethernet/Address" "Strength" "Security" "mode" "channel"))

(defun enwc-cm-get-wifi-tech-path ()
  "Return the ConnMan Wifi Technology path."
  (concat enwc-cm-dbus-technology-path "wifi"))

(defun enwc-cm-get-wired-tech-path ()
  "Return the ConnMan Ethernet Technology path."
  (concat enwc-cm-dbus-technology-path "ethernet"))

(defun enwc-cm-get-network (id)
  (nth id (enwc-cm-get-services)))

;; connect
(defun enwc-cm-connect (wired id)
  (let ((path (concat enwc-cm-dbus-service-path (number-to-string id))))
    (dbus-call-method :system
                      enwc-cm-dbus-service
                      path
                      enwc-cm-dbus-service-interface
                      "Connect")))

;; disconnect
(defun enwc-cm-disconnect (wired)
  (dbus-call-method :system
                    enwc-cm-dbus-service
                    "/net/connman/service/CONNECTED-SERVICE"
                    enwc-cm-dbus-service-interface
                    "Disconnect"))

;; scan
(defun enwc-cm-scan (wired)
  (let ((path (if wired
                  (enwc-cm-get-wired-tech-path)
                (enwc-cm-get-wifi-tech-path))))
    (dbus-call-method-asynchronously :system
                                     path
                                     enwc-cm-dbus-technology-interface
                                     "Scan"
                                     'enwc-process-scan
                                     :timeout 6000)))

;; (defun enwc-cm-scan (wired)
;;   (dbus-call-method :system
;;                  enwc-cm-dbus-service
;;                  "/net/connman/technology/(ethernet|wifi)"
;;                  "net.connman.Technology"
;;                  "Scan"))

;; get-network-props
;; Might combine this with enwc-nm-get-dbus-dict-entry
(defun enwc-cm-dict-assoc (dict prop)
  (let ((path-list (split-string prop "/"))
        (cur-list dict))
    (while (and path-list cur-list)
      (let ((cur-path (pop path-list)))
        (setq cur-list
              (when (assoc cur-path cur-list)
                (caadr (assoc cur-path cur-list))))))
    (if (and cur-list (consp cur-list)) (car cur-list) cur-list)))

(defun enwc-cm-get-nw-prop (id prop)
  (let ((network (enwc-cm-get-network id)))
    (car (cadr (assoc prop (cadr network))))))

(defun enwc-cm-get-network-props (id wired)
  (let ((network (enwc-cm-get-network id))
        props)
    ;; network should be a pair, (path . props)
    (unless network
      (error "Invalid Network Id %d" id))
    (setq props (cadr network))
    (mapcar
     (lambda (det)
       (enwc-cm-dict-assoc props det))
     enwc-cm-details-list)))

(defun enwc-cm-get-wireless-network-property (id prop)
  (enwc-cm-get-nw-prop id prop))

;; get-profile-props
(defun enwc-cm-get-profile-props (prof wired)
  (let ((network (enwc-cm-get-network prof))
        props
        ipv4-settings
        dns-settings)
    (unless network
      (error "Invalid Network Id %d" id))
    (setq props (cadr network))
    (setq ipv4-settings (enwc-cm-dict-assoc "IPv4/Configuration" props)
          dns-settings  (enwc-cm-dict-assoc "Domains/Configuration" props))
    `((addr    . ,(cadr (assoc "Address" ipv4-settings)))
      (netmask . ,(cadr (assoc "Netmask" ipv4-settings)))
      (gateway . ,(cadr (assoc "Gateway" ipv4-settings)))
      (dns1    . ,(nth 0 dns-settings))
      (dns2    . ,(nth 1 dns-settings)))))

;; get-current-nw-id
(defun enwc-cm-is-disconnected-p (service)
  (member (enwc-cm-dict-assoc (cadr service) "State")
          '("idle" "disconnect")))

(defun enwc-cm-is-connecting-p (service)
  (member (enwc-cm-dict-assoc (cadr service) "State")
          '("association" "configuration")))

(defun enwc-cm-is-connected-p (service)
  (member (enwc-cm-dict-assoc (cadr service) "State")
          '("ready" "online")))

(defun enwc-cm--find-connected-service (services num)
  (if services
      (if (enwc-cm-is-connected-p (car services))
          num
        (enwc-cm--find-connected-service (cdr services) (1+ num)))
    -1))

(defun enwc-cm-get-current-nw-id ()
  "Get the current network id."
  (let* ((services (enwc-cm-get-services)))
    (enwc-cm--find-connected-service services 0)))

;; save-profile
(defun enwc-cm-set-nw-prop (nw prop val &optional wired)
  (dbus-call-method :system
                    enwc-cm-dbus-service
                    (car nw)
                    enwc-cm-dbus-service-interface
                    "SetProperty"
                    :string prop
                    val))

(defun enwc-cm-save-profile (prof settings wired)
  (let ((network (enwc-cm-get-network prof))
        props
        ipv4-settings
        dns-settings)
    (unless network
      (error "Invalid Network Id %d" id))
    (setq props (cadr network))
    (setq ipv4-settings
          `((("Method" ("manual"))
             ("Address" (,(alist-get 'addr settings "")))
             ("Netmask" (,(alist-get 'netmask settings "")))
             ("Gateway" (,(alist-get 'gateway settings ""))))))
    (setq dns-settings
          `((,(alist-get 'dns1 settings ""))
            (,(alist-get 'dns2 settings ""))))
    (enwc-cm-set-nw-prop prof "IPv4.Configuration" ipv4-settings wired)
    (enwc-cm-set-nw-prop prof "Domains.Configuration" dns-settings wired)))

;; check-connecting-p
(defun enwc-cm--find-connecting-service (services)
  (when services
    (or (enwc-cm-is-connecting-p (car services))
        (enwc-cm--find-connecting-service (cdr services)))))

(defun enwc-cm-check-connecting-p ()
  (let ((services (enwc-cm-get-services)))
    (enwc-cm--find-connecting-service services)))

;; get-networks
(defun enwc-cm-get-services ()
  (dbus-call-method :system
                    enwc-cm-dbus-service
                    "/"
                    enwc-cm-dbus-manager-interface
                    "GetServices"))

(defun enwc-cm-get-networks ()
  (let ((services (enwc-cm-get-services)))
    (mapcar 'car services)))

;; Agent
;; ConnMan requires that an agent is registered, so we create one here.

(defun enwc-cm-agent-release (&rest args)
  )

(defun enwc-cm-agent-report-error (service err)
  )

(defun enwc-cm-agent-report-peer-error (peer err))

(defun enwc-cm-agent-request-browser (service url)
  (browse-url url))

(defun enwc-cm-agent-request-input (service fields)
  "Called when a user requests input.
SERVICE is the current service.
FIELDS is an associative list of requested fields."
  ;; Obtain the information from somewhere.
  )

(defun enwc-cm-agent-request-peer-authorization (peer fields))

(defun enwc-cm-agent-cancel ())

(defun enwc-cm-method-to-defun (method)
  (let ((case-fold-search nil))
    (combine-and-quote-strings
     (mapcar 'downcase
             (cdr (split-string
                   (replace-regexp-in-string "\\([[:upper:]]\\)" " \\1" method)
                   " ")))
     "-")))

(defmacro enwc-cm-agent-register-method (method)
  "Register a method METHOD with D-Bus.
METHOD is assumed to be the camel-case D-Bus method."
  `((dbus-register-method :session
                          enwc-cm-dbus-agent-service
                          enwc-cm-dbus-agent-path
                          enwc-cm-dbus-agent-interface
                          ,method
                          (quote ,(car (read-from-string
                                        (concat
                                         "enwc-cm-agent-"
                                         (enwc-cm-method-to-defun method))))))))

;; Run during setup.
(defun enwc-cm-setup ()

  ;; Setup the agent.
  (dbus-register-service :session enwc-cm-dbus-agent-service)

  (enwc-cm-agent-register-method "Release")
  (enwc-cm-agent-register-method "ReportError")
  (enwc-cm-agent-register-method "ReportPeerError")
  (enwc-cm-agent-register-method "RequestBrowser")
  (enwc-cm-agent-register-method "RequestInput")
  (enwc-cm-agent-register-method "RequestPeerAuthorization")
  (enwc-cm-agent-register-method "Cancel")

  (dbus-call-method :system
                    enwc-cm-dbus-manager-service
                    enwc-cm-dbus-manager-path
                    enwc-cm-dbus-manager-interface
                    "RegisterAgent"
                    :string enwc-cm-dbus-agent-path))


;; (defun enwc-cm-get-encryption-type (id)
;;   (enwc-cm-get-nw-prop id "Security"))

;; (defun enwc-cm-get-ip-addr (wired id)
;;   (let ((ipv4-config (enwc-cm-get-nw-prop id "IPv4.Configuration")))
;;     (car (cadr (assoc "Address" ipv4-config)))))

;; (defun enwc-cm-get-netmask (wired id)
;;   (let ((ipv4-config (enwc-cm-get-nw-prop id "IPv4.Configuration")))
;;     (car (cadr (assoc "Netmask" ipv4-config)))))

;; (defun enwc-cm-get-gateway (wired id)
;;   (let ((ipv4-config (enwc-cm-get-nw-prop id "IPv4.Configuration")))
;;     (car (cadr (assoc "Gateway" ipv4-config)))))

;; (defun enwc-cm-get-dns (wired id)
;;   (enwc-cm-get-nw-prop id "Nameservers.Configuration"))

;; (defun enwc-cm-save-nw-settings (wired id settings)
;;   (let* ((ipv4 (enwc-cm-get-nw-prop id "IPv4.Configuration"))
;;          (method (car (cadr (assoc "Method" ipv4))))
;;          (ip-addr (cdr (assoc "addr" settings)))
;;          (netmask (cdr (assoc "netmask" settings)))
;;          (gateway (cdr (assoc "gateway" settings)))
;;          new-ipv4-config new-dns-config)
;;     (setq new-ipv4-config
;;           (list (list (cons "Method" (cons (cons method nil) nil))
;;                       (cons "Address" (cons (cons ip-addr nil) nil))
;;                       (cons "Netmask" (cons (cons netmask nil) nil))
;;                       (cons "Gateway" (cons (cons gateway nil) nil)))))
;;     (setq new-dns-config
;;           (list (list (cdr (assoc "dns1" settings))
;;                       (cdr (assoc "dns2" settings)))))
;;     (enwc-cm-set-nw-prop wired id "IPv4.Configuration"
;;                          new-ipv4-config)
;;     (enwc-cm-set-nw-prop wired id "Nameservers.Configuration"
;;                          new-dns-config)))

(provide 'enwc-cm)

;; Local Variables:
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; enwc-cm.el ends here
