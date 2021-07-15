;;; enwc-edit.el --- Support for editing wireless network profiles

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

;; THIS IS EXPERIMENTAL

;;; Code:

(require 'wid-edit)

(eval-when-compile
  (defun enwc--break-by-words (str)
    "Break up string STR into a list of words."
    (cl-check-type str string)
    (split-string str "-\\|_\\| "))

  (defun enwc--sym-to-str (sym &optional seps)
    "Create a string from symbol SYM.
SEPS is a string specifying the separator to use to combine the words,
or \" \" if not specified."
    (cl-check-type sym symbol)
    (unless seps
      (setq seps " "))
    (cl-check-type seps string)
    (combine-and-quote-strings (enwc--break-by-words (symbol-name sym)) seps))

  (defun enwc--str-to-sym (str &optional seps)
    "Create a symbol from the string STR.
This will break STR into words, and then put it back together separating
each word by SEPS, which defaults to \"-\"."
    (cl-check-type str string)
    (unless seps
      (setq seps "-"))
    (cl-check-type seps string)
    (intern (combine-and-quote-strings (enwc--break-by-words str) seps))))

(defun enwc--int-to-byte-list (n)
  "Convert 32-bit integer N into a byte list."
  (cl-check-type n integer)
  (let (ret)
    (dotimes (x 4 ret)
      (push (logand n 255) ret)
      (setq n (lsh n -8)))))

(defun enwc--byte-list-to-int (bl)
  "Convert byte list BL into a 32-bit integer."
  (cl-check-type bl list)
  (let ((ret 0))
    (dolist (x bl ret)
      (setq ret (logior (lsh ret 8) x)))))

(defun enwc--htonl (n)
  "Convert 32-bit integer N from hardware to network byte order."
  (cl-check-type n integer)
  (enwc--byte-list-to-int (nreverse (enwc--int-to-byte-list n))))

;;;;;;;;;;;;;;;;;;;;;;;;
;; Profile Properties ;;
;;;;;;;;;;;;;;;;;;;;;;;;

;; Settings for access point AP
;;
;; IPv4 Settings:
;;   Address =
;;   Netmask =
;;   Gateway =
;;
;;   DNS 1   =
;;   DNS 2   =
;;
;; Security:
;;   Type    =
;;

(defun enwc-edit-view-entry ()
  "View the text of the entry at point.
This is mostly useful to view the text of the hidden entries."
  (interactive)
  (unless (get-buffer "*ENWC Edit*")
    (error "Not editing a network entry."))
  (unless (eq (current-buffer) (get-buffer "*ENWC Edit*"))
    (switch-to-buffer "*ENWC Edit*"))
  (unless (widget-at)
    (error "No widget at point"))
  (message (widget-field-value-get (widget-at))))

(define-widget 'enwc-profile-props-widget 'group
  "ENWC edit widget."
  :convert-widget 'identity
  :format "IPv4 Settings:\n %v"
  :value-to-internal 'enwc-profile-props-to-widget
  :value-to-external 'enwc-widget-to-profile-props
  :match #'(lambda nil t)
  :indent 1
  :args '((string :tag "Address")
          (string :tag "Netmask")
          (string :tag "Gateway")
          (string :tag "DNS1")
          (string :tag "DNS2")))

(defun enwc-profile-props-to-widget (widget props)
  "Create a profile props widget."
  (list
   (alist-get 'addr props "")
   (alist-get 'netmask props "")
   (alist-get 'gateway props "")
   (alist-get 'dns1 props "")
   (alist-get 'dns2 props "")))

(defun enwc-widget-to-profile-props (widget vals)
  "Convert widget values to a profile properties alist.
WIDGET is unused.
VALS is the list of widget values."
  (let ((addr (nth 0 vals))
        (netmask (nth 1 vals))
        (gateway (nth 2 vals))
        (dns1 (nth 3 vals))
        (dns2 (nth 4 vals)))
    `((addr . ,addr)
      (netmask . ,netmask)
      (gateway . ,gateway)
      (dns1 . ,dns1)
      (dns2 . ,dns2))))

;;;;;;;;;;;;;;
;; Security ;;
;;;;;;;;;;;;;;

(eval-when-compile
  (defmacro enwc--make-supplicant-multi (key &rest args)
    `(cons (quote ,key)
           (quote (checklist :tag ,(capitalize (enwc--sym-to-str key))
                             :format "%{%t%}: %v"
                             :sample-face bold
                             :indent ,(+ 4 (length (enwc--sym-to-str key)))
                             :args ,(mapcar
                                     (lambda (arg)
                                       `(item :tag ,(upcase arg) :value ,(downcase arg)))
                                     args)))))

  (defmacro enwc--make-supplicant-choice (key &rest args)
    `(cons (quote ,key)
           (quote (menu-choice :tag ,(capitalize (enwc--sym-to-str key))
                               :format "%[%t%]: %v"
                               :sample-face bold
                               :args
                               ,(mapcar
                                 (lambda (arg)
                                   `(item :tag ,(upcase arg) :value (downcase ,arg)))
                                 args)))))

  (defmacro enwc--make-supplicant-secret (key)
    `(cons (quote ,key)
           (quote (editable-field :tag ,(capitalize (enwc--sym-to-str key))
                                  :format "%{%t%}: %v"
                                  :sample-face bold
                                  :keymap enwc-edit-field-map
                                  :secret ?*))))

  (defmacro enwc--make-supplicant-entry (key)
    `(cons (quote ,key)
           (quote (editable-field :tag ,(capitalize (enwc--sym-to-str key))
                                  :sample-face bold
                                  :format "%{%t%}: %v"))))

  (defmacro enwc--make-supplicant-file (key)
    `(cons (quote ,key)
           (quote (file :tag ,(capitalize (enwc--sym-to-str key))
                        :format "%{%t%}: %v"
                        :sample-face bold
                        :must-match t))))

  (defmacro enwc--make-supplicant-list (key &rest args)
    `(cons (quote ,key)
           (quote (list :tag ,(capitalize (enwc--sym-to-str key))
                        :format "%{%t%}: %v"
                        :sample-face bold
                        :indent ,(length (enwc--sym-to-str key))
                        :args ,(mapcar (lambda (x) (cdr (eval x))) args))))))

(defconst enwc-supplicant-alist
  (list
   (enwc--make-supplicant-multi proto "WPA" "RSN")
   (enwc--make-supplicant-multi key-mgmt "None" "WPA-PSK" "WPA-EAP" "IEEE8021X")
   (enwc--make-supplicant-choice auth-alg "OPEN" "SHARED" "LEAP")
   (enwc--make-supplicant-multi pairwise "CCMP" "TKIP" "NONE")
   (enwc--make-supplicant-multi group "CCMP" "TKIP" "WEP104" "WEP40")
   (enwc--make-supplicant-secret psk)
   (enwc--make-supplicant-secret wep-key0)
   (enwc--make-supplicant-secret wep-key1)
   (enwc--make-supplicant-secret wep-key2)
   (enwc--make-supplicant-secret wep-key3)
   (enwc--make-supplicant-choice wep-tx-keyidx "0" "1" "2" "3")
   (enwc--make-supplicant-choice eap "TLS" "PEAP" "TTLS" "LEAP" "FAST")
   (enwc--make-supplicant-entry identity)
   (enwc--make-supplicant-entry anonymous-identity)
   (enwc--make-supplicant-secret password)
   (enwc--make-supplicant-file ca-cert)
   (enwc--make-supplicant-file client-cert)
   (enwc--make-supplicant-file private-key)
   (enwc--make-supplicant-secret private-key-passwd)
   (enwc--make-supplicant-file pac-file)
   (enwc--make-supplicant-list phase1
                               (enwc--make-supplicant-choice peapver "" "0" "1")
                               (enwc--make-supplicant-choice peaplabel "" "0" "1")
                               (enwc--make-supplicant-choice fast-provisioning "" "0" "1" "2" "3"))
   (enwc--make-supplicant-list phase2
                               (enwc--make-supplicant-choice auth "" "MD5" "MSCHAPV2" "OTP" "GTC" "TLS")
                               (enwc--make-supplicant-choice autheap "" "MD5" "MSCHAPV2" "OTP" "GTC" "TLS")
                               (enwc--make-supplicant-file ca-cert)
                               (enwc--make-supplicant-file client-cert)
                               (enwc--make-supplicant-file private-key)
                               (enwc--make-supplicant-secret private-key-passwd)))
  "An alist that maps supplicant entries to a widget type.

For more information, see the documentation for wpa_supplicant.")

(defcustom enwc-supplicant-template-alist
  `((wep . ((key-mgmt . ("none"))
            (wep-key0 . req)
            (wep-tx-keyidx . "0")))
    (wpa2 . ((proto . ("wpa" "rsn"))
             (key-mgmt . ("wpa-psk"))
             (pairwise . ("ccmp" "tkip"))
             (group . ("ccmp" "tkip"))
             (psk . req)))
    (leap . ((eap . "leap")
             (key-mgmt . ("ieee8021x"))
             (auth-alg . "leap")
             (identity . req)
             (password . req)))
    (eap-fast . ((proto . ("rsn" "wpa"))
                 (pairwise . ("ccmp" "tkip"))
                 (group . ("ccmp" "tkip"))
                 (key-mgmt . ("wpa-eap"))
                 (eap . "fast")
                 (identity . req)
                 (password . req)
                 (phase1 . ((fast-provisioning . "1")))
                 (pac-file . opt)))
    (eap-tls . ((key-mgmt . ("wpa-eap"))
                (pairwise . ("tkip"))
                (group . ("tkip"))
                (eap . "tls")
                (identity . req)
                (ca-cert . opt)
                (client-cert . opt)
                (private-key . req)
                (private-key-passwd . req)))
    (peap . ((proto . ("rsn"))
             (key-mgmt . ("wpa-eap"))
             (pairwise . ("ccmp"))
             (eap . "peap")
             (identity . req)
             (password . req)))
    (peap-tkip . ((proto . ("wpa"))
                  (key-mgmt . ("wpa-eap"))
                  (pairwise . ("tkip"))
                  (group . ("tkip"))
                  (eap . "peap")
                  (identity . req)
                  (password . req)
                  (ca-cert . opt)
                  (phase1 . ((peaplabel . "0")))
                  (phase2 . ((auth . "mschapv2"))))))
  "The alist of templates for security.
This should be an alist of the form (KEY . ((SUPPLICANT-KEY . INITIAL-INPUT) ...))
Each SUPPLICANT-KEY should be a key from `enwc-supplicant-alist', and INITIAL-INPUT
should be an acceptable value for SUPPLICANT-KEY.

If INITIAL-INPUT is the symbol req, then this option is required.
The value opt means that the option is optional."
  :group 'enwc
  :type '(alist :key-type symbol :value-type (alist :key-type symbol)))

(defun enwc--get-supplicant-entry (ent &optional sec-info)
  "Create a widget definition from ENT.

If optional parameter SEC-INFO is non-nil, then use it
for security information."
  (let ((init (cdr ent))
        (wid (assq (car ent) enwc-supplicant-alist))
        fin)
    (unless wid
      (error "Unknown supplicant type %s" (car ent)))
    ;; Set the initial value for the widget.
    (when (eq (cadr wid) 'list)
      (let (act-init)
        (dolist (arg (widget-get (cdr wid) :args))
          (push (alist-get (enwc--str-to-sym (downcase (widget-get arg :tag))) init "")
                act-init))
        (setq init (nreverse act-init))))
    (cons (cadr wid)
          (append (pcase init
                    (`req `(:required t :value ,(alist-get (car ent) sec-info "")))
                    (`opt `(:value ,(alist-get (car ent) sec-info "")))
                    (_ `(:value ,init)))
                  (cddr wid)))))

(defun enwc-create-template-menu (&optional sec-info)
  "Create the widget declaration for the menu of templates.

If specified, SEC-INFO is passed to the templates to initialize them."
  `(menu-choice
    :tag "Security"
    :indent 2
    ,@(mapcar
       (lambda (tm)
         `(list
           :tag ,(symbol-name (car tm))
           :menu-tag ,(symbol-name (car tm))
           ,@(mapcar
              (lambda (ent)
                (enwc--get-supplicant-entry ent sec-info))
              (cdr tm))))
       enwc-supplicant-template-alist)))

(defun enwc-display-sec-widget (&optional sec-info)
  "Create the menu of security templates.
If specified, SEC-INFO is passed to the templates to initialize them."
  (widget-create (enwc-create-template-menu sec-info)))

(defun enwc-sec-widget-data (widget)
  "Get the data from a security widget WIDGET."
  (let* ((type (widget-get (widget-get widget :choice) :tag))
         (values (widget-value widget))
         (template (assq (intern type) enwc-supplicant-template-alist)))
    (unless template
      (error "Unrecognized security template \"%s\"." type))
    (setq template (cdr template))
    (cons
     `(sec-type . ,(intern type))
     (cl-mapcar
      (lambda (val v)
        (let ((vl v))
          (when (or (eq (car val) 'phase1)
                    (eq (car val) 'phase2))
            (let ((subs
                   (mapcar
                    (lambda (arg)
                      (enwc--str-to-sym (downcase (widget-get arg :tag))))
                    (widget-get (alist-get (car val) enwc-supplicant-alist) :args))))
              (setq vl (cl-mapcar 'cons subs v))))
          (print vl)
          (cons (car val) vl)))
      template values))))

(defvar enwc-network-edit-widget nil
  "The network information widget used in the edit buffer.")

(defvar enwc-security-edit-widget nil
  "The security widget used in the edit buffer.")

(defun enwc-setup-edit-buffer ()
  "Setup the edit buffer.  This removes the old one if neccessary,
and redisplays the settings from the network profile
 with id `enwc-edit-id', which is set in `enwc-edit-entry-at-point'."
  (when (get-buffer "*ENWC Edit*")
    (kill-buffer "*ENWC Edit*"))
  (with-current-buffer (get-buffer-create "*ENWC Edit*")
    (let ((nw-info (enwc-get-profile-info enwc-edit-id)))

      (widget-insert (concat "Settings for access point "
                             (enwc-value-from-scan 'essid enwc-edit-id)
                             "\n"))
      (widget-insert "\n")
      (setq enwc-network-edit-widget
            (widget-create 'enwc-profile-props-widget :value nw-info))

      (widget-insert "\n")
      (setq enwc-security-edit-widget (enwc-display-sec-widget nw-info))

      (use-local-map enwc-edit-map)
      (widget-setup)))

  (switch-to-buffer "*ENWC Edit*"))

(defun enwc-edit-save ()
  "Save the network settings from the edit buffer."
  (interactive)
  (unless (get-buffer "*ENWC Edit*")
    (error "Not editing a network entry"))
  (unless (eq (current-buffer) (get-buffer "*ENWC Edit*"))
    (switch-to-buffer "*ENWC Edit*"))

  (enwc-save-nw-settings
   enwc-edit-id
   (append (widget-value enwc-network-edit-widget)
           (enwc-sec-widget-data enwc-security-edit-widget))))

(defun enwc-edit-entry-at-point ()
  "Edit the current network entry."
  (interactive)
  (setq enwc-edit-id (tabulated-list-get-id))
  ;; TODO: Use pop-to-buffer
  (enwc-setup-edit-buffer))

(defvar enwc-edit-map
  (let ((map (copy-keymap widget-keymap)))
    (define-key map (kbd "C-x C-s") 'enwc-edit-save)
    map)
  "The keymap for editing network profiles with ENWC.")

(defvar enwc-edit-field-map
  (let ((map (copy-keymap widget-field-keymap)))
    (define-key map (kbd "C-x C-a") 'enwc-edit-view-entry)
    map)
  "The keymap for editable fields within the ENWC edit buffer.")

;;; enwc-edit.el ends here
