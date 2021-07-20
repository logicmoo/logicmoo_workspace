;;; rtm.el --- An elisp implementation of the Remember The Milk API

;; Copyright (C) 2009 Friedrich Delgado Friedrichs
;; uses parts of org-rtm.el Copyright (C) 2008  Avdi Grimm
;; Modified by Philipp Middendorf (pmidden@secure.mailbox.org) 2016

;; Author: Friedrich Delgado Friedrichs <frie...@nomaden.org>
;; Created: Oct 18 2009
;; Version: 0.1
;; Package-Version: 20180329.1508
;; Package-Commit: 3e3d09387cb84801343ecca8fb02e82f213e7bbe
;; Package-Requires: ((cl-lib "1.0"))
;; Keywords: remember the milk productivity todo
;; URL: https://github.com/pmiddend/emacs-rtm

;; This product uses the Remember The Milk API but is not endorsed or
;; certified by Remember The Milk

;; This file is NOT part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Note by Philipp: This file was taken from the simple-rtm repository and
;; has some minor modifications so it doesn't give byte-compilation
;; warnings.

;;; Code:

(require 'cl-lib)
(require 'url-http)
(require 'url-util)
(require 'xml)
(require 'custom)

;;;; Customisation

(defgroup rtm nil
  "Options for emacs lisp integration of Remember The Milk"
  :tag "elisp RTM"
  :group 'applications)

(defcustom rtm-api-key "d40eb4df08dd52c1930afa9d79dceda0"
  "Your own API key for Remember The Milk."
  :type 'string :group 'rtm)
(defcustom rtm-api-shared-secret "39d8e367fdce977c"
  "Your shared secret for your Remember The Milk API Key.

Note that in an open source application it is not easily possible to
hide the secret. That's why it's probably the best solution for every
user to register their own API key.

See also
http://groups.google.com/group/rememberthemilk-api/browse_thread/thread/dcb035f162d4dcc8%3Fpli%3D1

You can register your own API key and secret under
http://www.rememberthemilk.com/services/api/requestkey.rtm

In the description just tell them you're going to use the emacs lisp
API Kit"
  :type 'string :group 'rtm)

;;;; constants and variables

(defconst rtm-rest-uri "http://api.rememberthemilk.com/services/rest/"
  "Endpoint URL for REST requests. See
  http://www.rememberthemilk.com/services/api/request.rest.rtm")

(defconst rtm-auth-uri "http://www.rememberthemilk.com/services/auth/"
  "Authentication service URL, see
  http://www.rememberthemilk.com/services/api/authentication.rtm")

(defvar rtm-auth-token ""
  "Auth token received from RTM Website, after the user authenticated
  your app")

(defvar rtm-auth-token-valid nil
  "Set to t after the auth token has been validated.")

(defconst rtm-ui-buffer-name "*rtm*"
  "Name for the rtm user interface buffer")

(defconst rtm-auth-token-file ".rtm-auth-token"
  "Name for storing the auth token for the current session")

(defvar rtm-current-timeline nil
  "The current timeline")

(defvar rtm-debug nil
  "debug level")

(make-variable-buffer-local 'rtm-auth-token-valid)
(put 'rtm-auth-token-valid 'permanent-local t)

;;;; API wrappers
(defmacro def-rtm-method (methodname rtm-method-name call-func result-func
                                     result-path &rest parms)
  (declare (indent 1))
  `(defun ,methodname ,parms
     (,result-func ,result-path
                   (,call-func ',rtm-method-name
                               ,@(mapcar (lambda (sym)
                                           `(cons ,(symbol-name sym) ,sym))
                                         ;; remove lambda keywords
                                         (cl-remove-if (lambda (sym)
                                                      (or (eq sym '&optional)
                                                          (eq sym '&rest)))
                                                    parms))))))

(defmacro def-rtm-macro (macro-name call-func result-func)
  (declare (indent 0))
  `(defmacro ,macro-name (methodname rtm-method-name result-path &rest parms)
     (declare (indent 1))
     `(def-rtm-method ,methodname ,rtm-method-name ,',call-func
                      ,',result-func
                      ',result-path ,@parms)))

(def-rtm-macro def-rtm-signed-scalar-method
               rtm-call-signed rtm-get-scalar-from-response)

(def-rtm-macro def-rtm-authenticated-scalar-method
               rtm-call-authenticated rtm-get-scalar-from-response)

(def-rtm-macro def-rtm-timeline-scalar-method
               rtm-call-timeline rtm-get-scalar-from-response)

(def-rtm-macro def-rtm-signed-list-method
               rtm-call-signed rtm-get-list-from-response)

(def-rtm-macro def-rtm-authenticated-list-method
               rtm-call-authenticated rtm-get-list-from-response)

(def-rtm-macro def-rtm-timeline-list-method
               rtm-call-timeline rtm-get-list-from-response)

;; awfully brief aliases, but those long names mess up indentation
;; recomendation: use only the authenticated aliases, and the long
;; names for those (rarely used) methods that are only signed
(defalias 'def-rtm-si-sca 'def-rtm-signed-scalar-method)
(defalias 'def-rtm-authenticated-scalar-method! 'def-rtm-timeline-scalar-method)
(defalias 'def-rtm-si-lis 'def-rtm-signed-list-method)
(defalias 'def-rtm-list 'def-rtm-authenticated-list-method)
(defalias 'def-rtm-list! 'def-rtm-timeline-list-method)

;; TODO: I removed the usages of the aliases above - do I have to rewrite
;; these calls as well?
(put 'def-rtm-si-sca 'lisp-indent-function 1)
(put 'def-rtm-authenticated-scalar-method 'lisp-indent-function 1)
(put 'def-rtm-authenticated-scalar-method! 'lisp-indent-function 1)
(put 'def-rtm-si-lis 'lisp-indent--function 1)
(put 'def-rtm-list 'lisp-indent-function 1)
(put 'def-rtm-list! 'lisp-indent-function 1)

;; note that, for modifying functions, it's mostly better to define
;; them via define-rtm-list!, since you will receive the transaction
;; *and* the result, while a function defined via define-rtm-scalar!
;; will only return the transaction

(defun rtm-call-unsigned (method &rest params)
  (let ((request (rtm-construct-request-url rtm-rest-uri
                                            (rtm-prepare-params method
                                                                params))))
    (rtm-do-request request)))

(defun rtm-call-signed (method &rest params)
  (let* ((unsigned-params (rtm-prepare-params method params))
         (all-params (append-api-sig unsigned-params))
         (request (rtm-construct-request-url rtm-rest-uri
                                             all-params)))
    (rtm-do-request request)))

(defun rtm-call-authenticated (method &rest params)
  (apply #'rtm-call-signed
         method
         `("auth_token" . ,(rtm-authenticate))
         params))

(defun rtm-call-timeline (method &rest params)
  (apply #'rtm-call-authenticated
         method
         `("timeline" . ,(rtm-timeline))
         params))

(defun rtm-get-nodes-from-node-list (node-name node-list)
  (cl-remove-if-not (lambda (el) (eq node-name
                                  (xml-node-name el)))
                 node-list))

(defun rtm-get-node-content-from-response (node-name response)
  (xml-node-children (car (rtm-get-nodes-from-node-list node-name
                                           response))))

(defun rtm-get-list-from-response (path response)
  (let ((rst path)
        (content response))
    (while rst
      (setq content (rtm-get-node-content-from-response (car rst) content))
      (setq rst (cdr rst)))
    content))

(defun rtm-get-scalar-from-response (path response)
  (car (rtm-get-list-from-response path response)))

;;;;; Actual api wrappers from
;; http://www.rememberthemilk.com/services/api/methods/
;;;;;; auth
(def-rtm-signed-scalar-method rtm-auth-check-token rtm.auth.checkToken
                              (auth token) auth_token)
;; api call response (without post-processing):
;; ((auth nil
;;        (token nil "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx")
;;        (perms nil "delete")
;;        (user
;;         ((id . "xxxxxxx")
;;          (username . "johndoe")
;;          (fullname . "John Doe")))))
(def-rtm-signed-scalar-method rtm-auth-get-frob rtm.auth.getFrob (frob))
(def-rtm-signed-scalar-method rtm-auth-get-token rtm.auth.getToken
                              (auth token) frob)
;; api call response (without post-processing):
;; ((auth nil (token nil "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX")
;; (perms nil "delete") (user (... ... ...))))

;;;;;; contacts
(def-rtm-timeline-list-method rtm-contacts-add rtm.contacts.add (contact) contact)
(def-rtm-timeline-list-method rtm-contacts-delete rtm.contacts.delete () contact_id)
(def-rtm-authenticated-list-method rtm-contacts-get-list rtm.contacts.getList (contacts))

;;;;;; groups
(def-rtm-timeline-list-method rtm-groups-add rtm.groups.add () group)
(def-rtm-timeline-list-method rtm-groups-add-contact rtm.groups.addContact ()
               group_id contact_id)
(def-rtm-timeline-list-method rtm-groups-delete rtm.groups.delete () group_id)
(def-rtm-authenticated-list-method rtm-groups-get-list rtm.groups.getList ())
(def-rtm-timeline-list-method rtm-groups-remove-contact rtm.groups.removeContact ()
               group_id contact_id)

;;;;;; lists
(def-rtm-timeline-list-method rtm-lists-add rtm.lists.add ()
               name &optional filter)
(def-rtm-timeline-list-method rtm-lists-archive rtm.lists.archive ()
               list_id)
(def-rtm-timeline-list-method rtm-lists-delete rtm.lists.delete ()
               list_id)
(def-rtm-authenticated-list-method rtm-lists-get-list rtm.lists.getList (lists))
;; example response (after result function):
;; ((list
;;   ((id . "7781815")
;;    (name . "Inbox")
;;    (deleted . "0")
;;    (locked . "1")
;;    (archived . "0")
;;    (position . "-1")
;;    (smart . "0")
;;    (sort_order . "0")))
;;  (list
;;   ((id . "7781820")
;;    (name . "All Tasks")
;;    (deleted . "0")
;;    (locked . "0")
;;    (archived . "0")
;;    (position . "0")
;;    (smart . "1")
;;    (sort_order . "0"))
;;   (filter nil))
;;  (list
;;   ((id . "7781818")
;;    (name . "Work")
;;    (deleted . "0")
;;    (locked . "0")
;;    (archived . "0")
;;    (position . "0")
;;    (smart . "0")
;;    (sort_order . "0")))
;;  (list
;;   ((id . "7781816")
;;    (name . "Private")
;;    (deleted . "0")
;;    (locked . "0")
;;    (archived . "0")
;;    (position . "0")
;;    (smart . "0")
;;    (sort_order . "0")))
;;  (list
;;   ((id . "7781819")
;;    (name . "Sent")
;;    (deleted . "0")
;;    (locked . "1")
;;    (archived . "0")
;;    (position . "1")
;;    (smart . "0")
;;    (sort_order . "0"))))
(def-rtm-timeline-list-method rtm-lists-set-default-list rtm.lists.setDefaultList ()
               list_id)
(def-rtm-timeline-list-method rtm-lists-set-name rtm.lists.setName ()
               list_id name)
(def-rtm-timeline-list-method rtm-lists-unarchive rtm.lists.unarchive ()
               list_id)

;;;;;; locations
(def-rtm-authenticated-list-method rtm-locations-get-list rtm.locations.getList (locations))

;;;;;; reflection
(def-rtm-signed-list-method rtm-reflection-get-methods rtm.reflection.getMethods
                            (methods))
(def-rtm-signed-scalar-method rtm-reflection-get-method-info
                              rtm.reflection.getMethodInfo () method_name)

;;;;;; settings
(def-rtm-authenticated-list-method rtm-settings-get-list rtm.settings.getList (settings))

;;;;;; tasks
(def-rtm-timeline-list-method rtm-tasks-add rtm.tasks.add ()
               name &optional parse list_id)

(def-rtm-timeline-list-method rtm-tasks-add-tags rtm.tasks.addTags ()
               list_id taskseries_id task_id tags)

(def-rtm-timeline-list-method rtm-tasks-complete rtm.tasks.complete ()
               list_id taskseries_id task_id)

(def-rtm-timeline-list-method rtm-tasks-delete rtm.tasks.delete ()
               list_id taskseries_id task_id)

(def-rtm-authenticated-list-method rtm-tasks-get-list rtm.tasks.getList (tasks)
              &optional list_id filter last_sync)
;; example response (after result function):
;; ((list
;;   ((id . "7781819")))
;;  (list
;;   ((id . "7781817")))
;;  (list
;;   ((id . "7781816"))
;;   (taskseries
;;    ((id . "35272531")
;;     (created . "2009-03-08T20:57:45Z")
;;     (modified . "2009-03-08T21:52:18Z")
;;     (name . "Try Remember The Milk")
;;     (source . "js")
;;     (url . "")
;;     (location_id . ""))
;;    (tags nil)
;;    (participants nil)
;;    (notes nil)
;;    (task
;;     ((id . "49791364")
;;      (due . "2009-03-08T20:57:00Z")
;;      (has_due_time . "1")
;;      (added . "2009-03-08T20:57:45Z")
;;      (completed . "2009-03-08T21:52:16Z")
;;      (deleted . "")
;;      (priority . "1")
;;      (postponed . "0")
;;      (estimate . "")))))
;;  (list
;;   ((id . "7781818")))
;;  (list
;;   ((id . "7781820"))))

(def-rtm-timeline-list-method rtm-tasks-move-priority rtm.tasks.movePriority ()
               list_id taskseries_id task_id direction)

(def-rtm-timeline-list-method rtm-tasks-move-to rtm.tasks.moveTo ()
               from_list_id to_list_id taskseries_id task_id)

(def-rtm-timeline-list-method rtm-tasks-postpone rtm.tasks.postpone ()
               list_id taskseries_id task_id)

(def-rtm-timeline-list-method rtm-tasks-remove-tags rtm.tasks.removeTags ()
               list_id taskseries_id task_id tags)

(def-rtm-timeline-list-method rtm-tasks-set-due-date rtm.tasks.setDueDate ()
               list_id taskseries_id task_id &optional due has_due_time parse)

(def-rtm-timeline-list-method rtm-tasks-set-estimate rtm.tasks.setEstimate ()
               list_id taskseries_id task_id &optional estimate)

(def-rtm-timeline-list-method rtm-tasks-set-location rtm.tasks.setLocation ()
               list_id taskseries_id task_id &optional location_id)

(def-rtm-timeline-list-method rtm-tasks-set-name rtm.tasks.setName ()
               list_id taskseries_id task_id name)

(def-rtm-timeline-list-method rtm-tasks-set-priority rtm.tasks.setPriority ()
               list_id taskseries_id task_id &optional priority)

(def-rtm-timeline-list-method rtm-tasks-set-recurrence rtm.tasks.setRecurrence ()
               list_id taskseries_id task_id &optional repeat)

(def-rtm-timeline-list-method rtm-tasks-set-tags rtm.tasks.setTags ()
               list_id taskseries_id task_id &optional tags)

(def-rtm-timeline-list-method rtm-tasks-set-url rtm.tasks.setURL ()
               list_id taskseries_id task_id &optional url)

(def-rtm-timeline-list-method rtm-tasks-uncomplete rtm.tasks.uncomplete ()
               list_id taskseries_id task_id)

;;;;;; tasks.notes
(def-rtm-timeline-list-method rtm-tasks-notes-add rtm.tasks.notes.add ()
               list_id taskseries_id task_id note_title note_text)

(def-rtm-timeline-list-method rtm-tasks-notes-delete rtm.tasks.notes.delete ()
               note_id)

(def-rtm-timeline-list-method rtm-tasks-notes-edit rtm.tasks.notes.edit ()
               note_id note_title note_text)

;;;;;; test
(defun rtm-test-echo ()
  (rtm-call-unsigned 'rtm.test.echo))

(def-rtm-authenticated-list-method rtm-test-login rtm.test.login ())

;;;;;; time
(def-rtm-signed-list-method rtm-time-convert rtm.time.convert ()
                            to_timezone &optional from_timezone time)

;;;;;; timelines
(def-rtm-authenticated-scalar-method rtm-timelines-create rtm.timelines.create (timeline))
(defun rtm-timeline ()
  (unless rtm-current-timeline
    (progn
      (setq rtm-current-timeline (rtm-timelines-create))))
  rtm-current-timeline)

;;;;;; timezones
(def-rtm-signed-list-method rtm-timezones-get-list rtm.timezones.getList ())

;;;;;; transactions
(def-rtm-timeline-list-method rtm-transactions-undo rtm.transactions.undo () transaction_id)

;;;; User authentication

(defun rtm-authenticate ()
  "Always use this function to call an authenticated method, it's the only one
that will update rtm-auth-token"
  (setq rtm-auth-token
        (let ((auth-token (or (rtm-get-stored-auth-token)
                              rtm-auth-token)))
          (if (and auth-token
                   (rtm-auth-token-valid auth-token))
              auth-token
            (rtm-get-new-auth-token))))
  rtm-auth-token)

(defun rtm-auth-token-valid (auth-token)
  (if rtm-auth-token-valid
      t
    (let ((token (ignore-errors (rtm-auth-check-token auth-token))))
      (if (and token
               (string-equal auth-token token))
          (setq rtm-auth-token-valid t)
        nil))))

(defun rtm-get-new-auth-token ()
  (let* ((frob (rtm-auth-get-frob))
         (auth-url (rtm-authentication-url 'delete frob))
         (auth-token nil))
    (while (not auth-token)
      (browse-url auth-url)
      (rtm-authentication-dialog auth-url)
      (setq auth-token
            (rtm-auth-get-token frob))
      (if (rtm-auth-token-valid auth-token)
          (rtm-store-auth-token auth-token)
        (setq auth-token nil)))
    auth-token))

(defun rtm-store-auth-token (auth-token)
  (let ((token-file (locate-user-emacs-file rtm-auth-token-file)))
    (unless (file-exists-p token-file)
      (with-temp-file token-file))
    (set-file-modes token-file #o600)
    (with-temp-file token-file
      (insert auth-token)))
  auth-token)

(defun rtm-get-stored-auth-token ()
  (let ((token-file (locate-user-emacs-file rtm-auth-token-file)))
    (if (file-exists-p token-file)
        (if (file-readable-p token-file)
            (with-temp-buffer
              (insert-file-contents token-file)
              (buffer-string))
          (error "Auth token store %s exists, but is not readable."
                 token-file))
      nil)))

(defun rtm-authentication-dialog (auth-url)
  (let ((rtm-buffer (generate-new-buffer rtm-ui-buffer-name)))
    (with-current-buffer rtm-buffer
      (insert "Please visit the following url to authenticate this
application:\n\n")
      (insert-text-button auth-url 'type 'rtm-url)
      (display-buffer rtm-buffer)
      ;; (redisplay)
      (read-from-minibuffer
       "Press RETURN if after authentication was granted")
      (kill-buffer rtm-buffer))))

(define-button-type 'rtm-url
  'action (lambda (x)
            (let ((button (button-at (point))))
              (browse-url
               (button-label button))))
  'follow-link t)

(define-button-type 'rtm-button
  'follow-link t)

(defun rtm-authentication-url (perms frob)
  (let* ((unsigned-params `(("api_key" . ,rtm-api-key)
                            ("perms" . ,(maybe-string perms))
                            ("frob" . ,frob)))
         (all-params (append-api-sig unsigned-params)))
    (rtm-construct-request-url rtm-auth-uri
                               all-params)))

;;;; WebAPI handling

(defun rtm-do-request (request)
  (if rtm-debug
      (message "request: %s" request))
  (rtm-parse-response (url-retrieve-synchronously request)))

;; adapted from avdi's code:
(defun rtm-api-sig (params)
  (let* ((param-copy (cl-copy-list params))
         (sorted-params (sort param-copy
                              (lambda (lhs rhs) (string< (car lhs) (car rhs)))))
         (joined-params (mapcar (lambda (param)
                                  (concat (car param) (cdr param)))
                                sorted-params))
         (params-str (cl-reduce 'concat joined-params))
         (with-secret (concat rtm-api-shared-secret params-str)))
    (md5 with-secret)))

(defun rtm-prepare-params (method params)
  (rtm-add-method+api method
                      (rtm-stringify-params (rtm-weed-empty-params params))))

(defun rtm-stringify-params (params)
  (mapcar #'rtm-stringify-param params))

(defun rtm-stringify-param (param)
  (let* ((name (car param))
         (value (cdr param)))
    (cons (rtm-stringify-param-name name)
          (rtm-stringify-value value))))

(defun rtm-stringify-param-name (name)
  (cond ((stringp name)
         name)
        ((symbolp name)
         (symbol-name name))))

;; note: because we can't really tell between parameter wasn't given
;; and explicitly set as nil (see rtm-weed-empty-params below), you
;; should give 'false rather than nil if you mean false
(defun rtm-stringify-value (value)
  (cond ((stringp value)
         value)
        ((eq t value)
         "true")
        ((null value)
         "false")
        ((listp value)
         (rtm-comma-separated-list value))
        ((symbolp value)
         (symbol-name value))
        ((numberp value)
         (number-to-string value))))

(defun rtm-comma-separated-list (lis)
  "turn a list into a comma separated string (and flatten it)"
  (cl-labels ((comsep (lis first)
                 (if (null lis)
                     ""
                   (concat (if first "" ",")
                           (rtm-stringify-value (car lis))
                           (comsep (cdr lis) nil)))))
    (comsep lis t)))


(defun rtm-weed-empty-params (params)
  (cl-remove-if (lambda (param)
               (and (listp param)
                    (not (null param))
                    (null (cdr param))))
             params))

(defun rtm-add-method+api (method params)
  (append `(("method" . ,(maybe-string method))
            ("api_key" . ,rtm-api-key))
          params))

;; adapted from avdi's code:
(defun rtm-construct-request-url (base-uri params)
  "Construct a URL for calling a method from params"
  (let* ((param-pairs (mapcar 'rtm-format-param params))
         (query (rtm-join-params param-pairs)))
    (string-to-unibyte (concat base-uri "?" query))))

;; adapted from avdi's code:
(defun rtm-format-param (param)
  (let ((key (car param))
        (value (cdr param)))
    ;; it's important that we sign the unencoded parameters, but of
    ;; course the request must be url-encoded
    (concat key "=" (url-hexify-string value))))

;; from avdi's code:
(defun rtm-join-params (params)
  (cl-reduce (lambda (left right) (concat left "&" right)) params))

;; adapted from avdi's code:
(defun rtm-construct-url (method)
  (concat rtm-rest-uri
          "?"
          "method=" method
          "&"
          "api_key=" rtm-api-key))

;; from avdi's code:
;; TODO Interpret the stat attribute and throw an error if it's not ok
(defun rtm-parse-response (response)
  (with-current-buffer response
    (let* ((node-list (xml-parse-region (point-min) (point-max)))
           (rsps (rtm-get-nodes-from-node-list 'rsp node-list)))
      (when (> (length rsps) 1)
        (warn
         "Got more than one <rsp> node in response, please examine!
Response:%s" (pp node-list)))
      (let* ((rsp (car rsps))
             (children (xml-node-children rsp))
             (stat (rtm-stat rsp)))
        (unless stat
          (warn "Weird, got no stat attribute in <rsp> node.
%s" (pp node-list)))
        (if (eq stat 'ok)
            children
          (let* ((err (car (rtm-get-nodes-from-node-list 'err children)))
                 (code (xml-get-attribute err 'code))
                 (msg (xml-get-attribute err 'msg)))
            (error "Error in server response: Code: %s\n
Message: \"%s\"" code msg)))))))

(defun rtm-stat (rsp)
  (let ((stat (xml-get-attribute-or-nil rsp 'stat)))
    (if stat
        (intern (downcase stat))
      stat)))

;;; example responses
;; failure:
;; ((rsp
;;   ((stat . "fail"))
;;   (err
;;    ((code . "97")
;;     (msg . "Missing signature")))))
;; success:
;; rtm.auth.getFrob:
;; ((rsp
;;   ((stat . "ok"))
;;   (frob nil "cce8d04e182212cddd5cdc815e09648fecd18e0e")))
;; rtm.test.echo:
;; ((rsp ((stat . "ok"))
;;       (api_key nil "00000000000000000000000000000000")
;;       (method nil "rtm.test.echo")))
(defun append-api-sig (unsigned-params)
  (let ((api-sig (rtm-api-sig unsigned-params)))
    (append unsigned-params
            `(("api_sig" . ,api-sig)))))

;;;; Misc/Helper functions
(defun maybe-string (symbol-or-string)
  (if (stringp symbol-or-string) symbol-or-string
    (symbol-name symbol-or-string)))

(provide 'rtm)

;;; rtm.el ends here
