;;; libbcel-client.el --- Handles connection to the Basecamp 3 API  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Damien Cassou

;; Author: Damien Cassou <damien@cassou.me>
;; Url: https://gitlab.petton.fr/bcel/libbcel
;; Package-requires: ((emacs "26.1"))
;; Version: 0.4.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This file takes care of communicating with Basecamp 3 API.  The
;; authentication part is handled by `libbcel-oauth.el'.

;;; Code:

(require 'request)
(require 'json)

(require 'libbcel-oauth)

(defcustom libbcel-client-account-id nil
  "The account id to connect to.
This is the first number appearing after basecamp.com in the URL
when you are on the basecamp website."
  :type 'string
  :group 'libbcel)

(defvar libbcel-client--oauth-store nil
  "Remembers the OAuth authentication data.")

(defun libbcel-client--oauth-store ()
  "Return the OAuth authentication data."
  (or libbcel-client--oauth-store
      (setq libbcel-client--oauth-store (libbcel-oauth-get-store))))

(defvar libbcel-client--log-directory (expand-file-name "libbcel-client-logs" temporary-file-directory)
  "Temporary directory where to put communication logs.")

(defun libbcel-client--make-log-subdir (url)
  "Create a subdirectory in `libbcel-client--log-directory' to store logs for a request on URL."
  (let* ((subdir-name (format "%s_%s"
                              (format-time-string "%F_%H:%M:%S")
                              (replace-regexp-in-string "/" "_" url)))
         (subdir (expand-file-name subdir-name libbcel-client--log-directory)))
    (make-directory subdir t)
    subdir))

(defun libbcel-client--write-log (content filename url)
  "Write CONTENT to FILENAME in a temporary directory based on URL.

CONTENT can be anything that can be printed with `princ'."
  (when content
    (with-temp-file (expand-file-name filename (libbcel-client--make-log-subdir url))
      ;; use #'format because CONTENT is not necessarily a string.
      (insert (format "%s" content))
      (ignore-errors (pp-buffer)))))

(cl-defun libbcel-client-request (access-token url &rest rest &allow-other-keys)
  "Call `request'.

URL, SUCCESS, ERROR are passed to `request'.

ACCESS-TOKEN is found in the result of the OAUTH2 authentication.
See `libbcel-oauth-get-access-token'."
  (let* ((request-log-level 'trace)
         (log-buffer-name (generate-new-buffer-name (format " *libbcel-client-%s*" url)))
         (request-log-buffer-name log-buffer-name)
         (log-filename (expand-file-name "request-logs" (libbcel-client--make-log-subdir url))))
    (apply
     #'request
     url
     :timeout 5
     :headers `(("User-Agent" . "bcel (damien@cassou.me)")
                ("Authorization" . ,(format "Bearer %s" access-token)))
     :parser #'json-read
     :complete (cl-function
                (lambda (&key data error-thrown symbol-status response &allow-other-keys)
                  (let ((save-silently t))
                    (with-current-buffer log-buffer-name
                      (write-file log-filename))
                    (libbcel-client--write-log data "data" url)
                    (libbcel-client--write-log (cdr error-thrown) (format "error:%s" (car error-thrown)) url)
                    (libbcel-client--write-log symbol-status (format "status:%s" symbol-status) url)
                    (libbcel-client--write-log response "response" url))))
     rest)))

(defun libbcel-client--get-url-from-token (access-token url &optional callback params)
  "Do a GET query to Basecamp 3 API at URL.
If PARAMS is non-nil it should be an alist that is passed to the GET request.

ACCESS-TOKEN is found in the result of the OAUTH2 authentication.
See `libbcel-oauth-get-access-token'.

When CALLBACK is non-nil, evaluate it with the response."
  (libbcel-client-request
   access-token
   url
   :type "GET"
   :params params
   :parser #'json-read
   :success (cl-function (lambda (&key data &allow-other-keys)
                           (when callback
                             (funcall callback data))))))

(defun libbcel-client--delete-url-from-token (access-token url &optional callback)
  "Do a DELETE query to Basecamp 3 API at URL.

ACCESS-TOKEN is found in the result of the OAUTH2 authentication.
See `libbcel-oauth-get-access-token'.

When CALLBACK is non-nil, evaluate it with the response."
  (libbcel-client-request
   access-token
   url
   :type "DELETE"
   :parser #'json-read
   :success (cl-function (lambda (&key data &allow-other-keys)
                           (when callback
                             (funcall callback data))))))

(defun libbcel-client--post-url-from-token (access-token url &optional callback)
  "Do a POST query to Basecamp 3 API at URL.

ACCESS-TOKEN is found in the result of the OAUTH2 authentication.
See `libbcel-oauth-get-access-token'.

When CALLBACK is non-nil, evaluate it with the response."
  (libbcel-client-request
   access-token
   url
   :type "POST"
   :parser #'json-read
   :success (cl-function (lambda (&key data &allow-other-keys)
                           (when callback
                             (funcall callback data))))))

(defun libbcel-client--get-path-from-token (access-token account-id path &optional callback)
  "Execute CALLBACK with the result of the GET call to PATH.

ACCESS-TOKEN can be retrieved with `libbcel-oauth-get-access-token'.

ACCOUNT-ID is the first number appearing after basecamp.com in
the URL when you are on the basecamp website."
  (libbcel-client--get-url-from-token
   access-token
   (format "https://3.basecampapi.com/%s/%s"
           account-id
           path)
   callback))

(defun libbcel-client-get-path (path &optional callback)
  "Execute CALLBACK with the result of a GET call to PATH."
  (libbcel-oauth-get-access-token
   (libbcel-client--oauth-store)
   (lambda (access-token)
     (libbcel-client--get-path-from-token access-token libbcel-client-account-id path callback))))

(defun libbcel-client-get-url (url &optional callback params)
  "Do a GET request on URL and evaluate CALLBACK with the result.
If PARAMS is non-nil it should be an alist that is passed to the GET request."
  (libbcel-oauth-get-access-token
   (libbcel-client--oauth-store)
   (lambda (access-token)
     (libbcel-client--get-url-from-token access-token url callback params))))

(defun libbcel-client-delete-url (url &optional callback)
  "Do a DELETE request on URL and evaluate CALLBACK with the result."
  (libbcel-oauth-get-access-token
   (libbcel-client--oauth-store)
   (lambda (access-token)
     (libbcel-client--delete-url-from-token access-token url callback))))

(defun libbcel-client-post-url (url &optional callback)
  "Do a POST request on URL and evaluate CALLBACK with the result."
  (libbcel-oauth-get-access-token
   (libbcel-client--oauth-store)
   (lambda (access-token)
     (libbcel-client--post-url-from-token access-token url callback))))

(provide 'libbcel-client)
;;; libbcel-client.el ends here
