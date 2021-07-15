;;; nngnorb.el --- Gnorb backend for Gnus -*- lexical-binding: t -*-

;; Copyright (C) 2018-2020  Free Software Foundation, Inc.

;; Author: Eric Abrahamsen <eric@ericabrahamsen.net.>

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a backend for supporting Gnorb-related stuff.  In Emacs 28
;; this file is not needed, and won't be loaded.

;; In Emacs 27 and below, Gnus searches are governed by nnir.el.
;; Because of the way nnir is set up, the actual function to call the
;; search is hardcoded to the server-type found in the nnir address.

;; The upshot is that, if you want to return arbitrary lists of
;; messages, possibly from multiple groups/servers, you can't simply
;; feed your own search function to nnir.  You have to create a whole
;; new Gnus server, and then associate your search function with that
;; server in `nnir-engines'.  Thus this library, which creates an
;; entire fake Gnus backend and requires users to add it to their Gnus
;; config, just so we can call our function.

;; It works by creating an nnir group with a gnorb address.  nnir then
;; looks up the gnorb address and sees that it should use
;; `nnir-run-gnorb' for the search, it calls that function, which ends
;; up calling back to `gnorb-run-search', which is the function that
;; does the real work.

;; In Emacs 28 and above, Gnorb searches are displayed using the
;; nnselect backend, which allows us to specify our own arbitrary
;; function -- `gnorb-run-search' -- for retrieving search results,
;; making the whole thing much simpler.

;;; Code:

(require 'gnus)
(eval-and-compile
  (require 'gnus-group)
  (require 'nnheader)
  (require 'nnir))

(declare-function gnorb-run-search "gnorb-gnus")

(defvar nngnorb-status-string "")

(gnus-declare-backend "nngnorb" 'post-mail 'virtual)

(add-to-list 'nnir-method-default-engines '(nngnorb . gnorb))

(add-to-list 'nnir-engines
	     '(gnorb nnir-run-gnorb))

(defun nnir-run-gnorb (query _server &optional _group)
  "Run the actual search for messages to display. See nnir.el for
some details of how this gets called.

As things stand, the query string can be given as one of two
different things. First is the ID string of an Org heading,
prefixed with \"id+\". This was probably a bad choice as it could
conceivably look like an org tags search string. Fix that later.
If it's an ID, then the entire subtree text of that heading is
scanned for gnus links, and the messages relevant to the subtree
are collected from the registry, and all the resulting messages
are displayed in an ephemeral group.

Otherwise, the query string can be a tags match string, a la the
Org agenda tags search. All headings matched by this string will
be scanned for gnus messages, and those messages displayed."
  (if (cdr-safe (assq 'articles query))
      ;; The work has already been done elsewhere.
      (cdr (assq 'articles query))
    (let ((q (cdr (assq 'query query))))
      (when (and (equal "5.13" gnus-version-number) (version< emacs-version "24.4"))
	(setq q (car q)))
      (gnorb-run-search q))))

(defun gnorb-gnus-nnir-search (str persist head-text ret)
  "Create an nnir group that is set up to run a Gnorb search."
  (let* ((nnir-address (gnorb-find-gnorb-server))
	 (name (if persist
		   (read-string
		    (format "Name for group (default %s): " head-text)
		    nil nil head-text)
		 (concat "gnorb-" str)))
	 (method (list 'nnir nnir-address))
	 (spec (list
		(cons 'nnir-specs (list (cons 'nnir-query-spec
					      `((query . ,str)))
					(cons 'nnir-group-spec
					      `((,nnir-address ,(list name))))))
		(cons 'nnir-artlist nil))))
    (if persist
	(progn
	  (switch-to-buffer gnus-group-buffer)
	  (gnus-group-make-group name method nil spec)
	  (gnus-group-select-group))
      (gnus-group-read-ephemeral-group name method nil ret nil nil spec))))

(defun gnorb-gnus-nnir-registry-search (articles)
  (let ((server (gnorb-find-gnorb-server)))
    (gnus-group-read-ephemeral-group
     "registry messages" `(nnir ,server)
     nil `(switch-to-buffer ,gnus-group-buffer)
     nil nil `((nnir-specs ((nnir-query-spec
			     ((query . "dummy")
			      (articles . ,articles)))
			    (nnir-group-spec
			     ((,server ("registry messages"))))))
	       (nnir-artlist)))))

(defun gnorb-find-gnorb-server (&optional no-error)
  "Try very hard to find a local nngnorb server.
If NO-ERROR is non-nil, return nil on failure, otherwise an
error."
  (or (catch 'found
	;; Try very hard to find the server.
	(when (assoc 'nngnorb gnus-secondary-select-methods)
	  (throw 'found
		 (format
		  "nngnorb:%s"
		  (nth 1 (assoc 'nngnorb
				gnus-secondary-select-methods)))))
	(dolist (s (append gnus-server-alist gnus-server-method-cache))
	  (when (eq 'nngnorb (cadr s))
	    (throw 'found (car s)))))
      (unless no-error
	(user-error
	 "Please add a \"nngnorb\" backend to your gnus installation."))))



(defun nngnorb-retrieve-headers (_articles &optional _group _server _fetch-old)
  (with-current-buffer nntp-server-buffer
    (erase-buffer))
  'nov)

(defun nngnorb-open-server (_server &optional _definitions)
  t)

(defun nngnorb-close-server (&optional _server)
  t)

(defun nngnorb-request-close ()
  t)

(defun nngnorb-server-opened (&optional _server)
  t)

(defun nngnorb-status-message (&optional _server)
  nngnorb-status-string)

(defun nngnorb-request-article (_article &optional _group _server _to-buffer)
  (setq nngnorb-status-string "No such group")
  nil)

(defun nngnorb-request-group (_group &optional _server _fast _info)
  (let (deactivate-mark)
    (with-current-buffer nntp-server-buffer
      (erase-buffer)
      (insert "411 no such news group\n")))
  (setq nngnorb-status-string "No such group")
  nil)

(defun nngnorb-close-group (_group &optional _server)
  t)

(defun nngnorb-request-list (&optional _server)
  (with-current-buffer nntp-server-buffer
    (erase-buffer))
  t)

(defun nngnorb-request-post (&optional _server)
  (setq nngnorb-status-string "Read-only server")
  nil)

(provide 'nngnorb)

;;; nnnil.el ends here
