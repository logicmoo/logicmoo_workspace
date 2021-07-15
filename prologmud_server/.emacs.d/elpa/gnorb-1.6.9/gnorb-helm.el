;;; gnorb-helm.el --- Interface between Helm and Gnorb  -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2020  Free Software Foundation, Inc.

;; Author: Eric Abrahamsen <eric@ericabrahamsen.net>

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

;; Convenience functions relying on the helm package.

;;; Code:

(require 'gnorb-gnus)
(require 'nngnorb)
(require 'gnorb-utils)

(declare-function 'helm-make-source "ext:helm-source")
(declare-function 'helm "ext:helm")
(declare-function 'helm-marked-candidates "ext:helm")

(defun gnorb-helm-gnus-registry-candidates ()
  "Return a list of candidates from the Gnus registry."
  (let ((check
	 (when (or gnus-ignored-from-addresses
		   message-alternative-emails)
	   (cond ((functionp gnus-ignored-from-addresses)
		  (lambda (adr) (funcall gnus-ignored-from-addresses adr)))
		 ((stringp gnus-ignored-from-addresses)
		  (lambda (adr)
		    (string-match-p
		     gnus-ignored-from-addresses adr)))
		 ((functionp message-alternative-emails)
		  (lambda (adr) (funcall message-alternative-emails adr)))
		 ((stringp message-alternative-emails)
		  (lambda (adr)
		    (string-match-p
		     message-alternative-emails adr))))))
	ret from recipient subject group)
    (maphash
     (lambda (msg-id data)
       (when (setq group (car-safe (cdr (assoc 'group data)))
		   from (car-safe (cdr (assoc 'sender data)))
		   subject (car-safe (cdr (assoc 'subject data)))
		   recipient (cdr (assoc 'recipient data)))
	 (push (cons (format
		      "%s: %s" ; display
		      (if (and check
			       (funcall check from))
			  (concat
			   "To: " (mapconcat #'identity recipient " "))
			from)
		      subject)
		     (cons msg-id group)) ; real
	       ret)))
     (slot-value gnus-registry-db 'data))
    ret))

;;;###autoload
(defun gnorb-helm-search-registry ()
  "Use helm and the Gnus registry to search messages."
  (interactive)
  (require 'helm)
  (unless (gnus-alive-p)
    (error "Gnus is not running"))
  (unless gnus-registry-enabled
    (error "The Gnus registry is not enabled"))
  (let* ((msgs (helm :sources
		     (helm-make-source "Gnus Registry" 'helm-source-sync
		       :candidates #'gnorb-helm-gnus-registry-candidates
		       :action (lambda (&rest _ignored) (helm-marked-candidates)))
		     :buffer "*helm Gnus Registry*"))
	 (server (gnorb-gnus-find-gnorb-server))
	 (artlist
	  (mapcar
	   (lambda (msg)
	     (pcase-let ((`(,group . ,artno) (gnorb-msg-id-request-head
					      (car msg) (cdr msg))))
	       (when (and artno (integerp artno) (> artno 0))
		 (vector group artno 100))))
	   msgs))
	 (name (make-temp-name "registry messages"))
	 (spec (list
		(cons 'nnir-specs (list (cons 'nnir-query-spec
					      `((query . "dummy")
						(articles . ,artlist)))
					(cons 'nnir-group-spec
					      `((,server ,(list name))))))
		(cons 'nnir-artlist nil))))
    (when msgs
      (switch-to-buffer gnus-group-buffer)
      (gnus-group-read-ephemeral-group
       name `(nnir ,server) nil `(switch-to-buffer ,gnus-group-buffer)
       nil nil spec))))

(provide 'gnorb-helm)
;;; gnorb-helm.el ends here
