;;; kmb.el --- Kill buffers matching a regexp w/o confirmation  -*- lexical-binding: t -*-

;; Copyright (C) 2017  Free Software Foundation, Inc.

;; Author: Tino Calancha <tino.calancha@gmail.com>
;; Keywords: lisp, convenience

;; Maintainer: Tino Calancha
;; Created: Wed May 24 13:19:18 JST 2017
;; Version: 0.1
;; Package-Requires: ((emacs "24.1"))
;; Last-Updated: Fri May 26 21:17:10 JST 2017
;;           By: calancha
;;     Update #: 1
;; Compatibility: GNU Emacs 24.x
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This file is part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Commentary:
;;
;;  This library provides the commands `kmb-kill-matching-buffers-no-ask'
;;  and `kmb-delete-process-and-kill-buffer-no-ask'.  The former kills
;;  buffers whose name matches a regular expression.  The latter,
;;  interactively kills the current buffer and if called from Lisp,
;;  then accepts a list of buffers to kill.
;;  Any of these commands ask for confirmation to kill the buffers.
;;  If one of the buffers is running a process, then the process is
;;  deleted before kill the buffer.
;; 
;;  This file also defines the commands `kmb-list-matching-buffers' and
;;  `kmb-list-buffers-matching-content' to list the buffers whose name
;;  or content match a regexp.
;;
;;
;;  Commands defined here:
;;
;;   `kmb-delete-process-and-kill-buffer-no-ask',
;;   `kmb-kill-matching-buffers-no-ask', `kmb-list-buffers-matching-content',
;;   `kmb-list-matching-buffers'.
;;
;;  Non-interactive functions defined here:
;;
;;   `kmb--show-matches'.
;;

;;
;;; Code:


(defun kmb--show-matches (buffers &optional count)
  "Show the name of BUFFERS in the echo area.

BUFFERS is a list of buffers.
If optional arg COUNT is non-nil, then it's the length
of BUFFERS."
  (if (null buffers)
      (message "No buffers matching regexp")
    (message "Found %d match%s: %s"
             (or count (length buffers))
             (if (cdr buffers) "es" "")
             (mapconcat #'buffer-name buffers ", "))))

(defun kmb-list-matching-buffers (regexp &optional with-process)
  "Return list of buffers whose name matching REGEXP.
If optional arg WITH-PROCESS is non-nil, then list just buffers
running a process."
  (interactive
   (let* ((prefix current-prefix-arg)
          (regexp
           (if prefix
               (read-string "List buffers running a process \
and matching regexp: ")
             (read-string "List buffers matching regexp: "))))
     (list regexp prefix)))
  (let ((buffers
         (delq nil
               (mapcar
                (lambda (x)
                  (when (string-match regexp (buffer-name x))
                    (cond (with-process
                           (and (get-buffer-process x) x))
                          (t x))))
                (buffer-list)))))
    (kmb--show-matches buffers)
    buffers))

(defun kmb-kill-matching-buffers-no-ask (regexp)
  "Kill all buffers whose name matching REGEXP without confirmation.
If a buffer is running a process, then delete the process before
kill the buffer."
  (interactive "sKill buffers matching regexp: ")
  (dolist (b (buffer-list))
    (when (string-match regexp (buffer-name b))
      (kmb-delete-process-and-kill-buffer-no-ask b))))

(defalias 'kmb-kill-matching-buffers 'kmb-kill-matching-buffers-no-ask)

(defun kmb-list-buffers-matching-content (regexp)
  "Return list of buffers whose content match REGEXP."
  (interactive "sList buffers whose content matches regexp: ")
  (let* ((count 0)
         (buffers
          (delq nil
                (mapcar
                 (lambda (b)
                   (let (str)
                     (with-current-buffer b
                       (setq str (buffer-substring-no-properties
                                  (point-min) (point-max))))
                     (when (string-match regexp str)
                       (setq count (1+ count)) b)))
                 (buffer-list)))))
    (kmb--show-matches buffers count)
    buffers))

(defun kmb-delete-process-and-kill-buffer-no-ask (&optional buffer)
  "Delete BUFFER without confirmation.
BUFFER is a buffer or a list of buffers.
If the buffer is running a process, then delete the processes
before kill the buffer.
Interactivelly, delete the current buffer."
  (interactive "i")
  (let* ((def (or buffer (current-buffer)))
         (buffers
          (delq nil
                (mapcar #'get-buffer (if (nlistp def) (list def) def))))
         (processes (process-list)))
    (dolist (buf buffers)
      (when (get-buffer-process buf)
        (dolist (proc processes)
          (when (eq buf (process-buffer proc))
            (set-process-query-on-exit-flag proc nil))))
      (when (buffer-modified-p buf)
        (with-current-buffer buf
          (set-buffer-modified-p nil)))
      (kill-buffer buf))))

(defalias 'kmb-kill-buffer 'kmb-delete-process-and-kill-buffer-no-ask)

;;;; ChangeLog:

;; 2017-05-31  Tino Calancha  <tino.calancha@gmail.com>
;; 
;; 	New file kmb.el
;; 
;; 	See following link for details: 
;; 	https://lists.gnu.org/archive/html/emacs-devel/2017-05/msg00686.html
;; 	* kmb.el: New file.
;; 


(provide 'kmb)

;;; kmb.el ends here
