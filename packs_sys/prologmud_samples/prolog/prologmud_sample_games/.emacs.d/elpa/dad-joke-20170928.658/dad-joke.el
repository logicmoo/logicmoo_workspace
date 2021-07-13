;;; dad-joke.el --- Get/display dad jokes  -*- lexical-binding: t -*-
;; Copyright 2017 by Dave Pearson <davep@davep.org>

;; Author: Dave Pearson <davep@davep.org>
;; Version: 1.4
;; Package-Version: 20170928.658
;; Package-Commit: bee47e7b746b403228fa7d7361cb095de19ac9ba
;; Keywords: games
;; URL: https://github.com/davep/dad-joke.el
;; Package-Requires: ((emacs "24"))

;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at your
;; option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
;; Public License for more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; dad-joke.el is a terrible bit of elisp code inspired by seeing
;; https://goo.gl/NXTJXk and also with https://goo.gl/ji4Viv in mind. With
;; "thanks" to Sue for being responsible for pointing me at the former, and
;; thus reminding me of the latter.

;;; Code:

(require 'url-vars)

(defconst dad-joke-server-url "https://icanhazdadjoke.com/"
  "URL for the dad joke server.")

(defconst dad-joke-user-agent "dad-joke.el (https://github.com/davep/dad-joke.el)"
  "User agent to send to the dad joke server.")

(defun dad-joke-get ()
  "Acquire a dad joke from the dad joke server."
  (let* ((url-mime-accept-string "text/plain")
         (url-request-extra-headers `(("User-Agent" . ,dad-joke-user-agent)))
         (url-show-status nil))
    (with-temp-buffer
      (url-insert-file-contents dad-joke-server-url)
      (buffer-string))))

;;;###autoload
(defun dad-joke (&optional insert)
  "Display a dad joke in the minibuffer.

If INSERT is non-nil the joke will be inserted into the current
buffer rather than shown in the minibuffer."
  (interactive "P")
  (let ((joke (dad-joke-get)))
    (if (zerop (length joke))
        (error "I didn't get the joke! :-(")
      (if insert
          (insert joke)
        (message "%s" joke)))))

(provide 'dad-joke)

;;; dad-joke.el ends here
