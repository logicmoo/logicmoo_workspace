;;; orgnav-clock.el --- Orgnav convenience functions for clocking into and our of items -*- lexical-binding: t -*-


;; Copyright (C) 2016 Facet Framer

;; Author: Facet Framer (facet@facetframer.com)
;; URL: github.com/facetframer/orgnav

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


;;; Commentary:
;;

;;; Code:

(require 'orgnav)

(defvar orgnav-clock-buffer nil "The buffer to search when clocking in.")
(defvar orgnav-clock-depth 2 "How many levels to show for clock operations.")

;;; Interactive entry points for clocking
(defun orgnav-clock-in (&rest properties)
  "Clock in to a node.  PROPERTIES is plist which can include the optional keys `:buffer`, `:node` and `depth`."
  (interactive (list :buffer orgnav-clock-buffer))
  (orgnav--assert-plist properties :depth :node :buffer)
  (-let (((&plist :buffer buffer :node node-point :depth depth) properties ))
    (setq buffer (or buffer (current-buffer) ))
    (setq depth (or depth orgnav-clock-depth ))
    (with-current-buffer buffer
      (save-excursion
        (orgnav-search-subtree node-point
                               :depth depth
                               :default-action 'orgnav--clock-action
                               :helm-buffer-name "*orgnav-clock-in*")))))

(defun orgnav-clock-in-point (&rest properties)
  "Convenience function to clock into a child of the current node.
PROPERTIES is a plist of properties as for `orgnav-clock-in`."
  (interactive)
  (apply 'orgnav-clock-in (plist-put properties :node (point))))


(defun orgnav-search-clocking ()
  "Start a search relative to the currently clocking activity."
  (interactive)
  (save-excursion
    (org-clock-goto)
    (orgnav-search-ancestors)))

(provide 'orgnav-clock)
;;; orgnav-clock.el ends here
