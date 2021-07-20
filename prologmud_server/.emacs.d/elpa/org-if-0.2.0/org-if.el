;;; org-if.el --- Interactive Fiction Authoring System for Org-Mode -*- lexical-binding: t -*-

;; Copyright © 2015 Philip Woods

;; Author: Philip Woods <elzairthesorcerer@gmail.com>
;; Keywords: if, org-if, org org-mode

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package helps you create Choose Your Own Adventure stye Interactive
;; Fiction using Emacs and Org-Mode.

;;; Code:

(require 'ob-org-if)
(require 'org-if-active)
(require 'org-if-interpreter)
(require 'org-if-link)
(require 'org-if-misc)
(require 'org-if-mode)
(require 'org-if-reader)



(provide 'org-if)
;;; org-if.el ends here
