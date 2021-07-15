;;; gnorb.el --- Glue code between Gnus, Org, and BBDB -*- lexical-binding: t -*-

;; Copyright (C) 2018-2020  Free Software Foundation, Inc.

;; Version: 1.6.9
;; Package-Requires: ((cl-lib "0.5"))

;; Maintainer: Eric Abrahamsen <eric@ericabrahamsen.net>

;; Author: Eric Abrahamsen <eric@ericabrahamsen.net>
;; Keywords: mail org gnus bbdb todo task

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

;; Load this file to load everything.

;;; Code:

(with-eval-after-load 'gnus
  (require 'gnorb-gnus)
  (require 'gnorb-registry))
(with-eval-after-load 'bbdb
  (require 'gnorb-bbdb))
(with-eval-after-load 'org
  (require 'gnorb-org))

(provide 'gnorb)
;;; gnorb.el ends here
