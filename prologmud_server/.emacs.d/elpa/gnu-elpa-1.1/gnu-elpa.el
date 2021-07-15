;;; gnu-elpa.el --- Advertize GNU ELPA packages  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Free Software Foundation, Inc.

;; Author: Stefan Monnier <monnier@iro.umontreal.ca>
;; Keywords:
;; Version: 1.1

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

;; This package adds placeholders for the features defined by GNU ELPA packages
;; such that the users are informed about the existence of those features and
;; so they can easily install the relevant package on the spot.

;;;; FIXME/TODO:

;; - Allow packages more control over what is auto-predefined.
;; - Don't just silently drop those packages with more than 10 autoloads.
;; - Allow more than `auto-mode-alist' and `autoload's, e.g. allow
;;   new menu entries.
;; - Merge with `gnu-elpa-keyring-update'?

;;; Code:

;; ¡¡ BEWARE !!  Don't try to load this file via `load' or `require'!
;; It's only used as a kind of trigger to find the function that is being
;; autoloaded.

;; This is done because:
;; - We want `gnu-elpa' to use `autoload', so that a subsequent call to
;;   `autoload' performed by the actual real package (if/hen it's installed)
;;   will override our pseudo-autoload.
;; - We don't want to use a separate file for every advertized package,
;;   so we pretend all the functions get "autoloaded" from `gnu-elpa.el'.

;; This file is not meant to be `require'd but to be loaded in response
;; to calling a function (i.e. via autoload) and it will find the package
;; that provides this function and suggest installing the package.
;;(provide 'gnu-elpa)

(require 'gnu-elpa-utils)

(gnu-elpa--perform-autoload)

;;; gnu-elpa.el ends here
