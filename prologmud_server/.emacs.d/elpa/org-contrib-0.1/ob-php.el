;;; ob-php.el --- Execute PHP within org-mode blocks.
;; Copyright 2016, 2021 stardiviner

;; Author: stardiviner <numbchild@gmail.com>
;; Maintainer: stardiviner <numbchild@gmail.com>
;; Keywords: org babel php
;; URL: https://github.com/stardiviner/ob-php
;; Created: 04th May 2016
;; Version: 0.0.1
;; Package-Requires: ((org "8"))

;; This file is not part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Execute PHP within org-mode blocks.

;;; Code:
(require 'org)
(require 'ob)

(defgroup ob-php nil
  "org-mode blocks for PHP."
  :group 'org)

(defcustom org-babel-php-command "php"
  "The command to execute babel body code."
  :group 'ob-php
  :type 'string)

(defcustom org-babel-php-command-options nil
  "The php command options to use when execute code."
  :group 'ob-php
  :type 'string)

(defcustom ob-php:inf-php-buffer "*php*"
  "Default PHP inferior buffer."
  :group 'ob-php
  :type 'string)

;;;###autoload
(defun org-babel-execute:php (body params)
  "Orgmode Babel PHP evaluate function for `BODY' with `PARAMS'."
  (let* ((cmd (concat org-babel-php-command " " org-babel-php-command-options))
         (body (concat "<?php\n" body "\n?>")))
    (org-babel-eval cmd body)))

;;;###autoload
(eval-after-load 'org
  '(add-to-list 'org-src-lang-modes '("php" . php)))

(defvar org-babel-default-header-args:php '())

(add-to-list 'org-babel-default-header-args:php
             '(:results . "output"))

(provide 'ob-php)

;;; ob-php.el ends here
