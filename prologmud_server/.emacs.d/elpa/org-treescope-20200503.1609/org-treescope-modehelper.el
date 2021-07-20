;;; org-treescope-modehelper.el --- File for populating org-treescope modes  -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Mehmet Tekman <mtekman89@gmail.com>

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Commentary:

;; see org-treescope.el

;;; Code:
(require 'cl-lib)

(defvar org-treescope-modehelper-list nil
  "Alist of KEY BINDING pairs, populated throughout.")

(defvar org-treescope-modehelper--publicfinishhook nil
  "Hook to run at the end of a public function.")

(defvar org-treescope-modehelper--orgbuffer nil
  "Apply match function to org buffer.")

(defun org-treescope-modehelper--runpublichook ()
  "Run the public finish hook."
  (run-hooks 'org-treescope-modehelper--publicfinishhook))

(provide 'org-treescope-modehelper)
;;; org-treescope-modehelper.el ends here
