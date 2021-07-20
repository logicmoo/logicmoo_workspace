;;; use-package-hydra.el --- Adds :hydra keyword to use-package macro

;; Copyright (C) 2018 Toon Claes

;; Author: Toon Claes <toon@iotcl.com>
;; Maintainer: Toon Claes <toon@iotcl.com>
;; Created: 6 Jan 2018
;; Modified: 18 Aug 2018
;; Version: 0.1
;; Package-Version: 0.2
;; Package-Commit: 8cd55a1128fbdf6327bb38a199d206225896d146
;; Package-Requires: ((emacs "24.3") (use-package "2.4"))
;; Keywords: convenience extensions tools
;; URL: https://gitlab.com/to1ne/use-package-hydra

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Provides support for the :hydra keyword, which is made available by
;; default by requiring `use-package'.

;;; Code:

(require 'use-package-core)

(defun use-package-hydra--name (name)
  "Build hydra name for the package NAME."
  (cl-gentemp (concat "hydra-" (symbol-name name))))

(defun use-package-hydra--normalize (name _keyword args)
  "Normalize the ARGS to be a list hydras.
It accepts a single hydra, or a list of hydras.  It is optional
provide a name for the hydra, if so there is a name generated
from NAME."
  (let (name result*)
    (while args
      (cond
       ;; single named hydra
       ((symbolp (car args))
        (setq result* (nconc result* (list args))
              args nil))
       ;; single unnamed hydra with docstring
       ((stringp (nth 2 args))
        (setq name (use-package-hydra--name name)
              result* (nconc result* (list (cons name args)))
              args nil))
       ;; TODO single unnamed hydra without docstring

       ;; list of hydras
       ((listp (car args))
        (setq result*
              (nconc result* (use-package-hydra--normalize name _keyword (car args)))
              args (cdr args)))
       ;; Error!
       (t
        (use-package-error
         (concat (symbol-name name)
                 " wants arguments acceptable to the `defhydra' macro,"
                 " or a list of such values")))))
    result*))

;;;###autoload
(defalias 'use-package-normalize/:hydra 'use-package-hydra--normalize
  "Normalize for the definition of one or more hydras.")

;;;###autoload
(defun use-package-handler/:hydra (name keyword args rest state)
  "Generate defhydra with NAME for `:hydra' KEYWORD.
ARGS, REST, and STATE are prepared by `use-package-normalize/:hydra'."
  (use-package-concat
   (mapcar #'(lambda (def) `(defhydra ,@def))
           args)
   (use-package-process-keywords name rest state)))

(add-to-list 'use-package-keywords :hydra t)

(provide 'use-package-hydra)
;;; use-package-hydra.el ends here
