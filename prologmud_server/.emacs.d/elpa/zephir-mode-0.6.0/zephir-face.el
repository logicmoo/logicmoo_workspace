;;; zephir-face.el --- Face definitions for Zephir code -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2020 Free Software Foundation, Inc

;; Author: Serghei Iakovlev <egrep@protonmail.ch>
;; Maintainer: Serghei Iakovlev <egrep@protonmail.ch>
;; Version: 0.6.0
;; URL: https://github.com/zephir-lang/zephir-mode
;; Keywords: languages
;; Package-Requires: ((emacs "24.3"))
;; Revision: $Format:%h (%cD %d)$

;; This file is NOT part of GNU Emacs.

;;;; License

;; This file is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Face definitions for Zephir code.

;;; Code:

;;;###autoload
(defgroup zephir-faces nil
  "Faces used in Zephir Mode"
  :tag "Zephir Faces"
  :group 'zephir
  :group 'faces)

(defface zephir-keyword-face
  '((t (:inherit font-lock-keyword-face)))
  "Zephir Mode face used to highlight keywords."
  :group 'zephir-faces
  :tag "Zephir Keyword")

(defface zephir-function-name-face
  '((t (:inherit font-lock-function-name-face)))
  "Zephir Mode face used to highlight function names."
  :group 'zephir-faces
  :tag "Zephir Function Name")

(defface zephir-function-call-face
  '((t ()))
  "Zephir Mode face used to highlight function names in calles."
  :group 'zephir-faces
  :tag "Zephir Function Call")

(defface zephir-method-call-face
  '((t (:inherit zephir-function-call-face)))
  "Zephir Mode face used to highlight method names in calles."
  :group 'zephir-faces
  :tag "Zephir Method Call")

(defface zephir-variable-name-face
  '((t (:inherit font-lock-variable-name-face)))
  "Zephir Mode face used to highlight variable names."
  :group 'zephir-faces
  :tag "Zephir Variable Name")

(defface zephir-property-name-face
  '((t (:inherit zephir-variable-name-face)))
  "Zephir Mode face used to highlight property names."
  :group 'zephir-faces
  :tag "Zephir Property Name")

(defface zephir-operator-face
  '((t ()))
  "Zephir Mode face used to operators."
  :group 'zephir-faces
  :tag "Zephir Operator")

(defface zephir-object-operator-face
  '((t (:inherit zephir-operator-face)))
  "Zephir Mode face used to object operators (‘->’)."
  :group 'zephir-faces
  :tag "Zephir Object Operator")

(defface zephir-type-face
  '((t (:inherit font-lock-type-face)))
  "Zephir Mode face used to highlight types."
  :group 'zephir-faces
  :tag "Zephir Type")

(defface zephir-constant-face
  '((t (:inherit font-lock-constant-face)))
  "Zephir Mode face used to highlight constants."
  :group 'zephir-faces
  :tag "Zephir Constant")

(defface zephir-magical-constant-face
  '((t (:inherit font-lock-builtin-face)))
  "Zephir Mode face used to highlight magical constants."
  :group 'zephir-faces
  :tag "Zephir Magical Constant")

(defface zephir-constant-assign-face
  '((t (:inherit font-lock-type-face)))
  "Zephir Mode face used to highlight constant assigning (‘const’ statement)."
  :group 'zephir-faces
  :tag "Zephir Constant Assign")

(defface zephir-this-face
  '((t (:inherit zephir-constant-face)))
  "Zephir Mode face used to highlight ‘this’ variables."
  :group 'zephir-faces
  :tag "Zephir this")

(defface zephir-class-declaration-face
  '((t (:inherit zephir-keyword-face)))
  "Zephir Mode Face used to class declarations (‘class’, ‘interface’)."
  :group 'zephir-faces
  :tag "Zephir Class Declaration")

(defface zephir-class-declaration-spec-face
  '((t (:inherit zephir-keyword-face)))
  "Zephir Mode Face used to highlight class declaration specification keywords
(‘implements’, ‘extends’)."
  :group 'zephir-faces
  :tag "Zephir Class Declaration Specification")

(defface zephir-namespace-declaration-face
  '((t (:inherit zephir-keyword-face)))
  "Zephir Mode Face used to highlight namespace declaration keyword."
  :group 'zephir-faces
  :tag "Zephir Namespace Declaration")

(defface zephir-import-declaration-face
  '((t (:inherit zephir-keyword-face)))
  "Zephir Mode Face used to highlight import statements (‘use ... as ...’)."
  :group 'zephir-faces
  :tag "Zephir Import Statement")

(defface zephir-class-modifier-face
  '((t (:inherit zephir-keyword-face)))
  "Zephir Mode Face used to highlight class modifiers (‘final’, ‘abstract’)."
  :group 'zephir-faces
  :tag "Zephir Class Modifier")

(defface zephir-method-modifier-face
  '((t (:inherit zephir-keyword-face)))
  "Zephir Mode Face used to highlight method modifiers (‘final’, ‘abstract’)."
  :group 'zephir-faces
  :tag "Zephir Method Modifier")

(provide 'zephir-face)
;;; zephir-face.el ends here
