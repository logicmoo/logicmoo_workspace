;;; gnu-elpa-maint.el --- Maintenance functions for gnu-elpa.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Free Software Foundation, Inc.

;; Author: Stefan Monnier <monnier@iro.umontreal.ca>
;; Keywords:
;; Version: 0

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

;; Extract the relevant features to advertise

;; This is expected to be run from within
;; .../elpa/packages/gnu-elpa/
;; where `.../elpa` is a clone of `elpa.git` in which
;; both `make externals` and `make` were performed (actually, only
;; the autoloads need to be generated).

;;; Code:

(require 'map)
(require 'radix-tree)

;; FIXME: Skip those packages that aren't released, e.g. w3 and sm-c-mode

(defun gnu-elpa--pkgs ()
  (let ((pkgs ())
        (default-directory (expand-file-name "../")))
    (dolist (d (directory-files "."))
      (unless (or (file-symlink-p d)
                  (member d '("." ".."))
                  (not (file-directory-p d)))
        (push d pkgs)))
    (nreverse pkgs)))

(defun gnu-elpa--features-for-pkg (pkg)
  (with-temp-buffer
    (emacs-lisp-mode)
    (when (let ((f (format "../%s/%s.el" pkg pkg)))
            (insert-file-contents f nil 0 16384) ;Arbitrary limit, just in case.
            (prog1
                (and (member (lm-header "auto-suggest") '(nil "yes" "t"))
                     (not (member (lm-header "version") '("0"))))
              (erase-buffer)))
      (let ((f (format "../%s/%s-autoloads.el" pkg pkg)))
        (if (file-readable-p f) (insert-file-contents f)))
      (goto-char (point-min))
      (let ((autoloads ())
            (forms ())
            (others ()))
        (condition-case nil
            (while t
              (pcase (read (current-buffer))
                ;; These are the main ones.
                (`(autoload ',f ,_ . ,extra) (push (cons f extra) autoloads))
                (form (push form others))))
          (end-of-file nil))
        (dolist (exp (prog1 others (setq others nil)))
          (pcase exp
            (`(add-to-list ',(or 'auto-mode-alist 'interpreter-mode-alist)
                           ,(or `'(,_ . ,f)
                                `(cons ,_ ',f)
                                `(cons ,_ #',f))
                           . ,_)
             (push exp (if (assq f autoloads) forms others)))
            (`(defalias ',f ,(or `',a `#',a) . ,_)
             (push `(unless (fboundp ',f) ,exp)
                   (if (assq a autoloads) forms others)))
            ;; Entries we can just ignore.
            (`(add-to-list 'load-path . ,_) nil)
            ((and `(defvar ,v . ,_)
                  (guard (string-match "-mode\\'" (symbol-name v))))
             nil)
            ;; Entries we could conceivably use, but it would take more
            ;; work to make use of them and/or the benefit is unclear.
            (`(if (fboundp 'register-definition-prefixes) . ,_) nil)
            (`(custom-autoload . ,_) nil)
            (`(eieio-defclass-autoload  . ,_) nil)
            (`(cl-defstruct . ,_) nil)
            (`(,(or 'put 'function-put) ,_
               ',(or 'interactive-only 'lisp-indent-function 'doc-string-elt)
               . ,_)
             nil)
            ;;
            (_ (push exp others))))
        ;; FIXME: We should only autoload a few key entry points per package,
        ;; so we arbitrarily limit the number of autoloads per package to 10.
        ;; For packages "larger" than that, we need to figure out a way for
        ;; the package to specify a subset of its autoloads to use here!
        (if (> (length autoloads) 10)
            (progn (message "Skipping package %s: too many autoloads (%d)" pkg
                            (length autoloads))
                   nil)
          `((:autoloads . ,autoloads)
            (:forms . ,forms)
            ;; (:auto-modes . ,auto-modes)
            ;; (:aliases . ,aliases)
            ;; (:others . ,others)
            ))))))

(defun gnu-elpa--all-features ()
  (let ((autoloads ())
        ;; (auto-modes ())
        ;; (aliases ())
        (forms ())
        ;; (others ())
        )
    (dolist (pkg (gnu-elpa--pkgs))
      (let ((f (gnu-elpa--features-for-pkg pkg)))
        (setq autoloads
              (nconc (mapcar (lambda (a) (cons pkg a)) (alist-get :autoloads f))
                     autoloads))
        ;; (setq auto-modes (nconc (alist-get :auto-modes f) auto-modes))
        ;; (setq aliases (nconc (alist-get :aliases f) aliases))
        (setq forms (nconc (alist-get :forms f) forms))
        ;; (setq others (nconc (alist-get :others f) others))
        ))
    `((:autoloads . ,(mapcar (lambda (x) (list (car x) (cadr x))) autoloads))
      (:forms . ,forms)
      ;; (:auto-modes . ,auto-modes)
      ;; (:aliases . ,aliases)
      ;; (:others . ,others)
      )))

(defun gnu-elpa--single-pkg (subtree)
  ;; Check to see if all the names inside `subtree' map to the
  ;; same package.  Return that package if yes, and nil otherwise.
  (catch 'multiple
    (let ((thepkg nil))
      (radix-tree-iter-mappings subtree
                                (lambda (_prefix pkg)
                                  (cond
                                   ((null thepkg) (setq thepkg pkg))
                                   ((eq thepkg pkg) nil)
                                   (t (throw 'multiple nil)))))
      thepkg)))

(defun gnu-elpa--prefixes (tree)
  (let ((todo nil)
        (done nil))
    (radix-tree-iter-subtrees
     tree (lambda (prefix subtree)
            (push (cons prefix subtree) todo)))
    (while todo
      (pcase-let* ((`(,prefix . ,subtree) (pop todo))
                   (thepkg (gnu-elpa--single-pkg subtree)))
        (if thepkg (push (cons prefix thepkg) done)
          ;; This same prefix is used by various packages, so we need
          ;; to dig deeper.
          (radix-tree-iter-subtrees
           subtree (lambda (subprefix subtree)
                     (push (cons (concat prefix subprefix) subtree) todo))))))
    done))

(defun gnu-elpa--make-features ()
  (let (;; Make sure we can load this file without load-source-file-function.
        (coding-system-for-write 'emacs-internal))
    (with-temp-file "gnu-elpa-features.el"
      (emacs-lisp-mode)
      (insert ";;; gnu-elpa-features.el --- Auto-generated autoloads  -*- lexical-binding:t -*-

;; ¡¡ This file is auto-generated by `gnu-elpa--make-features', DO NOT EDIT !!

;; Copyright (C) 2020-" (format-time-string "%Y") "  Free Software Foundation, Inc.

;; Author: gnu-elpa-maint.el

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

\;;; Code:

;; Don't edit this file, it's auto-generated by `gnu-elpa--make-features'!
\n")
      (let ((tree nil)
            (allforms nil))
        (insert "(dolist (x '(")
        (dolist (pkg (gnu-elpa--pkgs))
          (pcase-let* (((map :autoloads :forms)
                        (gnu-elpa--features-for-pkg pkg)))
            (when autoloads
              (dolist (autoload autoloads)
                (setq tree (radix-tree-insert
                            tree (symbol-name (car autoload)) pkg)))
              (setf allforms (append forms allforms))
              (dolist (x autoloads)
                (prin1 x (current-buffer))
                (insert "\n")))))
        (insert "))
  (let ((f (car x)))
    (unless (fboundp f)
      (apply #'autoload f \"gnu-elpa\" (cdr x)))))\n")
        (dolist (form (nreverse allforms))
          (prin1 form (current-buffer))
          (insert "\n"))
        ;; FIXME: Actually `gnu-elpa--autoloads-table' doesn't need to be
        ;; preloaded from `gnu-elpa-features' so it could be moved to
        ;; another file!
        (prin1 `(defconst gnu-elpa--autoloads-table ',(gnu-elpa--prefixes tree))
               (current-buffer)))
      ;; Use `\s' instead of a space character, so this code chunk is not
      ;; mistaken for an actual file-local section of gnu-elpa-maint.el.
      (insert "

\;;\sLocal\sVariables:
\;;\sno-byte-compile: t
\;;\sversion-control: never
\;;\sno-update-autoloads: t
\;; End:

\(provide 'gnu-elpa-features)
\;;; gnu-elpa-features.el ends here\n")
      (indent-region (point-min) (point-max))
      )))
            

(provide 'gnu-elpa-autoloads-make)
;;; gnu-elpa-maint.el ends here
