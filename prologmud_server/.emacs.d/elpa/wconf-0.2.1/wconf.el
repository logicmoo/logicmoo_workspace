;;; wconf.el --- Minimal window layout manager   -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2017  Free Software Foundation, Inc.

;; Author: Ingo Lohmar <i.lohmar@gmail.com>
;; URL: https://github.com/ilohmar/wconf
;; Version: 0.2.1
;; Keywords: windows, frames, layout
;; Package-Requires: ((emacs "24.4"))

;; This file is part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; See the file README.org

;;; Code:

(defgroup wconf nil
  "Easily use several window configurations."
  :group 'convenience)

(defcustom wconf-change-config-function #'wconf-change-config-default
  "Function called with current config whenever it is set."
  :group 'wconf
  :type 'function)

(defcustom wconf-file (expand-file-name "wconf-window-configs.el"
                                        user-emacs-directory)
  "File used to save and load window configurations."
  :group 'wconf
  :type 'file)

(defcustom wconf-fallback-buffer-name "*scratch*"
  "Name of the buffer to substitute for buffers which are not available."
  :group 'wconf
  :type 'string)

(defcustom wconf-no-configs-string "-----"
  "String to use if there are no configurations at all."
  :group 'wconf
  :type 'string)

(defcustom wconf-no-config-name "---"
  "String to use for the empty window configuration."
  :group 'wconf
  :type 'string)

;; internal variables and helper functions

(defvar wconf--configs nil
  "List of configurations; each item a list (active stored name).")

(defvar wconf--index nil
  "Index of currently shown configuration.  After clean and load
this can be nil although wconf--configs is not empty.")

(defvar wconf-string nil
  "String representing information on the current configuration.")

(require 'cl-lib)

(defsubst wconf--ensure-configs (&optional current)
  (unless wconf--configs
    (error "wconf: No window configurations"))
  (when (and current (not wconf--index))
    (error "wconf: No window configuration is currently used")))

(defsubst wconf--ensure-index (&optional index)
  (unless (<= 0 index (1- (length wconf--configs)))
    (error "wconf: No window configuration index %s" index)))

(defsubst wconf--not-from-minibuffer ()
  (when (minibuffer-window-active-p (frame-selected-window))
    (error "wconf: Cannot change window configs when minibuffer is active")))

(defun wconf--current-config ()
  (window-state-get (frame-root-window (selected-frame))
                    'writable))

(defun wconf- (index)
  (nth index wconf--configs))

(defun wconf--to-string (index)
  (if index
      (format "%s:%s"
              (number-to-string index)
              (cl-caddr (wconf- index)))
    (concat "-:" wconf-no-config-name)))

(defun wconf--update-info ()
  (when (functionp wconf-change-config-function)
    (funcall wconf-change-config-function
             ;; both will be nil if no list
             wconf--index
             (and wconf--index
                  (car (wconf- wconf--index))))))

(defun wconf--update-active-config ()
  (when wconf--index
    (setf (car (wconf- wconf--index)) (wconf--current-config))))

(defun wconf--use-config (index)
  (setq wconf--index index)
  (window-state-put (car (wconf- wconf--index))
                    (frame-root-window (selected-frame))
                    'safe)
  (wconf--update-info))

(defun wconf--reset ()
  "Remove all configurations."
  (setq wconf--configs nil)
  (setq wconf--index nil)
  (wconf--update-info))

(defun wconf--copy (wc)
  "Return a deep copy of WC, using `copy-tree'."
  (copy-tree wc t))

;; global stuff

(defun wconf-change-config-default (index _config)
  "Update `wconf-string' to represent configuration CONFIG at
position INDEX."
  (setq wconf-string (if wconf--configs
                         (wconf--to-string index)
                       wconf-no-configs-string))
  (force-mode-line-update))

(defun wconf-save (&optional filename)
  "Save stored configurations in FILENAME, defaults to
`wconf-file'."
  (interactive "F")
  (let ((filename (or filename wconf-file)))
    (with-temp-file filename
      (prin1 (mapcar #'cdr wconf--configs) ;-> (wc name)
             (current-buffer)))
    (message "wconf: Save stored configurations in %s" filename)))

(defun wconf--sanitize-buffer (b)
  (unless (get-buffer (cadr b))
    (setf (cadr b) wconf-fallback-buffer-name
          (cdr (assoc 'start b)) 1
          (cdr (assoc 'point b)) 1
          (cdr (assoc 'dedicated b)) nil)))

(defun wconf--sanitize-window-tree (node)
  (let ((buf (assoc 'buffer node)))
    (if buf                             ;in a leaf already
        (wconf--sanitize-buffer buf)
      (mapc (lambda (x)
              (when (and (consp x)
                         (memq (car x) '(leaf vc hc)))
                (wconf--sanitize-window-tree (cdr x))))
            node))))

;;;###autoload
(defun wconf-load (&optional filename)
  "Load stored configurations from FILENAME, defaults to
`wconf-file'."
  (interactive "f")
  (let ((filename (or filename wconf-file)))
    (unless (file-readable-p filename)
      (error "wconf: Cannot read file %s" filename))
    (wconf--reset)
    (with-temp-buffer
      (insert-file-contents filename)
      (goto-char (point-min))
      (setq wconf--configs
            (mapcar
             (lambda (f)                ;(wc name)
               (wconf--sanitize-window-tree (car f))
               (cons (wconf--copy (car f)) f))
             (read (current-buffer)))))
    (message "wconf: Load stored configurations from %s" filename))
  (wconf--update-info))

;; these functions affect the whole list of configs

;;;###autoload
(defun wconf-create (&optional new)
  "Clone the current configuration or create a new \"empty\" one.
The new configuration is appended to the list and becomes active.

With optional prefix argument NEW, or if there are no
configurations yet, create a new configuration from the current
window config."
  (interactive "P")
  (wconf--not-from-minibuffer)
  (wconf--update-active-config)
  (setq wconf--configs
        (append wconf--configs
                (list
                 (if (or new (not wconf--configs))
                     (progn
                       (message "wconf: Created new configuration %s"
                                (length wconf--configs))
                       (list (wconf--current-config)
                             (wconf--current-config)
                             "new"))
                   (wconf--ensure-configs 'current)
                   (let ((wc (wconf- wconf--index)))
                     (message "wconf: Cloned configuration %s"
                              (wconf--to-string wconf--index))
                     (list (wconf--copy (car wc))
                           (wconf--copy (cadr wc))
                           (cl-caddr wc)))))))
  (wconf--use-config (1- (length wconf--configs))))

(defun wconf-kill ()
  "Kill current configuration."
  (interactive)
  (wconf--ensure-configs 'current)
  (wconf--not-from-minibuffer)
  (let ((old-string (wconf--to-string wconf--index)))
    (setq wconf--configs
          (append (butlast wconf--configs
                           (- (length wconf--configs) wconf--index))
                  (last wconf--configs
                        (- (length wconf--configs) wconf--index 1))))
    (if wconf--configs
        (wconf--use-config (if (< (1- (length wconf--configs)) wconf--index)
                               (1- wconf--index)
                             wconf--index))
      (wconf--reset)
      (wconf--update-info))
    (message "wconf: Killed configuration %s" old-string)))

(defun wconf-swap (i j)
  "Swap configurations at positions I and J."
  (interactive
   (progn
     (wconf--ensure-configs 'current)   ;interactive?  then want current config
     (wconf--not-from-minibuffer)
     (list
      wconf--index
      (read-number "Swap current config with index: "))))
  (wconf--ensure-configs)
  (wconf--ensure-index i)
  (wconf--ensure-index j)
  (wconf--update-active-config)
  (let ((wc (wconf- i)))
    (setf (nth i wconf--configs) (wconf- j))
    (setf (nth j wconf--configs) wc))
  (when (memq wconf--index (list i j))
    (wconf--use-config wconf--index))
  (message "wconf: Swapped configurations %s and %s"
           (number-to-string i) (number-to-string j)))

;; manipulate single config

(defun wconf-rename (name)
  "Rename current configuration to NAME."
  (interactive
   (progn
     (wconf--ensure-configs 'current)
     (list
      (read-string "New window configuration name: "
                   (cl-caddr (wconf- wconf--index))))))
  (wconf--ensure-configs 'current)
  (setf (cl-caddr (wconf- wconf--index)) name)
  (message "wconf: Renamed configuration to \"%s\"" name)
  (wconf--update-info))

;; interaction b/w stored and active configs

;; these commands only make sense when there are wconf--configs, and
;; after wconf--index has become non-nil

(defsubst wconf--store (wc)
  (setf (cadr wc) (wconf--copy (car wc))))

(defsubst wconf--restore (wc)
  (setf (car wc) (wconf--copy (cadr wc))))

(defun wconf-store ()
  "Store currently active configuration."
  (interactive)
  (wconf--ensure-configs 'current)
  (wconf--update-active-config)
  (wconf--store (wconf- wconf--index))
  (message "wconf: Stored configuration %s" (wconf--to-string wconf--index)))

(defun wconf-store-all ()
  "Store all active configurations."
  (interactive)
  (wconf--ensure-configs 'current)
  (wconf--update-active-config)
  (mapc #'wconf--store wconf--configs)
  (message "wconf: Stored all configurations"))

(defun wconf-restore ()
  "Restore stored configuration."
  (interactive)
  (wconf--ensure-configs 'current)
  (wconf--not-from-minibuffer)
  (wconf--restore (wconf- wconf--index))
  (wconf--use-config wconf--index)
  (message "wconf: Restored configuration %s" (wconf--to-string wconf--index)))

(defun wconf-restore-all ()
  "Restore all stored configurations."
  (interactive)
  (wconf--ensure-configs 'current)
  (mapc #'wconf--restore wconf--configs)
  (wconf--use-config wconf--index)
  (message "wconf: Restored all configurations"))

;; change config

(defun wconf-switch-to-config (index &optional force)
  "Change to current config INDEX."
  (interactive "P")
  (wconf--ensure-configs)
  (wconf--not-from-minibuffer)
  (let ((index (or index
                   (read-number "Switch to config number: "))))
    (wconf--ensure-index index)
    ;; remember active config (w/o name etc)
    (wconf--update-active-config)
    ;; maybe use new configuration
    (if (and (eq wconf--index index)
             (not force))
        (message "wconf: Nothing to do")
      (wconf--use-config index)
      (message "wconf: Switched to configuration %s"
               (wconf--to-string index)))))

(defun wconf-use-previous ()
  "Switch to previous window configuration."
  (interactive)
  (wconf--ensure-configs)
  (wconf-switch-to-config (mod (1- (or wconf--index 1))
                               (length wconf--configs))))

(defun wconf-use-next ()
  "Switch to next window configuration."
  (interactive)
  (wconf--ensure-configs)
  (wconf-switch-to-config (mod (1+ (or wconf--index -1))
                               (length wconf--configs))))

;;;; ChangeLog:

;; 2017-02-23  Ingo Lohmar	 <i.lohmar@gmail.com>
;; 
;; 	Merge commit '833ae431a5b35739be3076ea4b586d84d6fe269f' from wconf
;; 
;; 	* commit '833ae431a5b35739be3076ea4b586d84d6fe269f':
;; 	 Bump version and copyright
;; 	 In minibuffer, prevent all commands that alter current config
;; 	 Fix a compiler warning
;; 	 Specify customization types
;; 
;; 2015-08-02  Ingo Lohmar	 <i.lohmar@gmail.com>
;; 
;; 	Add 'packages/wconf/' from commit
;; 	'b8ea22f80bff19222136d9495f685888dc682b9d'
;; 
;; 	git-subtree-dir: packages/wconf git-subtree-mainline:
;; 	6340c15fb07a979b6c7bd2e8913e499091c257ff git-subtree-split:
;; 	b8ea22f80bff19222136d9495f685888dc682b9d
;; 


(provide 'wconf)
;;; wconf.el ends here
