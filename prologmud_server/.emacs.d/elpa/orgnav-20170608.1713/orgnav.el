;;; orgnav.el --- Org tree navigation using helm -*- lexical-binding: t -*-

;; Copyright (C) 2016 Facet Framer

;; Author: Facet Framer (facet@facetframer.com)
;; URL: http://github.com/facetframer/orgnav
;; Version: 2.0.3
;; Keywords: convenience, outlines
;; Package-Requires: ((helm "2.7.0") (s "1.11.0") (dash "1.11.0") (emacs "24"))

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
;; A library to navigate inside org files.

;;; Code:

(require 'helm)
(require 's)
(require 'dash)
(require 'cl-seq)

(require 'orgnav-tree)
(require 'orgnav-log)


(defvar orgnav-search-history nil "List of orgnav searches.")



(defvar orgnav-mapping
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    ;; We can't call these things directly because we need
    ;; to quit helm

    ;; We should probably put a layer of naming
    ;; on top of this, so we don't refer to things
    ;; by index
    (define-key map (kbd "M-h") (lambda () (interactive) (helm-exit-and-execute-action 'orgnav--decrease-depth-action)))
    (define-key map (kbd "M-l") (lambda () (interactive) (helm-exit-and-execute-action 'orgnav--increase-depth-action)))
    (define-key map (kbd "M-.") (lambda () (interactive) (helm-exit-and-execute-action 'orgnav--explore-action)))
    (define-key map (kbd "M-,") (lambda () (interactive) (helm-exit-and-execute-action 'orgnav--explore-parent-action)))
    (define-key map (kbd "M-b") (lambda () (interactive) (helm-exit-and-execute-action 'orgnav--back-action)))
    (define-key map (kbd "M-r") (lambda () (interactive) (helm-exit-and-execute-action 'orgnav--rename-action)))
    (define-key map (kbd "M-g") (lambda () (interactive) (helm-exit-and-execute-action 'orgnav--goto-action)))
    (define-key map (kbd "M-c") (lambda () (interactive) (helm-exit-and-execute-action 'orgnav--clock-action)))
    (define-key map (kbd "M-a") (lambda () (interactive) (helm-exit-and-execute-action 'orgnav--explore-ancestors-action)))
    (define-key map (kbd "M-n") (lambda () (interactive) (helm-exit-and-execute-action 'orgnav--new-action)))
  (define-key map (kbd "M-v") (lambda () (interactive) (helm-exit-and-execute-action 'orgnav--show-path-action)))
    (define-key map (kbd "M-j") 'helm-next-line)
    (define-key map (kbd "M-k") 'helm-previous-line)
    map)
  "Keyboard mapping within helm.")

;; Private state variables
(defvar orgnav--var-buffer nil "Private state.  Which buffer to search.")
(defvar orgnav--var-default-action nil "Private state.  Action to carry out on pressing enter.")
(defvar orgnav--var-depth nil "Private state.  Depth of tree to show.")
(defvar orgnav--var-helm-buffer nil "Private state.  Name of the helm buffer.")
(defvar orgnav--var-point nil "Private state.  Point of tree to start searching.")
(defvar orgnav--var-result nil "Private state.  Variable to store the result of synchronous calls.")

;;; Interactive entry points for searching:
(defun orgnav-search-root (depth default-action)
  "Explore all nodes in the current org document.
Display DEPTH levels.  Run DEFAULT-ACTION on enter."
  (interactive (list 1 'orgnav--goto-action))
  (orgnav-search-subtree nil
                      :depth depth
                      :default-action default-action
                      :helm-buffer-name "*orgnav-search*"))

(defun orgnav-search-subtree (point &rest plist)
  "Explore the org subtree at POINT.  If POINT is nil explore the buffer.
This function returns immediately.
PLIST is a property list with optional properties:
`:depth' how levels are shown.
`:default-action' the action run on enter (goto pont by default)
`:buffer' is the buffer that you should search in
`:helm-buffer-name' the name of the helm search buffer (of use with `helm-resume')."
  (interactive (list (point)))
  (orgnav--assert-plist plist :depth :default-action :helm-buffer-name :input :buffer)
  (-let
      (
       ((&plist :depth depth
                :default-action default-action
                :helm-buffer-name helm-buffer-name
                :input input
                :buffer buffer) plist))

    (setq depth (or depth 1))
    (setq default-action (or default-action 'orgnav--goto-action))
    (setq buffer (or buffer (current-buffer)))
    (orgnav--search
     :candidate-func 'orgnav--get-desc-candidates
     :point point
     :depth depth
     :buffer buffer
     :default-action default-action
     :helm-buffer-name helm-buffer-name
     :input input)))

(defun orgnav-search-ancestors (&optional node &rest plist)
  "Search through the ancestors of NODE (by the default the current node).
PLIST is a property list with the following values
:default-action is run on enter (by default jump to node)
:helm-buffer-name is the name of the helm search buffer (useful with ‘helm-resume’)."
  (interactive)
  (orgnav--assert-plist plist :default-action :helm-buffer-name :input)
  (-let
      (((&plist :default-action default-action
                :helm-buffer-name helm-buffer-name
                :input input
                :buffer buffer
                ) plist))
    (setq node (or node (save-excursion (org-back-to-heading) (point))))
    (setq default-action (or default-action 'orgnav--goto-action))
    (setq buffer (or buffer (current-buffer)))
    (orgnav--search
     :candidate-func 'orgnav--get-ancestor-candidates
     :point node
     :depth nil
     :buffer buffer
     :default-action default-action
     :helm-buffer-name helm-buffer-name
     :input input)))

(defun orgnav-search-nearby (levels)
  "Search near the current node, LEVELS levels up."
  (interactive (list 1))
  (orgnav-search-subtree (orgnav-tree-get-ancestor (point) levels)))


;;; Functions that you might want to script
(defun orgnav-jump-interactive (base-filename base-heading-spec &optional depth)
  "Jump to an ancestor of the heading of BASE-FILENAME specified by BASE-HEADING-SPEC.  Display DEPTH levels."
  (let (node)
    (setq depth (or depth 2))
    (when base-filename (find-file base-filename))
    (setq node (orgnav--heading-lookup (current-buffer) base-heading-spec))
    (orgnav--goto-action
     (orgnav-search-subtree-sync
      node
      :depth depth))))

(defun orgnav--heading-lookup (buffer heading-specifier)
  "Convenience function lookup in BUFFER for HEADING-SPECIFIER (olp, string, marker or integer)."
  (with-current-buffer buffer
    (cond
     ((listp heading-specifier) (org-find-olp heading-specifier t))
     ((stringp heading-specifier) (org-find-exact-headline-in-buffer heading-specifier))
     ((markerp heading-specifier) heading-specifier)
     ((integerp heading-specifier) heading-specifier))))

(defun orgnav-search-subtree-sync (point &rest plist)
  "Search the tree at POINT.  Return the `(point)' at the selected node.
PLIST is a property list of settings:
:depth specifies the initial number of levels to show
:helm-buffer-name the name of the helm buffer (useful with ‘helm-resume’)"
  ;; Work around for helm's asychronicity
  (setq orgnav--var-result nil)
  (orgnav--assert-plist plist :depth :helm-buffer-name)
  (apply 'orgnav-search-subtree point :default-action 'orgnav--return-result-action plist)
  ;; RACE CONDITION
  (while (null orgnav--var-result)
    (sit-for 0.05))
  (prog1
      orgnav--var-result
  (setq orgnav--var-result nil)))

(defun orgnav-search-ancestors-sync (point &optional helm-buffer-name)
  "Search the ancestors of the node at POINT.
Return the `(point)' at the selected node.
Start searching in the buffer called HELM-BUFFER-NAME."
  ;; Work around for helm's asychronicity
  (setq orgnav--var-result nil)
  (orgnav-search-ancestors point
                        :default-action 'orgnav--return-result-action
                        :helm-buffer-name helm-buffer-name)
  ;; RACE CONDITION
  (while (null orgnav--var-result)
    (sit-for 0.05))
  (prog1
      orgnav--var-result
  (setq orgnav--var-result nil)))


;; Private functions
(defun orgnav--search (&rest plist)
  "Generic search function.
PLIST is a property list of *mandatory* values:
`:candidate-func' is a function that returns candidates.
`:point' is where we are searching relative t.
`:depth' is how many levels to display.
`:default-action' is the function to run on carriage return.
`:helm-buffer-name' is name of the helm buffer (relvant for `helm-resume').
`:buffer` is the buffer that you want to search.
`:input' is the initial search term"
  (push plist orgnav-search-history)

  (-let (((&plist
          :candidate-func candidate-func
          :point point
          :depth depth
          :default-action default-action
          :helm-buffer-name helm-buffer-name
          :input input
          :buffer buffer) plist))
    (when (not (orgnav--set-eq
              (orgnav--plist-keys plist)
              (list :candidate-func :point :depth :default-action :helm-buffer-name :input :buffer)))
      (error "Wrong keys: %S" (orgnav--plist-keys plist)))

    ;; Candidate functions appear to
    ;;   not be run in the current buffer, we need to keep track of the buffer
    (setq orgnav--var-buffer buffer)
    (setq orgnav--var-point point)
    (setq orgnav--var-depth depth)
    (setq helm-buffer-name (or helm-buffer-name "*orgnav-search*"))
    (setq orgnav--var-default-action default-action)

    (orgnav-log "orgnav--search candidate-func=%S action=%S header=%S depth=%S input=%S"
              candidate-func
              orgnav--var-default-action
              (orgnav-tree-get-heading orgnav--var-buffer orgnav--var-point)
              orgnav--var-depth
              input)
    (helm
     :sources (list (orgnav--make-source candidate-func default-action))
     :keymap orgnav-mapping
     :input input
     :buffer helm-buffer-name
     :input input)))

(defun orgnav--tweak-search (&rest tweaks)
  "Repeat the previous search with some TWEAKS."
  (apply 'orgnav--search
         (apply 'orgnav--plist-update
          (car orgnav-search-history)
          tweaks)))

(defun orgnav--make-source (candidate-func default-action)
  "Make helm source which gets candidates by calling CANDIDATE-FUNC.
by default run DEFAULT-ACTION when return pressed."
  (list
   (cons 'name "HELM at the Emacs")
   (cons 'candidates candidate-func)
   (cons 'action (orgnav--make-actions default-action))))

(defun orgnav--make-actions (default-action)
  "Actions for used by helm.  On return run DEFAULT-ACTION."
  (list
    (cons "Default action" default-action)
    (cons "Decrease depth `M-h`" 'orgnav--decrease-depth-action)
    (cons "Increase depth `M-l`" 'orgnav--increase-depth-action)
    (cons "Explore node `M-.`" 'orgnav--explore-action)
    (cons "Explore parent `M-,`" 'orgnav--explore-parent-action)
    (cons "Go back `M-b`" 'orgnav--back-action)

    (cons "View path `M-v`" 'orgnav--show-path-action)
    (cons "Create a new node `M-n`" 'orgnav--new-action)
    (cons "Rename node `M-r`" 'orgnav--rename-action)
    (cons "Go to node `M-g`" 'orgnav--goto-action)
    (cons "Clock into the node `M-c`" 'orgnav--clock-action)
    (cons "Explore ancestors of a node `M-a`" 'orgnav-search-ancestors)
    ))

(defun orgnav--make-candidate (point)
  "Construct a helm candidate from a node at POINT."
  (cons
   (orgnav--get-entry-str point)
   point))

(defun orgnav--get-desc-candidates ()
  "Helm candidate function for descendants."
  (with-current-buffer orgnav--var-buffer
    (save-excursion
      (mapcar
       'orgnav--make-candidate
       (orgnav-tree-get-descendants orgnav--var-point orgnav--var-depth)))))

(defun orgnav--get-ancestor-candidates ()
  "Find helm candidates for the ancestors of the location set by a search function."
  (with-current-buffer orgnav--var-buffer
    (save-excursion
      (when orgnav--var-point
          (goto-char orgnav--var-point))
      (mapcar 'orgnav--make-candidate
              (orgnav-tree-ancestors)))))

(defun orgnav--get-entry-str (point)
  "How orgnav should represent a the node at POINT."
  (save-excursion
    (goto-char point)
    (concat (s-repeat (org-outline-level) "*") " " (org-get-heading))))

(defun orgnav--filter-by-depth (headings min-level max-level)
  "Filter the nodes at points in HEADINGS.
Only returning those between with a level better MIN-LEVEL and MAX-LEVEL."
  (-filter (lambda (x)
             (save-excursion
               (goto-char x)
               (and
                (or (null min-level)
                    (>= (org-outline-level) min-level))
                (or (null max-level)
                    (<= (org-outline-level) max-level)))))
           headings))


;;; Actions
(defun orgnav--goto-action (helm-entry)
  "Go to the node represented by HELM-ENTRY."
  (interactive)
  (orgnav-log "Action: go to %S" helm-entry)
  (goto-char helm-entry)
  (org-reveal))

(defun orgnav--explore-action (helm-entry)
  "Start search again from HELM-ENTRY."
  (orgnav-log "Action: explore %S" helm-entry)
  (orgnav-search-subtree helm-entry
                      :depth 1
                      :default-action orgnav--var-default-action
                      :helm-buffer-name orgnav--var-helm-buffer))

(defun orgnav--explore-ancestors-action (helm-entry)
  "Start search again looking ancestors of HELM-ENTRY."
  (orgnav-log "Action: explore ancestors of %S" helm-entry)
  (orgnav-search-ancestors
   helm-entry
   :default-action orgnav--var-default-action
   :helm-buffer-name orgnav--var-helm-buffer))

(defun orgnav--back-action (ignored)
  "Go the the previous orgnav search view.  IGNORED is ignored."
  (orgnav-log "Action: Go back")
  (orgnav-log "orgnav--back-action: search history %S" orgnav-search-history)
  (pop orgnav-search-history)
  (apply 'orgnav--search (pop orgnav-search-history)))

(defun orgnav--explore-parent-action (ignored)
  "Start search again from one level higher.  Ignore IGNORED."
  (orgnav-log "Action: explore parent of search at %S"
            orgnav--var-point)
  (orgnav--tweak-search
   :point (orgnav-tree-get-parent orgnav--var-point)
   :depth 1))

(defun orgnav--increase-depth-action (ignored)
  "Search again showing nodes at a greater depth.  IGNORED is ignored."
  (orgnav-log "Action: Increasing depth of search")
  (orgnav--tweak-search
   :depth (max (+ orgnav--var-depth 1) 1)
   :input (orgnav--get-input)))

(defun orgnav--decrease-depth-action (ignored)
  "Search again hiding more descendents.  IGNORED is ignored."
  (orgnav-log "Action: decrease depth of search")
  (orgnav--tweak-search
   :depth (max (- orgnav--var-depth 1) 1)
   :input (orgnav--get-input)))

(defun orgnav--show-path-action (helm-entry)
  "Show the path to this HELM-ENTRY."
  (orgnav-log "Action: showing path to %S" helm-entry)
  (orgnav--popup
   (orgnav--format-path helm-entry))
  (orgnav--tweak-search :input (orgnav--get-input)))

(defun orgnav--new-action (helm-entry)
  "Create child under the select HELM-ENTRY.  IGNORED is ignored."
  (orgnav-log "Action: Creating a new node under %S" helm-entry)
  (let* (
         (point-function (lambda ()  (set-buffer orgnav--var-buffer) (goto-char helm-entry)))
         (org-capture-templates (list (list "." "Default action" 'entry (list 'function point-function) "* %(read-string \"Name\")"))))
    (org-capture nil ".")))

(defun orgnav--return-result-action (helm-entry)
  "A convenience action for synchronouse functions.
Store the location of HELM-ENTRY so that the synchronous functions can return them."
  (orgnav-log "Action: Saving %S to return" helm-entry)
  (setq orgnav--var-result helm-entry))

(defun orgnav--rename-action (helm-entry)
  "Action to rename HELM-ENTRY."
  (orgnav-log "Action: renaming %S" helm-entry)
  (let (heading)
    (setq heading
          (read-string "New name" (save-excursion
                              (goto-char helm-entry)
                              (org-get-heading))))
    (orgnav-tree-rename helm-entry heading)
    (orgnav-search-subtree orgnav--var-point
                        :depth orgnav--var-depth
                        :default-action orgnav--var-default-action
                        :helm-buffer-name orgnav--var-helm-buffer
                        :input (orgnav--get-input))))

(defun orgnav--clock-action (helm-entry)
  "Clock into the selected HELM-ENTRY."
  (orgnav-log "Action: Clocking into %S" helm-entry)
  (save-excursion
    (goto-char helm-entry)
    (org-clock-in)))

;;; Utility functions


(defun orgnav--assert-plist (plist &rest members)
  "Ensure that the property list PLIST has only keys in MEMBERS."
  (when (not  (cl-subsetp (orgnav--plist-keys plist) members))
      (error (format "%S plist contains keys not in %S" plist members))))

(defun orgnav--plist-keys (plist)
  "Return a list of the keys in PLIST."
  (if plist
      (cons (car plist) (orgnav--plist-keys (cddr plist)))
    nil))

(defun orgnav--set-eq (set1 set2)
  "Test if lists SET1 and SET2 have the same members."
  (and
   (cl-subsetp set1 set2)
   (cl-subsetp set2 set1)))

(defun orgnav--get-input ()
  "Get the current input of the helm search."
  helm-input)

(defun orgnav--reload ()
  "Unload and reload orgnav."
  (interactive)
  (unload-feature 'orgnav 't)
  (require 'orgnav-capture)
  (require 'orgnav-clock)
  (require 'orgnav)
  (require 'orgnav-refile)
  (require 'orgnav-tree))

(defun orgnav--plist-update (plist &rest update-plist)
  "Alter the property list PLIST by adding the key-value pairs in UPDATE-PLIST."
  (let (result)
    (setq result plist)
    (mapc
     (lambda (key)
       (setq result (plist-put result key (plist-get update-plist key))))
     (orgnav--plist-keys update-plist))
    result))

(defun orgnav--format-path (point)
  "Show the path to the org heading at POINT."
  (substring-no-properties
   (s-join "\n" (mapcar
                 (lambda (point) (orgnav-tree-get-heading orgnav--var-buffer point))
                 (orgnav-tree-ancestors point)))))

(defun orgnav--popup (message)
  "Show MESSAGE in a popup window."
  ;; Helm breaks the mini-buffer
  (let (popup-buffer)
    (setq popup-buffer (get-buffer-create "*orgnav-popup*"))
    (with-current-buffer popup-buffer
      (erase-buffer)
      (insert message))
    (display-buffer popup-buffer))
  (read-char "Press a key when finished:"))


(provide 'orgnav)
;;; orgnav.el ends here
