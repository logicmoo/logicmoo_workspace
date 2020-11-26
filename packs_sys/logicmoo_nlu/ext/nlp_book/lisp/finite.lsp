;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;; %    Example code from the book "Natural Language Processing in LISP"   %
;;; %                      published by Addison Wesley                      %
;;; %        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;;
;;; finite.lsp [Chapter  2] utilities for finite state networks

;;; abbreviations

(defvar abbreviations)

;;; basic network accessing

(defun initial_nodes (network)
;;  returns the list of initial nodes
   (nth 1 (assoc 'Initial network)))

(defun final_nodes (network)
;;  returns the list of final nodes
   (nth 1 (assoc 'Final network)))

(defun transitions (network)
;;  returns the list of transitions
   (cddr network))

;;; the subcomponents of a transition

(defun trans_node (transition)
   (getf transition 'From))

(defun trans_newnode (transition)
   (getf transition 'to))

(defun trans_label (transition)
   (getf transition 'by))
