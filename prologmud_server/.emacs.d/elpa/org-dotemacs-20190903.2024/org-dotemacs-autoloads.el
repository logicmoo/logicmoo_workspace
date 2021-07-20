;;; org-dotemacs-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "org-dotemacs" "org-dotemacs.el" (0 0 0 0))
;;; Generated autoloads from org-dotemacs.el

(autoload 'org-dotemacs-default-match "org-dotemacs" "\
Return the default tag match string based on items in `org-dotemacs-conditional-tags' (which see)." nil nil)

(autoload 'org-dotemacs-topo-sort "org-dotemacs" "\
Returns a list of items whose order is consistent with the supplied dependency GRAPH.
GRAPH is an alist whose keys are block names, and whose values are lists of block names 
on which the corresponding key depends.
BLOCKS is an alist of (NAME . CODE) pairs, containing the code blocks corresponding to 
the names in GRAPH. These BLOCKS will be evaluated as the return list is created.
If SKIPERRORS determines what to do about blocks which throw errors when evaluated;

A list of the following four values is returned.
1: list of block names sorted such that dependent blocks come after their dependencies in the list.
   Failed blocks, and blocks with failed dependencies will not be included in this list.
2: boolean indicating whether all blocks in the input GRAPH are present in the previous list.
3: list of failed blocks (i.e. blocks that threw errors when evaluated).
4: list of blocks which could not be processed because they depend on failed blocks, or are part of
   a circular dependency.

\(fn GRAPH BLOCKS &optional HALTONERROR)" nil nil)

(autoload 'org-dotemacs-load-file "org-dotemacs" "\
Load the elisp code from code blocks in org FILE under headers matching tag MATCH.
Tag matches supplied at the command line get priority over those supplied by the MATCH argument,
and if both of these are nil then `org-dotemacs-default-match' will be used to create a tag match.
If you need to override the command line tag-match set `org-dotemacs-tag-match' to nil.
If TARGET-FILE is supplied it should be a filename to save the elisp code to, but it should
not be any of the default config files .emacs, .emacs.el, .emacs.elc or init.el
 (the function will halt with an error in those cases). If TARGET-FILE is newer than FILE then
TARGET-FILE will be loaded and FILE will not be processed. Otherwise TARGET-FILE will be overwritten
by the code blocks in FILE.
The optional argument ERROR-HANDLING determines how errors are handled and takes default value
`org-dotemacs-error-handling' (which see).

\(fn &optional (MATCH org-dotemacs-tag-match) (FILE org-dotemacs-default-file) TARGET-FILE (ERROR-HANDLING org-dotemacs-error-handling))" t nil)

(autoload 'org-dotemacs-load-default "org-dotemacs" "\
Load code from `org-dotemacs-default-file' matching tag MATCH.
If SAVEP is non-nil then `org-dotemacs-load-file' will be called with TARGET-FILE
argument set to the value of `org-dotemacs-default-file' with the file ending 
replaced with \".el\".
The user will not be prompted for the location of any files.

\(fn &optional (MATCH org-dotemacs-tag-match) SAVEP)" t nil)

(autoload 'org-dotemacs-jump-to-block "org-dotemacs" "\
Jump to block named BLKNAME in FILE (`org-dotemacs-default-file' by default).
If called interactively from an \"org-dotemacs:\" line in the *Messages* buffer
the block mentioned on that line will be used for BLKNAME. Otherwise prompt for a block.
If DISPLAYONLY is non-nil display the block in another window but don't visit it.
See also `org-dotemacs-noselect-on-jump'.

\(fn BLKNAME &optional (FILE org-dotemacs-default-file) (DISPLAYONLY org-dotemacs-noselect-on-jump))" t nil)

(register-definition-prefixes "org-dotemacs" '("org-dotemacs-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; org-dotemacs-autoloads.el ends here
