;;; org-seek.el --- Searching Org-mode files with search tools.

;; Author: stardiviner <numbchild@gmail.com>
;; Maintainer: stardiviner <numbchild@gmail.com>
;; Keywords: org search ag pt
;; Package-Version: 20161217.502
;; Package-Commit: 1f51e6634e3b9a6a29d335d0d14370a6ffef2265
;; URL: https://github.com/stardiviner/org-seek.el
;; Created: 12th Dec 2016
;; Version: 0.1.1
;; Package-Requires: ((emacs "24.3") (ag "0.48"))

;;; Commentary:

;; You need to install at least one of searching tools: ag, pt, grep etc.

;;; Code:
;;; ----------------------------------------------------------------------------
;; load function `ag/read-from-minibuffer', `ag/search'
(require 'ag)
(require 'cl-lib)

(defgroup org-seek nil
  "Org files searching."
  :group 'org)

(defcustom org-seek-org-root-path org-directory
  "The default root path of your Org-mode files."
  :type 'string
  :group 'org-seek)

(defcustom org-seek-search-tool 'ag
  "Specify the search tool for searching.

The search tool can be: ag, pt, ripgrep, grep, ack."
  :type 'symbol
  :group 'org-seek)


(autoload 'ag/search "ag")
(autoload 'pt-regexp "pt")
(autoload 'ripgrep-regexp "ripgrep")

;;;###autoload
(defun org-seek-string (string directory)
  "Full context searching STRING using ag in a given DIRECTORY.

By default STRING is the symbol under point unless called with a
prefix, prompts for flags to pass to ag."
  (interactive
   (list
    (read-from-minibuffer "Search string in Org:" (thing-at-point 'symbol))
    (read-directory-name "Directory: " (expand-file-name org-seek-org-root-path))
    ))
  
  (cl-case org-seek-search-tool
    ('ag
     ;; (ag/search string directory :regexp nil :file-type 'org)
     (ag/search string directory
                :regexp nil :file-regex ".*\.org"))
    ('pt
     (pt-regexp string directory))
    ('ripgrep
     (ripgrep-regexp string directory))
    )
  )

;;;###autoload
(defun org-seek-regexp (regexp directory)
  "Full context searching REGEXP using ag in a given DIRECTORY.

By default REGEXP is the symbol under point unless called with a
prefix, prompts for flags to pass to ag."
  (interactive
   (list
    (read-from-minibuffer "Search regexp in Org:" (thing-at-point 'symbol))
    (read-directory-name "Directory: " (expand-file-name org-seek-org-root-path))
    ))
  
  (cl-case org-seek-search-tool
    ('ag
     (ag/search regexp directory
                :regexp nil :file-regex ".*\.org"))
    ('pt
     (pt-regexp regexp directory))
    ('ripgrep
     (ripgrep-regexp regexp directory))
    )
  )

;;;###autoload
(defun org-seek-headlines (string directory)
  "Search STRING in Org files headlines using ag in a given DIRECTORY.

By default STRING is the symbol under point unless called with a
prefix, prompts for flags to pass to ag."
  (interactive
   (list
    (read-from-minibuffer "Search headlines in Org:" (thing-at-point 'symbol))
    (read-directory-name "Directory: "
                         (expand-file-name (if current-prefix-arg org-seek-org-root-path ".")))
    ))
  
  (cl-case org-seek-search-tool
    ('ag
     (ag/search (concat "^(\\*)+\ .*" string) directory
                :regexp t :file-regex ".*\.org"))
    ('pt
     (pt-regexp (concat "^(\\*)+\ .*" string) directory))
    ('ripgrep
     (ripgrep-regexp (concat "^(\\*)+\ .*" string) directory))
    ;; TODO
    ;; ('grep
    ;;  (rgrep (concat "^(\\*)+\ .*" string) org directory))
    ;; TODO
    ;; ('ack
    ;;  (ack))
    )
  
  ;; the real shell command and regexp result:
  ;; ag --file-search-regex .\*.org --group --line-number --column --color --color-match 30\;43 --color-path 1\;32 --smart-case --stats -- \^\(\\\*\)\+.\*time .
  )

;;; ----------------------------------------------------------------------------

(provide 'org-seek)

;;; org-seek.el ends here
