;;; org2jekyll-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "org2jekyll" "org2jekyll.el" (0 0 0 0))
;;; Generated autoloads from org2jekyll.el

(autoload 'org2jekyll-input-directory "org2jekyll" "\
Compute the input folder from the FOLDER-NAME.

\(fn &optional FOLDER-NAME)" nil nil)

(autoload 'org2jekyll-output-directory "org2jekyll" "\
Compute the output folder from the optional FOLDER-NAME.

\(fn &optional FOLDER-NAME)" nil nil)

(autoload 'org2jekyll-init-current-buffer "org2jekyll" "\
Given an existing buffer, add the needed metadata to make it a post or page." t nil)

(autoload 'org2jekyll-create-draft "org2jekyll" "\
Create a new Jekyll blog post with TITLE.
The `'%s`' will be replaced respectively by the blog entry name, the author, the
 generated date, the title, the description, the tags and the categories." t nil)

(autoload 'org2jekyll-list-posts "org2jekyll" "\
Lists the posts folder." t nil)

(autoload 'org2jekyll-list-drafts "org2jekyll" "\
List the drafts folder." t nil)

(autoload 'org2jekyll-publish "org2jekyll" "\
Publish the current org file as post or page depending on the chosen layout.
Layout `'post`' is a jekyll post.
Layout `'default`' is a page (depending on the user customs)." t nil)

(autoload 'org2jekyll-publish-posts "org2jekyll" "\
Publish all posts." t nil)

(autoload 'org2jekyll-publish-pages "org2jekyll" "\
Publish all pages." t nil)

(autoload 'org2jekyll-mode "org2jekyll" "\
Functionality for publishing the current org-mode post to jekyll.
With no argument, the mode is toggled on/off.
Non-nil argument turns mode on.
Nil argument turns mode off.

This is a minor mode.  If called interactively, toggle the
`Org2jekyll mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `org2jekyll-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

Commands:
\\{org2jekyll-mode-map}

\(fn &optional ARG)" t nil)

(register-definition-prefixes "org2jekyll" '("org2jekyll-"))

;;;***

;;;### (autoloads nil "org2jekyll-utilities" "org2jekyll-utilities.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from org2jekyll-utilities.el

(register-definition-prefixes "org2jekyll-utilities" '("org2jekyll-tests-with-temp-buffer"))

;;;***

;;;### (autoloads nil nil ("org2jekyll-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; org2jekyll-autoloads.el ends here
