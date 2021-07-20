;;; org-static-blog-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "org-static-blog" "org-static-blog.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from org-static-blog.el

(autoload 'org-static-blog-publish "org-static-blog" "\
Render all blog posts, the index, archive, tags, and RSS feed.
Only blog posts that changed since the HTML was created are
re-rendered.

With a prefix argument, all blog posts are re-rendered
unconditionally.

\(fn &optional FORCE-RENDER)" t nil)

(autoload 'org-static-blog-publish-file "org-static-blog" "\
Publish a single POST-FILENAME.
The index, archive, tags, and RSS feed are not updated.

\(fn POST-FILENAME)" t nil)

(autoload 'org-static-blog-create-new-post "org-static-blog" "\
Creates a new blog post.
Prompts for a title and proposes a file name. The file name is
only a suggestion; You can choose any other file name if you so
choose.

\(fn &optional DRAFT)" t nil)

(autoload 'org-static-blog-create-new-draft "org-static-blog" "\
Creates a new blog draft.
Prompts for a title and proposes a file name. The file name is
only a suggestion; You can choose any other file name if you so
choose." t nil)

(autoload 'org-static-blog-mode "org-static-blog" "\
Blogging with org-mode and emacs.

\(fn)" t nil)

(register-definition-prefixes "org-static-blog" '("concat-to-dir" "org-static-blog-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; org-static-blog-autoloads.el ends here
