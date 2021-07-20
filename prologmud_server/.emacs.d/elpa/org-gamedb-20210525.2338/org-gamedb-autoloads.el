;;; org-gamedb-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "org-gamedb" "org-gamedb.el" (0 0 0 0))
;;; Generated autoloads from org-gamedb.el

(autoload 'org-gamedb-query "org-gamedb" "\
Make a QUERY to RESOURCE.
QUERY is a string and can be anything. Example queries are \"quantic\" for
companies and \"stardew\" for games.

RESOURCE is a resource defined by API. See available resources at
URL `https://www.giantbomb.com/api/documentation/'.

If you don't know what to query, just make an empty query!

Don't forget to set an API key first. See `org-gamedb-get-api-key'.

\(fn RESOURCE QUERY)" t nil)

(autoload 'org-gamedb-games-query "org-gamedb" "\
Make a QUERY to games resource.
QUERY is a string and can be anything. Example queries are \"quantic\" for
companies and \"stardew\" for games.

If you don't know what to query, just make an empty query!

Don't forget to set an API key. See `org-gamedb-get-api-key'.

\(fn QUERY)" t nil)

(autoload 'org-gamedb-get-api-key "org-gamedb" "\
Browse \"https://www.giantbomb.com/api/\" to get an api key.
You need to create a Giant Bomb account for it. Set your API key
to `org-gamedb-api-key' in your customization file.

Please respect \"Terms of Use\"." t nil)

(register-definition-prefixes "org-gamedb" '("org-gamedb-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; org-gamedb-autoloads.el ends here
