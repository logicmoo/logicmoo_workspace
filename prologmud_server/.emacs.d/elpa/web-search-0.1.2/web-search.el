;;; web-search.el --- Open a web search  -*- lexical-binding: t; -*-

;; Copyright (C) 2017, 2019  Xu Chunyang

;; Author: Xu Chunyang <mail@xuchunyang.me>
;; URL: https://github.com/xuchunyang/web-search.el
;; Package-Requires: ((emacs "24.3"))
;; Keywords: web, search
;; Version: 0.1.2

(defconst web-search-version "0.1.2" "Version number of `web-search.el'.")

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

;; Open a web search.  Idea from https://github.com/zquestz/s
;;
;; To use, in Emacs, use the command `web-search', in Terminal, use the Bash
;; script ./web-search under the same directory.
;;
;; Please see https://github.com/xuchunyang/web-search.el for documentation.

;;; Code:

(require 'cl-lib)


;;; Custom

(defgroup web-search nil
  "Open a web search."
  :group 'tools)

(defcustom web-search-providers
  '(
    ;; M-x sort-lines
    ("500px"             "https://500px.com/search?q=%s&type=photos" "Photos")
    ("Arch Package"      "https://www.archlinux.org/packages/?q=%s" "Arch Linux")
    ("Arch Wiki"         "https://wiki.archlinux.org/index.php?search=%s" "Arch Linux")
    ("Bandcamp"          "https://bandcamp.com/search?q=%s" "Music")       
    ("Bing"              "https://www.bing.com/search?q=%s" "Search")
    ("Debian Manpages"   "https://manpages.debian.org/jump?q=%s")
    ("Debian Package"    "https://packages.debian.org/search?keywords=%s&searchon=names&suite=stable&section=all")
    ("DuckDuckGo"        "https://duckduckgo.com/lite/?q=%s" "Search")
    ("Gist"              "https://gist.github.com/search?q=%s" "Code")
    ("GitHub"            "https://github.com/search?q=%s" "Code")
    ("Google"            "https://www.google.com/search?q=%s" "Search")
    ("Hacker News"       "https://hn.algolia.com/?q=%s" "Tech-News")
    ("MacPorts"          "https://www.macports.org/ports.php?by=name&substr=%s")
    ("Pinterest"         "https://www.pinterest.com/search/pins/?q=%s")
    ("Reddit"            web-search-providers--format-url:reddit)
    ("RubyGems"          "https://rubygems.org/search?query=%s")
    ("Soundcloud"        "https://soundcloud.com/search?q=%s" "Music")
    ("Stack Overflow"    "https://stackoverflow.com/search?q=%s" "Code")
    ("Wikipedia"         "https://en.wikipedia.org/wiki/Special:Search?search=%s" "Education")
    ("YouTube"           "https://www.youtube.com/results?search_query=%s")
    ("Zhihu"             "https://www.zhihu.com/search?type=content&q=%s" "Forums")
    ;; M-x sort-lines ends here
    )
  "Search providers, a list of (NAME URL TAG1 TAG2 ...).
URL can be either String or Function.
If it is a string, it must contains a %s token for the query string.
If it is a function, it must takes a argument which is the query string."
  :group 'web-search
  ;; Well, because I can't figure out how to write a more specific type
  :type 'sexp)

(defcustom web-search-default-provider "Google"
  "Default search provider."
  :group 'web-search
  :type (let ((providers
               (mapcar (lambda (p)
                         `(string :tag ,(car p) :value ,(car p)))
                       web-search-providers)))
          `(choice ,@providers)))


;;; Internal

(defun web-search-providers--format-url:reddit (query)
  "Build search URL base on QUERY.

If QUERY looks like \"/r/subreddit query\", treat as subreddit
and do restrict search on the subreddit. Otherwise, treat it as
regular reddit search"
  (if (string-match "^/r/\\([^ ]*\\) \\(.*\\)" query)
      (let ((subreddit (match-string 1 query))
            (new-query (match-string 2 query)))
        (format "https://www.reddit.com/r/%s/search?q=%s&restrict_sr=on"
                subreddit
                (url-hexify-string new-query)))
    (format "https://www.reddit.com/search?q=%s"
            (url-hexify-string query))))

(defun web-search--tags ()
  (delete-dups
   (cl-loop for p in web-search-providers
            append (cddr p))))

(defun web-search--find-providers (tag)
  "Return a list of providers which is tagged by TAG."
  (or (cl-loop for p in web-search-providers
               when (member (downcase tag) (mapcar #'downcase (cddr p)))
               collect p)
      (error "Unknown tag '%s'" tag)))

(defun web-search--format-url (query provider)
  "Format a URL for search QUERY on PROVIDER.
PROVIDER can be a string (the name of one provider) or a
list (one provider, i.e., one element of `web-search-providers')."
  (let ((url-or-func
         (cond
          ((listp provider)
           (cadr provider))
          ((stringp provider)
           (cl-loop for p in web-search-providers
                    when (string= (downcase provider)
                                  (downcase (car p)))
                    return (cadr p))))))
    (cond ((stringp url-or-func) (format url-or-func (url-hexify-string query)))
          ((functionp url-or-func) (funcall url-or-func query))
          (t  (error "Unknown provider '%S'" provider)))))

(defun web-search--format-urls (query providers)
  (mapcar (lambda (provider) (web-search--format-url query provider))
          providers))


;;; Commands

;;;###autoload
(defun web-search (query &optional providers tag)
  "Search for QUERY on website(s).

Without prefix argument, search on `web-search-default-provider'.
One C-u, choose a provider.
Two C-u, choose a tag (notes that a tag can match multiple providers)."
  (interactive
   (let* ((providers
           (if (equal current-prefix-arg '(4)) ; One C-u
               (let* ((default web-search-default-provider)
                      (prompt (format "Provider (default %s): " default))
                      (completion-ignore-case t))
                 (list (completing-read prompt web-search-providers nil t nil nil default)))
               (list web-search-default-provider)))
          (tag
           (when (equal current-prefix-arg '(16)) ; Two C-u
             (completing-read "Tag: " (web-search--tags))))
          (query
           (let ((initial (if (use-region-p)
                              (buffer-substring-no-properties (region-beginning) (region-end))
                              (current-word)))
                 (prompt (format "Search %s: "
                                 (if tag
                                     (mapconcat #'identity
                                                (mapcar #'car (web-search--find-providers tag))
                                                ", ") 
                                     (mapconcat #'identity providers ", ")))))
             (read-string prompt initial))))
     (list query providers tag)))
  (setq providers (or (and tag (web-search--find-providers tag))
                      providers
                      (list web-search-default-provider)))
  (mapc #'browse-url (web-search--format-urls query providers)))


;;; Batch

(defun web-search-batch-print-usage ()
  (princ (format "\
Web search from the terminal.

Usage: %s <query> [options]

Options:
  -h, --help              display help
  -l, --list-providers    list supported providers
      --list-tags         list available tags
  -o, --output            output only mode
  -p, --provider string   search provider (default \"%s\")
  -t, --tag string        search tag
  -v, --verbose           verbose mode
      --version           display version
      --completion        display completion code for Bash/Zsh
" (or (getenv "WEB_SEARCH_PROGRAM_NAME")
      "emacs -Q --batch -l web-search.el -f web-search-batch --")
web-search-default-provider)))

;; XXX: Improve command line argument handling, for exampke, 'web-search -op
;; Google' should be supported
(defun web-search-batch ()
  (unless noninteractive
    (funcall (if (fboundp 'user-error) #'user-error #'error)
             "`web-search-batch' can only be called in batch mode"))

  (setq argv (delete "--" argv))

  (when (or (member "-h" argv)
            (member "--help" argv))
    (web-search-batch-print-usage)
    (kill-emacs 0))

  (when (member "--version" argv)
    (princ (format "web-search.el %s\nGNU Emacs %s\n"
                   web-search-version emacs-version))
    (kill-emacs 0))

  (when (or (member "-l" argv)
            (member "--list-providers" argv))
    (mapc (lambda (p) (princ (format "%s\n" (car p)))) web-search-providers)
    (kill-emacs 0))

  (when (member "--list-tags" argv)
    (mapc (lambda (s) (princ (format "%s\n" s))) (sort (web-search--tags) #'string<))
    (kill-emacs 0))

  (let (arg
        verbose-mode
        output-only-mode
        query
        provider
        tag
        providers
        urls)
    (while (setq arg (pop argv))
      (pcase arg
        ((or "-v" "--verbose") (setq verbose-mode t))
        ((or "-o" "--output")  (setq output-only-mode t))
        ((or "-p" "--provider") (setq provider (pop argv)))
        ((or "-t" "--tag") (setq tag (pop argv)))
        (_ (setq query (if query
                           (concat query " " arg)
                         arg)))))
    (unless query
      (web-search-batch-print-usage)
      (kill-emacs 1))
    
    (setq providers (or (and tag (web-search--find-providers tag))
                        (and provider (list provider))
                        (list web-search-default-provider)))
    (setq urls (web-search--format-urls query providers))
    (when (or verbose-mode output-only-mode)
      (mapc (lambda (url) (princ (format "%s\n" url))) urls))
    (unless output-only-mode
      (mapc (lambda (url)
              (let ((proc (browse-url url)))
                (when (processp proc)
                  (accept-process-output proc))))
            urls))))

(provide 'web-search)
;;; web-search.el ends here
