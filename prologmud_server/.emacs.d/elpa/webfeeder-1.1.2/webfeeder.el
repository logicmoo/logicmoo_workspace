;;; webfeeder.el --- Build RSS and Atom webfeeds from HTML files -*- lexical-binding: t -*-

;; Copyright (C) 2019  Free Software Foundation, Inc.

;; Author: Pierre Neidhardt <mail@ambrevar.xyz>
;; Maintainer: Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://gitlab.com/Ambrevar/emacs-webfeeder
;; Version: 1.1.2
;; Package-Requires: ((emacs "25.1"))
;; Keywords: news, hypermedia, blog, feed, rss, atom

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Webfeeder is an Emacs library to generate RSS
;; (https://en.wikipedia.org/wiki/RSS) and Atom
;; (https://en.wikipedia.org/wiki/Atom_(Web_standard)) feeds from HTML files.
;;
;; Other webfeed generators have been written for Emacs, but either they are
;; tied to other projects like blog generators, or they only work on Org files
;; like `ox-rss'.  Since Webfeeder generates webfeeds from HTML files, it is
;; more general.
;;
;; The various elements of the HTML input are parsed with customizable
;; functions.  For instance, Webfeeder offers two functions to parse the title:
;; `webfeeder-title-libxml' (using libxml if your Emacs is linked against it)
;; and the less reliable `webfeeder-title-default'.  Feel free to write you own
;; function and bind `webfeeder-title-function' before generating the feeds.
;;
;; The generated feeds should be valid on https://validator.w3.org/feed/.  If not,
;; it's a bug, please report.
;;
;; The full list of customizable functions is documented in
;; `webfeeder-html-files-to-items'.
;;
;; The entry point is `webfeeder-build': consult its documentation for more
;; information.
;;
;; Example:
;;
;; (webfeeder-build
;;   "atom.xml"
;;   "./public"
;;   "https://example.org/"
;;   '("post1.html" "post2.html" "post3.html")
;;   :title "My homepage"
;;   :description "A collection of articles in Atom")

;;; Code:

(require 'rx)
(require 'subr-x)
(require 'dom)
(require 'shr)                          ; TODO: Only with libxml?
(require 'cl-lib)

(defun webfeeder-has-libxml-p ()
  (fboundp 'libxml-parse-html-region))

(defgroup webfeeder nil
  "Predefined configurations for `webfeeder'."
  :group 'nxml)

(defcustom webfeeder-default-author "Unknown author"
  "When input files don't provide an author and it is required, use this value."
  :type 'string)

(defvar webfeeder-author-function (if (webfeeder-has-libxml-p)
                                         'webfeeder-author-libxml
                                       'webfeeder-author-default)
  "Function to fetch the author from an HTML file.")

(defvar webfeeder-date-function (if (webfeeder-has-libxml-p)
                                       'webfeeder-date-libxml
                                       'webfeeder-date-default)
  "Function to fetch the date from an HTML file.")

(defvar webfeeder-title-function (if (webfeeder-has-libxml-p)
                                       'webfeeder-title-libxml
                                       'webfeeder-title-default)
  "Function to fetch the title from an HTML file.")

(defvar webfeeder-subtitle-function (if (webfeeder-has-libxml-p)
                                           'webfeeder-subtitle-libxml
                                         'webfeeder-subtitle-default)
  "Function to fetch the subtitle from an HTML file.")

(defvar webfeeder-body-function (if (webfeeder-has-libxml-p)
                                       'webfeeder-body-libxml
                                       'webfeeder-body-default)
  "Function to fecth the HTML body.
This can be customized to choose which part to include.")

(defvar webfeeder-categories-function 'ignore
  "Function to fecth the categories from the HTML.")

(defvar webfeeder-generator-function (if (webfeeder-has-libxml-p)
                                         'webfeeder-generator-libxml
                                       'webfeeder-generator-default)
  "Function to fetch the generator from an HTML file.")

(defun webfeeder-author-default (html-file)
  "Return the author from the HTML-FILE, or nil if not found.
This is less reliable than `webfeeder-author-libxml'."
  (let ((case-fold-search t))
    (with-temp-buffer
      (insert-file-contents html-file)
      (goto-char (point-min))
      (search-forward-regexp (rx line-start "<meta name=\"author\" content=\""
                                 (group (* (not (any "\"")))) "\""))
      (webfeeder--parse-xml-string (match-string 1)))))

(defun webfeeder-author-libxml (html-file)
  "Return the author from the HTML-FILE, or nil if not found.
This requires Emacs to be linked against libxml."
  (with-temp-buffer
    (insert-file-contents html-file)
    (let* ((dom (libxml-parse-html-region (point-min) (point-max)))
           (meta-tags (dom-by-tag dom 'meta)))
      (cl-loop for (_meta ((_ . name-value) (_ . content-value))) in meta-tags
               when (string= name-value "author")
               return content-value))))

(defun webfeeder-date-default (html-file)
  "Return the date from the HTML-FILE.
The date is returned as time value.  See `current-time-string'.
This is less reliable than `webfeeder-date-libxml'."
  (let ((case-fold-search t))
    (with-temp-buffer
      (insert-file-contents html-file)
      (goto-char (point-min))
      ;; TODO: Try with proper Org date.
      (search-forward-regexp (rx line-start "<!-- " (group (* any)) " -->"))
      (if (match-string 1)
          (date-to-time
           (concat (match-string 1) " DummyDateSuffix"))
        0))))

(defun webfeeder-date-libxml (html-file)
  "Return the date from the HTML-FILE.
The date is returned as time value.  See `current-time-string'.
This requires Emacs to be linked against libxml."
  (with-temp-buffer
    (insert-file-contents html-file)
    (let* ((dom (libxml-parse-html-region (point-min) (point-max)))
           (date (dom-text (car (dom-by-class dom "date")))))
      (date-to-time
       (concat (or (and date
                        (string-match (rx (group (repeat 4 digit) "-" (repeat 2 digit) "-" (repeat 2 digit)))
                                      date)
                        (match-string 1 date))
                   ;; Org publish adds a first comment with the timestamp.
                   (dom-text (car (dom-by-tag dom 'comment))))
               " DummyDateSuffix")))))

(defun webfeeder-title-default (html-file)
  "Return the title from the HTML-FILE.
This is less reliable than `webfeeder-title-libxml'."
  (let ((case-fold-search t))
    (with-temp-buffer
      (insert-file-contents html-file)
      (goto-char (point-min))
      (search-forward-regexp (rx line-start "<title>" (group (* (not (any "<")))) "</title>"))
      (webfeeder--parse-xml-string (match-string 1)))))

(defun webfeeder-title-libxml (html-file)
  "Return the title from the HTML-FILE.
This requires Emacs to be linked against libxml."
  (with-temp-buffer
    (insert-file-contents html-file)
    (let* ((dom (libxml-parse-html-region (point-min) (point-max)))
           (title (dom-text (car (dom-by-tag dom 'title)))))
      (if (string= "" title)
          nil
        title))))

(defun webfeeder-subtitle-default (html-file)
  "Return the subtitle from the HTML-FILE.
This is less reliable than `webfeeder-subtitle-libxml'."
  (let ((case-fold-search t))
    (with-temp-buffer
      (insert-file-contents html-file)
      (goto-char (point-min))
      (search-forward-regexp (rx line-start "<span class=\"subtitle\">" (group (* (not (any "<")))) "</span>"))
      (webfeeder--parse-xml-string (match-string 1)))))

(defun webfeeder-subtitle-libxml (html-file)
  "Return the subtitle from the HTML-FILE.
This requires Emacs to be linked against libxml."
  (with-temp-buffer
    (insert-file-contents html-file)
    (let* ((dom (libxml-parse-html-region (point-min) (point-max)))
           (subtitle (dom-text (car (dom-by-class dom "subtitle")))))
      (if (string= "" subtitle)
          nil
        subtitle))))

(defun webfeeder-body-libxml (html-file &optional _url exclude-toc)
  "Return the body of HTML-FILE as a string.
If EXCLUDE-TOC is non-nil, the table-of-contents is not included in the body.
This requires Emacs to be linked against libxml."
  (with-temp-buffer
    (insert-file-contents html-file)
    (let* ((dom (libxml-parse-html-region (point-min) (point-max)))
           content)
      (when exclude-toc
        (let ((toc (or (dom-by-tag dom 'nav)
                       ;; <nav> is only in HTML5.
                       (dom-by-id dom "table-of-contents"))))
          (dom-remove-node dom (car toc))))
      (let ((title (or (dom-by-tag dom 'header)
                       ;; The <header> is "fancy" HTML (as per Org designation).
                       (dom-by-class dom "title"))))
        (dom-remove-node dom (car title)))
      (setq content (car (dom-by-id dom "content")))
      (shr-dom-to-xml content))))

(defun webfeeder-body-default (html-file &optional url exclude-toc)
  "Return the body of HTML-FILE as a string.
Relative links are made absolute to URL.
If EXCLUDE-TOC is non-nil, the table-of-contents is not included in the body.
This is less reliable than `webfeeder-body-libxml'."
  (with-temp-buffer
    (insert-file-contents html-file)
    ;; TODO: Move to helper function.
    ;; Better: See `org-html-link-use-abs-url'.
    (goto-char (point-min))
    (while (re-search-forward (rx "href=\"" (group (* (not (any "\"")))) "\"") nil t)
      ;; Make relative links absolute.
      (let ((match (match-string 1)))
        (unless (string-match "://" match)
          (goto-char (match-beginning 1))
          (insert (concat (replace-regexp-in-string "/*$" "" url) "/"))
          (goto-char (match-end 0)))))
    (buffer-substring-no-properties
     (progn
       (goto-char (point-min))
       (unless (and exclude-toc
                    (search-forward "</nav>" nil 'noerror))
         (goto-char (point-min))
         (search-forward "</header>"))
       (point))
     (progn
       (goto-char (point-max))
       (search-backward "<div id=\"postamble\"")
       (point)))))


(defun webfeeder-generator-default (html-file)
  "Return the generator from the HTML-FILE.
This is less reliable than `webfeeder-generator-libxml'."
  (let ((case-fold-search t))
    (with-temp-buffer
      (insert-file-contents html-file)
      (goto-char (point-min))
      (search-forward-regexp (rx line-start "<meta name=\"generator\" content=\"" (group (* (not (any "\"")))) "\""))
      (match-string 1))))

(defun webfeeder-generator-libxml (html-file)
  "Return the generator from the HTML-FILE.
This requires Emacs to be linked against libxml."
  (with-temp-buffer
    (insert-file-contents html-file)
    (let* ((dom (libxml-parse-html-region (point-min) (point-max)))
           (meta-tags (dom-by-tag dom 'meta)))
      (cl-loop for (_meta ((_ . name-value) (_ . content-value))) in meta-tags
               when (string= name-value "generator")
               return content-value))))

(defun webfeeder--date-to-rfc822 (date)
  (format-time-string "%a, %d %b %Y %H:%M:%S %z" date))

(defun webfeeder--date-to-rfc3339 (date)
  (concat (format-time-string "%FT%T" date)
          (let ((zone (format-time-string "%z" date)))
            (concat (substring zone 0 3)
                    ":"
                    (substring zone 3)))))

(defun webfeeder-item-to-rss (item)
  "Return an RSS ITEM as a string.
The date is set to epoch if the item date is nil."
  (concat
   "<item>\n"
   (when (webfeeder-item-author item)
     ;; RSS <author> tag must start with the email.  If no e-mail is found, we
     ;; skip the tag altogether.  Since it's hard to parse email addresses, we
     ;; use `mail-extract-address-components' which expects the "NAME <EMAIL>"
     ;; format.
     (let ((name+addr (webfeeder--extract-name+email (webfeeder-item-author item))))
       (when (cadr name+addr)
         (concat "  <author>"
                 (cadr name+addr)
                 (if (car name+addr)
                     (format " (%s)" (xml-escape-string (car name+addr)))
                   "")
                 "</author>\n"))))
   "  <title>" (webfeeder-item-title item) "</title>\n"
   "  <description><![CDATA[" (webfeeder-item-body item) "]]></description>\n"
   (when (webfeeder-item-categories item)
     (mapconcat (lambda (cat) (concat "  <category>" cat "</category>"))
                (webfeeder-item-categories item)
                "\n"))
   "  <link>" (webfeeder-item-url item) "</link>\n"
   (format "  <guid isPermaLink=\"false\">%s</guid>\n" (webfeeder-item-url item))
   "  <pubDate>"
   (webfeeder--date-to-rfc822 (or (webfeeder-item-date item) 0))
   "</pubDate>\n"
   "</item>\n"))

(defun webfeeder--extract-name+email (address)
  "Like `mail-extract-address-components' but does not set the
address part if email is missing.
For instance, calling this function on \"foo\" returns (\"foo\" nil).
Calling it on foo@bar.abc returns (nil \"foo@bar.abc\")."
  (let ((name+addr (mail-extract-address-components address)))
    (unless (string-match "@" (cadr name+addr))
      (setcar name+addr (cadr name+addr))
      (setcdr name+addr nil))
    (when (string= (car name+addr)
                   (cadr name+addr))
      (setcdr name+addr nil))
    name+addr))

(defun webfeeder--format-atom-author (author)
  (concat "<author>"
          (let ((name+addr (webfeeder--extract-name+email author)))
            (cond
             ((and (car name+addr) (cadr name+addr))
              (format "<name>%s</name><email>%s</email>"
                      (xml-escape-string (car name+addr))
                      (cadr name+addr)))
             ((car name+addr)
              (format "<name>%s</name>" (xml-escape-string author)))
             (t
              (format "<email>%s</email>" (xml-escape-string author)))))
          "</author>\n"))

(defun webfeeder-item-to-atom (item)
  "Return an atom ITEM as a string.
The date is set to epoch if the item date is nil."
  (concat
   "<entry>\n"
   "  <title>" (webfeeder-item-title item) "</title>\n"
   (concat "  "
           (webfeeder--format-atom-author
            (or (webfeeder-item-author item)
                webfeeder-default-author)))
   (when (webfeeder-item-subtitle item)
     (concat "  <summary>" (webfeeder-item-subtitle item) "</summary>\n"))
   ;; TODO: What's the impact of chosing between HTML or XHTML as a type?  Can
   ;; we auto-detect this?
   "  <content type=\"html\"><![CDATA["
   (webfeeder-item-body item)
   "]]></content>\n"
   (when (webfeeder-item-categories item)
     (mapconcat (lambda (cat) (concat "  <category>" cat "</category>"))
                (webfeeder-item-categories item)
                "\n"))
   "  <link href=\"" (webfeeder-item-url item) "\"/>\n"
   "  <id>" (webfeeder-item-url item) "</id>\n"
   "  <updated>"
   (webfeeder--date-to-rfc3339 (or (webfeeder-item-date item) 0))
   "</updated>\n"
   "</entry>\n"))

;; TODO: Set constructor?
(cl-defstruct webfeeder-item
  author
  (date 0)
  title
  subtitle
  body
  url
  categories
  generator)

(defun webfeeder--xml-escape-string (string)
  "Like `xml-escape-string' but return nil on nil."
  (if string
      (xml-escape-string string)
    nil))

(defun webfeeder--parse-xml-string (string)
  "Call `xml-parse-string' on string."
  (when string
    (with-temp-buffer
      (insert string)
      (goto-char (point-min))
      (xml-parse-string))))

;;;###autoload
(defun webfeeder-html-files-to-items (project-dir url html-files)
  "Parse the source HTML-FILES and return a list of webfeeder-items.
PROJECT-DIR is where HTML files are also assumed to reside.
PROJECT-DIR is the local root of the website hosted at URL.
HTML parsing details can be customized through the following
variables:
- `webfeeder-author-function'
- `webfeeder-date-function'
- `webfeeder-title-function'
- `webfeeder-subtitle-function'
- `webfeeder-body-function'.
- `webfeeder-categories-function'.
- `webfeeder-generator-function'."
  (cl-loop for html-file in html-files
           for dest = (expand-file-name html-file project-dir)
           for feed-url = (concat (replace-regexp-in-string "/*$" "" url) "/" html-file)
           ;; Author needs not be escaped because it's parsed in
           ;; `webfeeder--format-atom-author'.
           for feed-author = (funcall webfeeder-author-function dest)
           for feed-date = (or (funcall webfeeder-date-function
                                        (expand-file-name html-file project-dir))
                               0)
           for feed-title = (or (webfeeder--xml-escape-string (funcall webfeeder-title-function dest)) feed-url)
           for feed-subtitle = (webfeeder--xml-escape-string (funcall webfeeder-subtitle-function dest))
           for feed-body = (funcall webfeeder-body-function dest feed-url 'exclude-toc)
           for feed-categories = (funcall webfeeder-categories-function dest)
           for feed-generator = (funcall webfeeder-generator-function dest)
           collect (make-webfeeder-item :url feed-url
                                        :author feed-author
                                        :date feed-date
                                        :title feed-title
                                        :subtitle feed-subtitle
                                        :body feed-body
                                        :categories feed-categories
                                        :generator feed-generator)))

;;;###autoload
(cl-defun webfeeder-make-rss (webfeed url feed-items
                                         &key title description
                                         author generator
                                         build-date predicate max-entries
                                         &allow-other-keys)
  "Return a webfeed string in RSS format that contains every FEED-ITEMS.

WEBFEED is the path where the feed is intended to be stored,
relative to URL.

A feed can have a TITLE and DESCRIPTION: if not, the URL will be
used.

When BUILD-DATE is nil, use `current-time'.  Otherwise it can be
a time expression as in `current-time'.  0 means EPOCH.

When provided, FEED-ITEMS are only added if (PREDICATE FEED-ITEM)
returns non-nil.

The number of items is limited to MAX-ENTRIES.  When nil, all
items are included.

FEED-ITEMS can be generated with `webfeeder-html-files-to-items'."
  (setq title (or title url))
  (with-temp-buffer
    (insert "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
            "<rss version=\"2.0\" xmlns:atom=\"http://www.w3.org/2005/Atom\">\n"
            "<channel>\n"
            (if author
                (concat "<author><name>" author "</name></author>\n")
              "")
            "<title>" title "</title>\n"
            "<description>" (or description title) "</description>\n"
            ;; TODO: Change generator name?
            (concat "<generator>" (or generator "Emacs webfeeder.el") "</generator>\n")
            "<link>" url "</link>\n"
            (format "<atom:link href=\"%s/%s\" rel=\"self\" type=\"application/rss+xml\"/>\n"
                    (string-trim-right url "/") webfeed)
            "<lastBuildDate>" (webfeeder--date-to-rfc822 (or build-date (current-time)))
            "</lastBuildDate>\n")
    (cl-loop with sorted-item-list = (sort feed-items
                                           (lambda (x y)
                                             (time-less-p (webfeeder-item-date y)
                                                          (webfeeder-item-date x))))
             repeat (or max-entries (length sorted-item-list))
             for item in sorted-item-list
             do (when (or (not predicate) (funcall predicate item))
                  (insert (webfeeder-item-to-rss item))))
    (insert "</channel>\n"
            "</rss>\n")
    ;; TODO: Return buffer and not string?
    (buffer-string)))

;;;###autoload
(cl-defun webfeeder-make-atom (webfeed url feed-items
                                          &key title subtitle ;; description ; Unused? See below.
                                          author generator
                                          build-date predicate max-entries
                                          &allow-other-keys)
  "Return a webfeed string in Atom format that contains every FEED-ITEMS.

WEBFEED is the path where the feed is intended to be stored,
relative to URL.

A feed can have a TITLE: if not, the URL will be used.

When BUILD-DATE is nil, use `current-time'.  Otherwise it can be
a time expression as in `current-time'.  0 means EPOCH.

When provided, FEED-ITEMS are only added if (PREDICATE FEED-ITEM)
returns non-nil.

The number of items is limited to MAX-ENTRIES.  When nil, all
items are included.

FEED-ITEMS can be generated with `webfeeder-html-files-to-items'."
  (setq title (or title url))
  (with-temp-buffer
    (let ((path (concat (string-trim-right url "/") "/" webfeed)))
      (insert "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
              "<feed xmlns=\"http://www.w3.org/2005/Atom\">"
              (if author
                  (webfeeder--format-atom-author author)
                "")
              "<title>" title "</title>\n"
              (if subtitle
                  (concat "<subtitle>" subtitle "</subtitle>\n")
                "")
              ;; TODO: <summary> is not supported at the top-level.  Where do we put description?
              ;; "<summary>" (or description title) "</summary>\n"
              ;; TODO: Change generator name?
              (concat "<generator>" (or generator "Emacs webfeeder.el") "</generator>\n")
              "<link href=\"" url "\"/>\n"
              (format "<link href=\"%s\" rel=\"self\"/>\n"
                      path)
              ;; REVIEW: Use UUID?
              "<id>" path "</id>\n"
              ;; TODO: Publishing date?  Org create two "date" classes, not
              ;; really distinguishable.
              "<updated>" (webfeeder--date-to-rfc3339 (or build-date (current-time)))
              "</updated>\n"))
    (cl-loop with sorted-item-list = (sort feed-items
                                           (lambda (x y)
                                             (time-less-p (webfeeder-item-date y)
                                                          (webfeeder-item-date x))))
             repeat (or max-entries (length sorted-item-list))
             for item in sorted-item-list
             do (when (or (not predicate) (funcall predicate item))
                  (insert (webfeeder-item-to-atom item))))
    (insert "</feed>\n")
    (buffer-string)))

;;;###autoload
(cl-defun webfeeder-build (webfeed project-dir url html-files
                                      &key title subtitle description
                                      author generator
                                      build-date predicate max-entries
                                      (builder 'webfeeder-make-atom))
  "Build a WEBFEED out of HTML-FILES.
The WEBFEED is an XML file that contains every blog post in a
machine-readable format.

The WEBFEED file is stored in PROJECT-DIR where HTML-FILES are
also assumed to reside.

For more details on the other arguments, see
`webfeeder-make-atom', `webfeeder-make-rss' and
`webfeeder-html-files-to-items'."
  (let ((feed-items (webfeeder-html-files-to-items project-dir url html-files)))
    (with-temp-file (expand-file-name webfeed project-dir)
      (insert (funcall builder webfeed url feed-items
               :author author
               :title title
               :subtitle subtitle
               :description description
               :generator generator
               :build-date build-date
               :predicate predicate
               :max-entries max-entries)))))

(provide 'webfeeder)
;;; webfeeder.el ends here
