;;; lsp-javacomp.el --- Provide Java IDE features powered by JavaComp.  -*- lexical-binding: t -*-

;; Version: 1.0
;; Package-Version: 20190124.1755
;; Package-Commit: 82aa4ad6ca03a74565c35e855b318b1887bcd89b
;; Package-Requires: ((emacs "25.1") (lsp-mode "3.0") (s "1.2.0"))
;; Keywords: java tools lsp
;; URL: https://github.com/tigersoldier/lsp-javacomp

;; This program is free software: you can redistribute it and/or modify
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

;; A lsp-mode client that provides Java code-completion and other IDE
;; features for Emacs. It's backed by JavaComp.

;; To use it, add the code below to your .emacs file:

;;    (require 'lsp-javacomp)
;;    (add-hook 'java-mode-hook #'lsp-javacomp-enable)

;;; Code:

(require 'cc-mode)
(require 'lsp-mode)
(require 'json)
(require 's)

(defalias 'lsp-javacomp-enable 'lsp)

(defcustom lsp-javacomp-server-install-dir (locate-user-emacs-file "javacomp/")
  "Install directory for JavaComp server.
Requires to be ended with a slash."
  :group 'lsp-javacomp
  :risky t
  :type 'directory)

(defcustom lsp-javacomp-java-executable "java"
  "Name or path of the java executable binary file."
  :type '(choice (const nil) string)
  :group 'lsp-javacomp)

(defcustom lsp-javacomp-java-options nil
  "List of command line options to be pased to java command."
  :type '(set string)
  :group 'lsp-javacomp)

(defcustom lsp-javacomp-server-log-level nil
  "Server log level.
This option sets the value of the logLevel initialization option.
Possible values are \"severe\", \"warning\", \"info\", \"fine\",
\"finer\", \"finest\". If it's set to nil, the option is not sent
to the server."
  :type '(choice
          (const nil)
          (const "severe")
          (const "warning")
          (const "info")
          (const "fine")
          (const "finer")
          (const "finest"))
  :group 'lsp-javacomp)

(defcustom lsp-javacomp-server-log-path nil
  "Server log path.
This option sets the value of the logPath initialization option."
  :type '(choice (const nil) string)
  :group 'lsp-javacomp)

(defcustom lsp-javacomp-server-ignore-paths nil
  "A list of string paths to be ignored by the server.
This option sets the value of the ignorePaths initialization option."
  :type '(repeat string)
  :group 'lsp-javacomp)

(defcustom lsp-javacomp-server-type-index-files nil
  "A list of string paths of the type index files.
This option sets the value of the typeIndexFiles initialization option."
  :type '(repeat string)
  :group 'lsp-javacomp)

(defconst lsp-javacomp-latest-release-url
  "https://api.github.com/repos/tigersoldier/JavaComp/releases/latest"
  "URL to retrieve the latest release of JavaComp server.")

(defun lsp-javacomp--server-jar-path ()
  "Return the path to the JavaComp server JAR file."
  (expand-file-name "javacomp.jar" lsp-javacomp-server-install-dir))

(defun lsp-javacomp--command ()
  "Return a list of the command and arguments to launch the JavaComp server."
  `( ,lsp-javacomp-java-executable
     ,@lsp-javacomp-java-options
     "-jar"
     ,(lsp-javacomp--server-jar-path)))

(defun lsp-javacomp--suggest-root ()
  "Retrieves the root directory of the java project root based on some well-known project files."
  (when (memq major-mode '(java-mode))
    (let ((project-files '("javacomp.json" "pom.xml" "build.gradle" ".project" "WORKSPACE")))
      (when-let ((root (seq-some (lambda (file)
                                   (locate-dominating-file default-directory file))
                                 project-files)))
        (expand-file-name root)))))

(defun lsp-javacomp--get-prefix ()
  "Get prefix for completion.

Return a cons of (start . end) for the bound of the prefix."
  (let* ((bound (bounds-of-thing-at-point 'symbol))
         (start (or (and bound (car bound)) (point)))
         (end (or (and bound (cdr bound)) (point))))
    ;; java-mode considers '@' as a symbol constituent. However JavaComp doesn't
    ;; take the leading '@' as part of the prefix. Remove the leading '@' from
    ;; the prefix.
    (when (and (< start end) (char-equal (char-after start) ?@))
      (setq start (1+ start)))
    (cons start end)))

(defun lsp-javacomp--get-init-params ()
  "Return initialization options."
  (list :logPath lsp-javacomp-server-log-path
        :logLevel lsp-javacomp-server-log-level
        :ignorePths lsp-javacomp-server-ignore-paths
        :typeIndexFiles lsp-javacomp-server-type-index-files))

;;;###autoload
(defun lsp-javacomp-install-server (&optional prompt-exists)
  "Download the JavaComp server JAR file if it does not exist.

If PROMPT-EXISTS is non-nil, show a message if the server jar
file already exists."
  (interactive '(t))
  (let ((jar-file (lsp-javacomp--server-jar-path)))
    (if (file-exists-p jar-file)
        (and prompt-exists (message "JavaComp server already exists."))
      (lsp-javacomp--download-server))))

;;;###autoload
(defun lsp-javacomp-update-server ()
  "Update JavaComp jar file to the latest version."
  (interactive)
  (lsp-javacomp--download-server))

(defun lsp-javacomp--download-server ()
  "Download latest JavaComp jar file."
  (message "Getting the latest JavaComp server...")
  (url-retrieve lsp-javacomp-latest-release-url #'lsp-javacomp--latest-release-callback))

(defun lsp-javacomp--latest-release-callback (stats)
  "Handle the `url-retrive' callback for JavaComp latest release request.

STATS is passed by `url-retrieve'.

See https://developer.github.com/v3/repos/releases/#get-the-latest-release"
  (search-forward "\n\n")
  (if-let (err (plist-get stats :error))
      (error "Failed to get the latest release of JavaComp server: %s" (car err))
    (let* ((release (json-read))
           (assets (alist-get 'assets release))
           (jar-asset (seq-find (lambda (asset)
                                  (s-match "^javacomp.*\\.jar$" (alist-get 'name asset)))
                                assets))
           (jar-url (alist-get 'browser_download_url jar-asset))
           (release-version (alist-get 'tag_name release)))
      (if jar-url
          (progn
            (message "Found JavaComp %s, downloading..." release-version)
            (make-directory (expand-file-name lsp-javacomp-server-install-dir) t)
            (url-copy-file jar-url (lsp-javacomp--server-jar-path) t))
        (error "Fail to get the URL of the JavaComp server")))))

(defun lsp-javacomp-document-content ()
  (interactive)
  (let* ((document-content (lsp-send-request (lsp-make-request
                                              "$JavaComp/documentContent"
                                              `(:uri ,(lsp--buffer-uri)))))
         (snapshot-content (gethash "snapshotContent" document-content))
         (buffer (get-buffer-create "*JavaComp document content*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert snapshot-content)
      (java-mode))
    (display-buffer-use-some-window buffer nil)))

(advice-add 'lsp--suggest-project-root :before-until #'lsp-javacomp--suggest-root)

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection #'lsp-javacomp--command)
  :major-modes '(java-mode)
  :server-id 'javacomp
  :multi-root nil
  :initialization-options #'lsp-javacomp--get-init-params
  :ignore-regexps '("^SLF4J: "
                    "^Listening for transport dt_socket at address: ")
  :library-folders-fn nil))

(provide 'lsp-javacomp)
;;; lsp-javacomp.el ends here
