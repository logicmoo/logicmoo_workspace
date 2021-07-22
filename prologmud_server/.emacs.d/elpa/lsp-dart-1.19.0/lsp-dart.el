;;; lsp-dart.el --- Dart support lsp-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Eric Dallo

;; Version: 1.19.0
;; Package-Requires: ((emacs "26.1") (lsp-treemacs "0.3") (lsp-mode "7.0.1") (dap-mode "0.6") (f "0.20.0") (dash "2.14.1") (pkg-info "0.4") (dart-mode "1.0.5"))
;; Keywords: languages, extensions
;; URL: https://emacs-lsp.github.io/lsp-dart

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Dart analysis server client for LSP mode

;;; Code:

(require 'f)
(require 'dash)
(require 'lsp-mode)

(require 'lsp-dart-protocol)
(require 'lsp-dart-utils)
(require 'lsp-dart-closing-labels)
(require 'lsp-dart-dap)
(require 'lsp-dart-test-support)
(require 'lsp-dart-test-output)
(require 'lsp-dart-test-tree)
(require 'lsp-dart-code-lens)
(require 'lsp-dart-outline)
(require 'lsp-dart-flutter-fringe-colors)
(require 'lsp-dart-flutter-widget-guide)
(require 'lsp-dart-commands)

(defgroup lsp-dart nil
  "LSP support for Dart, using dart analysis server."
  :prefix "lsp-dart-"
  :group 'applications
  :link '(url-link :tag "GitHub" "https://github.com/emacs-lsp/lsp-dart"))

(defcustom lsp-dart-server-command nil
  "The analysis_server executable to use."
  :type '(repeat string)
  :group 'lsp-dart)

(defcustom lsp-dart-extra-library-directories '()
  "List of directories which will be considered to be libraries."
  :risky t
  :type '(repeat string)
  :group 'lsp-dart)

(defcustom lsp-dart-only-analyze-projects-with-open-files nil
  "Analyze project root of open files even if not using workspace folders.
When set to non-nil, even if the file was not openned as a workspace project,
the analysis server will try to find the project root and analyze all project
files."
  :type 'boolean
  :group 'lsp-dart)

(defcustom lsp-dart-suggest-from-unimported-libraries t
  "Import suggestions happens only for non imported symbols.
When set to nil, completion will not include symbols that are not already
imported into the current file."
  :type 'boolean
  :group 'lsp-dart)

(defcustom lsp-dart-enable-sdk-formatter t
  "When to enable server formmating."
  :type 'boolean
  :group 'lsp-dart)

(defcustom lsp-dart-line-length 80
  "The number of characters the formatter should wrap code at."
  :type 'number
  :group 'lsp-dart)

(defcustom lsp-dart-show-todos nil
  "Whether to generate diagnostics for TODO comments.
If unspecified, diagnostics will not be generated."
  :type 'boolean
  :group 'lsp-dart)

(defcustom lsp-dart-complete-function-calls t
  "Completes functions/methods with their required parameters."
  :type 'boolean
  :group 'lsp-dart)


;;; Internal

(declare-function pkg-info-version-info "ext:pkg-info")

(defun lsp-dart--library-folders ()
  "Return the library folders path to analyze."
  (let ((sdk-lib (expand-file-name "lib" (lsp-dart-get-sdk-dir))))
    (if (string-prefix-p sdk-lib (buffer-file-name))
        (append (list (file-name-directory (buffer-file-name))) lsp-dart-extra-library-directories)
      lsp-dart-extra-library-directories)))

(defun lsp-dart--configuration (_workspace _items)
  "Return the client workspace configuration."
  (vector (lsp-ht ("enableSdkFormatter" lsp-dart-enable-sdk-formatter)
                  ("completeFunctionCalls" lsp-dart-complete-function-calls)
                  ("showTodos" lsp-dart-show-todos)
                  ("lineLength" lsp-dart-line-length))))

(defun lsp-dart--server-command ()
  "Generate LSP startup command."
  (let ((client-version (when (require 'pkg-info nil t)
                          (format "--client-version %s"
                                  (pkg-info-version-info 'lsp-dart)))))
    (or lsp-dart-server-command
        `(,(lsp-dart-dart-command)
          ,(expand-file-name (f-join (lsp-dart-get-sdk-dir) "bin/snapshots/analysis_server.dart.snapshot"))
          "--lsp"
          "--client-id emacs.lsp-dart"
          ,client-version))))

(defun lsp-dart--activate-features ()
  "Activate lsp-dart features if enabled."
  (when (lsp-dart-flutter-project-p) (lsp-dart-flutter-daemon-start))
  (when lsp-dart-flutter-widget-guides (lsp-dart-flutter-widget-guides-mode 1))
  (when lsp-dart-flutter-fringe-colors (lsp-dart-flutter-fringe-colors-mode 1))
  (when lsp-dart-closing-labels (lsp-dart-closing-labels-mode 1))
  (when lsp-dart-outline (lsp-dart-outline-mode 1))
  (when lsp-dart-flutter-outline (lsp-dart-flutter-outline-mode 1))
  (when (lsp-dart-test-file-p (buffer-file-name)) (lsp-dart-test-mode 1))
  (when lsp-dart-main-code-lens (lsp-dart-main-code-lens-mode 1))
  (when lsp-dart-test-code-lens (lsp-dart-test-code-lens-mode 1)))

(lsp-register-client
 (make-lsp-client :new-connection
                  (lsp-stdio-connection #'lsp-dart--server-command)
                  :activation-fn (lambda (filename &optional _)
                                   (or (derived-mode-p 'dart-mode)
                                       (string= (f-filename filename) "pubspec.yaml")))
                  :priority 1
                  :initialization-options
                  `((onlyAnalyzeProjectsWithOpenFiles . ,lsp-dart-only-analyze-projects-with-open-files)
                    (suggestFromUnimportedLibraries . ,lsp-dart-suggest-from-unimported-libraries)
                    (closingLabels . ,lsp-dart-closing-labels)
                    (outline . ,lsp-dart-outline)
                    (flutterOutline . ,lsp-dart-flutter-outline))
                  :library-folders-fn (lambda (_workspace) (lsp-dart--library-folders))
                  :notification-handlers (lsp-ht ("dart/textDocument/publishClosingLabels" (lambda (_workspace notification)
                                                                                             (run-hook-with-args 'lsp-dart-closing-labels-arrived-hook notification)))
                                                 ("dart/textDocument/publishOutline" (lambda (_workspace notification)
                                                                                       (run-hook-with-args 'lsp-dart-outline-arrived-hook notification)))
                                                 ("dart/textDocument/publishFlutterOutline" (lambda (_workspace notification)
                                                                                              (when (lsp-dart-flutter-project-p)
                                                                                                (run-hook-with-args 'lsp-dart-flutter-outline-arrived-hook notification))))
                                                 ("$/analyzerStatus" #'ignore))
                  :request-handlers (lsp-ht ("workspace/configuration" #'lsp-dart--configuration))
                  :after-open-fn #'lsp-dart--activate-features
                  :server-id 'dart_analysis_server))


;;; Public interface

;;;###autoload
(defun lsp-dart-version ()
  "Get the lsp-dart version as string.

The returned string includes the version from main file header,
 the current time and the Emacs version.

If the version number could not be determined, signal an error."
  (interactive)
  (let* ((version (and (require 'pkg-info nil t)
                       (pkg-info-version-info 'lsp-dart)))
         (lsp-dart-string (format "%s at %s @ Emacs %s"
                                  (or version "unknown")
                                  (format-time-string "%Y.%m.%d" (current-time))
                                  emacs-version))
         (dart-sdk-string (if (lsp-dart-get-sdk-dir)
                              (concat (propertize "[Dart SDK] " 'face 'font-lock-function-name-face)
                                      (lsp-dart-get-full-dart-version))
                            (concat "No Dart SDK found, `lsp-dart-sdk-dir` is: %s" lsp-dart-sdk-dir)))
         (flutter-sdk-dir-string (concat "[Flutter SDK] " (or (lsp-dart-get-flutter-sdk-dir) "Not found")))
         (flutter-project-string (concat "[Flutter project] " (if (lsp-dart-flutter-project-p) "true" "false")))
         (project-entrypoint-string (concat "[Project entrypoint] " (or (lsp-dart-get-project-entrypoint) "Not found"))))
    (lsp-dart-log (string-join (list lsp-dart-string
                                     dart-sdk-string
                                     flutter-sdk-dir-string
                                     flutter-project-string
                                     project-entrypoint-string)
                               "\n"))))


;;;###autoload(with-eval-after-load 'lsp-mode (require 'lsp-dart))

(provide 'lsp-dart)

;;; lsp-dart.el ends here

;; Local Variables:
;; End: