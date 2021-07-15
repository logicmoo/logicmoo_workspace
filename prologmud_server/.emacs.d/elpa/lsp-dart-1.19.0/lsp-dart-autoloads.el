;;; lsp-dart-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "lsp-dart" "lsp-dart.el" (0 0 0 0))
;;; Generated autoloads from lsp-dart.el

(autoload 'lsp-dart-version "lsp-dart" "\
Get the lsp-dart version as string.

The returned string includes the version from main file header,
 the current time and the Emacs version.

If the version number could not be determined, signal an error." t nil)
(with-eval-after-load 'lsp-mode (require 'lsp-dart))

(register-definition-prefixes "lsp-dart" '("lsp-dart-"))

;;;***

;;;### (autoloads nil "lsp-dart-closing-labels" "lsp-dart-closing-labels.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from lsp-dart-closing-labels.el

(register-definition-prefixes "lsp-dart-closing-labels" '("lsp-dart-closing-labels"))

;;;***

;;;### (autoloads nil "lsp-dart-code-lens" "lsp-dart-code-lens.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from lsp-dart-code-lens.el

(register-definition-prefixes "lsp-dart-code-lens" '("lsp-dart-"))

;;;***

;;;### (autoloads nil "lsp-dart-commands" "lsp-dart-commands.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from lsp-dart-commands.el

(autoload 'lsp-dart-pub-get "lsp-dart-commands" "\
Run pub get on a Dart or Flutter project.
If it is Flutter project, run `flutter pub get` otherwise run
`pub get`." t nil)

(autoload 'lsp-dart-pub-upgrade "lsp-dart-commands" "\
Run pub upgrade on a Dart or Flutter project.
If it is Flutter project, run `flutter pub upgrade` otherwise run
`pub upgrade`." t nil)

(autoload 'lsp-dart-pub-outdated "lsp-dart-commands" "\
Run pub outdated on a Dart or Flutter project.
If it is Flutter project, run `flutter pub outdated` otherwise run
`pub outdated`." t nil)

(register-definition-prefixes "lsp-dart-commands" '("lsp-dart-"))

;;;***

;;;### (autoloads nil "lsp-dart-dap" "lsp-dart-dap.el" (0 0 0 0))
;;; Generated autoloads from lsp-dart-dap.el

(register-definition-prefixes "lsp-dart-dap" '("lsp-dart-dap-"))

;;;***

;;;### (autoloads nil "lsp-dart-devtools" "lsp-dart-devtools.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from lsp-dart-devtools.el

(autoload 'lsp-dart-open-devtools "lsp-dart-devtools" "\
Open Dart DevTools for the current debug session." t nil)

(register-definition-prefixes "lsp-dart-devtools" '("lsp-dart-devtools-"))

;;;***

;;;### (autoloads nil "lsp-dart-flutter-colors" "lsp-dart-flutter-colors.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from lsp-dart-flutter-colors.el

(register-definition-prefixes "lsp-dart-flutter-colors" '("lsp-dart-flutter-colors"))

;;;***

;;;### (autoloads nil "lsp-dart-flutter-daemon" "lsp-dart-flutter-daemon.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from lsp-dart-flutter-daemon.el

(autoload 'lsp-dart-flutter-daemon-mode "lsp-dart-flutter-daemon" "\
Major mode for `lsp-dart-flutter-daemon-start`.

\(fn)" t nil)

(register-definition-prefixes "lsp-dart-flutter-daemon" '("lsp-dart-flutter-daemon-"))

;;;***

;;;### (autoloads nil "lsp-dart-flutter-fringe-colors" "lsp-dart-flutter-fringe-colors.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from lsp-dart-flutter-fringe-colors.el

(register-definition-prefixes "lsp-dart-flutter-fringe-colors" '("lsp-dart-flutter-fringe-"))

;;;***

;;;### (autoloads nil "lsp-dart-flutter-widget-guide" "lsp-dart-flutter-widget-guide.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from lsp-dart-flutter-widget-guide.el

(register-definition-prefixes "lsp-dart-flutter-widget-guide" '("lsp-dart-flutter-widget-guide"))

;;;***

;;;### (autoloads nil "lsp-dart-outline" "lsp-dart-outline.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from lsp-dart-outline.el

(autoload 'lsp-dart-show-outline "lsp-dart-outline" "\
Show an outline tree and focus on it if IGNORE-FOCUS? is nil.

\(fn IGNORE-FOCUS\\=\\?)" t nil)

(autoload 'lsp-dart-show-flutter-outline "lsp-dart-outline" "\
Show a Flutter outline tree and focus on it if IGNORE-FOCUS? is nil.

\(fn IGNORE-FOCUS\\=\\?)" t nil)

(register-definition-prefixes "lsp-dart-outline" '("lsp-dart-"))

;;;***

;;;### (autoloads nil "lsp-dart-test-output" "lsp-dart-test-output.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from lsp-dart-test-output.el

(register-definition-prefixes "lsp-dart-test-output" '("lsp-dart-test-"))

;;;***

;;;### (autoloads nil "lsp-dart-test-support" "lsp-dart-test-support.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from lsp-dart-test-support.el

(autoload 'lsp-dart-run-test-at-point "lsp-dart-test-support" "\
Run test at point." t nil)

(autoload 'lsp-dart-debug-test-at-point "lsp-dart-test-support" "\
Debug test at point." t nil)

(autoload 'lsp-dart-run-test-file "lsp-dart-test-support" "\
Run Dart/Flutter test command only for current buffer." t nil)

(autoload 'lsp-dart-run-all-tests "lsp-dart-test-support" "\
Run each test from project." t nil)

(autoload 'lsp-dart-visit-last-test "lsp-dart-test-support" "\
Visit the last ran test going to test definition." t nil)

(autoload 'lsp-dart-run-last-test "lsp-dart-test-support" "\
Run the last ran test." t nil)

(autoload 'lsp-dart-debug-last-test "lsp-dart-test-support" "\
Debug the last ran test." t nil)

(autoload 'lsp-dart-test-process-mode "lsp-dart-test-support" "\
Major mode for dart tests process.

\(fn)" t nil)

(register-definition-prefixes "lsp-dart-test-support" '("lsp-dart-test-"))

;;;***

;;;### (autoloads nil "lsp-dart-test-tree" "lsp-dart-test-tree.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from lsp-dart-test-tree.el

(register-definition-prefixes "lsp-dart-test-tree" '("lsp-dart-"))

;;;***

;;;### (autoloads nil "lsp-dart-utils" "lsp-dart-utils.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from lsp-dart-utils.el

(register-definition-prefixes "lsp-dart-utils" '("lsp-dart-"))

;;;***

;;;### (autoloads nil nil ("lsp-dart-pkg.el" "lsp-dart-protocol.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; lsp-dart-autoloads.el ends here
