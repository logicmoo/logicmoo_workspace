;;; phps-mode-test-integration.el --- Tests for integration -*- lexical-binding: t -*-

;; Copyright (C) 2017-2021  Free Software Foundation, Inc.

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; Run from terminal make test-integration


;;; Code:

(require 'ert)
(require 'phps-mode)
(require 'phps-mode-test)

(eval-when-compile
  (require 'phps-mode-macros))

(defun phps-mode-test-integration--incremental-vs-initial-buffers ()
  "Test for object-oriented PHP file."

  (phps-mode-test--incremental-vs-intial-buffer
   "<?php\nnamespace myNamespace\n{\n    class myClass\n    {\n        public function myFunction()\n        {\n            echo 'my statement';\n        }\n    }\n}\n"
   "Integration-test 1 for regular PHP with namespaces, classes and functions"

   ;; Make changes - insert a new function
   (goto-char 145)
   (insert "\n\n        public function myFunctionB()\n        {\n            echo 'my second statement';\n        }\n"))

  (phps-mode-test--incremental-vs-intial-buffer
   "<?php\nnamespace myNamespace\n{\n    class myClass\n    {\n        public function myFunction()\n        {\n            echo 'my statement';\n        }\n    }\n}\n"
   "Integration-test 2 for regular PHP with namespaces, classes and functions"

   ;; Make changes - insert a new function
   (goto-char 145)
   (insert "\n\n        public function myFunctionB()\n        {\n            echo 'my second statement';\n        }\n")

   ;; Make changes - remove first function
   (goto-char 55)
   (push-mark nil t t)
   (goto-char 145)
   (execute-kbd-macro (kbd "<backspace>")))

  (phps-mode-test--incremental-vs-intial-buffer
   ""
   "Integration-test 3 for function-oriented PHP"

   ;; Make changes
   (goto-char 1)
   (insert "<?php\nfunction myFunctionA()\n{\n    echo 'my second statement';\n}\n"))

  (phps-mode-test--incremental-vs-intial-buffer
   "<?php\nnamespace myNamespace\n{\n    class myClass\n    {\n        public function myFunction()\n        {\n            echo 'my statement';\n        }\n    }\n}\n"
   "Integration-test 4 for regular PHP with namespaces, classes and functions, minor insert"

   ;; Make changes
   (goto-char 132)
   (insert " is a complex one"))

  (phps-mode-test--incremental-vs-intial-buffer
   "<?php\nnamespace myNamespace\n{\n    class myClass\n    {\n        public function myFunction()\n        {\n            echo 'my statement';\n        }\n    }\n}\n"
   "Integration-test 5 for regular PHP with namespaces, classes and functions, single deletion"

   ;; Make changes - insert a echo
   (goto-char 132)
   (backward-delete-char-untabify 1))

  (phps-mode-test--incremental-vs-intial-buffer
   "<?php\nnamespace myNamespace\n{\n    class myClass\n    {\n        public function myFunction()\n        {\n        echo 'my statement';\n        }\n    }\n}\n"
   "Integration-test 6 for regular PHP with namespaces, classes and functions, single indent line"

   ;; Make changes
   (goto-char 110)
   (insert "    "))

  (phps-mode-test--incremental-vs-intial-buffer
   "<?php\nnamespace myNamespace\n{\n    class myClass\n    {\n        public function myFunction()\n        {\n        echo 'my statement';\n        }\n    }\n}\n"
   "Integration-test 7 for regular PHP with namespaces, classes and functions, with multiple, isolated edits"

   ;; Make changes
   (goto-char 110)
   (insert "    ")

   ;; Make changes
   (goto-char 28)
   (insert "One"))

  (phps-mode-test--incremental-vs-intial-buffer
   "<?php\nif ($myCondition) {}\n"
   "Integration-test 8 for regular PHP with newline between curly brackets"

   ;; Make changes
   (goto-char 26)
   (execute-kbd-macro (kbd "RET")))

  (phps-mode-test--incremental-vs-intial-buffer
   "<?php\n/**\n * @see Something\n * @see here\n *\n */\n"
   "Integration-test 9 for regular PHP with newline in doc comment block"

   ;; Make changes
   (goto-char 41)
   (execute-kbd-macro (kbd "RET")))

  (phps-mode-test--incremental-vs-intial-buffer
   "<?php\necho 'my comment';\n"
   "Integration-test 10 insert code at end of buffer"

   ;; Make changes
   (goto-char (point-max))

   (insert "\necho 'my comments';\n"))

  (phps-mode-test--incremental-vs-intial-buffer
   ""
   "Integration-test 11 insert code in empty buffer using macro, use several passes"

   ;; Make changes - Pass 1
   (goto-char (point-max))
   (execute-kbd-macro "<?php")
   (execute-kbd-macro (kbd "RET"))
   (execute-kbd-macro "echo 'was here';")
   (execute-kbd-macro (kbd "RET"))
   (execute-kbd-macro (kbd "RET"))
   (phps-mode-lex-analyzer--process-changes)

   ;; Pass 2
   (execute-kbd-macro "if ($myCondition) {")
   (execute-kbd-macro (kbd "RET"))
   (execute-kbd-macro "echo 'my special condition';")
   (phps-mode-lex-analyzer--process-changes)

   ;; Pass 3
   (execute-kbd-macro (kbd "TAB"))
   (execute-kbd-macro (kbd "RET")))

  (phps-mode-test--incremental-vs-intial-buffer
   "<?php\n$str = <<<EOD\nExample of string\nspanning multiple lines\nusing heredoc syntax.\nEOD;\n\n/* More complex example, with variables. */\nclass foo\n{\n    var $foo;\n    var $bar;\n\n    function __construct()\n    {\n        $this->foo = 'Foo';\n        $this->bar = array('Bar1', 'Bar2', 'Bar3');\n    }\n}\n\n$foo = new foo();\n$name = 'MyName';\n\necho <<<EOT\nMy name is \"$name\". I am printing some $foo->foo.\nNow, I am printing some {$foo->bar[1]}.\nThis should print a capital 'A': \x41\nEOT;\n?>\n"
   "Integration-test 12 complex HEREDOC adding new line with variable in it"
   (goto-char 63)
   (insert "inserting $variable "))

  (phps-mode-test--incremental-vs-intial-buffer
   "<?php\n$str = <<<EOD\nExample of string\nspanning multiple lines\nusing heredoc syntax.\nEOD;\n\n/* More complex example, with variables. */\nclass foo\n{\n    var $foo;\n    var $bar;\n\n    function __construct()\n    {\n        $this->foo = 'Foo';\n        $this->bar = array('Bar1', 'Bar2', 'Bar3');\n    }\n}\n\n$foo = new foo();\n$name = 'MyName';\n\necho <<<EOT\nMy name is \"$name\". I am printing some $foo->foo.\nNow, I am printing some {$foo->bar[1]}.\nThis should print a capital 'A': \x41\nEOT;\n?>\n"
   "Integration-test 13 complex HEREDOC with removed heredoc delimiter"
   (goto-char 85)
   (kill-line))

  (phps-mode-test--incremental-vs-intial-buffer
   "<?php\n$str = <<<EOD\nExample of string\nspanning multiple lines\nusing heredoc syntax.\nEOD;\n\n/* More complex example, with variables. */\nclass foo\n{\n    var $foo;\n    var $bar;\n\n    function __construct()\n    {\n        $this->foo = 'Foo';\n        $this->bar = array('Bar1', 'Bar2', 'Bar3');\n    }\n}\n\n$foo = new foo();\n$name = 'MyName';\n\necho <<<EOT\nMy name is \"$name\". I am printing some $foo->foo.\nNow, I am printing some {$foo->bar[1]}.\nThis should print a capital 'A': \x41\nEOT;\n?>\n"
   "Integration-test 14 complex HEREDOC with removed heredoc delimiter semicolon"
   ;; (message "\nTokens: %s" phps-mode-lex-analyzer--tokens)
   ;; (message "States: %s\n" phps-mode-lex-analyzer--states)
   (goto-char 88)
   (delete-char 1))

  (phps-mode-test--incremental-vs-intial-buffer
   "<?php\n\nif (\n    true\n    && false\n) {\n    echo 'My long string here';\n    if (\nfalse\n        || true\n    ) {\n        echo 'Two lines of'\n            . 'strings here';\n    }\n}"
   "Integration-test 15 white-space changes to see if nesting is maintained."
   (goto-char 80)
   (execute-kbd-macro (kbd "<tab>")))

  (phps-mode-test--incremental-vs-intial-buffer
   "<?php\n\nif (\n    true\n    && false\n) {\n    echo 'My long string here';\n    if (\nfalse\n        || true\n    ) {\n        echo 'Two lines of'\n            . 'strings here';\n    }\n}"
   "Integration-test 16 white-space changes to see if nesting is maintained."
   (goto-char 80)
   (execute-kbd-macro (kbd "<tab>"))
   (goto-char 117)
   (execute-kbd-macro (kbd "<return>")))

  )

(defun phps-mode-test-integration ()
  "Run test for integration."
  ;; (setq debug-on-error t)
  (phps-mode-test-integration--incremental-vs-initial-buffers)
  )

(phps-mode-test-integration)

(provide 'phps-mode-test-integration)

;;; phps-mode-test-integration.el ends here
