;;; wcheck-mode.el --- General interface for text checkers  -*- lexical-binding: t -*-

;; Copyright (C) 2009-2020  Free Software Foundation, Inc.

;; Author: Teemu Likonen <tlikonen@iki.fi>
;; Maintainer: Teemu Likonen <tlikonen@iki.fi>
;; Created: 2009-07-04
;; URL: https://github.com/tlikonen/wcheck-mode
;; Keywords: text spell check languages ispell
;; Version: 2020.10.4

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; The license text: <http://www.gnu.org/licenses/gpl-3.0.html>


;; INSTALLATION
;;
;; Put this file to some directory in your Emacs's "load-path" and add
;; the following lines to Emacs's initialization file (~/.emacs):
;;
;;     (autoload 'wcheck-mode "wcheck-mode"
;;       "Toggle wcheck-mode." t)
;;     (autoload 'wcheck-change-language "wcheck-mode"
;;       "Switch wcheck-mode languages." t)
;;     (autoload 'wcheck-actions "wcheck-mode"
;;       "Open actions menu." t)
;;     (autoload 'wcheck-jump-forward "wcheck-mode"
;;       "Move point forward to next marked text area." t)
;;     (autoload 'wcheck-jump-backward "wcheck-mode"
;;       "Move point backward to previous marked text area." t)
;;
;; See customize group "wcheck" for information on how to configure
;; Wcheck mode. (M-x customize-group RET wcheck RET)


;;; Commentary:
;;
;; A general interface for text checkers
;;
;; Wcheck mode is a general-purpose text-checker interface for Emacs
;; text editor.  Wcheck mode a minor mode which provides an on-the-fly
;; text checker.  It checks the visible text area, as you type, and
;; possibly highlights some parts of it.  What is checked and how are all
;; configurable.
;;
;; Wcheck mode can use external programs or Emacs Lisp functions for
;; checking text.  For example, Wcheck mode can be used with
;; spell-checker programs such as Ispell, Enchant and Hunspell, but
;; actually any tool that can receive text from standard input stream
;; and send text to standard output can be used.  Wcheck mode sends parts
;; of buffer's content to an external program or an Emacs Lisp function
;; and, relying on their output, decides if some parts of text should be
;; marked in the buffer.

;;; Code:


(eval-when-compile
  ;; Silence compiler
  (declare-function outline-show-entry "outline"))


;;; Settings


;;;###autoload
(defgroup wcheck nil
  "General interface for text checkers."
  :group 'applications)


(defconst wcheck--language-data-customize-interface
  '(choice
    :format "%[Option%] %v"

    (cons :tag "Program" :format "%v"
          (const :tag "Program" :format "%t: " program)
          (choice :format "%[Type%] %v"
                  (file :tag "Filename" :format "\n\t\t%t: %v")
                  (function :tag "Function" :format "\n\t\t%t: %v")))

    (cons :tag "Arguments" :format "%v"
          (const :format "" args)
          (repeat :tag "Arguments"
                  (string :format "%v")))

    (cons :tag "Output parser function" :format "%v"
          (const :tag "Output parser" :format "%t: " parser)
          (choice :format "%[Parser%] %v" :value nil
                  (const :tag "Lines" wcheck-parser-lines)
                  (const :tag "Whitespace" wcheck-parser-whitespace)
                  (function :tag "Custom function"
                            :format "%t:\n\t\t%v")))

    (cons :tag "Connection type" :format "%v"
          (const :tag "Connection: " :format "%t" connection)
          (choice :format "%[Type%] %v" :value nil
                  (const :tag "pipe (nil)" nil)
                  (const :tag "pty" :match (lambda (widget value)
                                             (or (eq value t)
                                                 (eq value 'pty)))
                         pty)))

    (cons :tag "Face" :format "%v"
          (const :tag "Face" :format "%t: " face)
          (symbol :format "%v"))

    (cons :tag "Syntax table" :format "%v"
          (const :tag "Syntax table" :format "%t: " syntax)
          (variable :format "%v"))

    (cons :tag "Regexp start" :format "%v"
          (const :tag "Regexp start" :format "%t: " regexp-start)
          (regexp :format "%v"))

    (cons :tag "Regexp body" :format "%v"
          (const :tag "Regexp body" :format "%t: " regexp-body)
          (regexp :format "%v"))

    (cons :tag "Regexp end" :format "%v"
          (const :tag "Regexp end" :format "%t: " regexp-end)
          (regexp :format "%v"))

    (cons :tag "Regexp discard" :format "%v"
          (const :tag "Regexp discard" :format "%t: " regexp-discard)
          (regexp :format "%v"))

    (cons :tag "Regexp case" :format "%v"
          (const :tag "Regexp" :format "%t: " case-fold)
          (choice :format "%[Case%] %v" :value nil
                  :match (lambda (widget value) t)
                  :value-to-internal (lambda (widget value)
                                       (if value t nil))
                  (const :tag "sensitive" nil)
                  (const :tag "insensitive" t)))

    (cons
     :tag "Read or skip faces" :format "%v"
     (const :tag "Read or skip faces" :format "%t" read-or-skip-faces)
     (repeat
      :tag ""
      (cons :format "%v"

            (choice :format "%[Major mode%] %v"
                    (const :tag "All major modes"
                           :match (lambda (widget value) (null value))
                           nil)
                    (repeat
                     :tag "Select major modes"
                     :match (lambda (widget value)
                              (or (symbolp value) (consp value)))
                     :value-to-internal (lambda (widget value)
                                          (if (symbolp value)
                                              (list value)
                                            value))
                     :value-to-external (lambda (widget value)
                                          (if (and (consp value)
                                                   (symbolp (car value))
                                                   (null (cdr value)))
                                              (car value)
                                            value))
                     (symbol :format "%v")))

            (choice :format "%[Operation mode%] %v"
                    (const :tag "Read everything" nil)
                    (cons :tag "Read selected faces" :format "%v"
                          (const :tag "Read selected faces"
                                 :format "%t" read)
                          (repeat :tag "" (sexp :format "%v")))
                    (cons :tag "Skip selected faces" :format "%v"
                          (const :tag "Skip selected faces"
                                 :format "%t" skip)
                          (repeat :tag "" (sexp :format "%v")))))))

    (cons :tag "Action program" :format "%v"
          (const :tag "Action program" :format "%t: " action-program)
          (choice :format "%[Type%] %v"
                  (file :tag "Filename" :format "\n\t\t%t: %v")
                  (function :tag "Function" :format "\n\t\t%t: %v")))

    (cons :tag "Action program's arguments" :format "%v"
          (const :format "" action-args)
          (repeat :tag "Action program's arguments"
                  (string :format "%v")))

    (cons :tag "Action parser function" :format "%v"
          (const :tag "Action parser" :format "%t: "
                 action-parser)
          (choice :format "%[Parser%] %v" :value nil
                  (const :tag "Ispell" wcheck-parser-ispell-suggestions)
                  (const :tag "Lines" wcheck-parser-lines)
                  (const :tag "Whitespace" wcheck-parser-whitespace)
                  (function :tag "Custom function"
                            :format "%t:\n\t\t%v")))

    (cons :tag "Action autoselect mode" :format "%v"
          (const :tag "Action autoselect" :format "%t: " action-autoselect)
          (choice :format "%[Mode%] %v" :value nil
                  :match (lambda (widget value) t)
                  :value-to-internal (lambda (widget value)
                                       (if value t nil))
                  (const :tag "off" nil)
                  (const :tag "on" t)))))


;;;###autoload
(defcustom wcheck-language-data
  ;; FIXME: Auto-fill by looking at installed spell-checkers and dictionaries!
  nil
  "Language configuration for `wcheck-mode'.

The variable is an association list (alist) and its elements are
of the form:

    (LANGUAGE (KEY . VALUE) [(KEY . VALUE) ...])

LANGUAGE is a name string for this particular configuration unit
and KEY and VALUE pairs denote settings for the language.

Below is the documentation of possible KEYs and corresponding
VALUEs. The documentation is divided in two parts: checker
options and action options. The first part describes all options
related to checking the content of an Emacs buffer (and possibly
marking some of it). The second part describes options which
configure actions which user can choose for a marked text on
buffer.

NOTE: There is also variable `wcheck-language-data-defaults'
which is used to define default values. The defaults are used
when a language-specific option in `wcheck-language-data' does
not exist or is not valid.


Checker options
---------------

The checker options configure LANGUAGE's text-checking and
text-marking features. With these you can configure how buffer's
content is examined, what checker engine is used and how text is
marked in the buffer.

program
args
    `program' is either the name (a string) of an external
    executable program or an Emacs Lisp function (a symbol or a
    lambda expression). They are used as the checker engine for
    the LANGUAGE. When `program' names an external executable
    program then `args' are the command-line arguments (a list of
    strings) for the program.

    `wcheck-mode' collects text strings from the buffer and sends
    them to `program' to analyze. When `program' is an external
    executable program the collected strings are sent (each on a
    separate line) through the standard input stream to the
    program. The program must write to standard output stream all
    the strings which it thinks should be marked in the Emacs
    buffer. The output of the program is then parsed with
    `parser' function (see below).

    When `program' is an Emacs Lisp function (a symbol or a
    lambda expression) the function is called with one argument:
    a list of strings collected from the buffer. The function is
    supposed to check them and return a list of strings (or nil).
    The returned strings will be marked in the buffer.

    See options `regexp-start', `regexp-body' and `regexp-end'
    below for details on how text is collected from the buffer.

parser
    VALUE of this option is an Emacs Lisp function which is
    responsible for parsing the output of `program'. This parser
    function is only used when `program' is an external
    executable program (not a function).

    The parser function is run without arguments and within the
    context of a buffer that contains all the output from the
    external program. The point is located at the beginning of
    the buffer. From that buffer the `parser' function should
    collect all the strings that are meant to be marked in the
    buffer that is being checked. The function must return them
    as a list of strings or nil if there are none to be marked.

    For the most common cases there are two parser functions
    already implemented:

        `wcheck-parser-lines' turns each line in program's output
        to a separate string. You should use this function as the
        output parser if you spell-check with Ispell-like program
        with its \"-l\" command-line option. They output each
        misspelled word on a separate line. This is the default
        output parser.

        `wcheck-parser-whitespace' turns each whitespace-
        separated token in the output to a separate string.

connection
    The VALUE is used to set variable `process-connection-type'
    when starting the process for LANGUAGE. If the VALUE is nil
    use a pipe for communication; if it's `pty' (or t) use a PTY.
    The default is to use a pipe (nil). (This option is ignored
    when the program is a function.)

face
    A symbol referring to the face which is used to mark text with
    this LANGUAGE. The default is `wcheck-default-face'.

syntax
    VALUE is a variable (a symbol) referring to an Emacs syntax
    table. This option temporarily sets the effective syntax
    table when buffer's content is scanned with `regexp-start',
    `regexp-body', `regexp-end' and `regexp-discard' (see below)
    as well as when `program', `parser', `action-program' and
    `action-parser' functions are called. The default value is
    `text-mode-syntax-table'. This option does not affect syntax
    table settings anywhere else. See the Info node
    `(elisp)Syntax Tables' for more information on the topic.

regexp-start
regexp-body
regexp-end
    Regular expression strings which match the start of a string
    body, characters within the body and the end of the body,
    respectively.

    This is how they are used in practice: `wcheck-mode' scans
    buffer's content and looks for strings that match the
    following regular expression

        REGEXP-START\\(REGEXP-BODY\\)REGEXP-END

    The regular expression back reference \\1 is used to extract
    `regexp-body' part from the matched string. That string is
    then matched against `regexp-discard' (see below) and if it
    doesn't match the string is sent to the text checker program
    or function to analyze.

    Strings returned from the program or function are quoted for
    regular expression special characters (with `regexp-quote'
    function) and marked in Emacs buffer using the following
    construction: `regexp-start + STRING + regexp-end'. The
    STRING part is marked with `face' (see above).

    You can't use grouping constructs `\\( ... \\)' in
    `regexp-start' because the back reference `\\1' is used for
    separating the `regexp-body' match string from the
    `regexp-start' and `regexp-end' match strings. You can use
    \"shy\" groups `\\(?: ... \\)' which do not record the
    matched substring. Grouping constructs `\\( ... \\)' are
    allowed in `regexp-body' and `regexp-end'. Just note that the
    first group and back reference \\1 is already taken.

    The default values for the regular expressions are

        \\=\\<\\='*         (regexp-start)
        \\w+?         (regexp-body)
        \\='*\\=\\>         (regexp-end)

    Effectively they match a series of word characters defined in
    the effective syntax table. Single quotes (\\=') at the start
    and end of a word are excluded. This is probably a good thing
    when using `wcheck-mode' as a spelling checker.

regexp-discard
    The string that matched `regexp-body' is then matched against
    the value of this option. If this regular expression matches,
    then the string is discarded and won't be sent to the
    text-checker program or function to analyze. You can use this
    to define exceptions to the `regexp-body' match. The default
    value is

        \\\\=`\\='+\\\\='

    which discards the body string if it consists only of single
    quotes. This was chosen as the default because the default
    syntax table `text-mode-syntax-table' defines single quote as
    a word character. It's probably not useful to mark individual
    single quotes in a buffer when `wcheck-mode' is used as a
    spelling checker.

    If you don't want to have any discarding rules set this
    option to empty string (\"\").

case-fold
    This boolean value is used to temporarily bind the value of
    variable `case-fold-search'. The nil value means
    case-sensitive and a non-nil means case-insensitive search.
    The default is case-sensitive (nil). This option is effective
    with `regexp-start', `regexp-body', `regexp-end' and
    `regexp-discard' as well as when `program', `parser',
    `action-program' and `action-parser' functions are called.

read-or-skip-faces
    This option controls which faces `wcheck-mode' should read or
    skip when scanning buffer's content. The value must be a list
    and its items are also lists:

        (MAJOR-MODE [OPERATION-MODE [FACE ...]])

    MAJOR-MODE is a symbol or a list of symbols. Symbols refer to
    the major mode(s) which the settings are for. Use nil as the
    MAJOR-MODE to define default settings. Settings that come
    after the pseudo major-mode nil are ignored.

    OPERATION-MODE is symbol `read' or `skip' defining whether
    the FACEs should be read or skipped. If it's `read' then only
    the listed faces are read. If it's `skip' then the listed
    faces are skipped and all other faces are read. If there is
    no OPERATION-MODE at all (i.e., the list has just one
    element, MAJOR-MODE) then everything is read.

    The rest of the items are FACEs. They are typically symbols
    but some Emacs modes may use strings, property lists or cons
    cells for defining faces. For more information see Info
    node `(elisp) Special Properties'. Use nil as the face to
    refer to the normal text which does not have a face text
    property.

    Example:

        (read-or-skip-faces
         ((emacs-lisp-mode c-mode) read
          font-lock-comment-face font-lock-doc-face)
         (org-mode skip font-lock-comment-face org-link)
         (text-mode)
         (nil read nil))

    It says that in `emacs-lisp-mode' and `c-mode' only the text
    which have been highlighted with `font-lock-comment-face' or
    `font-lock-doc-face' is read (i.e., checked). In `org-mode'
    faces `font-lock-comment-face' and `org-link' are
    skipped (i.e., not checked) and all other faces are read. In
    `text-mode' everything is read. Finally, in all other major
    modes only the normal text (nil) is read.

    Most likely not all `read-or-skip-faces' settings are
    specific to a certain language so it could be more useful to
    put them in variable `wcheck-language-data-defaults' instead.
    That way they are used with all languages. Normally the
    global default is equivalent to

        (read-or-skip-faces
         (nil))

    which means that in all major modes read everything. It is
    sometimes useful to have this setting in language-specific
    options because the parsing stops right there. Therefore it
    overrides all global settings which user may have changed
    with variable `wcheck-language-data-defaults'.

    Note: You can use command `\\[what-cursor-position]' with a
    prefix argument to see what faces are active at the cursor
    position. Then you can use the information to configure this
    option.


Action options
--------------

\"Actions\" are any kind of operations that can be executed for
marked text in an Emacs buffer. Actions are presented to user
through a menu which is activated either by (1) clicking the
right mouse button on a marked text or (2) executing interactive
command `wcheck-actions' while the cursor (the point) is on a
marked text.

If you use `wcheck-mode' as a spelling checker then it's natural
to configure an action menu that offers spelling suggestions for
the misspelled word. The action menu could also have an option to
add the word to spell-checker's dictionary, so that the word is
recognized next time.

action-program
action-args
    `action-program' is either the name (a string) of an external
    executable program or an Emacs Lisp function (a symbol or a
    lambda expression). When it's the name of an executable
    program then `action-args' are the command-line arguments (a
    list of strings) for the program.

    When `action-program' is an external executable program the
    marked text is sent to the program through the standard input
    stream. The program should send its feedback data (usually
    suggested substitute strings) to the standard output stream.
    The output is parsed with `action-parser' function (see
    below) and function's return value is used to construct an
    action menu for user. The format and effect of
    `action-parser' function's return value is described below.

    When `action-program' is an Emacs Lisp function the function
    is called with one argument: a vector returned by
    `wcheck-marked-text-at' function. The `action-program'
    function is supposed to gather some substitute suggestion
    strings or give other actions for the marked text in the
    buffer. Function's return value is used to construct an
    action menu for user. The format and effect of
    `action-program' function's return value is described below.

action-parser
    VALUE of this option is an Emacs Lisp function which is
    responsible for parsing the output of `action-program'. This
    parser function is only used when `action-program' is an
    external executable program (not a function).

    The parser function is run with one argument: a vector
    returned by `wcheck-marked-text-at' for the marked text in
    question. The parser function is called within the context of
    a buffer that contains all the output from `action-program'.
    The point is located at the beginning of the buffer.

    The `action-parser' function should examine the buffer for
    interesting information (such as spelling suggestions) and
    return them in the format that is described below.

    For the most common cases there are three parser functions
    already implemented:

        `wcheck-parser-ispell-suggestions' parses substitute
        suggestions from the output of Ispell or compatible
        program, such as Enchant. Use this function as the
        `action-parser' if you get spelling suggestions from an
        Ispell-like program with its \"-a\" command-line option.

        `wcheck-parser-lines' function turns each line in the
        output to individual substitute suggestions.

        `wcheck-parser-whitespace'. Each whitespace-separated
        token in the program's output is a separate suggestion.

action-autoselect
    If this option is non-nil and the action menu has only one
    menu item then the item is chosen automatically without
    actually showing the menu. If this option is nil (which is
    the default) then the menu is always shown.


The return value of `action-program' function and `action-parser'
function must be a list. The empty list (nil) means that there
are no actions available for the marked text. Otherwise each
elements in the list must be either a string or a cons cell. If
an element is a string it is an individual substitute suggestion
string for the original marked text. The same string is shown in
the actions menu. When user chooses such option from the action
menu the original text is substituted in the Emacs buffer.

If an element is a cons cell it must be one of

    (\"Menu item\" . \"substitute string\")
    (\"Menu item\" . some-function)

The \"car\" value of the cons cell must be a string. The string
is shown in the action menu as one of the options. The \"cdr\"
value of the cons cell defines the action that is taken for the
menu option. If the \"cdr\" value is a string then that string is
the substitute string. If the \"cdr\" value is a function (a
symbol or a lambda expression) then that function is called when
user chooses the menu option. The function is called with one
argument: a vector returned by `wcheck-marked-text-at' function
for the marked text in question.

Effectively `action-program' function or `action-program'
executable program with `action-parser' function provide a
feature that can offer spelling suggestions for user: just return
suggestions as a list of strings. Alternatively they can offer
any kind of useful actions by calling custom functions. There are
a lot of possibilities.


For configuration examples, see the README file in URL
`https://github.com/tlikonen/wcheck-mode'."

  :group 'wcheck
  :type
  `(repeat
    (list :format "%v"
          (string :tag "Language")
          (repeat :inline t
                  :tag "Options"
                  ,wcheck--language-data-customize-interface))))


;;;###autoload
(defconst wcheck--language-data-defaults-hard-coded
  '((parser . wcheck-parser-lines)
    (connection . nil)
    (face . wcheck-default-face)
    (syntax . text-mode-syntax-table)
    (regexp-start . "\\<'*")
    (regexp-body . "\\w+?")
    (regexp-end . "'*\\>")
    (regexp-discard . "\\`'+\\'")
    (case-fold . nil)
    (read-or-skip-faces (nil))
    (action-autoselect . nil))
  "Hard-coded default language configuration for `wcheck-mode'.
This constant is for Wcheck mode's internal use only. This
provides useful defaults if both `wcheck-language-data' and
`wcheck-language-data-defaults' fail.")


;;;###autoload
(defcustom wcheck-language-data-defaults
  wcheck--language-data-defaults-hard-coded
  "Default language configuration for `wcheck-mode'.
These default values are used when language-specific settings
don't provide a valid value. `wcheck-mode' will choose some
useful defaults even if this variable is not (properly) set. See
variable `wcheck-language-data' for information about possible
settings.

Here's an example value for the variable:

    ((parser . wcheck-parser-lines)
     (action-parser . wcheck-parser-ispell-suggestions)
     (connection . nil)
     (face . wcheck-default-face)
     (syntax . text-mode-syntax-table)
     (regexp-start . \"\\\\=\\<\\='*\")
     (regexp-body . \"\\\\w+?\")
     (regexp-end . \"\\='*\\\\=\\>\")
     (regexp-discard . \"\\\\\\=`\\='+\\\\\\='\")
     (case-fold . nil)
     (read-or-skip-faces
      ((emacs-lisp-mode c-mode) read
       font-lock-comment-face font-lock-doc-face)
      (message-mode read nil
       message-header-subject message-cited-text)))"

  :group 'wcheck
  :type `(repeat ,wcheck--language-data-customize-interface))


;;;###autoload
(defcustom wcheck-language ""
  "Default language for `wcheck-mode'.

Normally the global value defines the language for new buffers.
If a buffer-local value exists it is used instead. This variable
becomes automatically buffer-local when `wcheck-mode' is turned
on in a buffer, so changing the global value does not affect
buffers which already have `wcheck-mode' turned on.

User is free to set this variable directly (e.g., in programs)
but in interactive use it is usually better to use the command
`\\[wcheck-change-language]' instead. The command can change
language immediately while `wcheck-mode' is turned on, whereas
changing just the value of this variable takes effect only when
`wcheck-mode' is turned on next time."
  :type '(string :tag "Default language")
  :group 'wcheck)
(make-variable-buffer-local 'wcheck-language)


;;;###autoload
(defface wcheck-default-face
  '((t (:underline "red")))
  "Default face for marking strings in a buffer.
This is used when language does not define a face."
  :group 'wcheck)


;;; Variables


(defvar wcheck-mode nil)
(defvar wcheck-mode-map (make-sparse-keymap)
  "Keymap for `wcheck-mode'.")

(defvar wcheck--timer nil)
(defvar wcheck--timer-idle .3
  "`wcheck-mode' idle timer delay (in seconds).")
(defvar wcheck--timer-paint-event-count 0)

(defvar wcheck--timer-paint-event-count-std 3
  "Run buffer paint event this many times in a row.
With too low values all data from external processes may not have
arrived and window gets only partially painted. A higher value
increases the probability that windows get fully painted but it
also makes `wcheck-jump-forward' and `wcheck-jump-backward'
slower. A suitable compromise may be 3 or 4.")

(defvar wcheck--change-language-history nil
  "Language history for command `wcheck-change-language'.")

(defvar wcheck--buffer-data nil)

(defvar wcheck--jump-step 5000)


;;; Macros


(defmacro wcheck--define-condition (name superclass &optional message)
  (declare (indent defun))
  `(progn
     (put ',name 'error-conditions
          (append (get ',superclass 'error-conditions) (list ',name)))
     (put ',name 'error-message ,message)
     ',name))


(defmacro wcheck--loop-over-reqs-engine (key var &rest body)
  `(dolist (,var (delq nil (mapcar (lambda (buffer)
                                     (when (wcheck--buffer-data-get
                                            :buffer buffer ,key)
                                       buffer))
                                   (wcheck--buffer-data-get-all :buffer))))
     (when (buffer-live-p ,var)
       (with-current-buffer ,var
         ,@body))))


(defmacro wcheck--loop-over-read-reqs (var &rest body)
  (declare (indent 1))
  `(wcheck--loop-over-reqs-engine :read-req ,var ,@body))
(defmacro wcheck--loop-over-paint-reqs (var &rest body)
  (declare (indent 1))
  `(wcheck--loop-over-reqs-engine :paint-req ,var ,@body))
(defmacro wcheck--loop-over-jump-reqs (var &rest body)
  (declare (indent 1))
  `(wcheck--loop-over-reqs-engine :jump-req ,var ,@body))


(defmacro wcheck--with-language-data (var-lang bindings &rest body)
  (declare (indent 2))
  (let ((language (make-symbol "--wck-language--")))
    `(let* ((,language ,(cadr var-lang))
            ,@(when (car var-lang)
                `((,(car var-lang) ,language)))
            ,@(mapcar
               (lambda (var)
                 (cond ((symbolp var)
                        (list var `(wcheck-query-language-data
                                    ,language ',var)))
                       ((and var (listp var))
                        (list (car var) `(wcheck-query-language-data
                                          ,language ',(cadr var))))))
               bindings))
       ,@body)))


;;; Conditions


(wcheck--define-condition wcheck--error error)
(wcheck--define-condition wcheck--language-does-not-exist-error wcheck--error)
(wcheck--define-condition wcheck--program-not-configured-error wcheck--error)
(wcheck--define-condition wcheck--not-a-list-of-strings-error wcheck--error)
(wcheck--define-condition wcheck--funcall-error wcheck--error)
(wcheck--define-condition wcheck--action-error wcheck--error)
(wcheck--define-condition wcheck--action-program-error wcheck--action-error)
(wcheck--define-condition wcheck--parser-function-not-configured-error
  wcheck--action-error)
(wcheck--define-condition wcheck--overlay-not-found-error wcheck--error)


;;; Interactive commands


;;;###autoload
(defun wcheck-change-language (language &optional global)
  "Change language for current buffer (or globally).
Change `wcheck-mode' language to LANGUAGE. The change is
buffer-local but if GLOBAL is non-nil (prefix argument if called
interactively) then change the global default language."
  (interactive
   (let* ((comp (mapcar #'car wcheck-language-data))
          (default (cond ((and current-prefix-arg
                               (member (default-value 'wcheck-language) comp))
                          (default-value 'wcheck-language))
                         ((member wcheck-language comp)
                          wcheck-language)
                         ((car comp))
                         (t ""))))
     (list (completing-read
            (format (if current-prefix-arg
                        "Global default language (%s): "
                      "Language for the current buffer (%s): ")
                    default)
            comp nil t nil 'wcheck--change-language-history default)
           current-prefix-arg)))

  (condition-case error-data
      (when (stringp language)
        ;; Change the language, locally or globally, and update buffer
        ;; database, if needed.
        (if global
            ;; Just change the global value and leave.
            (setq-default wcheck-language language)

          ;; Change the buffer-local value.
          (setq wcheck-language language)
          ;; If the mode is currently turned on check if language's
          ;; checker program or function is configured and if all is OK
          ;; request update for the buffer.
          (when wcheck-mode
            (if (wcheck--program-configured-p wcheck-language)
                ;; It's OK; update the buffer.
                (progn
                  (wcheck--buffer-lang-proc-data-update
                   (current-buffer) wcheck-language)
                  (wcheck--buffer-data-set (current-buffer) :read-req t)
                  (wcheck--remove-overlays))

              (signal 'wcheck--program-not-configured-error wcheck-language))))

        ;; Return the language.
        language)

    (wcheck--program-not-configured-error
     (wcheck-mode -1)
     (message "Checker program is not configured for language \"%s\""
              (cdr error-data)))))


(defun wcheck--mode-turn-on ()
  ;; Turn the mode on, but first some checks.
  (let ((buffer (current-buffer))
        (language wcheck-language))
    (condition-case error-data
        (cond
         ((minibufferp buffer)
          (signal 'wcheck--error "Can't use `wcheck-mode' in a minibuffer"))

         ((not (wcheck--language-exists-p language))
          (signal 'wcheck--language-does-not-exist-error language))

         ((not (wcheck--program-configured-p language))
          (signal 'wcheck--program-not-configured-error language))

         (t
          (make-local-variable 'wcheck-language)
          (wcheck--add-local-hooks buffer)
          (wcheck--add-global-hooks)
          (wcheck--buffer-lang-proc-data-update buffer language)
          (wcheck--timer-start)
          (wcheck--buffer-data-set buffer :read-req t)))

      (wcheck--program-not-configured-error
       (wcheck-mode -1)
       (message "Checker program is not configured for language \"%s\""
                (cdr error-data)))

      (wcheck--language-does-not-exist-error
       (wcheck-mode -1)
       (message "Language \"%s\" does not exist" (cdr error-data))))))


(defun wcheck--mode-turn-off ()
  (let ((buffer (current-buffer)))
    ;; We clear overlays form the buffer, remove the buffer from buffer
    ;; database.
    (wcheck--remove-overlays)
    (wcheck--buffer-lang-proc-data-update buffer nil)

    ;; If there are no buffers using wcheck-mode anymore, stop the idle
    ;; timer and remove global hooks.
    (when (null (wcheck--buffer-data-get-all :buffer))
      (wcheck--timer-stop)
      (wcheck--remove-global-hooks))
    (wcheck--remove-local-hooks buffer)))


(defun wcheck--mode-line-lang ()
  (condition-case nil
      (let (lang-code)
        (catch 'enough
          (mapc (lambda (c)
                  (when (char-equal ?w (char-syntax c))
                    (push c lang-code)
                    (when (>= (length lang-code) 2)
                      (throw 'enough t))))
                (wcheck--buffer-data-get :buffer (current-buffer) :language)))
        (apply #'string (nreverse lang-code)))
    (error "")))


;;;###autoload
(define-minor-mode wcheck-mode
  "General interface for text checkers.

With optional (prefix) ARG turn on the mode if ARG is positive,
otherwise turn it off. If ARG is not given toggle the mode.

Wcheck is a minor mode for automatically checking and marking
strings in Emacs buffer. Wcheck sends (parts of) buffer's content
to a text-checker back-end and, relying on its output, decides if
some parts of text should be marked.

Wcheck can be used with external spell-checker programs such as
Ispell and Enchant, but actually any tool that can receive text
stream from standard input and send text to standard output can
be used. The checker back-end can also be an Emacs Lisp function.

Different configuration units are called \"languages\". See the
documentation of variables `wcheck-language-data',
`wcheck-language-data-defaults' and `wcheck-language' for
information on how to configure Wcheck mode. You can access and
configure the variables through customize group `wcheck'.

Interactive command `wcheck-change-language' is used to switch
languages. Command `wcheck-actions' gives an action menu for the
marked text at point (also accessible through the right-click
mouse menu). Commands `wcheck-jump-forward' and
`wcheck-jump-backward' move point to next/previous marked text
area.

A note for Emacs Lisp programmers: Emacs Lisp function
`wcheck-marked-text-at' returns information about marked text at
a buffer position. Function `wcheck-query-language-data' can be
used for querying effective configuration data for any language."

  :init-value nil
  :lighter (" W:" (:eval (wcheck--mode-line-lang)))
  :keymap wcheck-mode-map

  (condition-case error-data
      (if wcheck-mode
          (wcheck--mode-turn-on)
        (wcheck--mode-turn-off))

    (wcheck--error
     (wcheck-mode -1)
     (message "%s" (cdr error-data)))))


;;; Timers


(defun wcheck--timer-start ()
  "Start `wcheck-mode' idle timer if it's not running already."
  (unless wcheck--timer
    (setq wcheck--timer
          (run-with-idle-timer wcheck--timer-idle t
                               #'wcheck--timer-read-event))))


(defun wcheck--timer-stop ()
  "Stop `wcheck-mode' idle timer."
  (when wcheck--timer
    (cancel-timer wcheck--timer)
    (setq wcheck--timer nil)))


(defun wcheck--funcall-after-idle (function &rest args)
  (apply #'run-with-idle-timer
         (+ wcheck--timer-idle (wcheck--current-idle-time-seconds))
         nil function args))


(defun wcheck--timer-paint-event-run (&optional count)
  (if (integerp count)
      (let ((at-least (max count wcheck--timer-paint-event-count)))
        (if (> wcheck--timer-paint-event-count 0)
            (setq wcheck--timer-paint-event-count at-least)
          (setq wcheck--timer-paint-event-count at-least)
          (wcheck--funcall-after-idle #'wcheck--timer-paint-event)))
    (if (> (setq wcheck--timer-paint-event-count
                 (1- wcheck--timer-paint-event-count))
           0)
        (wcheck--funcall-after-idle #'wcheck--timer-paint-event)
      (wcheck--timer-jump-event))))


(defun wcheck--force-read (buffer)
  (redisplay t)
  (wcheck--buffer-data-set buffer :read-req t)
  (wcheck--timer-read-event))


(defun wcheck--timer-read-event ()
  "Send windows' content to checker program or function.

This function is usually called by the `wcheck-mode' idle timer.
The function walks through all windows which belong to buffers
that have requested update. It reads windows' content and sends
it checker program or function associated with the buffer's
language. Finally, this function starts another idle timer for
marking strings in buffers."

  (wcheck--loop-over-read-reqs buffer
    (unless (wcheck--buffer-data-get :buffer buffer :jump-req)
      ;; We are about to fulfill buffer's window-reading request so
      ;; remove the request. Reset also the list of received strings and
      ;; visible window areas.
      (wcheck--buffer-data-set buffer :read-req nil)
      (wcheck--buffer-data-set buffer :strings nil)
      (wcheck--buffer-data-set buffer :areas nil)

      ;; Walk through all windows which belong to this buffer.
      (let (area-alist strings)
        (walk-windows (lambda (window)
                        (when (eq buffer (window-buffer window))
                          ;; Store the visible buffer area.
                          (push (cons (window-start window)
                                      (window-end window t))
                                area-alist)))
                      'nomb t)

        ;; Combine overlapping buffer areas and read strings from all
        ;; areas.
        (let ((combined (wcheck--combine-overlapping-areas area-alist)))
          (wcheck--buffer-data-set buffer :areas combined)
          (dolist (area combined)
            (setq strings (append (wcheck--read-strings
                                   buffer (car area) (cdr area))
                                  strings))))
        ;; Send strings to checker engine.
        (wcheck--send-strings buffer strings))))

  ;; Start a timer which will mark text in buffers/windows.
  (wcheck--timer-paint-event-run wcheck--timer-paint-event-count-std))


(defun wcheck--send-strings (buffer strings)
  "Send STRINGS for the process that handles BUFFER.
STRINGS is a list of strings to be sent as input for the external
process which handles BUFFER. Each string in STRINGS is sent as
separate line."
  (wcheck--with-language-data
      (nil (wcheck--buffer-data-get :buffer buffer :language))
      (program syntax (case-fold-search case-fold))

    (condition-case nil
        (cond ((or (wcheck--buffer-data-get :buffer buffer :process)
                   (stringp program))
               (process-send-string
                (wcheck--start-get-process buffer)
                (concat (mapconcat #'identity strings "\n") "\n"))
               (condition-case nil
                   (with-current-buffer
                       (process-buffer (wcheck--buffer-data-get
                                        :buffer buffer :process))
                     (erase-buffer))
                 (error nil)))

              ((functionp program)
               (when (buffer-live-p buffer)
                 (with-current-buffer buffer
                   (let ((received
                          (save-match-data
                            (condition-case nil
                                (with-syntax-table (eval syntax)
                                  (funcall program strings))
                              (error (signal 'wcheck--funcall-error nil))))))
                     (if (wcheck--list-of-strings-p received)
                         (when received
                           (wcheck--buffer-data-set buffer :strings received)
                           (wcheck--buffer-data-set buffer :paint-req t))
                       (signal 'wcheck--not-a-list-of-strings-error nil)))))))

      (wcheck--not-a-list-of-strings-error
       (with-current-buffer buffer
         (wcheck-mode -1)
         (message (concat "Checker function did not return a list of "
                          "strings (or nil)"))))

      (wcheck--funcall-error
       (message "Checker function signaled an error")))))


(defun wcheck--receive-strings (process string)
  "`wcheck-mode' process output handler function."
  (let ((buffer (wcheck--buffer-data-get :process process :buffer)))
    (wcheck--with-language-data
        (nil (wcheck--buffer-data-get :process process :language))
        (parser syntax (case-fold-search case-fold))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer

          ;; If process is running proceed to collect and paint the
          ;; strings.
          (condition-case error-data
              (if (wcheck--process-running-p process)
                  (with-current-buffer (process-buffer process)
                    (save-excursion
                      (goto-char (point-max))
                      (insert string)
                      (let ((parsed-strings
                             (save-match-data
                               (save-excursion
                                 (goto-char (point-min))
                                 (condition-case nil
                                     (with-syntax-table (eval syntax)
                                       (funcall parser))
                                   (error (signal 'wcheck--funcall-error
                                                  nil)))))))
                        (when (and parsed-strings
                                   (wcheck--list-of-strings-p parsed-strings))
                          (wcheck--buffer-data-set
                           buffer :strings parsed-strings)
                          (wcheck--buffer-data-set buffer :paint-req t)))))

                ;; It's not running. Turn off the mode.
                (wcheck-mode -1)
                (signal 'wcheck--error
                        (format "Process is not running for buffer \"%s\""
                                (buffer-name buffer))))

            (wcheck--funcall-error
             (message "Checker output parser function signaled an error"))

            (wcheck--error
             (message "%s" (cdr error-data)))))))))


(defun wcheck--timer-paint-event ()
  "Mark strings in windows.

This is normally called by the `wcheck-mode' idle timer. This
function marks (with overlays) strings in the buffers that have
requested it."

  (wcheck--loop-over-paint-reqs buffer
    (unless (wcheck--buffer-data-get :buffer buffer :jump-req)
      (wcheck--remove-overlays))
    ;; We are about to mark text in this buffer so remove this buffer's
    ;; request.
    (wcheck--buffer-data-set buffer :paint-req nil)
    ;; Walk through the visible text areas and mark text based on the
    ;; string list returned by an external process.
    (when wcheck-mode
      (dolist (area (wcheck--buffer-data-get :buffer buffer :areas))
        (wcheck--paint-strings buffer (car area) (cdr area)
                               (wcheck--buffer-data-get :buffer buffer
                                                        :strings)
                               ;; If jump-req is active then paint
                               ;; invisible text too.
                               (wcheck--buffer-data-get :buffer buffer
                                                        :jump-req)))))

  (wcheck--timer-paint-event-run))


(defun wcheck--timer-jump-event ()
  (require 'outline)
  (wcheck--loop-over-jump-reqs buffer
    (let* ((jump-req (wcheck--buffer-data-get :buffer buffer :jump-req))
           (start (wcheck--jump-req-start jump-req))
           (bound (wcheck--jump-req-bound jump-req))
           (window (wcheck--jump-req-window jump-req)))

      (wcheck--buffer-data-set buffer :jump-req nil)

      (condition-case nil
          (cond ((> bound start)
                 (let ((ol (wcheck--overlay-next start bound)))
                   (cond (ol
                          (if (and (window-live-p window)
                                   (eq buffer (window-buffer window)))
                              (set-window-point window (overlay-end ol))
                            (goto-char (overlay-end ol)))
                          (when (invisible-p (point))
                            (outline-show-entry))
                          (message "Found from line %s"
                                   (line-number-at-pos (point)))
                          (wcheck--force-read buffer))
                         ((< bound (point-max))
                          (wcheck--jump-req buffer window (1+ bound)
                                            (+ (1+ bound) wcheck--jump-step)))
                         (t
                          (signal 'wcheck--overlay-not-found-error nil)))))
                ((< bound start)
                 (let ((ol (wcheck--overlay-previous start bound)))
                   (cond (ol
                          (if (and (window-live-p window)
                                   (eq buffer (window-buffer window)))
                              (set-window-point window (overlay-start ol))
                            (goto-char (overlay-start ol)))
                          (when (invisible-p (point))
                            (outline-show-entry))
                          (message "Found from line %s"
                                   (line-number-at-pos (point)))
                          (wcheck--force-read buffer))
                         ((> bound (point-min))
                          (wcheck--jump-req buffer window (1- bound)
                                            (- (1- bound) wcheck--jump-step)))
                         (t
                          (signal 'wcheck--overlay-not-found-error nil)))))
                (t
                 (signal 'wcheck--overlay-not-found-error nil)))

        (wcheck--overlay-not-found-error
         (message "Found nothing")
         (wcheck--force-read buffer))))))


;;; Hooks


(defun wcheck--add-local-hooks (buffer)
  (with-current-buffer buffer
    (dolist (hook '((kill-buffer-hook . wcheck--hook-kill-buffer)
                    (window-scroll-functions . wcheck--hook-window-scroll)
                    (after-change-functions . wcheck--hook-after-change)
                    (change-major-mode-hook . wcheck--hook-change-major-mode)
                    (outline-view-change-hook
                     . wcheck--hook-outline-view-change)))
      (add-hook (car hook) (cdr hook) nil t))))


(defun wcheck--remove-local-hooks (buffer)
  (with-current-buffer buffer
    (dolist (hook '((kill-buffer-hook . wcheck--hook-kill-buffer)
                    (window-scroll-functions . wcheck--hook-window-scroll)
                    (after-change-functions . wcheck--hook-after-change)
                    (change-major-mode-hook . wcheck--hook-change-major-mode)
                    (outline-view-change-hook
                     . wcheck--hook-outline-view-change)))
      (remove-hook (car hook) (cdr hook) t))))


(defun wcheck--add-global-hooks ()
  (dolist (hook '((window-size-change-functions
                   . wcheck--hook-window-size-change)
                  (window-configuration-change-hook
                   . wcheck--hook-window-configuration-change)))
    (add-hook (car hook) (cdr hook))))


(defun wcheck--remove-global-hooks ()
  (dolist (hook '((window-size-change-functions
                   . wcheck--hook-window-size-change)
                  (window-configuration-change-hook
                   . wcheck--hook-window-configuration-change)))
    (remove-hook (car hook) (cdr hook))))


(defun wcheck--hook-window-scroll (window _window-start)
  "`wcheck-mode' hook for window scroll.
Request update for the buffer when its window have been scrolled."
  (with-current-buffer (window-buffer window)
    (when wcheck-mode
      (wcheck--buffer-data-set (current-buffer) :read-req t))))


(defun wcheck--hook-window-size-change (frame)
  "`wcheck-mode' hook for window size change.
Request update for the buffer when its window's size has
changed."
  (walk-windows (lambda (window)
                  (with-current-buffer (window-buffer window)
                    (when wcheck-mode
                      (wcheck--buffer-data-set (current-buffer)
                                               :read-req t))))
                'nomb
                frame))


(defun wcheck--hook-window-configuration-change ()
  "`wcheck-mode' hook for window configuration change.
Request update for the buffer when its window's configuration has
changed."
  (walk-windows (lambda (window)
                  (with-current-buffer (window-buffer window)
                    (when wcheck-mode
                      (wcheck--buffer-data-set (current-buffer)
                                               :read-req t))))
                'nomb
                'currentframe))


(defun wcheck--hook-after-change (_beg _end _len)
  "`wcheck-mode' hook for buffer content change.
Request update for the buffer when its content has been edited."
  ;; The buffer that has changed is the current buffer when this hook
  ;; function is called.
  (when wcheck-mode
    (wcheck--buffer-data-set (current-buffer) :read-req t)))


(defun wcheck--hook-outline-view-change ()
  "`wcheck-mode' hook for outline view change.
Request update for the buffer when its outline view has changed."
  (when wcheck-mode
    (wcheck--buffer-data-set (current-buffer) :read-req t)))


(defun wcheck--hook-kill-buffer ()
  "`wcheck-mode' hook for kill-buffer operation.
Turn off `wcheck-mode' when buffer is being killed."
  (wcheck-mode -1))


(defun wcheck--hook-change-major-mode ()
  "`wcheck-mode' hook for major mode change.
Turn off `wcheck-mode' before changing major mode."
  (wcheck-mode -1))


;;; Processes


(defun wcheck--start-get-process (buffer)
  "Start or get external process for BUFFER.
Start a new process or get already existing process for BUFFER.
Return the object of that particular process or nil if the
operation was unsuccessful."
  ;; If process for this BUFFER exists return it.
  (or (wcheck--buffer-data-get :buffer buffer :process)
      ;; It doesn't exist so start a new one.
      (wcheck--with-language-data
          (nil (wcheck--buffer-data-get :buffer buffer :language))
          (program args (process-connection-type connection))

        (when (wcheck--program-executable-p program)
          ;; Start the process.
          (let ((proc (apply #'start-process "wcheck" nil program args)))
            ;; Add the process Lisp object to database.
            (wcheck--buffer-data-set buffer :process proc)
            ;; Set the output handler function and the associated buffer.
            (set-process-filter proc #'wcheck--receive-strings)
            (set-process-buffer proc (generate-new-buffer
                                      (concat " *wcheck-process <"
                                              (buffer-name buffer) ">*")))
            ;; Prevent Emacs from querying user about running processes
            ;; when killing Emacs.
            (set-process-query-on-exit-flag proc nil)
            ;; Return the process object.
            proc)))))


(defun wcheck--buffer-lang-proc-data-update (buffer language)
  "Update process and language data for BUFFER.
Calling this function is the primary way to maintain the language
and process data associated to BUFFER. If LANGUAGE is nil remove
BUFFER from the list."
  (when (and (bufferp buffer)
             (or (stringp language)
                 (not language)))

    ;; Construct a list of currently used processes.
    (let ((old-processes (remq nil (wcheck--buffer-data-get-all :process))))

      ;; Remove dead buffers and possible minibuffers from the list.
      (dolist (item (wcheck--buffer-data-get-all :buffer))
        (when (or (not (buffer-live-p item))
                  (minibufferp item))
          (wcheck--buffer-data-delete item)))

      (if language
          (progn
            ;; LANGUAGE was given. If data for this buffer does not
            ;; exist create it.
            (unless (wcheck--buffer-data-get :buffer buffer)
              (wcheck--buffer-data-create buffer))
            ;; Add this BUFFER's language info and reset the process
            ;; info.
            (wcheck--buffer-data-set buffer :language language)
            (wcheck--buffer-data-set buffer :process nil))

        ;; LANGUAGE was not given so this normally means that
        ;; wcheck-mode is being turned off for this buffer. Remove
        ;; BUFFER's data.
        (wcheck--buffer-data-delete buffer))

      ;; Construct a list of processes that are still used.
      (let ((new-processes (remq nil (wcheck--buffer-data-get-all :process))))
        ;; Stop those processes which are no longer needed.
        (dolist (proc old-processes)
          (unless (memq proc new-processes)
            (kill-buffer (process-buffer proc))
            (delete-process proc))))))

  (wcheck--buffer-data-get :buffer buffer))


;;; Read and paint strings


(defun wcheck--read-strings (buffer beg end &optional invisible)
  "Return a list of text elements in BUFFER.
Scan BUFFER between positions BEG and END and search for text
elements according to buffer's language settings (see
`wcheck-language-data'). If INVISIBLE is non-nil read all buffer
areas, including invisible ones. Otherwise skip invisible text."

  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (save-excursion

        (when font-lock-mode
          (save-excursion
            (funcall (if (fboundp 'font-lock-ensure)
                         #'font-lock-ensure
                       #'font-lock-fontify-region)
                     (min beg end) (max beg end))))

        (wcheck--with-language-data
            (language (wcheck--buffer-data-get :buffer buffer :language))
            (regexp-start regexp-body regexp-end regexp-discard
                          syntax (case-fold-search case-fold))

          (let ((regexp
                 (concat regexp-start "\\(" regexp-body "\\)" regexp-end))
                (face-p (wcheck--generate-face-predicate language major-mode))
                (search-spaces-regexp nil)
                (old-point 0)
                strings)

            (with-syntax-table (eval syntax)
              (goto-char beg)
              (save-match-data
                (while (and (re-search-forward regexp end t)
                            (> (point) old-point))
                  (cond ((and (not invisible)
                              (invisible-p (match-beginning 1)))
                         ;; This point is invisible. Let's jump forward
                         ;; to next change of "invisible" property.
                         (goto-char (next-single-char-property-change
                                     (match-beginning 1) 'invisible buffer
                                     end)))

                        ((and (funcall face-p)
                              (or (equal regexp-discard "")
                                  (not (string-match
                                        regexp-discard
                                        (match-string-no-properties 1)))))
                         ;; Add the match to the string list.
                         (push (match-string-no-properties 1) strings)))
                  (setq old-point (point)))))
            (delete-dups strings)))))))


(defun wcheck--paint-strings (buffer beg end strings &optional invisible)
  "Mark strings in buffer.

Mark all strings in STRINGS which are visible in BUFFER within
position range from BEG to END. If INVISIBLE is non-nil paint all
buffer areas, including invisible ones. Otherwise skip invisible
text."

  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (save-excursion

        (wcheck--with-language-data
            (language (wcheck--buffer-data-get :buffer buffer :language))
            (regexp-start regexp-end syntax (case-fold-search case-fold)
                          (ol-face face) action-program)

          (let ((face-p (wcheck--generate-face-predicate language major-mode))
                (search-spaces-regexp nil)
                (ol-keymap (make-sparse-keymap))
                (ol-mouse-face nil)
                (ol-help-echo nil)
                regexp old-point)

            (when action-program
              (define-key ol-keymap [down-mouse-3] 'wcheck--mouse-click-overlay)
              (define-key ol-keymap [mouse-3] 'undefined)
              (setq ol-mouse-face 'highlight
                    ol-help-echo "mouse-3: show actions"))

            (with-syntax-table (eval syntax)
              (save-match-data
                (dolist (string strings)
                  (setq regexp (concat regexp-start "\\("
                                       (regexp-quote string) "\\)"
                                       regexp-end)
                        old-point 0)
                  (goto-char beg)

                  (while (and (re-search-forward regexp end t)
                              (> (point) old-point))
                    (cond ((and (not invisible)
                                (invisible-p (match-beginning 1)))
                           ;; The point is invisible so jump forward to
                           ;; the next change of "invisible" text
                           ;; property.
                           (goto-char (next-single-char-property-change
                                       (match-beginning 1) 'invisible buffer
                                       end)))
                          ((funcall face-p)
                           ;; Make an overlay.
                           (wcheck--make-overlay
                            buffer ol-face ol-mouse-face ol-help-echo ol-keymap
                            (match-beginning 1) (match-end 1))))
                    (setq old-point (point))))))))))))


;;; Jump forward or backward


(defun wcheck--overlay-next (start bound)
  (unless (>= start (point-max))
    (catch 'overlay
      (dolist (ol (overlays-at start))
        (when (overlay-get ol 'wcheck-mode)
          (throw 'overlay ol)))
      (let ((pos start))
        (while (and (setq pos (next-overlay-change pos))
                    (< pos (min bound (point-max))))
          (dolist (ol (overlays-at pos))
            (when (overlay-get ol 'wcheck-mode)
              (throw 'overlay ol))))))))


(defun wcheck--overlay-previous (start bound)
  (unless (<= start (point-min))
    (catch 'overlay
      (let ((pos start))
        (while t
          (setq pos (previous-overlay-change pos))
          (dolist (ol (overlays-at pos))
            (when (overlay-get ol 'wcheck-mode)
              (throw 'overlay ol)))
          (when (<= pos (max bound (point-min)))
            (throw 'overlay nil)))))))


(defun wcheck--line-start-at (pos)
  (save-excursion
    (goto-char pos)
    (line-beginning-position)))


(defun wcheck--line-end-at (pos)
  (save-excursion
    (goto-char pos)
    (line-end-position)))


(defun wcheck--jump-req (buffer window start bound)
  (unless (= start bound)
    (with-current-buffer buffer
      (setq bound (funcall (if (> bound start)
                               'wcheck--line-end-at
                             'wcheck--line-start-at)
                           bound))
      (message "Searching in lines %d-%d..."
               (line-number-at-pos start)
               (line-number-at-pos bound))
      (wcheck--buffer-data-set buffer :jump-req (wcheck--jump-req-create
                                                 window start bound))
      (wcheck--buffer-data-set buffer :areas (list (cons (min start bound)
                                                         (max start bound))))
      (wcheck--send-strings buffer (wcheck--read-strings
                                    buffer (min start bound)
                                    (max start bound) t))
      (wcheck--timer-paint-event-run wcheck--timer-paint-event-count-std))))


(defun wcheck--invisible-text-in-area-p (buffer beg end)
  (catch 'invisible
    (let ((pos (min beg end))
          (end (max beg end)))
      (when (invisible-p pos)
        (throw 'invisible t))
      (while (and (setq pos (next-single-char-property-change
                             pos 'invisible buffer))
                  (< pos end))
        (when (invisible-p pos)
          (throw 'invisible t))))))


;;;###autoload
(defun wcheck-jump-forward ()
  "Move point forward to next marked text area."
  (interactive)
  (let ((buffer (current-buffer))
        (window (selected-window)))
    (unless wcheck-mode
      (wcheck-mode 1))
    (when wcheck-mode
      (wcheck--buffer-data-set buffer :jump-req nil)
      (let ((ol (wcheck--overlay-next
                 (point) (window-end (selected-window) t))))
        (if (and ol (not (wcheck--invisible-text-in-area-p
                          buffer (point) (overlay-end ol))))
            (goto-char (overlay-end ol))
          (if (eobp)
              (message "End of buffer")
            (wcheck--jump-req buffer window (point)
                              (+ (point) wcheck--jump-step))))))))


;;;###autoload
(defun wcheck-jump-backward ()
  "Move point backward to previous marked text area."
  (interactive)
  (let ((buffer (current-buffer))
        (window (selected-window)))
    (unless wcheck-mode
      (wcheck-mode 1))
    (when wcheck-mode
      (wcheck--buffer-data-set buffer :jump-req nil)
      (let ((ol (wcheck--overlay-previous
                 (point) (window-start (selected-window)))))
        (if (and ol (not (wcheck--invisible-text-in-area-p
                          buffer (point) (overlay-start ol))))
            (goto-char (overlay-start ol))
          (if (bobp)
              (message "Beginning of buffer")
            (wcheck--jump-req buffer window (point)
                              (- (point) wcheck--jump-step))))))))


;;; Actions


(defun wcheck-marked-text-at (pos)
  "Return information about marked text at POS.

POS is a buffer position. The return value is a vector of five
elements: (1) the marked text string, (2) buffer position at the
beginning of the text, (3) position at the end of the text, (4)
the overlay object which marks the text and (5) the current
language as a string. The return value is nil if there are no
marked text at POS.

If you need more information about the current language settings
use `wcheck-query-language-data' for querying effective language
settings."

  (let ((overlay (catch 'my-overlay
                   (dolist (ol (overlays-at pos))
                     (when (overlay-get ol 'wcheck-mode)
                       (throw 'my-overlay ol))))))
    (when overlay
      (let ((start (overlay-start overlay))
            (end (overlay-end overlay)))
        (vector (buffer-substring-no-properties start end)
                start end overlay
                (wcheck--buffer-data-get
                 :buffer (current-buffer) :language))))))


;;;###autoload
(defun wcheck-actions (pos &optional event)
  "Offer actions for marked text.

This function is usually called through a right mouse button
event or interactively by a user. In both cases function's
arguments are filled automatically.

If buffer position POS is on marked text (and action program is
properly configured) show a menu of actions. When this function
is called interactively POS is automatically the current point
position. Optional EVENT argument is a mouse event which is
present if this function is called through a right mouse button
click on marked text. If EVENT is non-nil use a graphic toolkit's
menu (when available) for selecting actions. Otherwise use a text
menu.

When user chooses one of the options from the menu the related
action is executed. It could mean that the original marked text
is replaced with the chosen substitute. Menu options can trigger
any kind of actions, though."

  (interactive "d")
  (condition-case error-data
      (let ((marked-text (or (wcheck-marked-text-at pos)
                             (wcheck-marked-text-at (1- pos))))
            (return-value nil))

        (if (not marked-text)
            (signal 'wcheck--action-error "There is no marked text here")
          (let* ((start (copy-marker (aref marked-text 1)))
                 (end (copy-marker (aref marked-text 2)))
                 (actions (wcheck--get-actions marked-text))
                 (choice (cond ((and (null (cdr actions))
                                     (wcheck-query-language-data
                                      (aref marked-text 4) 'action-autoselect))
                                (cdar actions))
                               ((and event (display-popup-menus-p))
                                (wcheck--choose-action-popup actions event))
                               (t (wcheck--choose-action-minibuffer actions)))))

            (cond ((and (stringp choice)
                        (markerp start)
                        (markerp end))
                   (with-current-buffer (marker-buffer start)
                     (if buffer-read-only
                         (signal 'wcheck--action-error "Buffer is read-only")
                       (delete-region start end)
                       (goto-char start)
                       (insert choice)
                       (setq return-value choice))))
                  ((functionp choice)
                   (funcall choice marked-text)
                   (setq return-value choice)))

            (if (markerp start) (set-marker start nil))
            (if (markerp end) (set-marker end nil))))
        return-value)

    (wcheck--action-program-error
     (message "Action program is not configured for language \"%s\""
              (cdr error-data)))

    (wcheck--parser-function-not-configured-error
     (message "Parser function is not configured for language \"%s\""
              (cdr error-data)))

    (wcheck--error
     (message "%s" (cdr error-data)))))


(defun wcheck--get-actions (marked-text)
  "Get actions from external program or a function.

MARKED-TEXT must be a vector such as the one returned by
`wcheck-marked-text-at' function."

  (wcheck--with-language-data
      (language (aref marked-text 4))
      ((program action-program)
       (args action-args)
       (parser action-parser)
       (case-fold-search case-fold)
       syntax)

    (with-syntax-table (eval syntax)
      (cond ((not (wcheck--action-program-configured-p language))
             (signal 'wcheck--action-program-error language))

            ((and (stringp program)
                  (not parser))
             (signal 'wcheck--parser-function-not-configured-error language))

            ((stringp program)
             (with-temp-buffer
               (insert (aref marked-text 0))
               (apply #'call-process-region (point-min) (point-max)
                      program t t nil args)
               (goto-char (point-min))
               (wcheck--clean-actions
                (save-match-data
                  (condition-case nil (funcall parser marked-text)
                    (error (signal 'wcheck--funcall-error
                                   (concat "Action parser function "
                                           "signaled an error"))))))))

            ((functionp program)
             (wcheck--clean-actions
              (save-match-data
                (condition-case nil (funcall program marked-text)
                  (error (signal 'wcheck--funcall-error
                                 (concat "Action function signaled "
                                         "an error")))))))))))


(defun wcheck--clean-actions (actions)
  (when (listp actions)
    (delete nil (mapcar (lambda (item)
                          (cond ((stringp item)
                                 (cons (wcheck--clean-string item)
                                       item))
                                ((and (consp item)
                                      (stringp (car item))
                                      (or (functionp (cdr item))
                                          (stringp (cdr item))))
                                 (cons (wcheck--clean-string (car item))
                                       (cdr item)))))
                        actions))))


(defun wcheck--clean-string (string)
  (if (equal string "")
      "[Empty string]"
    (setq string (replace-regexp-in-string "[^[:print:]]+" "" string))
    (if (not (string-match "[^[:space:]]" string))
        "[Space or control chars]"
      (replace-regexp-in-string "\\(?:\\` +\\| +\\'\\)" "" string))))


(defun wcheck--choose-action-popup (actions event)
  "Create a pop-up menu to choose an action.
ACTIONS is a list of strings. EVENT is the mouse event that
originated this sequence of function calls. Return user's
choice (a string) or nil."
  (let ((menu (list "Choose"
                    (cons "" (if actions
                                 (mapcar (lambda (item)
                                           (cons (wcheck--clean-string
                                                  (car item))
                                                 (cdr item)))
                                         actions)
                               (list "[No actions]"))))))
    (x-popup-menu event menu)))


(defun wcheck--read-key (prompt)
  (if (fboundp 'read-key)
      (read-key prompt)
    (read-char prompt)))


(defun wcheck--choose-action-minibuffer (actions)
  "Create a text menu to choose a substitute action.
ACTIONS is a list of strings. Return user's choice (a string)
or nil."
  (if actions
      (let ((chars (append (number-sequence ?1 ?9) (list ?0)
                           (number-sequence ?a ?z)))
            alist)

        (with-temp-buffer
          (setq mode-line-format (list "--- Choose %-")
                cursor-type nil
                truncate-lines t)

          (let (sug string)
            (while (and actions chars)
              (setq sug (car actions)
                    actions (cdr actions)
                    string (concat (propertize (format "%c)" (car chars))
                                               'face 'bold)
                                   " " (wcheck--clean-string (car sug)) "  ")
                    alist (cons (cons (car chars) (cdr sug)) alist)
                    chars (cdr chars))
              (insert string)
              (when (and actions chars
                         (> (+ (- (point) (line-beginning-position))
                               (length (concat "x) " (caar actions))))
                            (window-width)))
                (delete-char -2)
                (newline 1))))

          (delete-char -2)
          (goto-char (point-min))
          (setq buffer-read-only t)

          (let* ((window-min-height 2)
                 (split-window-keep-point t)
                 (window (split-window-vertically
                          (- 0 (min (count-lines (point-min) (point-max))
                                    (- (window-body-height) 2))
                             1)))
                 (prompt
                  (apply #'propertize
                         (let ((last (caar alist)))
                           (format "Number %s(%s):"
                                   (if (memq last (number-sequence ?a ?z))
                                       "or letter "
                                     "")
                                   (cond ((= last ?1) "1")
                                         ((memq last (number-sequence ?2 ?9))
                                          (format "1-%c" last))
                                         ((= last ?0) "1-9,0")
                                         ((= last ?a) "1-9,0,a")
                                         ((memq last (number-sequence ?b ?z))
                                          (format "1-9,0,a-%c" last))
                                         (t ""))))
                         minibuffer-prompt-properties)))
            (set-window-buffer window (current-buffer))
            (set-window-dedicated-p window t)
            ;; Return the choice or nil.
            (cond ((cdr (assq (wcheck--read-key prompt) alist)))
                  (t (message "Abort") nil)))))
    (message "No actions")
    nil))


(defun wcheck-parser-lines (&rest _ignored)
  "Parser for newline-separated output.
Return current buffer's lines as a list of strings."
  (delete-dups (split-string (buffer-substring-no-properties
                              (point-min) (point-max))
                             "\n+" t)))


(defun wcheck-parser-whitespace (&rest _ignored)
  "Parser for whitespace-separated output.
Split current buffer's content to whitespace-separated tokens and
return them as a list of strings."
  (delete-dups (split-string (buffer-substring-no-properties
                              (point-min) (point-max))
                             "[ \f\t\n\r\v]+" t)))


(defun wcheck-parser-ispell-suggestions (&rest _ignored)
  "Parser for Ispell-compatible programs' spelling suggestions."
  (let ((search-spaces-regexp nil))
    (when (re-search-forward "^& [^ ]+ \\([0-9]+\\) [0-9]+: \\(.+\\)$" nil t)
      (let ((count (string-to-number (match-string-no-properties 1)))
            (words (split-string (match-string-no-properties 2) ", " t)))
        (delete-dups (nbutlast words (- (length words) count)))))))


;;; Face information functions


(defun wcheck--collect-faces (beg end)
  "Return a list of faces between positions BEG and END."
  (let ((pos beg)
        face faces)
    (while (< pos end)
      (setq face (get-text-property pos 'face)
            pos (1+ pos))
      (if (and face (listp face))
          (setq faces (append face faces))
        (push face faces)))
    (delete-dups faces)))


(defun wcheck--major-mode-face-settings (language mode)
  "Return read/skip face settings for MODE."
  (let ((data (wcheck-query-language-data language 'read-or-skip-faces))
        conf)
    (catch 'answer
      (while data
        (setq conf (pop data))
        (when (or (eq nil (car conf))
                  (eq mode (car conf))
                  (and (listp (car conf))
                       (memq mode (car conf))))
          (throw 'answer conf))))))


(defun wcheck--face-found-p (user-faces buffer-faces)
  "Return t if a symbol in USER-FACES is found from BUFFER-FACES.
Both arguments are lists."
  (catch 'found
    (dolist (face user-faces)
      (when (member face buffer-faces)
        (throw 'found t)))))


(defun wcheck--generate-face-predicate (language mode)
  "Generate a face predicate function for scanning buffer.
Return a predicate function that is used to decide whether
`wcheck-mode' should read or paint text at the current point
position with LANGUAGE and MODE. The called predicate function
will return a boolean."
  (let* ((face-settings (wcheck--major-mode-face-settings
                         language mode))
         (mode (nth 1 face-settings))
         (faces (nthcdr 2 face-settings)))
    (cond ((not font-lock-mode)
           (lambda () t))
          ((eq mode 'read)
           (lambda ()
             (wcheck--face-found-p
              faces (wcheck--collect-faces
                     (match-beginning 1) (match-end 1)))))
          ((eq mode 'skip)
           (lambda ()
             (not (wcheck--face-found-p
                   faces (wcheck--collect-faces
                          (match-beginning 1) (match-end 1))))))
          (t (lambda () t)))))


;;; Miscellaneous low-level functions


(defun wcheck--language-data-valid-p (key value)
  (cond ((and (eq key 'syntax)
              (syntax-table-p (and (boundp value) (eval value)))))
        ((and (eq key 'face)
              (facep value)))
        ((and (memq key '(regexp-start regexp-body regexp-end regexp-discard))
              (stringp value)))
        ((and (memq key '(program action-program))
              (or (stringp value)
                  (functionp value)
                  (and value (symbolp value)
                       (error "Invalid %s value: %S" key value)))))
        ((and (eq key 'args)
              (wcheck--list-of-strings-p value)))
        ((and (eq key 'action-args)
              (wcheck--list-of-strings-p value)))
        ((and (memq key '(parser action-parser))
              (or (functionp value)
                  (and value
                       (error "%s not a function: %S" key value)))))
        ((memq key '(connection case-fold action-autoselect)))
        ((and (eq key 'read-or-skip-faces)
              (wcheck--list-of-lists-p value)))))


(defun wcheck-query-language-data (language key)
  "Query `wcheck-mode' language data.

Return LANGUAGE's value for KEY. Valid keys (symbols) are
described in the documentation of user variable
`wcheck-language-data'. If that variable does not define
a (valid) value for the KEY then query the value from
`wcheck-language-data-defaults' or use internal defaults."

  (when (wcheck--language-exists-p language)
    (let* ((data
            (and (wcheck--list-of-lists-p wcheck-language-data)
                 (assq key (cdr (assoc language wcheck-language-data)))))
           (default
             (and (wcheck--list-of-lists-p wcheck-language-data-defaults)
                  (assq key wcheck-language-data-defaults)))
           (hard-coded
            (and (wcheck--list-of-lists-p
                  wcheck--language-data-defaults-hard-coded)
                 (assq key wcheck--language-data-defaults-hard-coded)))
           (conf
            (list (when (wcheck--language-data-valid-p key (cdr data))
                    data)
                  (when (wcheck--language-data-valid-p key (cdr default))
                    default)
                  (when (wcheck--language-data-valid-p key (cdr hard-coded))
                    hard-coded))))

      (if (eq key 'read-or-skip-faces)
          (apply #'append (mapcar #'cdr conf))
        (cdr (assq key conf))))))


(defun wcheck--language-exists-p (language)
  "Return t if LANGUAGE exists in `wcheck-language-data'."
  (and (wcheck--list-of-lists-p wcheck-language-data)
       (member language (mapcar #'car wcheck-language-data))
       (stringp language)
       (> (length language) 0)
       t))


(defun wcheck--program-executable-p (program)
  "Return non-nil if PROGRAM is executable regular file."
  (when (stringp program)
    (let ((f (executable-find program)))
      (and f
           (file-regular-p f)
           (file-executable-p f)))))


(defun wcheck--program-configured-p (language)
  (let ((program (wcheck-query-language-data language 'program)))
    (or (wcheck--program-executable-p program)
        (functionp program))))


(defun wcheck--action-program-configured-p (language)
  (let ((program (wcheck-query-language-data language 'action-program)))
    (or (wcheck--program-executable-p program)
        (functionp program))))


(defun wcheck--list-of-strings-p (object)
  (and (listp object)
       (not (memq nil (mapcar #'stringp object)))))


(defun wcheck--list-of-lists-p (object)
  (and (listp object)
       (not (memq nil (mapcar #'listp object)))))


(defun wcheck--process-running-p (process)
  (eq 'run (process-status process)))


(defun wcheck--current-idle-time-seconds ()
  "Return current idle time in seconds.
The returned value is a floating point number."
  (let* ((idle (or (current-idle-time)
                   '(0 0 0)))
         (high (nth 0 idle))
         (low (nth 1 idle))
         (micros (nth 2 idle)))
    (+ (* high 65536)
       low
       (/ micros 1000000.0))))


(defun wcheck--combine-overlapping-areas (alist)
  "Combine overlapping items in ALIST.
ALIST is a list of (A . B) items in which A and B are integers.
Each item denote a buffer position range from A to B. This
function returns a new list which has items in increasing order
according to A's and all overlapping A B ranges are combined."
  (let ((alist (sort (copy-sequence alist)
                     (lambda (a b)
                       (< (car a) (car b)))))
        final previous)
    (while alist
      (while (not (equal previous alist))
        (setq previous alist
              alist (append (wcheck--combine-two (car previous) (cadr previous))
                            (nthcdr 2 previous))))
      (setq final (cons (car alist) final)
            alist (cdr alist)
            previous nil))
    (nreverse final)))


(defun wcheck--combine-two (a b)
  (let ((a1 (car a))
        (a2 (cdr a))
        (b1 (car b))
        (b2 (cdr b)))
    (cond ((and a b)
           (if (>= (1+ a2) b1)
               (list (cons a1 (if (> b2 a2) b2 a2)))
             (list a b)))
          ((not a) (list b))
          (t (append (list a) b)))))


;;; Overlays


(defun wcheck--make-overlay (buffer face mouse-face help-echo keymap beg end)
  "Create an overlay to mark text.
Create an overlay in BUFFER from range BEG to END. FACE,
MOUSE-FACE, HELP-ECHO and KEYMAP are overlay's properties."
  (let ((overlay (make-overlay beg end buffer)))
    (dolist (prop `((wcheck-mode . t)
                    (face . ,face)
                    (mouse-face . ,mouse-face)
                    (modification-hooks wcheck--remove-changed-overlay)
                    (insert-in-front-hooks wcheck--remove-changed-overlay)
                    (insert-behind-hooks wcheck--remove-changed-overlay)
                    (evaporate . t)
                    (keymap . ,keymap)
                    (help-echo . ,help-echo)))
      (overlay-put overlay (car prop) (cdr prop)))))


(defun wcheck--remove-overlays (&optional beg end)
  "Remove `wcheck-mode' overlays from current buffer.
If optional arguments BEG and END exist remove overlays from
range BEG to END. Otherwise remove all overlays."
  (remove-overlays beg end 'wcheck-mode t))


(defun wcheck--remove-changed-overlay (overlay after _beg _end &optional _len)
  "Hook for removing overlay which is being edited."
  (unless after
    (delete-overlay overlay)))


(defun wcheck--mouse-click-overlay (event)
  "Overlay mouse-click event.
Send the mouse pointer position and mouse event to the
`wcheck-actions' function."
  (interactive "e")
  (wcheck-actions (posn-point (event-end event)) event))


;;; Buffer data access functions


(defconst wcheck--buffer-data-keys
  '(:buffer :process :language :read-req :paint-req :jump-req :areas :strings))


(defun wcheck--buffer-data-key-index (key)
  "Return the index of KEY in buffer data object."
  (let ((index 0))
    (catch 'answer
      (dolist (data-key wcheck--buffer-data-keys nil)
        (if (eq key data-key)
            (throw 'answer index)
          (setq index (1+ index)))))))


(defun wcheck--buffer-data-create (buffer)
  "Create data instance for BUFFER.
But only if it doesn't exist already."
  (unless (wcheck--buffer-data-get :buffer buffer)
    (let ((data (make-vector (length wcheck--buffer-data-keys) nil)))
      (aset data (wcheck--buffer-data-key-index :buffer) buffer)
      (push data wcheck--buffer-data))))


(defun wcheck--buffer-data-delete (buffer)
  "Delete all data associated to BUFFER."
  (let ((index (wcheck--buffer-data-key-index :buffer)))
    (setq wcheck--buffer-data
          (delq nil (mapcar (lambda (item)
                              (unless (eq buffer (aref item index))
                                item))
                            wcheck--buffer-data)))))


(defun wcheck--buffer-data-get (key value &optional target-key)
  "Query the first matching KEY VALUE pair and return TARGET-KEY.
If optional TARGET-KEY is not given return all data associated
with the matching KEY VALUE."
  (catch 'answer
    (let ((index (wcheck--buffer-data-key-index key)))
      (dolist (item wcheck--buffer-data)
        (when (equal value (aref item index))
          (throw 'answer (if target-key
                             (aref item (wcheck--buffer-data-key-index
                                         target-key))
                           item)))))))


(defun wcheck--buffer-data-get-all (&optional key)
  "Return every buffer's value for KEY.
If KEY is nil return all buffer's all data."
  (if key
      (let ((index (wcheck--buffer-data-key-index key)))
        (mapcar (lambda (item)
                  (aref item index))
                wcheck--buffer-data))
    wcheck--buffer-data))


(defun wcheck--buffer-data-set (buffer key value)
  "Set KEY's VALUE for BUFFER."
  (let ((item (wcheck--buffer-data-get :buffer buffer)))
    (when item
      (aset item (wcheck--buffer-data-key-index key) value))))


(defun wcheck--jump-req-create (window start bound)
  (when (and (number-or-marker-p start)
             (number-or-marker-p bound)
             (windowp window))
    (vector window start bound)))


(defun wcheck--jump-req-window (jump-req)
  (aref jump-req 0))
(defun wcheck--jump-req-start (jump-req)
  (aref jump-req 1))
(defun wcheck--jump-req-bound (jump-req)
  (aref jump-req 2))

;;;; ChangeLog:

;; 2020-10-04  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Update version number: 2020.10.4
;; 
;; 2020-10-04  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Remove -pkg.el, COPYING and Makefile
;; 
;; 2020-10-04  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	README: In install instructions refer to GNU Elpa only
;; 
;; 2020-10-04  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Use the same copyright and version header as in the GNU Elpa repository
;; 
;; 2019-06-26  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	Fix the first line of wcheck-mode.el file
;; 
;; 2019-06-18  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Reword some error messages
;; 
;; 2019-06-17  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Version 2019.6.17
;; 
;; 2019-06-16  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Call outline-show-entry function instead of deprecated show-entry
;; 
;; 2019-06-16  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Make --generate-face-predicate to take advantage of lexical closures
;; 
;; 2019-06-16  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Update copyright year (2019)
;; 
;; 2019-06-16  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Add (require 'outline) to wcheck--timer-jump-event function
;; 
;; 	The function uses show-entry which is in outline package.
;; 
;; 2019-06-16  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Switch to lexical-binding
;; 
;; 2019-06-16  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Improve the doc string of wcheck--generate-face-predicate function
;; 
;; 	The documentation now speaks of "predicate function" instead of
;; 	"predicate expression".
;; 
;; 2019-06-16  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Refer to "OpenPGP key" instead of just "PGP"
;; 
;; 2019-06-16  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Add read-or-skip-faces example and section headings
;; 
;; 2019-04-01  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Mention GNU Elpa in the README
;; 
;; 2018-01-26  Frank Fischer  <frank-fischer@shadow-soft.de>
;; 
;; 	Fix `wcheck--language-exists-p` if executable does not exist
;; 
;; 	The function `executable-find` might return nil if `program` does not 
;; 	exist. In this case `file-regular-p` raises a nil error. We now check 
;; 	for `nil` explicitly.
;; 
;; 2017-02-19  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Change PGP key's url
;; 
;; 2016-07-12  Paul Eggert	 <eggert@cs.ucla.edu>
;; 
;; 	Fix some quoting problems in doc strings
;; 
;; 	Most of these are minor issues involving, e.g., quoting `like this' 
;; 	instead of 'like this'.	 A few involve escaping ` and ' with a preceding
;; 	\= when the characters should not be turned into curved single quotes.
;; 
;; 2016-02-08  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	Different logic for wcheck--program-executable-p
;; 
;; 2016-02-08  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	If exists use font-lock-ensure, else font-lock-fontify-region
;; 
;; 2016-01-30  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Version 2016.1.30
;; 
;; 2016-01-30  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Revert "Use jit-lock-fontify-now instead of font-lock-fontify-region"
;; 
;; 	This reverts commit 2e4827b16b88635d0ffb6addef530de060e59503.
;; 
;; 	In Emacs 25 jit-lock-fontify-now gives an error.
;; 
;; 2016-01-05  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Fix wcheck--generate-face-predicate: return lambda in all cases
;; 
;; 2016-01-05  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Remove Marmalade package archive
;; 
;; 2016-01-05  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Reformat comments
;; 
;; 2016-01-05  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Update copyright year
;; 
;; 2016-01-05  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Make generated face predicate a lambda form
;; 
;; 2015-12-03  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Small changes to introduction texts
;; 
;; 2015-11-29  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	New version
;; 
;; 2015-11-29  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Add link to PGP key
;; 
;; 2015-10-16  Syohei YOSHIDA  <syohex@gmail.com>
;; 
;; 	Update MELPA URL
;; 
;; 2015-10-15  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Eval wcheck--buffer-data-key-index only once in wcheck--buffer-data-get
;; 
;; 2015-10-15  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Precalculate (expt 2 16): 65536
;; 
;; 2015-10-15  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Update copyright year: 2015
;; 
;; 2015-10-15  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Update package version: 2015.10.15
;; 
;; 2015-10-15  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Merge branch 'master' of github.com:tlikonen/wcheck-mode
;; 
;; 	* 'master' of github.com:tlikonen/wcheck-mode:
;; 	 Allow exec-path search to find external program
;; 
;; 	Conflicts:
;; 	wcheck-mode.el
;; 
;; 2015-08-29  justbur  <justin@burkett.cc>
;; 
;; 	Allow exec-path search to find external program
;; 
;; 2015-07-04  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	Sync changes from GNU Elpa (minor fixes; no functional changes)
;; 
;; 2014-06-21  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Version 2014.6.21
;; 
;; 2014-06-21  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Use PUSH instead of ADD-TO-LIST
;; 
;; 	Duplicates are removed later.
;; 
;; 2014-06-21  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Small README fixes
;; 
;; 2014-06-20  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Enchant must be restarted after calling enchant-add-to-dictionary
;; 
;; 	Mention the fact in README file.
;; 
;; 2014-05-17  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Add elpa pkg file to the repository (version 2014.5.17)
;; 
;; 2014-05-17  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Adjust Makefile
;; 
;; 2014-04-20  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Use prefix wcheck-- in internal functions and variables
;; 
;; 2014-04-20  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Update copyright year to 2014
;; 
;; 2014-04-20  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Delete obsolete variable wcheck-read-or-skip-faces
;; 
;; 2014-04-20  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Add pointer to Melpa package archive
;; 
;; 2013-06-13  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Remove "see examples below" from the doc string of wcheck-language-data
;; 
;; 	Examples are not in the doc string anymore (but in the README).
;; 
;; 2013-06-13  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Makefile: Only byte-compile wcheck-mode.el
;; 
;; 2013-06-05  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Makefile: add rules to make *.elc files (byte-compile)
;; 
;; 2013-06-05  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Silence byte-compiler warning about SHOW-ENTRY function
;; 
;; 2013-06-05  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Use jit-lock-fontify-now instead of font-lock-fontify-region
;; 
;; 2013-05-15  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Drop the .org suffix from README (mentioned doc string)
;; 
;; 2013-05-15  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Fix typo in the doc string of wcheck-language-date
;; 
;; 2013-04-28  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Mention ~/.emacs.d/init.el too as one of the Emacs init files
;; 
;; 2013-04-28  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Add a missing "is" to README
;; 
;; 2013-04-18  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Add link to Marmalade package archive
;; 
;; 2013-04-18  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Remove "mode" from "Speck mode" link text
;; 
;; 2013-04-18  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Add hyperlinks to Emacs, Flyspell and Speck mode
;; 
;; 2013-04-18  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Add another empty lines before headings
;; 
;; 2013-04-18  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Add another <>'s around the email address
;; 
;; 2013-04-18  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Convert README to Markdown markup (and rename: .org -> .md)
;; 
;; 2013-04-18  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Rephrase README.org file (FIXME and email examples)
;; 
;; 2013-04-17  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Archive README.org too in elpa tar package
;; 
;; 2013-04-17  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Leave author and timestamp out from exported README.org
;; 
;; 2013-04-14  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Fix bug in SIGNAL call: add FORMAT
;; 
;; 2013-04-14  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Add Makefile to the repository
;; 
;; 	This can be used to create ELPA tar packages.
;; 
;; 2013-04-14  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Make README.org more org-export-friendly
;; 
;; 	Remove heading numbering and table of contents. Add TITLE and LANGUAGE 
;; 	cookies.
;; 
;; 2013-04-14  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Remove version information from wcheck-mode.el
;; 
;; 2013-04-07  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Change docstring Github URL to the wcheck-mode's main page
;; 
;; 2013-04-07  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Update copyright year in README
;; 
;; 2013-04-07  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Update the version constant string too
;; 
;; 2013-04-07  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Version 2013.04.07
;; 
;; 	No real improvements but there's no point in having small changes lying 
;; 	unpublished either.
;; 
;; 2013-03-30  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Update copyright year to 2013
;; 
;; 2012-11-04  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Add variable and interactive function wcheck-version
;; 
;; 2012-10-21  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Minor rephrasing in the README's "features" section
;; 
;; 2012-03-03  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Use COPY-SEQUENCE instead of COPY-TREE
;; 
;; 	It was always unnecessary to copy the whole tree before SORT function.
;; 
;; 2012-02-18  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Add full licence text (COPYING)
;; 
;; 2012-01-29  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Update README.org url in doc string
;; 
;; 2012-01-29  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Version 2012.01.29 (update copyrights too)
;; 
;; 2011-12-03  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Fix a small typo in README
;; 
;; 2011-11-20  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Add "URL" header which points to GitHub
;; 
;; 2011-10-20  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Rephrase references to non-existing functions in README
;; 
;; 2011-10-20  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Don't mention "Aspell" in the documentation
;; 
;; 	It is questionable if wcheck-mode works directly with Aspell executable 
;; 	because it seems to buffer the IO and therefore may not respond quickly 
;; 	enough for wcheck-mode.
;; 
;; 2011-10-08  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	A minor document fix
;; 
;; 2011-09-18  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Add a fully functional "Add to dictionary" example
;; 
;; 	This example works with Enchant spelling checker in GNU/Linux systems.
;; 
;; 2011-08-14  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Clarify that wcheck-mode is a minor mode
;; 
;; 2011-08-14  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Add (regexp-discard . "") to email detect example
;; 
;; 2011-07-27  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Simplify the customize code of wcheck-language-data
;; 
;; 2011-07-27  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Version 2011.07.27
;; 
;; 2011-07-27  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Update link to README.org
;; 
;; 2011-07-27  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Remove "A" from the README file's subtitle
;; 
;; 2011-07-18  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Put the subtitle under the main title and modify it a bit
;; 
;; 2011-07-18  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Minor README formatting
;; 
;; 2011-07-06  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Put case-fold in language options rather than email parser function
;; 
;; 	Now that language option "case-fold" is effective with checker function
;; 	(and all) change the email example in README.org so that the option is 
;; 	set in language options rather than in the email checker function.
;; 
;; 2011-07-04  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Remove duplicate word "function" from doc string
;; 
;; 2011-07-04  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Change README to README.org in a doc string
;; 
;; 2011-07-04  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Version 2011.07.04
;; 
;; 2011-07-04  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Change README link to point to a newer commit
;; 
;; 2011-07-04  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Merge branch 'action-autoselect'
;; 
;; 	* action-autoselect:
;; 	 Replace nested IF structure with COND in wcheck-actions
;; 	 State that action-autoselect=nil is the default
;; 	 New feature: the only action menu item can be chosen automatically
;; 
;; 	Conflicts:
;; 	wcheck-mode.el
;; 
;; 2011-07-03  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Document where "syntax" and "case-fold" are used
;; 
;; 2011-07-03  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Make "syntax" and "case-fold" effective with action-program and -parser
;; 
;; 2011-07-03  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Make "syntax" and "case-fold" effective with "parser" function
;; 
;; 2011-07-03  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Make "syntax" and "case-fold" effective with "program" function
;; 
;; 	Language options "syntax" and "case-fold" are now effective when the 
;; 	checker "program" function is called.
;; 
;; 2011-07-03  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Don't bind variable "language" unnecessarily
;; 
;; 2011-07-03  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Use org-mode markup in README and rename it to README.org
;; 
;; 2011-07-02  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Replace nested IF structure with COND in wcheck-actions
;; 
;; 	Also optimize (= 1 (length actions)) to (null (cdr actions)). There's no 
;; 	need to traverse down the whole list.
;; 
;; 2011-07-02  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	State that action-autoselect=nil is the default
;; 
;; 2011-07-02  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Add a table of contents to README file
;; 
;; 2011-07-02  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Remove examples from wcheck-language-data doc and replace it with URL
;; 
;; 	There is probably no need for maintaining examples in two places. A link 
;; 	to README file in the GitHub repository will do.
;; 
;; 2011-07-02  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	New feature: the only action menu item can be chosen automatically
;; 
;; 	Introduce new language option "action-autoselect". If this option is 
;; 	non-nil and the action menu has only one menu item then the item is 
;; 	chosen automatically without actually showing the menu.
;; 
;; 2011-07-02  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Accept any value for case-fold option in customize system
;; 
;; 	Previously the customize system accepted only symbols t or nil for 
;; 	case-fold option. Now all non-nil values are accepted and internally 
;; 	translated to t.
;; 
;; 2011-07-01  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Put quotes `...' around the example function add-word-to-dictionary
;; 
;; 2011-06-30  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Remove a duplicate sentence from wcheck-language-data doc
;; 
;; 2011-06-30  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Minor rephrasing in README
;; 
;; 2011-06-29  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	More accurate description for OPERATION-MODE of read-or-skip-faces
;; 
;; 2011-06-29  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Revert "Emphasize wcheck-language-data-defaults in read-or-skip-faces
;; 	doc"
;; 
;; 	This reverts commit 08c4c99a3611a2bbdc2a887ad741832aa43010a8.
;; 
;; 2011-06-29  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Revert "Add an explicit OPERATION-MODE "nil" in README's examples"
;; 
;; 	This reverts commit 0805ab15f05455676335c4f1f2644b6a2cbd1217.
;; 
;; 2011-06-29  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Revert "Clarify that OPERATION-MODE can be also be nil"
;; 
;; 	This reverts commit 1a9fa359b6c07d80c30c6214c7b8ea14a0c3396f.
;; 
;; 2011-06-29  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Clarify that OPERATION-MODE can be also be nil
;; 
;; 2011-06-29  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Add a note for programmers about wcheck-marked-text-at
;; 
;; 2011-06-29  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Rephrase examples
;; 
;; 2011-06-29  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Add an explicit OPERATION-MODE "nil" in README's examples
;; 
;; 2011-06-29  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Emphasize wcheck-language-data-defaults in read-or-skip-faces doc
;; 
;; 	Also, use explicit, rather than implicit, "nil" as the OPERATION-MODE in 
;; 	examples when "read everything" mode is wanted.
;; 
;; 2011-06-29  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Ensure case-fold-search=t in the example email detect function
;; 
;; 2011-06-29  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	README: Add a section about source code repository
;; 
;; 2011-06-29  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Version 2011.06.29
;; 
;; 2011-06-29  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	The "args" option can no longer be a string
;; 
;; 	The "args" option has been a quite long time now a list of strings but 
;; 	strings were still allowed for backward-compatibility. Now there are 
;; 	other areas in the code which necessarily break the 
;; 	backward-compatibility it's perhaps a good time to discontinue 
;; 	maintaining backward-compatibility in this option too.
;; 
;; 2011-06-29  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Rephrase documentation and comments
;; 
;; 2011-06-29  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	README: Move copyright and license text to the end
;; 
;; 2011-06-26  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Use plural "actions" with the install instruction's autoload
;; 
;; 2011-06-26  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Copy examples from wcheck-language-data doc to README
;; 
;; 2011-06-26  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Rephrase some parts of README
;; 
;; 2011-06-25  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Add \< and \> to regexps in "email" language example
;; 
;; 2011-06-25  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Rephrase grouping construct \(\) description in doc string
;; 
;; 2011-06-24  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Clarify the use of regexp-start, -body and -end
;; 
;; 2011-06-24  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Improve wcheck-language-data doc
;; 
;; 2011-06-22  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Fix a typo in wcheck-language-data doc string
;; 
;; 2011-06-22  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Clearer reference to the license text
;; 
;; 2011-06-22  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Escape \\< with \\=\\< in wcheck-language-data doc string
;; 
;; 2011-06-22  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Fix type in doc: "wcheck-parse-" "wcheck-parser-"
;; 
;; 2011-06-22  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Correction in wcheck-language-data doc: "variable" -> "value"
;; 
;; 2011-06-21  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Add README file
;; 
;; 2011-06-21  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	New feature: call custom functions from suggestion menu
;; 
;; 	As the suggestion menu is no longer only for spelling suggestions it is 
;; 	renamed to "actions". Configuration options have been renamed too: 
;; 	action-program etc.
;; 
;; 2011-05-28  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Korjataan wcheck-jump-backward -bugi
;; 
;; 	Hyppy taaksepäin puskurin alusta alkavaan maalattuun tekstiin ei 
;; 	toiminut. Vika oli funktiossa wcheck-overlay-previous.
;; 
;; 	
;; 
;; 2011-05-28  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Mainostetaan funktiota wcheck-marked-text-at julkiseen käyttöön
;; 
;; 	
;; 
;; 2011-05-28  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Tyylillistä hienosäätöä funktioon wcheck-make-overlay
;; 
;; 	
;; 
;; 2011-05-28  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Poistetaan turha #' lambda-rakenteiden edestä
;; 
;; 	
;; 
;; 2011-02-27  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Makron wcheck-with-language-data muuttujille paremmat nimet
;; 
;; 	
;; 
;; 2011-02-27  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Lisätään makroihin sisennysasetukset: (declare (indent ...))
;; 
;; 	Samalla myös sisennetään makrojen sisällä oleva koodi uudelleen. Tämä 
;; 	muutos koskee pelkkää ohjelmakoodin muotoilua. Se on nyt makrojen osalta 
;; 	helpommin luettava. Mitään muutoksia ohjelman toimintaa ei ole tehty.
;; 
;; 	
;; 
;; 2011-02-20  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Wcheck-versionumero: 2011.02.20
;; 
;; 	
;; 
;; 2011-02-20  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Uusi funktio: wcheck-process-running-p
;; 
;; 	
;; 
;; 2011-02-05  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Uusi ominaisuus: tarkistusohjelman tulosteen jäsennysfunktio
;; 
;; 	Nyt käyttäjä voi määrittää oman jäsenninfunktionsa tarkistusohjelman 
;; 	tulostetta varten. Omasta jäsenninfunktiosta on hyötyä silloin, kun 
;; 	tarkistusohjelma ei tulostakaan (maalattavaksi tarkoitettuja) 
;; 	merkkijonoja puhtaasti omilla riveillään. Esimerkiksi voikkospell- 
;; 	ohjelmaa varten voisi tehdä seuraavanlaisen kieliasetuksen:
;; 
;; 	    ("voikkospell"
;; 	    (program . "/usr/bin/voikkospell")
;; 	    (args "ignore_dot=1")
;; 	    (parser . (lambda ()
;; 			(let (words)
;; 			  (while (re-search-forward "^W: " nil t)
;; 			    (push (buffer-substring-no-properties
;; 				   (point) (line-end-position))
;; 				  words))
;; 			  words))))
;; 
;; 	
;; 
;; 2011-02-05  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Yksinkertaisempi silmukka: -read-strings ja -paint-strings
;; 
;; 	
;; 
;; 2011-01-09  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Versio 2011.01.09
;; 
;; 	
;; 
;; 2011-01-09  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Muutetaan alun kommentissa olevaa kuvausta
;; 
;; 	
;; 
;; 2011-01-09  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Muutetaan wcheck-mode-komennon kuvausta
;; 
;; 	
;; 
;; 2011-01-09  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Monipuolisempi read-or-skip-faces
;; 
;; 	Nyt asetuksissa major-tila voi olla paitsi symboli mutta myös lista 
;; 	symboleista. Näin ollen samalla kertaa voi määrittää useampia 
;; 	major-tiloja.
;; 
;; 	
;; 
;; 2011-01-09  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Käsitellään käyttäjän funktioissa tapahtuvat virheilmoitukset
;; 
;; 	Käyttäjän määrittämät oikoluku- ja oikolukuehdotusfunktiot saattavat 
;; 	kaatua virheeseen. Nyt mahdolliset virhetilanteet käsitellään ja 
;; 	annetaan siitä käyttäjälle ilmoitus.
;; 
;; 	
;; 
;; 2011-01-09  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Poistetaan maininta vanhasta muuttujasta wcheck-read-or-skip-faces
;; 
;; 	
;; 
;; 2011-01-09  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Käsitellään hyppytoiminnossa myös puskurin alku ja loppu
;; 
;; 	Jos puskurin alussa pyydetään hyppäämään taaksepäin tai lopussa 
;; 	eteenpäin, ei mitään etsittävää tietenkään ole. Nyt sekin tilanne 
;; 	käsitellään.
;; 
;; 	
;; 
;; 2011-01-09  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Poistetaan jump-reqistä turha hyppysuuntatieto
;; 
;; 	Hyppysuunta päätellään nyt start- ja bound-tietojen perusteella.
;; 
;; 	
;; 
;; 2011-01-09  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Hiotaan oikolukuehdotusten valikkoa
;; 
;; 	Oikolukuehdotusten tulosteesta poistetaan mahdolliset ohjausmerkit, 
;; 	kuten rivinvaihdot ja muut näkymättömät merkit. Myös välilyönnit 
;; 	poistetaan alusta ja lopusta. Tämä tekee ehdotusvalikosta siistimmän, 
;; 	mikäli se sisältää tällaisia epätavallisia merkkejä. Emacs-puskuriin 
;; 	korvattavaan merkkijonoon ei tietenkään kosketa.
;; 
;; 	
;; 
;; 2011-01-09  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Hiotaan virheilmoituksia
;; 
;; 	
;; 
;; 2011-01-09  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Muutetaan mode-line-tieto: "W:" ja kielen kaksi ensimmäistä merkkiä
;; 
;; 	
;; 
;; 2011-01-04  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Lisätään kuvaus muuttujaan wcheck-timer-paint-event-count-std
;; 
;; 	
;; 
;; 2011-01-04  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Muutetaan wcheck-timer-idlen määrittelytyyppi: defconst -> defvar
;; 
;; 	
;; 
;; 2011-01-04  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Tallennetaan jump-reqiin myös nykyinen ikkuna
;; 
;; 	Ikkunatiedon avulla voidaan siirtää ikkunakohtainen "point", vaikka 
;; 	kyseinen ikkuna ei olisikaan enää valittuna silloin, kun etsitty kohta 
;; 	löytyy wcheck-jump-*-komentojen jälkeen.
;; 
;; 	
;; 
;; 2011-01-04  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Muutetaan hylätyn korjausehdotusvalinnan virheilmoitusta
;; 
;; 	
;; 
;; 2011-01-04  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Uusi ominaisuus: etsi seuraava/edellinen merkitty kohta
;; 
;; 	Komennot ovat wcheck-jump-forward ja wcheck-jump-backward.
;; 
;; 	
;; 
;; 2011-01-04  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Ajetaan paint-event-sarja vain kerran
;; 
;; 	read-eventejä voi tulla useita peräkkäin, ja aiemmin jokainen niistä 
;; 	käynnisti oman paint-event-sarjansa. Nyt wcheck-timer-paint-event-run- 
;; 	rajapinnan avulla valvotaan, että paint-event-sarja ajetaan vain kerran.
;; 
;; 	
;; 
;; 2011-01-02  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Lisää virheenkäsittely condition-case-rakenteella
;; 
;; 	
;; 
;; 2011-01-02  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Poistetaan ylimääräinen parametri signal-funktiolta
;; 
;; 	
;; 
;; 2011-01-02  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Käytetään signal-funktiota myös funktiossa wcheck-send-strings
;; 
;; 	
;; 
;; 2011-01-02  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Hiotaan vielä oikolukuehdotusten ohjelmavirheenkäsittelyä
;; 
;; 	
;; 
;; 2011-01-02  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Siirretään run-with-idle-timer-toiminto omaan funktioonsa
;; 
;; 	
;; 
;; 2011-01-02  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	wcheck-define-condition-makroon lisää automatiikkaa
;; 
;; 	
;; 
;; 2011-01-02  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Siirretään signaalien määrittelyt samaan paikkaan
;; 
;; 	
;; 
;; 2011-01-02  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Käsitellään wcheck-moden käynnistysvirheet uudella tavalla
;; 
;; 	Virheiden käsittelyssä käytetään nyt virhesignaaleja ja condition-case- 
;; 	rakennetta. Samalla siirretään wcheck-moden käynnistys- ja 
;; 	sammutusoperaatiot omiin funktioihinsa.
;; 
;; 	
;; 
;; 2011-01-02  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Käsitellään oikolukuehdotusten virheet toisella tavalla
;; 
;; 	Käytetään Emacsin condition-case-rakennetta virheiden käsittelyyn. 
;; 	Esimerkiksi oikolukuehdotusohjelma palautti aiemmin symbolin "error", 
;; 	jos sen toiminnassa tuli virhe. Nyt se lähettää signaalin 
;; 	wcheck-suggestion-program-error, joka sitten käsitellään kutsuvassa 
;; 	funktiossa.
;; 
;; 	
;; 
;; 2011-01-02  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Lisätään makro wcheck-define-condition
;; 
;; 	Tällä makrolla voi helposti lisätä omia virheilmoitusluokkia.
;; 
;; 	
;; 
;; 2011-01-02  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Siirretään makrojen määritykset samaan paikkaan
;; 
;; 	
;; 
;; 2011-01-01  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Päivitetään Copyright-vuosi ja versionumero: 2011.01.01
;; 
;; 	
;; 
;; 2011-01-01  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Abstrahoidaan read-key-toiminto funktioon wcheck-read-key
;; 
;; 	Funktio read-key on vain uusissa Emacsin versioissa; aiemmissa pitänee 
;; 	käyttää funktiota read-char. Tämä muutos siirtää merkinlukukyselyn omaan 
;; 	funktioonsa (wcheck-read-key), joka käyttää joko read-keytä tai 
;; 	read-charia.
;; 
;; 	
;; 
;; 2011-01-01  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Siirretään wcheck-loop-over-* makrojen yhteiset osat omaksi makroksi
;; 
;; 	
;; 
;; 2010-12-29  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Lähetetään merkkijonot tarkistinfunktiolle vain, jos niitä ylipäätään on
;; 
;; 	
;; 
;; 2010-12-29  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Abstrahoidaan ja järjestellään koodia uudelleen
;; 
;; 	Funktioiden nimiä on muutettu. Koodia on abstrahoitu lisäämällä makrot 
;; 	wcheck-loop-over-*-reqs ja siirtämällä puskurikohtaisia tietoja 
;; 	wcheck-buffer-data-* -rajapinnan taakse.
;; 
;; 	wcheck-buffer-data-* -rajapinta käyttää nyt sisäisesti vektoreita; 
;; 	aiemmin se käytti property-listaa.
;; 
;; 	Ulkoisesti ohjelman toiminta ei muutu.
;; 
;; 	
;; 
;; 2010-12-29  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Dokumentteja ajan tasalle: tarkistusohjelma voi olla myös funktio
;; 
;; 	
;; 
;; 2010-12-29  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Muutetaan monet word-sanat string-sanoiksi
;; 
;; 	Wcheck alkoi sanojentarkistusohjelmana, mutta se oikeastaan tukee mitä 
;; 	tahansa merkkijonoja. Siispä vaihdetaan ohjelmakoodiinkin muuttujien ja 
;; 	funktioiden nimet käyttämään string-sanaa.
;; 
;; 	
;; 
;; 2010-12-29  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Uusi macro: wcheck-with-language-data
;; 
;; 	Tämä vähentää ja selkeyttää toistuvia
;; 
;; 	    (let ((... (wcheck-query-language-data ...))
;; 		 (... (wcheck-query-language-data ...))
;; 		 (... (wcheck-query-language-data ...))
;; 		 ...)
;; 	      ...)
;; 
;; 	-rakenteita.
;; 
;; 	
;; 
;; 2010-12-27  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Päivitetään versionumero: 2010.12.27
;; 
;; 	
;; 
;; 2010-12-27  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Muutetaan virheilmoitus yhdenmuotoiseksi muiden vastaavien kanssa
;; 
;; 	
;; 
;; 2010-12-27  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Vaihdetaan muuttujan nimi paremmin tarkoitusta kuvaamvaksi
;; 
;; 	Kyse on funktion wcheck-get-suggestions paikallisesta muuttujasta.
;; 
;; 	
;; 
;; 2010-12-27  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Muutetaan wcheck-customize-ryhmän otsikko
;; 
;; 	
;; 
;; 2010-12-27  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Muuttujan dokumentin kielellistä säätöä
;; 
;; 	
;; 
;; 2010-12-27  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Asennusohjeen kielellistä säätöä
;; 
;; 	
;; 
;; 2010-12-27  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Muuttujien dokumentit ja esimerkkiasetukset ajan tasalle
;; 
;; 	
;; 
;; 2010-12-27  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Customize tukemaan uusia read-or-skip-faces-toimintoja
;; 
;; 	
;; 
;; 2010-12-27  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Muutetaan wcheck-query-language-data-funktion rakennetta
;; 
;; 	Suurimmat muutokset ovat sisäisessä toiminnassa, mutta myös ulkoisessa 
;; 	toiminnassa on pieni muutos. Jos kysytään asetusta read-or-skip-faces, 
;; 	palautetaan yhdistettynä listana sekä kielikohtaiset että 
;; 	oletusasetukset. Tämä lisää wcheck-modeen uuden ominaisuuden: 
;; 	kielikohtaisista read-or-skip-faces-asetuksista puuttuvia asetuksia 
;; 	luetaan oletusasetuksista. Aiemmin kielikohtaisten read-or-skip-faces- 
;; 	asetusten lisäksi ei luettu oletusasetuksia, vaikka kyseinen kieli ei 
;; 	määrittäisi face-asetuksia jollekin major-tilalle.
;; 
;; 	
;; 
;; 2010-12-27  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Muutetaan face-predikaattitoiminto selkeämmäksi
;; 
;; 	Piilotetaan enemmän koodia wcheck-generate-face-predicate -funktioon.
;; 
;; 	
;; 
;; 2010-12-27  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Customize-asetusten hienosäätöä
;; 
;; 	
;; 
;; 2010-12-27  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Lisätään ;;;###autoload
;; 
;; 	
;; 
;; 2010-12-27  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Vaihdetaan esimerkkiin "Finnish" entisen "suomi":n tilalle
;; 
;; 	
;; 
;; 2010-12-26  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Uusi ominaisuus: käyttäjä voi nyt vaikuttaa oletusasetuksiin
;; 
;; 	Lisätään käyttäjälle muuttuja wcheck-language-data-defaults, jolla 
;; 	voidaan tarjota kaikille kielikohtaisille asetuksille oletusasetukset. 
;; 	Samalla myös poistetaan muuttuja wcheck-read-or-skip-faces ja siirretään 
;; 	se muuttujan wcheck-language-data-defaults osaksi. Se on asetus nimeltä 
;; 	read-or-skip-faces.
;; 
;; 	Lisäksi nyt on mahdollista kielikohtaisesti määrittää 
;; 	read-or-skip-faces. Kielikohtaista asetusta käytetään ensisijaisesti, 
;; 	mutta mikäli sitä ei ole määritelty, käytetään mahdollista 
;; 	oletusasetusta.
;; 
;; 	
;; 
;; 2010-12-26  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Muutetaan korjausehdotuspuskurin ulkonäköä
;; 
;; 	Aiemmin korjausehdotus tulostettiin muodossa "(1) sana". Nyt ensimmäinen 
;; 	sulje jätetään pois.
;; 
;; 	
;; 
;; 2010-12-26  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Tulostetaan [Empty string], mikäli korvattava teksti on tyhjä merkkijono
;; 
;; 	
;; 
;; 2010-12-26  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Uusi ominaisuus: oikolukuohjelma voi olla myös Emacs Lisp -funktio
;; 
;; 	Nyt tekstintarkistusohjelmaksi ja korjausehdotusohjelmaksi voi 
;; 	määritellä myös funktion. Ulkoista ohjelmaa ei välttämättä tarvita. 
;; 	Ominaisuus voi olla hyödyllinen, jos haluaa dynaamisesti muuttaa 
;; 	oikoluvun toimintaa.
;; 
;; 	
;; 
;; 2010-12-23  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Käytetään Emacs Lisp -tiedostojen vakiintuneita otsakkeita
;; 
;; 	
;; 
;; 2010-10-30  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	wcheck-choose-suggestion-minibuffer siirtää kursorin rivin loppuun
;; 
;; 	Kun korjausehdotusten lista tulostetaan puskuriin, siirretään kursori 
;; 	lopuksi puskurin ensimmäisen rivin loppuun. Aiemmin kursori siirrettiin 
;; 	puskurin alkuun, mikä aiheutti pienen esteettisen ongelman, jos 
;; 	show-paren-mode on päällä. Nimittäin puskurin alussa olevat suljemerkit
;; 	"(1)" näytettiin värjättynä, koska kursori oli toisen sulkeen päällä. 
;; 	Siirtämällä kursori rivin loppuun sulkeiden värjäys show-paren-modessa 
;; 	saadaan estettyä.
;; 
;; 	
;; 
;; 2010-10-30  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Tarkennetaan Ispell-tulosteen käsittelyä
;; 
;; 	Nyt jäsennenfunktio wcheck-parse-suggestions-ispell huomioi Ispellin
;; 	&-rivin lukumääräkentän: korjausehdotuksista kerätään vain sen osoittama 
;; 	sanojen määrä.
;; 
;; 	
;; 
;; 2010-08-07  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Korjataan vääränlainen sisennys wcheck-language-datan kuvauksessa
;; 
;; 	
;; 
;; 2010-08-05  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Hyödynnetään vaakasuuntainen tila paremmin oikolukuehdotuspuskurissa
;; 
;; 	Tekstipohjaisen oikolukuehdotuspuskurin vaakasuuntainen tila 
;; 	hyödynnetään nyt viimeistä merkkiä myöten, kun puskuriin ladotaan 
;; 	korjausehdotuksia.
;; 
;; 	
;; 
;; 2010-08-05  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Täydennetään funktion wcheck-spelling-suggestions kuvausta
;; 
;; 	
;; 
;; 2010-08-05  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Lisätään autoload-komento alun asennusohjeisiin (-spelling-suggestions)
;; 
;; 	
;; 
;; 2010-08-04  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Täydennetään funktioiden kuvauksia ym. pientä järjestelyä
;; 
;; 	
;; 
;; 2010-08-04  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Välitetään hiiritapahtuma oikolukuehdotusfunktiolle
;; 
;; 	Tämän muutoksen tarkoitus on estää funktiota x-popup-menu poistumasta 
;; 	Emacsin Quitin avulla. Jos funktiolle x-popup-menu antaa ensimmäiseksi 
;; 	argumentiksi t:n (kuten aiemmin oli) ja käyttäjä ei valitse hiirellä 
;; 	mitään, funktio ei palauta arvoa vaan poistuu Quitilla. Samalla myöskään 
;; 	muuta oikolukuehdotuskoodia (wcheck-spelling-suggestions) ei ajeta. 
;; 	Tästä on seurannut sellainen haitta, että puskurimerkit (marker) jäävät 
;; 	puskuriin eikä roskienkeruu niitä koskaan poista. Ainakin teoriassa 
;; 	suuri määrä puskurimerkkejä voi hidastaa tekstin muokkausta.
;; 
;; 	Nyt kun hiiritapahtuma välitetään x-popup-menulle saakka, kyseinen 
;; 	funktio ei keskeydy Quitilla vaan palauttaa arvon nil (mikäli käyttäjä 
;; 	ei valitse mitään). Näin ollen muukin oikolukuehdotuskoodi suoritetaan 
;; 	loppuun saakka ja puskurimerkit saadaan poistettua.
;; 
;; 	Lisäksi funktiota wcheck-spelling-suggestions on järjestetty siten, että 
;; 	sepalauttaa nyt puskurissa korvatun merkkijonon tai nil, mikäli mitään 
;; 	ei korvattu.
;; 
;; 	
;; 
;; 2010-08-04  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Varmempi tapa tutkia, onko hiirivalikko käytössä
;; 
;; 	Vaihdetaan muuttujan window-system testauksen tilalle funktio 
;; 	display-popup-menus-p, joka on oikea ja paljon luotettavampi tapa 
;; 	tutkia, onko hiirivalikko käytettävissä.
;; 
;; 	
;; 
;; 2010-08-03  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Dokumenttien kielellistä hienosäätöä
;; 
;; 	
;; 
;; 2010-08-03  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Vaihdetaan näppäimenlukufunktiota read-char-exclusive -> read-key
;; 
;; 	
;; 
;; 2010-08-03  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Varmatoimisempi wcheck-choose-suggestion-minibuffer
;; 
;; 	Varmistetaan, ettei yritetä avata liian korkeaa ikkunaa ja siten 
;; 	aiheuteta virheilmoitusta.
;; 
;; 	
;; 
;; 2010-08-03  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Lisätään autoload-vihje funktiolle wcheck-spelling-suggestions
;; 
;; 	
;; 
;; 2010-08-03  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Järjestellään uudelleen funktiota wcheck-choose-suggestion-minibuffer
;; 
;; 	
;; 
;; 2010-08-03  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Tallennetaan täsmäystiedot jäsenninfunktion kutsumisen ajaksi
;; 
;; 	
;; 
;; 2010-08-03  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Hiotaan dokumentteja ja merkkijonoja
;; 
;; 	
;; 
;; 2010-08-03  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Muutetaan jäsenninfunktion nimi: wcheck-parse-suggestions-lines
;; 
;; 	
;; 
;; 2010-08-02  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Hiotaan tekstipohjaista oikolukuehdotusvalikkoa
;; 
;; 	
;; 
;; 2010-08-02  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Täydennetään funktioiden kuvauksia
;; 
;; 	
;; 
;; 2010-08-02  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Uusi ominaisuus: korjausehdotukset väärinkirjoitetuille sanoille
;; 
;; 	Nyt on mahdollista määritellä kielikohtaisesti ohjelma, joka palauttaa 
;; 	sanalle korjausehdotuksia. Käyttäjä voi myös määritellä funktion, joka 
;; 	jäsentää ohjelman tulosteen ja etsii siitä korjausehdotukset. Yleisimmin 
;; 	tarvittavat jäsenninfunktiot on jo määritelty, muun muassa "ispell -a"
;; 	-tyyppiselle tulosteelle. Löydetyt korjausehdotukset näytetään 
;; 	käyttäjälle graafisena tai tekstivalikkona riippuen siitä, ajetaanko 
;; 	komento hiiren vai näppäimistön avulla.
;; 
;; 	
;; 
;; 2010-08-02  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Lisätään predikaattifunktio wcheck-list-of-strings-p
;; 
;; 	
;; 
;; 2010-08-02  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Tallennetaan ja palautetaan säännöllisten lausekkeiden täsmäystiedot
;; 
;; 	Laitetaan wcheck-moden sisäisten etsimistoimintojen ympärille
;; 	(save-match-data ...), jolloin mahdolliset käyttäjän jäljiltä olevat 
;; 	täsmäystiedot säilyvät.
;; 
;; 	
;; 
;; 2010-08-02  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Varmistetaan, ettei välilyöntejä tulkita väärin regexpissä
;; 
;; 	Emacs Lispissä muuttuja search-spaces-regexp vaikuttaa siihen, kuinka 
;; 	säännöllisessä lausekkeessa oleva välilyönti tulkitaan. Asetetaan tämä 
;; 	muuttuja nil-tilaan, jotta välilyönnit täsmäävät vain itseensä.
;; 
;; 	
;; 
;; 2010-03-12  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Optimoidaan hieman funktiota wcheck-collect-faces
;; 
;; 	
;; 
;; 2010-03-12  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Siirretään ajastimen käynnistys ja sammutus omiin funktioihinsa
;; 
;; 	
;; 
;; 2010-02-24  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Toteutetaan wcheck-combine-overlapping-areas järkevämmin
;; 
;; 	Poistetaan turha APPEND, joka edellisessä toteutuksessa kopioi joka 
;; 	kerta listan uudelleen. Muutetaan se nopeammaksi CONSiksi, joka 
;; 	täydentää samaa listaa. Roskienkeruulle jää vähemmän töitä. :-)
;; 
;; 	
;; 
;; 2010-02-22  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Uusi ominaisuus: mahdollisuus lukea vain tiettyjä alueita puskurista
;; 
;; 	Tämä muutos lisää wcheck-tilaan uuden ominaisuuden, joka antaa 
;; 	käyttäjälle mahdollisuuden valita, mitä tekstialueita puskurista 
;; 	luetaan. Toiminto perustuu puskurin face-tietoihin eli siihen, miten 
;; 	käytössä oleva major-tila on merkinnyt puskurin sisältöä. Asetukset 
;; 	määritellään muuttujassa wcheck-read-or-skip-faces, ja sille on myös 
;; 	helppokäyttöinen customize-käyttöliittymä.
;; 
;; 	Tärkein käyttökohde tälle ominaisuudelle on rajata oikoluku vain 
;; 	sellaiseen osaan tekstistä, joka on kirjoitettu ihmiskielellä. 
;; 	Esimerkiksi sähköpostissa (message-mode) voidaan rajata oikoluku vain 
;; 	varsinaiseen viestiosaan ja Subject-kenttään. Ohjelmointitiloissa 
;; 	voidaan oikolukea vain kommentit ja funktioiden dokumenttiosat.
;; 
;; 	
;; 
;; 2010-02-22  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Lisätään matalan tason funktiot face-tietojen käsittelyyn
;; 
;; 	Näiden funktioiden tarkoitus on toimia varsinaisen työn tekijänä uudelle 
;; 	ominaisuudelle, joka mahdollistaa ainoastaan tiettyjen alueiden 
;; 	lukemisen puskurista. Kyseinen uusi ominaisuus lisätään käyttöön 
;; 	myöhemmin.
;; 
;; 	
;; 
;; 2010-02-22  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Kielellistä hienosäätöä funktioiden kuvauksissa
;; 
;; 	
;; 
;; 2010-02-22  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Käytetään matalan tason customize-tyyppejä wcheck-language-data'ssa
;; 
;; 	Muutetaan muuttujan wcheck-language-data customize käyttämään matalan 
;; 	tason tyyppejä, kuten repeat ja list, aiemman alist-tyypin sijaan. Tämä 
;; 	ei vaikuta toimintaan mitenkään, mutta koodi on ehkä hieman selkeämpi.
;; 
;; 	
;; 
;; 2010-02-22  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Korjataan muuttujan wcheck-language-data kuvausta
;; 
;; 	
;; 
;; 2010-02-22  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Käytetään #'-lukijamakroa funktion edessä
;; 
;; 	
;; 
;; 2010-02-13  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Hyväksytään args-asetus vain, jos listan jäsenet ovat merkkijonoja
;; 
;; 	
;; 
;; 2010-02-13  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Muutetaan ulkoisen ohjelman argumentit listaksi
;; 
;; 	Aiemmin ulkoiselle ohjelmalle on määritelty argumentit merkkijonona 
;; 	muodossa (args . "-l -d fi_FI"). Tämä merkkijono on sitten 
;; 	automaattisesti hajotettu listaksi ("-l" "-d" "fi_FI"), jotta ulkoiselle 
;; 	ohjelmalle menevät argumentit saadaan erotettua.
;; 
;; 	Nyt käyttäjä määrittelee argumentit jo alun alkaen listana, jossa 
;; 	jokainen elementti on yksi argumentti. Tämä on varmatoimisempi, koska ei 
;; 	olla enää riippuvaisia funktiosta, joka hajottaa argumenttimerkkijonon 
;; 	listaksi. Teoriassa hajotusfunktio ei aina tuottaisi sellaista tulosta, 
;; 	jota käyttäjä odottaa.
;; 
;; 	Edelleenkin vanha määrittelytapa (yksi merkkijono) toimii, joten 
;; 	yhteensopivuutta ei ole rikottu. Merkkijonona määritellyt argumentit 
;; 	muutetaan kuitenkin sisäisesti listaksi, ja myös customize ohjaa 
;; 	käyttäjää tekemään sen listaksi.
;; 
;; 	
;; 
;; 2010-02-13  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Sallitaan connection-argumentille arvoksi sekä "t" että "pty"
;; 
;; 	
;; 
;; 2010-02-13  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Korjataan muuttujan wcheck-language-data kuvausta
;; 
;; 	
;; 
;; 2010-02-10  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Tarkennetaan muuttujan wcheck-language kuvausta
;; 
;; 	
;; 
;; 2010-01-16  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Päivitetään Copyright-vuosi
;; 
;; 	
;; 
;; 2010-01-16  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Tehdään kielestä puskurikohtainen aina, kun oikoluku menee päälle
;; 
;; 	Aiemmin kielimuuttujaa ei muutettu puskurikohtaiseksi automaattisesti, 
;; 	joten globaalin oletuskielen muuttaminen saattoi muuttaa joidenkin 
;; 	puskurien kieltä. Se ei onneksi vaikuttanut oikoluvun ollessa päällä, 
;; 	mutta seuraavalla kerralla oikolukukieli saattoi olla eri. Vika on nyt 
;; 	korjattu.
;; 
;; 	Lisäksi kielenvaihtokomentoa on muutettu siten, että globaalin 
;; 	oletuskielen muuttaminen ei enää vaikuta mihinkään muuhun kuin muuttujan 
;; 	wcheck-language globaaliin arvoon. Ennen se saattoi muuttaa 
;; 	puskurikohtaista oikolukukieltäkin.
;; 
;; 	
;; 
;; 2009-08-23  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Yksinkertaistetaan kielenvaihtofunktiota
;; 
;; 	Pieni toiminnallinen muutos on se, että mikäli kieltä vastaava ohjelma 
;; 	ei olekaan ajettava, wcheck-mode poistetaan päältä myös kutsuttaessa 
;; 	funktiota ei-interaktiivisesti. Aiemmin se poistettiin päältä vain, kun 
;; 	funktiota käytetään interaktiivisesti.
;; 
;; 	
;; 
;; 2009-07-27  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Käytetään varmuuden vuoksi ei-destruktiivisia keinoja
;; 
;; 	
;; 
;; 2009-07-26  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Käytetään vain PLIST-PUT-funktion arvoa eikä sen sivutuotetta
;; 
;; 	
;; 
;; 2009-07-26  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Revert "Yksinkertaistetaan tietokantafunktiota"
;; 
;; 	This reverts commit r2805
;; 
;; 	Pelkkä PLIST-PUT-operaatio ei olekaan täysin luotettava Emacs Lispissä. 
;; 	Common Lisp on kyllä parempi...
;; 
;; 	
;; 
;; 2009-07-26  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Korjataan kirjoitusvirhe; hienosäätöä
;; 
;; 	
;; 
;; 2009-07-26  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Yksinkertaistetaan tietokantafunktiota
;; 
;; 	Funktio plist-put kirjoittaa tiedon suoraan alkuperäiseen 
;; 	property-listaan, joten muita komentoja ei tarvita.
;; 
;; 	
;; 
;; 2009-07-25  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Vielä yksi (when (buffer-live-p ...) ...) -tarkistus
;; 
;; 	
;; 
;; 2009-07-25  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Tehostetaan append-komentoja
;; 
;; 	
;; 
;; 2009-07-25  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Korjataan kirjoitusvirhe
;; 
;; 	
;; 
;; 2009-07-25  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Hienosäätöä customize-valikkoon
;; 
;; 	
;; 
;; 2009-07-25  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Siirretään funktion wcheck-receive-words paikkaa
;; 
;; 	Nyt tämä sekä ajastinfunktiot sijaitsevat loogisessa järjestyksessä sen 
;; 	suhteen, miten funktioita kutsutaan oikoluvun ollessa käynnissä.
;; 
;; 	
;; 
;; 2009-07-25  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Siirretään ylläpitotehtäviä funktioon, jossa niitä muutenkin tehdään
;; 
;; 	
;; 
;; 2009-07-25  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Parannetaan virhetilanteiden tarkistusta ja virheilmoituksia
;; 
;; 	
;; 
;; 2009-07-25  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Yksinkertaistetaan kommenttia
;; 
;; 	
;; 
;; 2009-07-24  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Tehostetaan sanojen lukemista
;; 
;; 	Lisp-kielessä on paljon hitaampaa lisätä elementtejä listan loppuun kuin 
;; 	listan alkuun. Aiemmin puskurista luetut sanat kerättiin listamuuttujan 
;; 	perään, mikä on siis hitaampaa. Koska tässä tapauksessa sanojen 
;; 	järjestyksellä ei ole mitään väliä, tehostetaan toimintaa lisäämällä 
;; 	luetut sanat listamuuttujan alkuun.
;; 
;; 	
;; 
;; 2009-07-23  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Kielellistä säätöä kommentteihin ja funktion kuvaukseen
;; 
;; 	Korjataan kirjoitusvirhe
;; 
;; 	
;; 
;; 2009-07-23  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Poistetaan turha roina funktiosta wcheck-send-words
;; 
;; 	
;; 
;; 2009-07-23  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Välitetään kaikki tiedot funktiolle wcheck-make-overlay
;; 
;; 	Tätä funktiota kutsutaan sen verran tiheään tahtiin, että funktio 
;; 	kannattaa pitää mahdollisimman yksinkertaisena ja välttää turhaa työtä. 
;; 	Siispä tarpeelliset tiedot välitetään nyt funktiolle parametreina, 
;; 	jottei niitä tarvitse joka kerta erikseen selvittää. Tällä perutaan 
;; 	muutos r2783.
;; 
;; 	
;; 
;; 2009-07-22  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Kielellistä hienosäätöä dokumentteihin
;; 
;; 	
;; 
;; 2009-07-22  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Pienennetään wcheck-moden reagointiviivettä 0,4 sekuntiin
;; 
;; 	
;; 
;; 2009-07-22  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Lisätään koodin väliotsikoita
;; 
;; 	
;; 
;; 2009-07-22  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	wcheck-receive-words varmemmaksi
;; 
;; 	Lisätään tarkistus, että puskuri on vielä olemassa.
;; 
;; 	
;; 
;; 2009-07-22  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Muutetaan tietokannan kyselyfunktio geneerisemmäksi
;; 
;; 	
;; 
;; 2009-07-22  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Muutetaan wcheck-mode ja -change-language käyttämään uutta tietokantaa
;; 
;; 	
;; 
;; 2009-07-22  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Asetetaan ajastinfunktiot hakemaan tiedot uudesta tietokannasta
;; 
;; 	
;; 
;; 2009-07-22  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Muutetaan lukemis- ja maalauspyyntöfunktioiden nimet
;; 
;; 	
;; 
;; 2009-07-22  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Muutetaan wcheck-make-overlay hakemaan kielitiedot tietokannasta
;; 
;; 	
;; 
;; 2009-07-22  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Muutetaan wcheck-paint-words hakemaan kielitiedot tietokannasta
;; 
;; 	
;; 
;; 2009-07-22  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Muutetaan wcheck-send-words hakemaan kielitiedot tietokannasta
;; 
;; 	
;; 
;; 2009-07-22  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Muutetaan wcheck-read-words hakemaan kielitiedot tietokannasta
;; 
;; 	
;; 
;; 2009-07-22  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Siirretään koukkujen asennus omiin funktioihinsa
;; 
;; 	
;; 
;; 2009-07-22  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Poistetaan turha wcheck-end-process
;; 
;; 	
;; 
;; 2009-07-22  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Muutetaan prosessit puskurikohtaisiksi
;; 
;; 	Aiemmin ulkoiset prosessit ovat olleet kielikohtaisia. Se toimii yleensä 
;; 	mutta aiheuttaa eräissä tilanteissa sen, että ulkoisen prosessin 
;; 	palauttamat sanat päätyvät väärälle puskurille eikä sanoja merkitä siinä 
;; 	puskurissa, missä pitäisi. Nyt kun prosessit ovat puskurikohtaisia, ei 
;; 	tällaista ongelmaa ole.
;; 
;; 	Samalla muutetaan prosessinkäsittelyfunktiot käyttämään uutta puskurien, 
;; 	prosessien ja kielten tietokantaa.
;; 
;; 	
;; 
;; 2009-07-22  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	wcheck-update-buffer-data uuden tietokannan mukaiseksi
;; 
;; 	
;; 
;; 2009-07-22  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Lisätään matalan tason tietokantafunktiot
;; 
;; 	
;; 
;; 2009-07-22  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Siirretään overlayt uuteen paikkaan
;; 
;; 	
;; 
;; 2009-07-22  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Poistetaan turhia testejä funktiosta wcheck-combine-two
;; 
;; 	
;; 
;; 2009-07-19  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Korjataan kirjoitusvirhe funktion kuvauksesta
;; 
;; 	
;; 
;; 2009-07-18  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Merkitään funtio-objektit #'-merkillä
;; 
;; 	Tämä ei ole pakollista Emacs-Lispissä, mutta käytäntö ei ole 
;; 	pahitteeksi, koska päällepäin näkyy, että objektin on tarkoitus viitata 
;; 	funktioon. Common Lispiin tämä kuuluu kiinteämmin.
;; 
;; 	
;; 
;; 2009-07-18  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Maalataan sanat tiedossa olevan tekstialueen perusteella
;; 
;; 	Ennen funktio wcheck-paint-words maalasi sanalistan sanat kaikissa 
;; 	ikkunoissa, jotka kuuluvat kyseiselle puskurille. Nykyään sanojen 
;; 	lukemisen jäljiltä on jo tiedossa ikkunoissa näkyvät puskurin alueet, 
;; 	joten käytetään samaa tietoa myös sanojen maalaamisessa. Se hieman 
;; 	nopeuttaa toimintaa, koska ei tarvitse selata ikkunoita läpi lainkaan.
;; 
;; 	
;; 
;; 2009-07-18  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Luetaan vain tarpeelliset tekstialueet
;; 
;; 	Aiemmin wcheck-tila luki sisällön kaikista ikkunoista, myös silloin, kun 
;; 	kaikki ikkunat näyttivät täysin samaa kohtaa puskurissa. Tämän muutoksen 
;; 	jälkeen ikkunoiden sisältöä ei lueta suoraa vaan ensin kerätään tieto, 
;; 	mitä puskurin aluetta mikäkin ikkuna näyttää. Sen jälkeen yhdistetään 
;; 	aluetiedot ja luetaan tekstialueet. Näin poistuu turha päällekkäisyys ja 
;; 	toiminta nopeutuu huomattavasti silloin, kun sama puskuri on näkyvissä 
;; 	useassa ikkunassa.
;; 
;; 	
;; 
;; 2009-07-18  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Kielellistä hienosaatöä muuttujan wcheck-language-data ohjeeseen
;; 
;; 	
;; 
;; 2009-07-17  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Customize-tyypin "face" tilalle "symbol"
;; 
;; 	
;; 
;; 2009-07-17  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Lisätään tarkistus ulkoisen prosessin päälläolosta
;; 
;; 	
;; 
;; 2009-07-17  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Poistetaan wcheck-mode päältä käyttämällä parametria -1 eikä 0
;; 
;; 	Molemmat toimivat mutta -1 on määritelty selvemmin.
;; 
;; 	
;; 
;; 2009-07-17  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Hajotetaan merkkijono funktiolla split-string-and-unquote
;; 
;; 	Ulkoiselle ohjelmalle menevät komentoriviparametrit hajotettiin aiemmin 
;; 	funktiolla split-string. Tähän käyttöön sen huonona puolena on, että se 
;; 	vain jakaa merkkijonon välilyöntien kohdalta eikä ota huomioon 
;; 	mahdollisia lainausmerkeissä olevia lausekkeita. Näin käyttäjän 
;; 	tarkoittamat parametrit eivät välttämättä mene oikein ulkoiselle 
;; 	ohjelmalle. Funktio split-string-and-unquote ottaa huomioon 
;; 	lainausmerkit ja \-merkin.
;; 
;; 	
;; 
;; 2009-07-09  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Määritellään eräät muuttujat myös ajon aikana, ei pelkästään kääntämisen
;; 
;; 	
;; 
;; 2009-07-06  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Muutos muuttujan kuvauksessa: buffer-specific --> buffer-local
;; 
;; 	
;; 
;; 2009-07-06  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Muotoillaan uudelleen muuttujan wcheck-language-data dokumenttia
;; 
;; 	
;; 
;; 2009-07-06  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Täsmennetään muuttujan wcheck-language-data kuvausta
;; 
;; 	
;; 
;; 2009-07-06  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Siirretään "syntax" ja "face" samaan järjestykseen kuin customizessa
;; 
;; 	
;; 
;; 2009-07-06  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Lisätään asetus "connection"
;; 
;; 	Tämän perusteella asetetaan muuttuja process-connection-type kyseiselle 
;; 	kielelle.
;; 
;; 	
;; 
;; 2009-07-06  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Lisätään asetus "case-fold"
;; 
;; 	Tällä voi vaikuttaa kielikohtaisesti, että otetaanko kirjainkoko 
;; 	huomioon.
;; 
;; 	
;; 
;; 2009-07-06  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Uudenlainen customize-valikko wcheck-language-datalle
;; 
;; 	Uusi valikko ei pidä esillä kaikkia mahdollisia kielikohtaisia asetuksia 
;; 	vaan monet asetukset poimitaan erikseen alavalikosta. Tämä säästää tilaa 
;; 	customize-sivulla.
;; 
;; 	
;; 
;; 2009-07-06  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	wcheck-query-language-data tarkistaa asetusten oikeellisuuden
;; 
;; 	Funktio wcheck-query-language-data asetetaan nyt tarkistamaan 
;; 	muuttujassa wcheck-language-data määriteltyjen asetusten oikeellisuus. 
;; 	Mikäli avainsanaa vastaava arvo on väärää tyyppiä, voidaan palauttaa 
;; 	oletusasetus.
;; 
;; 	Tämän muutoksen seurauksena on myös mahdollista antaa avainsanalle 
;; 	arvoksi "nil". Aiemmin funktio wcheck-query-language-data oletti, että 
;; 	jos arvo on "nil", niin arvoa ei ole määritelty lainkaan ja palautettiin 
;; 	mahdollisesti oletusarvo. Tätä ominaisuutta ei vielä hyödynnetä 
;; 	mihinkään, eikä tämä puute ole aiheuttanut bugeja ohjelman aiempaan 
;; 	toimintaan.
;; 
;; 	
;; 
;; 2009-07-04  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Pientä wcheck-mode-tekstin uudelleenmuotoilua
;; 
;; 	
;; 
;; 2009-06-27  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	"function" on tarpeeton, koska "lambda" sisältää sen jo
;; 
;; 	
;; 
;; 2009-06-27  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Muutetaan moodi-ilmaisin pieniksi kirjaimiksi: Wck --> wck
;; 
;; 	Pienellä kirjoitetut moodin ilmaisimet tuntuvat olevan melko yleinen 
;; 	käytäntö puskurikohtaisissa minor-tiloissa.
;; 
;; 	
;; 
;; 2009-06-27  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Poistetaan turha rivinvaihto prosessille lähetettävästä merkkijonosta
;; 
;; 	
;; 
;; 2009-06-27  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	(mapconcat 'concat ...) --> (mapconcat 'identity ...)
;; 
;; 	Ulkoisesti toiminta ei muutu, mutta se on sisäisesti hieman 
;; 	yksinkertaisempi.
;; 
;; 	
;; 
;; 2009-06-19  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Muutetaan syntax-taulukon tyyppiä customize-järjestelmässä
;; 
;; 	Nyt tyypiksi oletetaan muuttuja eikä symboli, joka on yleisempi käsite.
;; 
;; 	
;; 
;; 2009-06-19  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Muutetaan wcheck-language-data-defaults vakioksi (defconst)
;; 
;; 	Samalla siirretään se toiseen paikkaan tiedoston sisällä.
;; 
;; 	
;; 
;; 2009-06-19  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Yksinkertaisempi tapa tarkistaa tekstin invisible-ominaisuus
;; 
;; 	
;; 
;; 2009-06-15  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Hiljennetään byte-compiler määrittelemällä muuttujia
;; 
;; 	
;; 
;; 2009-06-15  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Tarkennetaan funktion kuvausta
;; 
;; 	
;; 
;; 2009-05-31  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Poistetaan turhat format-funktiot
;; 
;; 	message-funktio sisältää jo format-funktion toiminnon. Ei tarvitse 
;; 	käyttää molempia yhdessä.
;; 
;; 	
;; 
;; 2009-05-27  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Ajetaan maalaustoiminto varmuuden vuoksi vielä yhden kerran lisää
;; 
;; 	
;; 
;; 2009-05-27  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Käytetään muuttujan kuvauksessa symbolia `wcheck-mode'
;; 
;; 	Lainausmerkeissä (`...') olevan Lisp-symbolin käyttö tekee symbolista 
;; 	hyperlinkin, kun muuttujan kuvausta lukee "C-h v" -komennolla. Tämä on 
;; 	suositeltava käytäntö muuttujien ja funktioiden kuvauksissa.
;; 
;; 	
;; 
;; 2009-05-26  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Pienennetään wcheck-moden reagointiviivettä
;; 
;; 	
;; 
;; 2009-05-26  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Minibufferille oma virheilmoitus
;; 
;; 	Kun wcheck-moden yrittää kytkeä päälle minibufferissa, annetaan siitä 
;; 	erityinen virheilmoitus.
;; 
;; 	
;; 
;; 2009-05-26  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Uusi nimi funktiolle: ...-overlay-word -> ...-changed-overlay
;; 
;; 	
;; 
;; 2009-05-26  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Loputkin funktioiden kuvaukset ja koodin kommentit englanniksi
;; 
;; 	
;; 
;; 2009-05-24  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Kielenvaihtotoiminnolle oma kielihistoriamuuttuja
;; 
;; 	
;; 
;; 2009-05-24  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Koodin väliotsikot englanniksi
;; 
;; 	
;; 
;; 2009-05-24  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Kielellistä hienosäätöä
;; 
;; 	
;; 
;; 2009-05-22  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Kutsutaan maalausajastinta useita kertoja peräkkäin
;; 
;; 	Aiemmin maalausajastinta kutsuttiin vain yhden kerran eli pienen viiveen 
;; 	jälkeen oikolukuajastimesta. Eräissä tilanteissa osa ikkunassa näkyvästä 
;; 	tekstistä jäi maalaamatta, koska tieto ei ollut vielä saapunut 
;; 	ulkoiselta prosessilta. Näin tapahtuu usein silloin, kun käynnistää 
;; 	wcheck-moden ensimmäistä kertaa Emacs-istunnon aikana. Oikoluku- tai muu 
;; 	ulkoinen ohjelma joutuu ensin latautumaan levyltä ja se ei ehdi 
;; 	käsitellä tietoa ajastimelle riittävän nopeasti.
;; 
;; 	Tämän muutoksen seurauksena maalausajastinta kutsutaan kolmesti 
;; 	peräkkäin, jolloin todennäköisesti kaikki tieto ulkoisilta prosesseilta 
;; 	on saapunut. Edelleen käytössä ovat idle-ajastimet, eli töitä tehdään 
;; 	vain, kun käyttäjä on toimettomana. Maalausajastin ja sen useammat 
;; 	kutsumiskerrat eivät tee mitään ellei mikään puskuri ole pyytänyt 
;; 	maalaamista.
;; 
;; 	
;; 
;; 2009-05-22  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Käsitellään oikein tilanne, jossa joutenoloaika on nolla
;; 
;; 	Opetetaan funktio wcheck-current-idle-time-seconds käsittelemään 
;; 	tilanne, jossa joutenoloaikaa ei ole lainkaan. Aiemmin tuli 
;; 	virheilmoitus, koska aritmeettiset funktiot eivät toimi parametrilla
;; 	"nil".
;; 
;; 	
;; 
;; 2009-05-22  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Lisätään ;;;###autoload -määritykset
;; 
;; 	
;; 
;; 2009-05-22  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Lisätään ikuisen silmukan tarkistus funktioon wcheck-paint-words
;; 
;; 	
;; 
;; 2009-05-22  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Sallitaan lausekkeen täsmääminen heti edellisen perään
;; 
;; 	Merkkijonoja etsiessään aiempi toteutus siirtyi joka kerta 
;; 	automaattisesti yhden merkin verran eteenpäin ennen kuin etsii seuraavaa 
;; 	merkkijonoa. Aiemmassa toteutuksessa oli se huono puoli, että puskurissa 
;; 	ei voi koskaan täsmätä kaksi välittömästi peräkkäin olevaan merkkijonoa.
;; 
;; 	Esimerkiksi, jos säännöllinen lauseke on "\w\{4\}" ja puskurissa on sana
;; 	"12345678", niin aiempi toteutus täsmäsi vain sanan ensimmäiseen neljän 
;; 	merkin osaan "1234", koska sen jälkeen siirryttiin yksi merkki eteenpäin 
;; 	eikä loppuosa enää voinut täsmätä. Uusi toteutus täsmää sekä alkuosaan
;; 	"1234" ja loppuosaan "5678".
;; 
;; 	Uudessa toteutuksessa poistutaan silmukasta heti, jos etsimiskerralla ei 
;; 	siirrytty lainkaan eteenpäin. Tällä estetään ikuinen silmukka, mikäli 
;; 	lauseke täsmää aina samassa paikassa.
;; 
;; 	
;; 
;; 2009-05-22  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Funktio wcheck-paint-words englanniksi
;; 
;; 	
;; 
;; 2009-05-22  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Maalataan vain näkyvät tekstialueet, hypätään näkymättömien yli
;; 
;; 	
;; 
;; 2009-05-22  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Muodostetaan säännöllinen lauseke valmiiksi ennen silmukkaa
;; 
;; 	
;; 
;; 2009-05-22  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Päivitetään myös outline-näkyvyyden muuttuessa
;; 
;; 	Lisätään puskurikohtainen koukku, joka pyytää puskurille 
;; 	oikolukupäivitystä, kun outline-tilassa tekstiä piilottaa tai avaa.
;; 
;; 	
;; 
;; 2009-05-22  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Poistetaan globaalit koukut ajastimesta riippumatta
;; 
;; 	
;; 
;; 2009-05-22  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Funktio wcheck-read-words englanniksi
;; 
;; 	
;; 
;; 2009-05-22  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Uusi tapa lukea tekstiä ikkunasta
;; 
;; 	Aiempi toteutus luki ikkunassa näkyviä merkkijonoja "visuaalisesti" ja 
;; 	rivi kerrallaan. Toiminnan tarkoituksena on hypätä näkymättömien 
;; 	tekstialueiden yli. Uusi toteutus toimii matalammalla tasolla. Se ei lue 
;; 	tekstiä visuaalisesti riveittäin, mutta tutkimalla text-properties- ja 
;; 	overlay-tietoja se hyppää näkymättömien tekstialueiden yli.
;; 
;; 	
;; 
;; 2009-05-20  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Lasketaan uuden ajastimen käynnistysviive edellisestä eteenpäin
;; 
;; 	Oikolukuajastin käynnistyy muuttujan wcheck-timer-idle osoittaman ajan 
;; 	kuluttua siitä, kun käyttäjä jää toimettomaksi. Aiemmin maalausajastin 
;; 	puolestaan käynnistyi tuo aika kerrottuna kahdella. Nyt muutetaan 
;; 	toimintaa siten, että maalausajastimen viive lasketaan 
;; 	oikolukuajastimesta eteenpäin wcheck-timer-idlen osoittaman ajan verran. 
;; 	Laskemisessa käytetään apuna uutta apufunktiota.
;; 
;; 	Lopputulos on käytännössä sama, mutta tämä muutos mahdollistaa helpommin 
;; 	useampien ajastimien laittamisen peräkkäin.
;; 
;; 	
;; 
;; 2009-05-20  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Käännetään ajastinfunktiot englanniksi
;; 
;; 	
;; 
;; 2009-05-20  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Siirretään maalauspyyntölistan muokkaus aiemmaksi
;; 
;; 	
;; 
;; 2009-05-20  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Siirretään oikolukupyyntölistan muokkaus aiemmaksi
;; 
;; 	
;; 
;; 2009-05-20  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Muutetaan funktion nimeä: wcheck-timer-event -> wcheck-timer-read-event
;; 
;; 	Koska on kaksi erillistä ajastintoimintoa, on parempi tehdä nimistä 
;; 	yksilöllisemmät.
;; 
;; 	
;; 
;; 2009-05-20  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Tehdään maalausajastimesta nimetty funktio (ei lambda)
;; 
;; 	
;; 
;; 2009-05-20  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Tarkistetaan prosessi funktion wcheck-program-executable-p avulla
;; 
;; 	Aiemmin käytössä oli file-executable-p, joka ei ole ihan niin 
;; 	luotettava.
;; 
;; 	
;; 
;; 2009-05-20  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Lisätään asennusohjeet wcheck-mode.el-tiedoston alkuun
;; 
;; 	
;; 
;; 2009-05-17  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Englanninkielinen kuvaus ja koodin kommentit funktiolle wcheck-mode
;; 
;; 	Samalla myös pientä hienosäätöä muuttujan wcheck-language-data 
;; 	kuvaukseen.
;; 
;; 	
;; 
;; 2009-05-17  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Tehdään oletuskielestä customize-muuttuja
;; 
;; 	
;; 
;; 2009-05-17  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Poistetaan customizesta vaatimus, että ohjelman täytyy olla olemassa
;; 
;; 	
;; 
;; 2009-05-17  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Muutetaan oletuskielen tunnistusta varmatoimisemmaksi
;; 
;; 	
;; 
;; 2009-05-17  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Funktion wcheck-change-language kuvaus englanniksi
;; 
;; 	Lisätään myös kommentteja koodiin ja käännetään interaktiiviset 
;; 	ilmoitukset englanniksi.
;; 
;; 	
;; 
;; 2009-05-17  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Laitetaan oletusasetukset näkymään customizeen
;; 
;; 	
;; 
;; 2009-05-17  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Lisätään muuttujan wcheck-language-date esimerkkiin regexp-discard
;; 
;; 	
;; 
;; 2009-05-17  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Muuttujalle wcheck-language-data kuvaus ja customize-määritys
;; 
;; 	Samalla vaihdetaan avainsana regexp-word regexp-body'ksi, koska kyse ei 
;; 	välttämättä ole sanoista.
;; 
;; 	
;; 
;; 2009-05-17  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	wcheck-make-overlayn parametriksi puskuri, ei ikkuna
;; 
;; 	Funktion wcheck-make-overlay parametrina oli "window" eli ikkuna, jossa 
;; 	alleviivaukset näkyvät. Nykyversiossa alleviivaukset näkyvät kaikissa 
;; 	ikkunoissa, ja koko window-parametria ei enää käytetä mihinkään.
;; 
;; 	Laitetaan tilalle uusi parametri "buffer" ja merkitään alleviivaukset 
;; 	eksplisiittisesti tuohon puskuriin eikä siihen, mikä sattuu olemaan 
;; 	aktiivisena sillä hetkellä, kun funktiota kutsutaan. Näin funktio toimii 
;; 	luotettavammin ja on riippumattomampi.
;; 
;; 	
;; 
;; 2009-05-16  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Tyhmän moka r2652:ssa rikkoi oletuskielen määrittelyn, korjataan
;; 
;; 	
;; 
;; 2009-05-16  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	wcheck-language-valid-p tiiviimpään and-rakennemuotoon
;; 
;; 	
;; 
;; 2009-05-16  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Sammutetaan komennolla (wcheck-mode 0) eikä vain (setq wcheck-mode nil)
;; 
;; 	
;; 
;; 2009-05-16  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Oikoluvun käynnistyksessä tarkistetaan, onko ohjelmalla suoritusoikeudet
;; 
;; 	
;; 
;; 2009-05-16  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Kielenvaihtofunktio tarkistaa, onko ohjelmalla suoritusoikeudet
;; 
;; 	Jos ohjelmalla, joka vastaa valittua kieltä, ei ole suoritusoikeuksia, 
;; 	sammutetaan oikoluku ja annetaan käyttäjälle varoitus.
;; 
;; 	
;; 
;; 2009-05-16  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Lisätään funktio wcheck-program-executable-p
;; 
;; 	
;; 
;; 2009-05-16  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Sammutetaan globaalit koukut vasta, kun oikolukua ei tarvita
;; 
;; 	Aiemmin globaalit koukut poistettiin aina, kun jostakin puskurista 
;; 	kytkettiin oikoluku pois. Se on tietysti väärin. Nyt ne poistetaan 
;; 	samalla kertaa ajastimen poiston kanssa eli vasta, kun mikään puskuri ei 
;; 	tarvitse oikolukua.
;; 
;; 	
;; 
;; 2009-05-16  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Turha kommentti pois koodista
;; 
;; 	Tarve päivittää oikoluku "C-x o" -komennon jälkeen on jo poistunut, 
;; 	koska nyt maalataan kaikkien näkyvissä olevien ikkunoiden puskurit.
;; 
;; 	
;; 
;; 2009-05-16  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Varmistetaan, että oletuskieleksi tulee merkkijono
;; 
;; 	
;; 
;; 2009-05-16  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Käynnistetään ajastin muuttujan wcheck-timer perusteella
;; 
;; 	
;; 
;; 2009-05-16  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Siirretään ajastuksen sammutus wcheck-mode-funktioon
;; 
;; 	
;; 
;; 2009-05-16  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Ajastinfunktiolle uusi nimi: wcheck-timer-event
;; 
;; 	
;; 
;; 2009-05-16  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Käytetään kommunikointiin PTY:itä, jotta shell-skriptit toimivat
;; 
;; 	
;; 
;; 2009-05-16  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Kielenvaihto pyytää päivitystä vain oikoluvun ollessa päällä
;; 
;; 	Funktio wcheck-change-language tarkistaa, kutsuttiinko sitä 
;; 	interaktiivisesti ja onko oikoluku päällä nykyisessä puskurissa. Jos 
;; 	molemmat ehdot toteutuvat, pyydetään päivitystä oikolukuun. Aiemmin 
;; 	päivitystä pyydettiin aina eli myös silloin, kun oikoluku ei ole päällä.
;; 
;; 	
;; 
;; 2009-05-16  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Poistetaan error-kutsu kielenvaihtofunktiosta
;; 
;; 	Kun funktiota kutsutaan Lisp-koodista, palautetaan vain "nil", mikäli 
;; 	parametriksi ei annettu merkkijonoa.
;; 
;; 	
;; 
;; 2009-05-16  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Muutetaan tietueen nimi: discard -> regexp-discard
;; 
;; 	
;; 
;; 2009-05-16  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	setq-default eikä setq, kun määritellään oletuskieliasetukset
;; 
;; 	
;; 
;; 2009-05-16  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Minibuf-virheilmoitus pois; sopimattoman kielen ilmoitukseen kielen nimi
;; 
;; 	
;; 
;; 2009-05-16  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Sananlukuajastin tarkistaa nyt, onko kyseinen kieli sallittu
;; 
;; 	Ajastin ei enää lue kieltä muuttujasta wcheck-buffer-process-data vaan 
;; 	vaihtaa kyseiseen puskuriin komennolla "(with-current-buffer)" ja katsoo 
;; 	muuttujaa wcheck-language. Jos kyseisessä muuttujassa ei ole sallittua 
;; 	kieltä, sammutetaan oikoluku.
;; 
;; 	
;; 
;; 2009-05-16  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Tyhjennetään vastaanotettujen sanojen lista myöhemmin
;; 
;; 	
;; 
;; 2009-05-16  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Lähetetään prosessille rivinvaihto ennen sanamerkkijonoa
;; 
;; 	Ennen kuin sanamerkkijono lähetetään ulkoiselle prosessille, lähetetään 
;; 	rivinvaihto. Tällä voidaan kai teoriassa estää se, että lähetysjonossa 
;; 	edellinen teksti ei pääty rivinvaihtoon ja uusi teksti alkaa heti 
;; 	suoraan sen perään. En usko, että tällä on käytännössä vaikutusta.
;; 
;; 	
;; 
;; 2009-05-16  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Poistetaan turha tarkistus parametrien oikeellisuudesta
;; 
;; 	
;; 
;; 2009-05-16  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	case-fold-search "nil", kun luetaan sanoja ikkunasta
;; 
;; 	Tämä tarkoittaa, että kirjainlajilla (A vai a) on merkitystä 
;; 	säännöllisten lausekkeiden toiminnalle.
;; 
;; 	
;; 
;; 2009-05-16  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Muotoillaan kooditekstiä uudelleen (ei toiminnallista muutosta)
;; 
;; 	
;; 
;; 2009-05-16  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Viitataan nykyiseen puskuriin eikä ikkunan sisältämään puskuriin
;; 
;; 	Tämä on pieni mitätön muutos, jossa "(window-buffer window)" muutetaan 
;; 	muotoon "(current-buffer)" tilanteessa, jossa molemmat joka tapauksessa 
;; 	viittaavat samaan. Ympärillä on kuitenkin "(with-current-buffer ...)".
;; 
;; 	
;; 
;; 2009-05-16  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Poistetaan maalaukset riippumatta siitä, onko oikoluku päällä
;; 
;; 	Maalausajastin tarkistaa, onko oikoluku päällä ja toimii vain, mikäli se 
;; 	on päällä. Maalit voidaan kuitenkin pyyhkiä pois puskurista riippumatta 
;; 	oikoluvun päälläolosta. Näin ehkä voidaan varmistaa, että puskuriin ei 
;; 	jää maalia.
;; 
;; 	
;; 
;; 2009-05-16  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Oma ajastinkierros sanojen maalaamiselle
;; 
;; 	Tämä muutos tekee sanojen maalaamiselle oman ajastimen, joka 
;; 	käynnistetään yhden kerran sananlukuajastimesta. Funktio, joka ottaa 
;; 	sanat vastaan ulkoiselta prosessilta, ei siis enää maalaa sanoja vaan 
;; 	ainoastaan lisää ne puskurikohtaiseen listaan ja laittaa 
;; 	maalaamispyynnön em. maalausajastimelle.
;; 
;; 	
;; 
;; 2009-05-16  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Maalataan paluusanat automaattisesti
;; 
;; 	Yksi ajastin lähettää päivitystä pyytäneen puskurin kaikista ikkunoista 
;; 	olevat sanat ulkoiselle prosessille. Ne ottaa automaattisesti vastaan 
;; 	funktio wcheck-receive-words, joka saman tien maalaa sanat kaikkiin 
;; 	ikkunoihin, joissa kyseinen puskuri on näkyvissä.
;; 
;; 	Ratkaisu näyttää toimivan mutta on hidas. On hidasta maalata sanoja 
;; 	automaattisesti heti, kun ne otetaan vastaan ulkoiselta ohjelmalta. 
;; 	Tapahtuma kannattanee viivästää tilanteeseen, jossa ei tehdä mitään. 
;; 	Ehkä sanat vastaanottanut funktio voisi tallentaa sanat 
;; 	puskurikohtaiseen muuttujaan wcheck-received-words ja jättää pelkän 
;; 	päivityspyynnön toiselle ajastimelle. Ajastin sitten päivittää ikkunat, 
;; 	kun tulee tyhjä hetki.
;; 
;; 	
;; 
;; 2009-05-16  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Tehdään ajastimen viiveestä muuttuja
;; 
;; 	Muuttuja esitellään vakioksi (defconst).
;; 
;; 	
;; 
;; 2009-05-16  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Parannetaan wcheck-language-muuttujan kuvausta
;; 
;; 	
;; 
;; 2009-05-16  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Muutetaan apufunktioryhmän otsikkoa
;; 
;; 	
;; 
;; 2009-05-16  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Siirretään pari koukkua samaan paikkaan muiden kanssa
;; 
;; 	
;; 
;; 2009-05-16  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	Poistetaan overlayt kielen vaihtamisen jälkeen
;; 
;; 	
;; 
;; 2009-05-16  Teemu Likonen  <tlikonen@iki.fi>
;; 
;; 	wcheck-mode.el repositoryyn
;; 
;; 	Tämä versio lukee vain aktiivista ikkunaa ja pyyhkii muista saman 
;; 	puskurin sisältävistä ikkunoista pois alleviivaukset. Yksi ongelma on 
;; 	se, että ikkunanvaihto "C-x o" ei päivitä uutta ikkunaa.
;; 
;; 	
;; 



(provide 'wcheck-mode)

;;; wcheck-mode.el ends here
