;;; wcheck-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "wcheck-mode" "wcheck-mode.el" (0 0 0 0))
;;; Generated autoloads from wcheck-mode.el

(let ((loads (get 'wcheck 'custom-loads))) (if (member '"wcheck-mode" loads) nil (put 'wcheck 'custom-loads (cons '"wcheck-mode" loads))))

(defvar wcheck-language-data nil "\
Language configuration for `wcheck-mode'.

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
`https://github.com/tlikonen/wcheck-mode'.")

(custom-autoload 'wcheck-language-data "wcheck-mode" t)

(defconst wcheck--language-data-defaults-hard-coded '((parser . wcheck-parser-lines) (connection) (face . wcheck-default-face) (syntax . text-mode-syntax-table) (regexp-start . "\\<'*") (regexp-body . "\\w+?") (regexp-end . "'*\\>") (regexp-discard . "\\`'+\\'") (case-fold) (read-or-skip-faces (nil)) (action-autoselect)) "\
Hard-coded default language configuration for `wcheck-mode'.
This constant is for Wcheck mode's internal use only. This
provides useful defaults if both `wcheck-language-data' and
`wcheck-language-data-defaults' fail.")

(defvar wcheck-language-data-defaults wcheck--language-data-defaults-hard-coded "\
Default language configuration for `wcheck-mode'.
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
       message-header-subject message-cited-text)))")

(custom-autoload 'wcheck-language-data-defaults "wcheck-mode" t)

(defvar wcheck-language "" "\
Default language for `wcheck-mode'.

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
`wcheck-mode' is turned on next time.")

(custom-autoload 'wcheck-language "wcheck-mode" t)

(defface wcheck-default-face '((t (:underline "red"))) "\
Default face for marking strings in a buffer.
This is used when language does not define a face." :group (quote wcheck))

(autoload 'wcheck-change-language "wcheck-mode" "\
Change language for current buffer (or globally).
Change `wcheck-mode' language to LANGUAGE. The change is
buffer-local but if GLOBAL is non-nil (prefix argument if called
interactively) then change the global default language.

\(fn LANGUAGE &optional GLOBAL)" t nil)

(autoload 'wcheck-mode "wcheck-mode" "\
General interface for text checkers.

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
used for querying effective configuration data for any language.

\(fn &optional ARG)" t nil)

(autoload 'wcheck-jump-forward "wcheck-mode" "\
Move point forward to next marked text area.

\(fn)" t nil)

(autoload 'wcheck-jump-backward "wcheck-mode" "\
Move point backward to previous marked text area.

\(fn)" t nil)

(autoload 'wcheck-actions "wcheck-mode" "\
Offer actions for marked text.

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
any kind of actions, though.

\(fn POS &optional EVENT)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "wcheck-mode" '("wcheck-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; wcheck-mode-autoloads.el ends here
