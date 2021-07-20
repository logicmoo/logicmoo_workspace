;;; org-roam-bibtex-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "orb-anystyle" "orb-anystyle.el" (0 0 0 0))
;;; Generated autoloads from orb-anystyle.el

(autoload 'orb-anystyle "orb-anystyle" "\
Run anystyle COMMAND with `shell-command'.
ARGS is a plist with the following recognized keys:

Anystyle CLI options
==========
1) EXEC :exec      => string (valid executable)
- default value can be set through `orb-anystyle-executable'

2) COMMAND :command   => symbol or string
- valid values: find parse help check license train

3) Global options can be passed with the following keys.

FMODEL    :finder-model => string (valid file path)
PMODEL    :parser-model => string (valid file path)
PDFINFO   :pdfinfo      => string (valid executable)
PDFTOTEXT :pdftotext    => string (valid executable)
ADAPTER   :adapter      => anything
STDOUT    :stdout       => boolean
HELP      :help         => boolean
VERBOSE   :verbose      => boolean
VERSION   :version      => boolean
OVERWRITE :overwrite    => boolean
FORMAT    :format       => string, symbol or list of unquoted symbols

- FORMAT must be one or more output formats accepted by anystyle commands:
  parse => bib csl json ref txt xml
  find  => bib csl json ref txt ttx xml
- string must be space- or comma-separated, additional spaces are
  ignored

Default values for some of these options can be set globally via
the following variables: `orb-anystyle-finder-model',
`orb-anystyle-parser-model', `orb-anystyle-pdfinfo-executable',
`orb-anystyle-pdftotext-executable'.

4) Command options can be passed with the following keys:

CROP   :crop         => integer or cons cell of integers
LAYOUT :layout       => boolean
SOLO   :solo         => boolean

- Command options are ignored for commands other than find
- anystyle help -c flag is not supported

Default values for these options can be set globally via the
following variables: `orb-anystyle-find-crop',
`orb-anystyle-find-layout', `orb-anystyle-find-solo'.

5) INPUT  :input   => string (file path)

6) OUTPUT :output  => string (file path)

`shell-command'-related options
==========

7) BUFFER :buffer  => buffer-or-name

- `shell-command''s OUTPUT-BUFFER
- can be a cons cell (OUTPUT-BUFFER . ERROR-BUFFER)
- when nil, defaults to `orb-anystyle-default-buffer'

anystyle CLI command synopsis:
anystyle [global options] command [command options] [arguments...].

Homepage: https://anystyle.io
Github: https://github.com/inukshuk/anystyle-cli
Courtesy of its authors.

\(fn COMMAND &key (EXEC orb-anystyle-executable) VERBOSE HELP VERSION ADAPTER ((:finder-model FMODEL) orb-anystyle-finder-model) ((:parser-model PMODEL) orb-anystyle-parser-model) (PDFINFO orb-anystyle-pdfinfo-executable) (PDFTOTEXT orb-anystyle-pdftotext-executable) FORMAT STDOUT OVERWRITE (CROP orb-anystyle-find-crop) (SOLO orb-anystyle-find-solo) (LAYOUT orb-anystyle-find-layout) INPUT OUTPUT (BUFFER orb-anystyle-default-buffer))" nil nil)

(function-put 'orb-anystyle 'lisp-indent-function '1)

(register-definition-prefixes "orb-anystyle" '("orb-anystyle-"))

;;;***

;;;### (autoloads nil "orb-core" "orb-core.el" (0 0 0 0))
;;; Generated autoloads from orb-core.el

(autoload 'orb-process-file-field "orb-core" "\
Look up documents associated with the BibTeX entry and choose one.
Process the BibTeX 'file' field (`bibtex-completion-pdf-field')
or search in the `bibtex-completion-library-path' for a file or files with
the CITEKEY as filename sans extension.

If `orb-file-field-extensions' is non-nil, return only file paths
matching the respective extensions.

If `orb-abbreviate-file-name' is non-nil, force an abbreviated
file name.

Mendeley, Zotero, normal paths are all supported.  If there are
multiple files found, the user will be prompted to select one.

\(fn CITEKEY)" nil nil)

(autoload 'orb-autokey-generate-key "orb-core" "\
Generate citation key from ENTRY according to `orb-autokey-format'.
Return a string.  If optional CONTROL-STRING is non-nil, use it
instead of `orb-autokey-format'.

\(fn ENTRY &optional CONTROL-STRING)" nil nil)

(register-definition-prefixes "orb-core" '("orb-"))

;;;***

;;;### (autoloads nil "orb-helm" "orb-helm.el" (0 0 0 0))
;;; Generated autoloads from orb-helm.el

(register-definition-prefixes "orb-helm" '("helm-source-orb-insert" "orb-helm-insert"))

;;;***

;;;### (autoloads nil "orb-ivy" "orb-ivy.el" (0 0 0 0))
;;; Generated autoloads from orb-ivy.el

(register-definition-prefixes "orb-ivy" '("orb-i"))

;;;***

;;;### (autoloads nil "orb-pdf-scrapper" "orb-pdf-scrapper.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from orb-pdf-scrapper.el

(autoload 'orb-pdf-scrapper-run "orb-pdf-scrapper" "\
Run Orb PDF Scrapper interactive process.
KEY is note's citation key.

\(fn KEY)" t nil)

(register-definition-prefixes "orb-pdf-scrapper" '("orb-"))

;;;***

;;;### (autoloads nil "orb-utils" "orb-utils.el" (0 0 0 0))
;;; Generated autoloads from orb-utils.el

(register-definition-prefixes "orb-utils" '("orb-"))

;;;***

;;;### (autoloads nil "org-roam-bibtex" "org-roam-bibtex.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from org-roam-bibtex.el

(autoload 'orb-edit-note "org-roam-bibtex" "\
Open an Org-roam note associated with the CITEKEY or create a new one.

This function allows to use Org-roam as a backend for managing
bibliography notes.  It relies on `bibtex-completion' to get
retrieve bibliographic information from a BibTeX file.

Implementation details and features:

1. This function first calls `org-roam-find-ref' trying to find
the note file associated with the CITEKEY.  The Org-roam key can
be set with '#+ROAM_KEY:' in-buffer keyword.

2. If the Org-roam reference has not been found, the function
calls `org-roam-node-find' passing to it the title associated
with the CITEKEY as retrieved by `bibtex-completion-get-entry'.
The prompt presented by `org-roam-node-find' will thus be
pre-populated with the record title.

3. Optionally, when `orb-preformat-templates' is non-nil, any
prompt wildcards in `orb-templates' or
`org-roam-capture-templates', associated with the bibtex record
fields as specified in `orb-preformat-templates', will be
preformatted.  Both `org-capture-templates' (%^{}) and
`org-roam-capture-templates' (`s-format', ${}) prompt syntaxes
are supported.

See `orb-preformat-keywords' for more details on how
to properly specify prompts for replacement.

Please pay attention when using this feature that by setting
title for preformatting, it will be impossible to change it in
the `org-roam-node-find' interactive prompt since all the
template expansions will have taken place by then.  All the title
wildcards will be replace with the BibTeX field value.

4. Optionally, if you are using Projectile and Persp-mode and
have a dedicated workspace to work with your Org-roam collection,
you may want to set the perspective name and project path in
`orb-persp-project' and `orb-switch-persp' to t.  In this case,
the perspective will be switched to the Org-roam notes project
before calling any Org-roam functions.

\(fn CITEKEY)" nil nil)

(autoload 'orb-insert-link "org-roam-bibtex" "\
Insert a link to an Org-roam bibliography note.
If the note does not exist yet, it will be created using
`orb-edit-note' function.

\\<universal-argument-map>\\<org-roam-bibtex-mode-map> The
customization option `orb-insert-link-description' determines
what will be used as the link's description.  It is possible to
override the default value with numerical prefix ARG:

`C-1' \\[orb-insert-link] will force `title'
`C-2' \\[orb-insert-link] will force `citekey'
`C-0' \\[orb-insert-link] will force `citation'

If a region of text is active (selected) when calling `orb-insert-link',
the text in the region will be replaced with the link and the
text string will be used as the link's description — similar to
`org-roam-insert'.

Normally, the case of the link description will be preserved.  It
is possible to force lowercase by supplying either one or three
universal arguments `\\[universal-argument]'.

Finally, `bibtex-completion-cache' will be re-populated if either
two or three universal arguments `\\[universal-argument]' are supplied.

The customization option `orb-insert-interface' allows to set the
completion interface backend for the candidates list.  Available
interfaces are `helm-bibtex', `ivy-bibtex' and `orb-insert-generic'.

With `helm-bibtex' or `ivy-bibtex', choosing the action \"Edit
note & insert a link\" will insert the desired link.  For
convenience, this action is made default for the duration of an
`orb-insert-link' session.  It will not persist when `helm-bibtex' or
`ivy-bibtex' proper are run.  It is possible to run other
`helm-bibtex' or `ivy-bibtex' actions.  When action other than
\"Edit note & insert a link\" is run, no link will be inserted,
although the session can be resumed later with `helm-resume' or
`ivy-resume', respectively, where it will be possible to select
the \"Edit note & insert a link\" action.

When using `orb-insert-generic', a simple list of available
citation keys is presented using `completion-read' and after
choosing a candidate the appropriate link will be inserted.

\(fn &optional ARG)" t nil)

(autoload 'orb-note-actions "org-roam-bibtex" "\
Run an interactive prompt to offer note-related actions.
The prompt interface can be set in `orb-note-actions-interface'.
In addition to default actions, which are not supposed to be
modified, there is a number of prefined extra actions
`orb-note-actions-extra' that can be customized.  Additionally,
user actions can be set in `orb-note-actions-user'." t nil)

(autoload 'orb-org-ref-edit-note "org-roam-bibtex" "\
Open an Org-roam note associated with the CITEKEY or create a new one.
Set `org-ref-notes-function' to this function if your
bibliography notes are managed by Org-roam and you want some
extra integration between the two packages.

This is a wrapper function around `orb-edit-note'
intended for use with Org-ref.

\(fn CITEKEY)" nil nil)

(autoload 'orb-edit-notes "org-roam-bibtex" "\
Open or create an Org-roam note associated with the first key from KEYS.
This function replaces `bibtex-completion-edit-notes'.  Only the
first key from KEYS will actually be used.

\(fn KEYS)" nil nil)

(defvar org-roam-bibtex-mode nil "\
Non-nil if Org-Roam-Bibtex mode is enabled.
See the `org-roam-bibtex-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `org-roam-bibtex-mode'.")

(custom-autoload 'org-roam-bibtex-mode "org-roam-bibtex" nil)

(autoload 'org-roam-bibtex-mode "org-roam-bibtex" "\
Sets `orb-edit-note' as a function for editing bibliography notes.
Affects Org-ref and Helm-bibtex/Ivy-bibtex.

When called interactively, toggle `org-roam-bibtex-mode'. with
prefix ARG, enable `org-roam-bibtex-mode' if ARG is positive,
otherwise disable it.

When called from Lisp, enable `org-roam-bibtex-mode' if ARG is
omitted, nil, or positive.  If ARG is `toggle', toggle
`org-roam-bibtex-mode'.  Otherwise, behave as if called
interactively.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "org-roam-bibtex" '("orb-" "org-roam-"))

;;;***

;;;### (autoloads nil nil ("orb-compat.el" "org-roam-bibtex-pkg.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; org-roam-bibtex-autoloads.el ends here
