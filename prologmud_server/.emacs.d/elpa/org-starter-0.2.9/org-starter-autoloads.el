;;; org-starter-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "org-starter" "org-starter.el" (0 0 0 0))
;;; Generated autoloads from org-starter.el

(defvar org-starter-mode nil "\
Non-nil if Org-Starter mode is enabled.
See the `org-starter-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `org-starter-mode'.")

(custom-autoload 'org-starter-mode "org-starter" nil)

(autoload 'org-starter-mode "org-starter" "\
Turn on/off features of org-starter.

This is a minor mode.  If called interactively, toggle the
`Org-Starter mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value 'org-starter-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

This includes the following features:

- Activate a function advice around `find-file-noselect', so
  `org-starter-enable-local-variables' option is respected.

- When Org files are loaded, set file-local variables defined as
  :local-variables option in `org-starter-define-file'.

\(fn &optional ARG)" t nil)

(autoload 'org-starter-find-file-by-key "org-starter" "\
Visit an Org file quickly by key.

With this command, you can quickly open a file by a key sequence
specified as :key property of the file.

To access a file which is not assigned a key, you can select it
using `completing-read' by pressing \"/\" key.

If a universal prefix is given as ARG, open the file in other
window.

If two universal prefix arguments (C-u C-u) is given, call a function
specified as `org-starter-alternative-find-function' with the file
as the argument.

\(fn &optional ARG)" t nil)

(autoload 'org-starter-alternative-find-file-by-key "org-starter" "\
Visit an Org file using `org-starter-alternative-find-function'.

This is like `org-starter-find-file-by-key' but uses
`org-starter-alternative-find-function' to visit a file.
Keys are configured as :key properties of files, which are the same
as `org-starter-find-file-by-key'.

To access a file which is not assigned a key, you can select it
using `completing-read' by pressing \"/\" key.

If a universal prefix is given as ARG, visit the file using
`find-file'.

Extra commands are configured in
`org-starter-extra-alternative-find-file-map'.

\(fn &optional ARG)" t nil)

(autoload 'org-starter-refile-by-key "org-starter" "\
Run `org-refile' with the target limited to a file by key.

With this command, you can quickly refile the current entry to a file by a key
sequence specified as :key property of the file.

ARG is passed to `org-refile' function.

You can also access keybindings defined in
`org-starter-extra-refile-map'.
For example, you can run a normal `org-refile' by pressing \"/\" key
by default.

\(fn &optional ARG)" t nil)

(autoload 'org-starter-undefine-file "org-starter" "\
Delete an entry with FILENAME from the list of known files.

\(fn FILENAME)" t nil)

(autoload 'org-starter-add-agenda-custom-command "org-starter" "\
`org-add-agenda-custom-command' with extra features.

This function basically adds (KEY DESC TYPE MATCH SETTINGS FILES) to
`org-agnda-custom-commands', but if it also checks if KEY does not
conflict with existing custom agenda commands.

You can also define a group using this function by omitting TYPE,
MATCH, SETTINGS, and FILES passed to it.

Some extra features may be added in the future.

\(fn KEY DESC &optional TYPE MATCH SETTINGS FILES)" nil nil)

(function-put 'org-starter-add-agenda-custom-command 'lisp-indent-function '2)

(autoload 'org-starter-add-block-agenda-command "org-starter" "\
Define a block agenda.

An entry consisting KEY, DESC, LIST-OF-TYPE-MATCH-SETTINGS-FILES,
SETTINGS, and FILES are added to `org-agenda-custom-commands'.

\(fn KEY DESC &rest LIST-OF-TYPE-MATCH-SETTINGS-FILES &key SETTINGS FILES &allow-other-keys)" nil nil)

(function-put 'org-starter-add-block-agenda-command 'lisp-indent-function '2)

(autoload 'org-starter-add-to-refile-targets "org-starter" "\
Add the current buffer to `org-refile-targets' temporarily." t nil)

(autoload 'org-starter-cleanup-entries "org-starter" "\
Remove missing files and directories from various variables.

This function removes files and directories that no longer exists from the
following variables:

- `org-starter-known-directories'
- `org-starter-known-files'

If ALL is non-nil, variable `org-agenda-files' and
`org-refile-targets' are also checked for missing entries.

\(fn &optional ALL)" t nil)

(autoload 'org-starter-verify-configuration "org-starter" "\
Check the current configuration." t nil)

(autoload 'org-starter-select-file "org-starter" "\
Select a file from known files and agenda files.

This function select an Org file using `completing-read' with PROMPT.

If this function is called interactively, it visits the selected file.
If a prefix argument is given, it visits the selected file in
other window.

If this function is called non-interactively, it returns the file path
of the selected file.

\(fn PROMPT)" t nil)

(autoload 'org-starter-select-file-other-window "org-starter" "\
A variant of `org-starter-select-file' targetting other window." t nil)

(autoload 'org-starter-select-file-alternative "org-starter" "\
Select a file and visit it using the alternative command.

This function behaves like `org-starter-select-file' but uses
`org-starter-alternative-find-function' to visit a selected file." t nil)

(autoload 'org-starter-load-file "org-starter" "\
Load FILE using org-starter.

\(fn FILE)" t nil)

(autoload 'org-starter-load-all-known-files "org-starter" "\
Load all files registered in `org-starter-known-files' into Emacs.

This can be convenient in some situations where you want ensure that all org
files are in buffers." t nil)

(autoload 'org-starter-load-all-files-in-path "org-starter" "\
Load all org files in `org-starter-path' into Emacs.

This can be convenient in some situations where you want ensure that all org
files are in buffers.

`org-starter-get-all-files-in-path' is used to get a list of org files." t nil)

(autoload 'org-starter-load-config-files "org-starter" "\
Load config files in `org-starter-path'." t nil)

(autoload 'org-starter-find-config-file "org-starter" "\
Visit an existing config file for org-starter." t nil)

(register-definition-prefixes "org-starter" '("org-starter-"))

;;;***

;;;### (autoloads nil nil ("org-starter-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; org-starter-autoloads.el ends here
