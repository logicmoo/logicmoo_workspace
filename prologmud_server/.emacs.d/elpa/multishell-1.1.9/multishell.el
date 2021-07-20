;;; multishell.el --- Easily use multiple shell buffers, local and remote  -*- lexical-binding:t -*-

;; Copyright (C) 1999-2020 Free Software Foundation, Inc. and Ken Manheimer

;; Author: Ken Manheimer <ken.manheimer@gmail.com>
;; Version: 1.1.9
;; Created: 1999 -- first public availability
;; Keywords: processes
;; Package-Requires: ((cl-lib "0.5"))
;; URL: https://github.com/kenmanheimer/EmacsMultishell
;;
;;; Commentary:
;;
;; Easily use and navigate multiple shell buffers, including remote shells.
;; Fundamentally, multishell is the function `multishell-pop-to-shell' -
;; a la `pop-to-buffer' - plus a keybinding. Together, they enable you to:
;;
;; * Get to the input point from wherever you are in a shell buffer,
;;   ... or to any of your shell buffers, from anywhere inside emacs.
;;
;; * Use universal arguments to launch and choose among alternate shell buffers,
;;   ... and change which is the current default.
;;
;; * Easily restart disconnected shells, or shells from prior sessions
;;   ... the latter from Emacs builtin savehist minibuf history persistence
;;
;; * Append a path to a new shell name to launch a shell in that directory,
;;   ... and use a path with Emacs tramp syntax to launch a remote shell -
;;   for example:
;;
;;   * `#root/sudo:root@localhost:/etc` for a buffer named "*#root*" with a
;;     root shell starting in /etc.
;;
;;   * `/ssh:example.net:` for a shell buffer in your homedir on example.net.
;;     The buffer will be named "*example.net*".
;;
;;   * `#ex/ssh:example.net|sudo:root@example.net:/var/log` for a root shell
;;     starting in /var/log on example.net named "*#ex*".
;;
;;   * 'interior/ssh:gateway.corp.com|ssh:interior.corp.com:' to go via
;;     gateway.corp.com to your homedir on interior.corp.com.  The buffer
;;     will be named "*interior*". You could append a sudo hop, and so on.
;;
;; * Thanks to tramp, file visits from the shell will seamlessly be in
;;   the auspices of the target account, and relative to the current
;;   directory, on the host where the shell is running.
;;
;; * Manage your list of shells, current and past, as a collection.
;;
;; See the `multishell-pop-to-shell' docstring for details.
;;
;; Customize-group `multishell' to select and activate a keybinding and set
;; various behaviors. Customize-group `savehist' to preserve buffer
;; names/paths across emacs restarts.
;;
;; Please use
;; [the multishell repository](https://github.com/kenmanheimer/EmacsMultishell)
;; issue tracker to report problems, suggestions, etc, and see that
;; repository for a bit more documentation.
;;
;; Change Log:
;;
;; * 2020-10-30 1.1.9 Ken Manheimer:
;;   - Require cl-lib when compiling for cl-letf macro.
;; * 2020-10-28 1.1.8 Ken Manheimer:
;;   - Change back to having multishell-list require multishell,
;;     rather than the other way around, and remove now unnecessary
;;     new autoloads.
;;   - Bump version for ELPA release.
;; * 2020-10-28 1.1.7 Ken Manheimer:
;;   - Forward compatibility: 'cl-progv' rather than 'progv', resolves
;;     multishell-list error in recent emacs versions.
;;   - Incorporate gnu refinements (thanks!)
;; * 2016-06-27 1.1.6 Ken Manheimer (unreleased):
;;   - When starting a remote shell, if cd fails to an inital remote
;;     directory, try again without the cd.
;; * 2016-02-11 1.1.5 Ken Manheimer:
;;   - Rectify multishell list sorting to preserve recentness
;;   - Increment the actual multishell-version setting, neglected for 1.1.4.
;; * 2016-02-11 1.1.4 Ken Manheimer:
;;   - hookup multishell-list as completion help buffer.
;;     Mouse and keyboard selections from help listing properly exits
;;     minibuffer.
;; * 2016-02-09 1.1.3 Ken Manheimer:
;;   multishell-list:
;;   - add some handy operations, like cloning new entry from existing
;;   - add optional behaviors to existing operations for returning to
;;     stopped shells without restarting them.
;;   - solidify maintaining focus on current entry
;;   - fix miscellaneous.
;; * 2016-01-31 1.1.2 Ken Manheimer:
;;   - Settle puzzling instability of multishell-all-entries
;;     - The accumulations was putting items going from more to less active
;;       categories to be put at the end, not beginning.
;;     - Also, using history for prompting changes history - implement
;;       no-record option to avoid this when needed.
;;   - Implement simple edit-in-place multishell-replace-entry and use in
;;     multishell-list-edit-entry.
;;   - Remove now unnecessary multishell-list-revert-buffer-kludge.
;;   - Rectify byte compiler offenses, and other fixes - thanks to Stefan
;;     Monnier for pointing out many of the corrections.
;;   - Avoid directly calling tramp functions unnecessarily.
;; * 2016-01-30 1.1.1 Ken Manheimer:
;;   - shake out initial multishell-list glitches:
;;     - (Offer to) delete shell buffer, if present, when deleting entry.
;;     - Set recency (numeric rank) as initial sort field
;;     - Recompute list on most operations that affect the order, and try to
;;       preserve stability. (Kludgy solution, needs work.)
;;   - Set version to 1.1.1 - multishell-list addition should have been 1.1.0.
;; * 2016-01-30 1.0.9 Ken Manheimer:
;;   - Add multishell-list for managing the collection of current and
;;     history-registered shells: edit, delete, and switch/pop to entries.
;;     Easy access by invoking `multishell-pop-to-shell' from in the
;;     `multishell-pop-to-shell' universal arg prompts.
;;   - Duplicate existing shell buffer names in completions, for distinction.
;;   - Add paths to buffers started without one, when multishell history dir
;;     tracking is enabled.
;;   - Major code cleanup:
;;     - Simplify multishell-start-shell-in-buffer, in particular using
;;       shell function, rather than unnecessarily going underneath it.
;;     - Establish multishell-name-from-entry as canonical name resolver.
;;     - Fallback to eval-after-load in emacs versions that lack
;;       with-eval-after-load (eg, emacs 23).
;;     - save-match-data, where match-string is used
;;     - resituate some helpers
;; * 2016-01-24 1.0.8 Ken Manheimer:
;;   - Work around the shell/tramp mishandling of remote+sudo+homedir problem!
;;     The work around is clean and simple, basically using high-level `cd'
;;     API and not messing with the low-level default-directory setting.
;;     (Turns out the problem was not in my local config. Good riddance to the
;;     awkward failure handler!)
;;   - Clean up code resolving the destination shell, starting to document the
;;     decision tree in the process. See getting-to-a-shell.md in the
;;     multishell repository, https://github.com/kenmanheimer/EmacsMultishell
;;   - There may be some shake-out on resolving the destination shell, but
;;     this release gets the fundamental functionality soundly in place.
;; * 2016-01-23 1.0.7 Ken Manheimer:
;;   - Remove notes about tramp remote+sudo+homedir problem. Apparently it's
;;     due to something in my local site configuration (happens with -q but
;;     not -Q).
;; * 2016-01-22 1.0.6 Ken Manheimer:
;;   - Add multishell-version function.
;;   - Tweak commentary/comments/docstrings.
;;   - Null old multishell-buffer-name-history var, if present.
;; * 2016-01-16 1.0.5 Ken Manheimer:
;;   - History now includes paths, when designated.
;;   - Actively track current directory in history entries that have a path.
;;     Custom control: multishell-history-entry-tracks-current-directory
;;   - Offer to remove shell's history entry when buffer is killed.
;;     (Currently the only UI mechanism to remove history entries.)
;;   - Fix - prevent duplicate entries for same name but different paths
;;   - Fix - recognize and respect tramp path syntax to start in home dir
;;   - Simplify history var name, migrate existing history if any from old name
;; * 2016-01-04 1.0.4 Ken Manheimer - Released to ELPA
;; * 2016-01-02 Ken Manheimer - working on this in public, but not yet released.
;;
;; TODO and Known Issues:
;;
;; * Add custom shell launch prep actions
;;   - for, eg, port knocking, interface activations
;;   - shell commands to execute when shell name or path matches a regexp
;;   - list of (regexp, which - name, path, or both, command)
;; * Investigate whether we can recognize and provide for failed hops.
;;   - Tramp doesn't provide useful reactions for any hop but the first
;;   - Might be stuff we can do to detect and convey failures?
;;   - Might be no recourse but to seek tramp changes.
;; * Try minibuffer field boundary at beginning of tramp path, to see whether
;;   the field boundary magically enables tramp path completion.

;;; Code:

(require 'comint)
(require 'shell)
(require 'savehist)
(eval-when-compile (require 'cl-lib))

(defvar multishell-version "1.1.9")
(defun multishell-version (&optional here)
  "Return string describing the loaded multishell version."
  (interactive "P")
  (let ((msg (concat "Multishell " multishell-version)))
    (if here (insert msg)
      (if (called-interactively-p 'interactive)
          (message "%s" msg)
        msg))))

(defgroup multishell nil
  "Allout extension that highlights outline structure graphically.

Customize `allout-widgets-auto-activation' to activate allout-widgets
with allout-mode."
  :group 'shell)

(defcustom multishell-command-key "\M- "
  "The key to use if `multishell-activate-command-key' is true.

You can instead manually bind `multishell-pop-to-shell' using emacs
lisp, eg: (global-set-key \"\\M- \" \\='multishell-pop-to-shell)."
  :type 'key-sequence)

(defvar multishell--responsible-for-command-key nil
  "Coordination for multishell key assignment.")
(defun multishell-activate-command-key-setter (symbol setting)
  "Implement `multishell-activate-command-key' choice."
  (set-default symbol setting)
  (when (or setting multishell--responsible-for-command-key)
    (multishell-implement-command-key-choice (not setting))))
(defun multishell-implement-command-key-choice (&optional unbind)
  "If settings dicate, implement binding of multishell command key.

If optional UNBIND is true, globally unbind the key.

* `multishell-activate-command-key' - Set this to get the binding or not.
* `multishell-command-key' - The key to use for the binding, if appropriate."
  (when (bound-and-true-p multishell-command-key)
    (if unbind
        (global-unset-key multishell-command-key)
      (when (bound-and-true-p multishell-activate-command-key)
        (setq multishell--responsible-for-command-key t)
        (global-set-key multishell-command-key 'multishell-pop-to-shell)))))

(defcustom multishell-activate-command-key nil
  "Set this to impose the `multishell-command-key' binding.

You can instead manually bind `multishell-pop-to-shell' using emacs
lisp, eg: (global-set-key \"\\M- \" \\='multishell-pop-to-shell)."
  :type 'boolean
  :set #'multishell-activate-command-key-setter)

;; Implement the key customization whenever the package is loaded:
(if (fboundp 'with-eval-after-load)
    (with-eval-after-load "multishell"
      (multishell-implement-command-key-choice))
  (eval-after-load "multishell"
    '(multishell-implement-command-key-choice)))

(defcustom multishell-pop-to-frame nil
  "*If non-nil, jump to a frame already showing the shell, if another one is.

Otherwise, disregard already-open windows on the shell if they're
in another frame, and open a new window on the shell in the
current frame.

\(Use `pop-up-windows' to change multishell other-window vs
current-window behavior.)"
  :type 'boolean)

(defcustom multishell-history-entry-tracks-current-directory t
  "Maintain shell's current directory in its multishell history entry.

When set, the history entry for shells started with explicit
paths will track the shell's current working directory.

If `savehist-save-minibuffer-history' is enabled, the current
working directory of shells will be conveyed between Emacs sessions."
 :type 'boolean)

(defvar multishell-history nil
  "Name/path entries, most recent first.")
;; Migrate the few pre 1.0.5 users to changed history var:
(when (and (not multishell-history)
           (boundp 'multishell-buffer-name-history)
           multishell-buffer-name-history)
  (setq multishell-history multishell-buffer-name-history
        multishell-buffer-name-history nil))

(defvar multishell-primary-name "*shell*"
  "Default shell name for un-modified multishell-pop-to-shell buffer target.

This is set by `multishell-pop-to-shell' as the current default,
when invoked with doubled universal argument.

If you want the designated primary that you have at the end of
one emacs session to be resumed at the next, customize
`savehist-additional-variables' to include the
`multishell-primary-name'.")

(defvar multishell-completing-read nil
  "Internal use, conveying whether or not we're in the midst of a multishell
completing-read.")

;; Multiple entries happen because completion also adds name to history.
(defun multishell-register-name-to-path (name path)
  "Add or replace entry associating NAME with PATH in `multishell-history'.

If NAME already had a PATH and new PATH is empty, retain the prior one.

Promote added/changed entry to the front of the list."
  ;; Add or promote to the front, tracking path changes in the process.
  (let* ((entries (multishell-history-entries name))
         (path (or path "")))
    (dolist (entry entries)
      (when (string= path "")
        ;; Retain explicit established path.
        (setq path (cadr (multishell-split-entry entry))))
      (setq multishell-history (delete entry multishell-history)))
    (setq multishell-history (push (concat name path)
                                   multishell-history))))

(defun multishell-replace-entry (entry revised)
  "Replace every instance of ENTRY in `multishell-history' with REVISED.

Revised entry is situated where former one was.

Returns non-nil iff any changes were made."
  (let ((candidates multishell-history)
        did-revisions)
    (while (setq candidates (member entry candidates))
      (setcar candidates revised)
      (setq candidates (cdr candidates)
            did-revisions t))
    did-revisions))

(defun multishell-history-entries (name)
  "Return `multishell-history' entry that starts with NAME, or nil if none."
  (let (got)
    (dolist (entry multishell-history)
      (when (and (string-equal name (multishell-name-from-entry entry))
                 (not (member entry got)))
        (push entry got)))
    got))

;;;###autoload
(defun multishell-pop-to-shell (&optional arg name here)
  "Easily navigate to and within multiple shell buffers, local and remote.

Use a single `universal-argument' (\\[universal-argument]) to launch and choose between
nalternate shell buffers, and a doubled universal argument to also set your
choice as the ongoing default.  Append a path to a new shell name to launch
a shell in that directory, and use Emacs tramp syntax to launch a remote
shell. There is a shortcut to manage your list of current and
historical shells, collectively, using `multishell-list' - see below.

Customize-group `multishell' to set up a key binding and tweak behaviors.

Manage your collection of current and historical shells by
recursively invoking \\[multishell-pop-to-shell] at the `multishell-pop-to-shell'
universal argument prompts, eg:

  \\[universal-argument] \\[multishell-pop-to-shell] \\[multishell-pop-to-shell]

\(That will be just a few keys if you do the above customization.)

Hit ? in the listing buffer for editing commands.

==== Basic operation:

 - If the current buffer is in shell-mode then focus is moved to
   the process input point.

   \(Use a universal argument go to a different shell buffer
   when already in a buffer that has a process - see below.)

 - If not in a shell buffer, go to a window that is already
   showing a shell buffer, if any.

   In this case, the cursor is not moved to the process input
   point. Repeating the command once you're in the buffer will
   then move the cursor to the process input point.

   We respect `pop-up-windows', so you can adjust it to set the
   other-buffer/same-buffer behavior.

 - Otherwise, start a new shell buffer, using the current
   directory as the working directory.

If a buffer with the resulting name exists and its shell process
was disconnected or otherwise stopped, it's resumed.

===== Universal arg to start and select between named shell buffers:

You can assign a distinct name to new shell buffers by prefixing
your \\[multishell-pop-to-shell] invocation with a single or double
`universal-argument', \\[universal-argument]:

 - With a single universal argument, prompt for the buffer name
   to use (without the asterisks that shell mode will put around
   the name), defaulting to `shell'.

   Completion is available.

   This combination makes it easy to start and switch across
   multiple shell restarts.

 - A double universal argument will prompt for the name and set
   the default to that name, so the target shell becomes the
   primary.

   See `multishell-primary-name' for info about preserving the
   setting across emacs restarts.

 - Manage your collection of current and historical shells by
   recursively invoking \\[multishell-pop-to-shell] at the `multishell-pop-to-shell'
   universal argument prompts, or at any time via
   \\[multishell-list]. Hit ? in the listing buffer for editing
   commands.

===== Select starting directory and remote host:

The shell buffer name you give to the prompt for a universal arg
can include an appended path. That will be used for the startup
directory. You can use tramp remote syntax to specify a remote
shell. If there is an element after a final `/', that's used for
the buffer name. Otherwise, the host, domain, or path is used.

For example:

* `#root/sudo:root@localhost:/etc' for a buffer named \"*#root*\" with a
  root shell starting in /etc.

* `/ssh:example.net:' for a shell buffer in your homedir on example.net.
  The buffer will be named \"*example.net*\".

* `#ex/ssh:example.net|sudo:root@example.net:/var/log' for a root shell
  starting in /var/log on example.net named \"*#ex*\".

* `interior/ssh:gateway.corp.com|ssh:interior.corp.com:' to go
  via gateway.corp.com to your homedir on interior.corp.com.  The
  buffer will be named \"*interior*\". You could append a sudo
  hop to the path, combining the previous example, and so on.

File visits from the shell, and many common emacs activities like
dired, will be on the host where the shell is running, in the
auspices of the target account, and relative to the current
directory.

You can change the startup path for a shell buffer by editing it
at the completion prompt. The new path will not take effect for
an already-running shell.

To remove a shell buffer's history entry, kill the buffer and
affirm removal of the entry when prompted.

===== Activate savehist to retain shell buffer names and paths across Emacs restarts:

To have emacs maintain your history of shell buffer names and paths,
customize the savehist group to activate savehist."

  (interactive "P")

  (let ((token '(token)))
    (if (window-minibuffer-p)
        (throw 'multishell-minibuffer-exit token)
      (let ((got (catch 'multishell-minibuffer-exit
                   (multishell-pop-to-shell-worker arg name here))))
        ;; Handle catch or plain fall-through - see cond comments for protocol.
        (cond
         ;; Caught token from recursive invocation in minibuffer:
         ((equal token got) (multishell-list))
         ;; Caught specifaction of multishell args, eg from multishell-list:
         ((listp got) (multishell-pop-to-shell-worker (nth 2 got)
                                                      (nth 0 got)
                                                      (nth 1 got)))
         ;; Regular fallthrough - just relay the result:
         (t got))))))

(defun multishell-pop-to-shell-worker (&optional arg name here)
  "Do real work of `multishell-pop-to-shell', which see."
  (let* ((from-buffer (current-buffer))
         (from-buffer-is-shell (derived-mode-p 'shell-mode))
         (primary-name-unbracketed (multishell-unbracket
                                    multishell-primary-name))
         (fallthrough-name (if from-buffer-is-shell
                               (buffer-name from-buffer)
                             primary-name-unbracketed))
         (doublearg (equal arg '(16)))
         (target-name-and-path
          (multishell-resolve-target-name-and-path
           (cond (name name)
                 (arg
                  (or (multishell-read-unbracketed-entry
                       (format "Shell buffer name [%s]%s "
                               primary-name-unbracketed
                               (if doublearg " <==" ":")))
                      primary-name-unbracketed))
                 (t fallthrough-name))))
         (use-path (cadr target-name-and-path))
         (target-shell-buffer-name (car target-name-and-path))
         (target-buffer (get-buffer target-shell-buffer-name))
         (curr-buff-proc (get-buffer-process from-buffer))
         inwin
         already-there)

    ;; Register early so the entry is pushed to the front:
    (multishell-register-name-to-path (multishell-unbracket
                                       target-shell-buffer-name)
                                      use-path)

    (when doublearg
      (setq multishell-primary-name target-shell-buffer-name))

    ;; Situate:

    (cond

     ((and (or curr-buff-proc from-buffer-is-shell)
           (not arg)
           (eq from-buffer target-buffer)
           (not (eq target-shell-buffer-name (buffer-name from-buffer))))
      ;; In a shell buffer, but not named - stay in buffer, but go to end.
      (setq already-there t))

     ((string= (buffer-name) target-shell-buffer-name)
      ;; Already in the specified shell buffer:
      (setq already-there t))

     ((or (not target-buffer)
          (not (setq inwin
                     (multishell-get-visible-window-for-buffer target-buffer))))
      ;; No preexisting shell buffer, or not in a visible window:
      (when (not (get-buffer target-shell-buffer-name))
        (message "Creating new shell buffer '%s'" target-shell-buffer-name))
      (if here
          (switch-to-buffer target-shell-buffer-name)
        (pop-to-buffer target-shell-buffer-name pop-up-windows)))

     ;; Buffer exists and already has a window - jump to it:
     (t (if (and multishell-pop-to-frame
                 inwin
                 (not (equal (window-frame (selected-window))
                             (window-frame inwin))))
            (select-frame-set-input-focus (window-frame inwin)))
        (if (not (string= (buffer-name (current-buffer))
                          target-shell-buffer-name))
            (if here
                (switch-to-buffer target-shell-buffer-name)
              (pop-to-buffer target-shell-buffer-name t)))))

    ;; We're in the buffer. Activate:

    (if (not (comint-check-proc (current-buffer)))
        (multishell-start-shell-in-buffer use-path))

    ;; If the destination buffer has a stopped process, resume it:
    (let ((process (get-buffer-process (current-buffer))))
      (if (and process (equal 'stop (process-status process)))
          (continue-process process)))

    (when (or already-there
             (equal (current-buffer) from-buffer))
      (goto-char (point-max))
      (and (get-buffer-process from-buffer)
           (goto-char (process-mark (get-buffer-process from-buffer)))))))

(defun multishell-delete-history-name (name &optional ask)
  "Remove all multishell history entries for NAME.

if optional ask is non-nil (default nil), ask before each deletion.

Return the last entry deleted."
  (let (got)
    (dolist (entry (multishell-history-entries name) got)
      (when (and entry
                 (or (not ask)
                     (y-or-n-p (format "Remove multishell history entry `%s'? "
                                       entry))))
        (setq got entry
              multishell-history (delete entry multishell-history))))))

(defun multishell-kill-buffer-query-function ()
  "Offer to remove multishell-history entry for buffer."
  ;; Removal choice is crucial, so users can, eg, kill a shell with huge
  ;; output backlog, while keeping the history entry to easily restart it.
  ;;
  ;; We use kill-buffer-query-functions instead of kill-buffer-hook because:
  ;;
  ;; 1. It enables the user to remove the history without actually killing a
  ;;    running buffer, by not confirming the subsequent running-proc query.
  ;; 2. kill-buffer-hooks often fails to run when killing shell buffers!
  ;;    It's probably due to failures in other hooks - beyond our control -
  ;;    and anyway, I like the first reason well enough.

  ;; (Use condition-case to avoid inadvertant disruption of kill-buffer
  ;; activity.  kill-buffer happens behind the scenes a whole lot.)
  (condition-case err
      (and (derived-mode-p 'shell-mode)
           (multishell-delete-history-name
            (multishell-unbracket (buffer-name))
            t))
    (error
     (message "multishell-kill-buffer-query-function error: %s" err)))
  t)
(add-hook 'kill-buffer-query-functions #'multishell-kill-buffer-query-function)

(defun multishell-get-visible-window-for-buffer (buffer)
  "Return visible window containing buffer."
  (catch 'got-a-vis
    (walk-windows
     (function (lambda (win)
                 (if (and (eq (window-buffer win) buffer)
                          (equal (frame-parameter
                                  (selected-frame) 'display)
                                 (frame-parameter
                                  (window-frame win) 'display)))
                     (throw 'got-a-vis win))))
     nil 'visible)
    nil))

(defun multishell-all-entries (&optional active-duplicated)
  "Return multishell history, with active buffers listed first.

Optional ACTIVE-DUPLICATED will return a copy of
`multishell-history' with unbracketed names of active buffers,
sans paths, appended to the list, so they have short and long
completions."
  ;; Reorder so active lead present lead historical entries:
  (let (active-entries active-names present past splat name buffer)
    (dolist (entry multishell-history)
      (setq splat (multishell-split-entry entry)
            name (car splat)
            buffer (and name (get-buffer (multishell-bracket name))))
      (if (buffer-live-p buffer)
          (if (comint-check-proc buffer)
              (setq active-entries (push entry active-entries)
                    active-names (push name active-names))
            (setq present (push entry present)))
        (setq past (push entry past))))
    ;; Reverse present and past lists
    (setq multishell-history (append (reverse active-entries)
                                     (reverse present)
                                     (reverse past)))
    (if active-duplicated
        (append multishell-history active-names)
      multishell-history)))

(defun multishell-read-unbracketed-entry (prompt &optional initial no-record)
  "PROMPT for shell buffer name, sans asterisks.

Optional INITIAL is preliminary value to be edited.

Optional NO-RECORD prevents changes to `multishell-history'
across the activity.

Input and completion can include associated path, if any.

Return what's provided, if anything, else nil."
  (let* ((was-multishell-history multishell-history)
         (candidates (multishell-all-entries 'active-duplicated))
         (multishell-completing-read t)
         (got
          ;; Use `cl-letf' to dynamically bind multishell-list to
          ;; display-completion-list, so multishell-list is used when doing
          ;; minibuffer-completion-help.
          (cl-letf (((symbol-function 'display-completion-list)
                     #'multishell-list))
              (completing-read prompt
                               ;; COLLECTION:
                               (reverse candidates)
                               ;; PREDICATE:
                               nil
                               ;; REQUIRE-MATCH:
                               'confirm
                               ;; INITIAL-INPUT
                               initial
                               ;; HIST:
                               'multishell-history))))
    (when no-record
      (setq multishell-history was-multishell-history))
    (if (not (string= got ""))
        got
      nil)))

(defun multishell-resolve-target-name-and-path (shell-spec)
  "Given name/tramp-style address shell spec, resolve buffer name and directory.

The name is the part of the string up to the first `/' slash, if
any. Missing pieces are filled in from remote path elements, if
any, and multishell history. Given a tramp-style remote address
and no name part, either the user@host is used for the buffer
name, if a user is specified, or just the host.

Return them as a list: (name path), with name asterisk-bracketed
and path nil if none is resolved."
  (let* ((splat (multishell-split-entry (or shell-spec "")))
         (path (cadr splat))
         (name (or (car splat) (multishell-name-from-entry path))))
    (when (not path)
      ;; Get path from history, if present.
      (dolist (entry
               (multishell-history-entries
                (multishell-unbracket name)))
        (when (or (not path) (string= path ""))
          (setq path (cadr (multishell-split-entry entry))))))
    (list (multishell-bracket name) path)))

(defun multishell-name-from-entry (entry)
  "Derive a name for a shell buffer according to ENTRY."
  (if (not entry)
      (multishell-unbracket multishell-primary-name)
    (let* ((splat (multishell-split-entry entry))
           (name (car splat))
           (path (cadr splat)))
      (or name
          (if (file-remote-p path)
              (let ((host (file-remote-p path 'host))
                    (user (file-remote-p path 'user)))
                (cond ((and host user)
                       (format "%s@%s" user host))
                      (host host)
                      (user user)
                      ((system-name))))
            (multishell-unbracket multishell-primary-name))))))

(declare-function tramp-dissect-file-name "tramp")
(declare-function tramp-cleanup-connection "tramp")

(defun multishell-start-shell-in-buffer (where)
  "Start, restart, or continue a shell in current-buffer on WHERE.

If WHERE is remote and includes a directory, cd to that directory on the
remote host.

If cd fails to an included remote directory, try again without the cd."
  (let* ((is-active (comint-check-proc (current-buffer))))

    (when (and where (not is-active))

      ;; FIXME: file-remote-p does not imply Tramp.
      ;; Why do we need to do something special for Tramp here?
      (when (and (derived-mode-p 'shell-mode) (file-remote-p where))
        ;; Returning to disconnected remote shell - do some tidying.
        ;; FIXME: Without this cleanup, occasionally restarting a disconnected
        ;; remote session, particularly one that includes sudo, results in
        ;; an untraceable "Args out of range" error. That never happens if
        ;; we precedeed connection attempts with this cleanup -
        ;; prophylactic.
        (tramp-cleanup-connection
         (tramp-dissect-file-name default-directory 'noexpand)
         'keep-debug 'keep-password))

      (if (not (file-remote-p where))
          (cd where)
        (message "Connecting to %s" where)
        (condition-case err
            (cd where)
          ;; "cd: No such directory found via CDPATH environment variable"
          (error
           (if (string=
                (cadr err)
                "No such directory found via CDPATH environment variable")
               ;; Try again without dir part of remote where:
               (let* ((final-colon-at (string-match ":[^:]+$" where))
                      (sans-dir-path (substring where 0 (1+ final-colon-at)))
                      (dir-path (substring where (1+ final-colon-at))))
                 (message "Failed to cd to %s, trying again without..."
                          dir-path)
                 (sit-for .5)
                 (cd sans-dir-path))
             (signal (car err) (cdr err)))))))

    (shell (current-buffer))))

(defun multishell-homedir-shorthand-p (dirpath)
  "t if dirpath is an unexpanded remote homedir spec."
  ;; Workaround to recognize tramp-style homedir shorthand, "...:" and "...:~".
  (let ((localname (file-remote-p dirpath 'localname)))
    (and localname
         (or
          ;; No directory path and no connection to expand homedir:
          (string= localname "")
          ;; Original path doesn't equal expanded homedir:
          (save-match-data
            (not (string-match (concat (regexp-quote localname) "/?$")
                               dirpath)))))))
;; (assert (multishell-homedir-shorthand-p "/ssh:myhost.net:")
;; (assert (not (multishell-homedir-shorthand-p "/home/klm")))
;; (assert (not (multishell-homedir-shorthand-p "/ssh:myhost.net:/home/me")))

(defun multishell-track-dirchange (name newpath)
  "Change multishell history entry to track current directory."
  (let* ((entries (multishell-history-entries name)))
    (dolist (entry entries)
      (let* ((name-path (multishell-split-entry entry))
             (path (or (cadr name-path) "")))
        (when path
          (let* ((old-localname (or (file-remote-p path 'localname)
                                    path))
                 (newentry
                  (if (multishell-homedir-shorthand-p path)
                      (concat entry newpath)
                    (replace-regexp-in-string (concat (regexp-quote
                                                       old-localname)
                                                      "$")
                                              ;; REPLACEMENT
                                              newpath
                                              ;; STRING
                                              entry
                                              ;; FIXEDCASE
                                              t
                                              ;; LITERAL
                                              t
                                              )))
                 (membership (member entry multishell-history)))
            (when membership
              (setcar membership newentry))))))))

(defvar multishell-was-default-directory ()
  "Provide for tracking directory changes.")
(make-variable-buffer-local 'multishell-was-default-directory)
(defun multishell-post-command-business ()
  "Do multishell bookkeeping."
  ;; Update multishell-history with dir changes.
  (condition-case err
      (when (and multishell-history-entry-tracks-current-directory
                 (derived-mode-p 'shell-mode))
        (let ((curdir (if (file-remote-p default-directory)
                          (file-remote-p default-directory 'localname)
                        default-directory)))
          (when (not (string= curdir (or multishell-was-default-directory "")))
            (multishell-track-dirchange (multishell-unbracket (buffer-name))
                                        curdir))
          (setq multishell-was-default-directory curdir)))
    ;; To avoid disruption as a pervasive hook function, swallow all errors:
    (error
     (message "multishell-post-command-business error: %s" err))))
(add-hook 'post-command-hook #'multishell-post-command-business)

(defun multishell-split-entry (entry)
  "Given multishell name/path ENTRY, return the separated name and path pair.

Returns nil for empty parts, rather than the empty string."
  (save-match-data
    (string-match "^\\([^/]*\\)\\(/?.*\\)?" entry)
    (let ((name (match-string 1 entry))
          (path (match-string 2 entry)))
      (and (string= name "") (setq name nil))
      (and (string= path "") (setq path nil))
      (list name path))))
(defun multishell-bracket (name)
  "Return a copy of name, ensuring it has an asterisk at the beginning and end."
  (if (not (string= (substring name 0 1) "*"))
      (setq name (concat "*" name)))
  (if (not (string= (substring name -1) "*"))
      (setq name (concat name "*")))
  name)
(defun multishell-unbracket (name)
  "Return a copy of name, removing asterisks, if any, at beginning and end."
  (if (string= (substring name 0 1) "*")
      (setq name (substring name 1)))
  (if (string= (substring name -1) "*")
      (setq name (substring name 0 -1)))
  name)

(provide 'multishell)

;;; multishell.el ends here
