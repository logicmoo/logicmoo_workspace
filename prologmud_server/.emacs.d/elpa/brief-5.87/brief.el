;;; brief.el --- Brief Editor Emulator (Brief Mode)

;; Copyright (C) 2018  Free Software Foundation, Inc.

;; Author:       Luke Lee <luke.yx.lee@gmail.com>
;; Maintainer:   Luke Lee <luke.yx.lee@gmail.com>
;; Keywords:     brief, emulations, crisp
;; Version:      5.87
;; Package-Type: multi

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;
;; This Extended Brief editor emulator (Brief mode) was originally
;; based on the CRiSP mode emulator (listed below), with completely
;; rewriting over 17+ years.  Almost all of the original Brief 3.1
;; Editor keys are implemented and extended.
;;
;; A lot of editor behaviors were also adapted to Emacs.  One example
;; is that this Brief emulator is able to respect `visual-line-mode',
;; `toggle-truncate-lines' and hidden/abbreviated mode texts (which
;; usually shown as "..." in Emacs) such as org-mode, hideshow mode
;; and hideif mode (hide C/C++ "#ifdef" conditional compilation units,
;; another package that I had rewritten).  When texts are hidden at
;; cursor line, Brief line-oriented commands like line-cut, line-copy,
;; line-deletion ... will operate on all hidden lines.
;;
;; For a full key commands list of this Brief Mode, please read the
;; file `README.org' in this package.
;;
;; For a complete description of the functions extended, please search
;; the "brief.el" source code for ";;; Brief Extension:".
;; Main TOC is:
;;
;;  * Visual mode, line truncation and hidden texts
;;  * Fast line number computation cache
;;  * Huge clipboard/Xselection texts
;;  * External Xselection helper programs `xsel' and `xclip'
;;  * Fast cursor movement
;;  Key Binding Compatibility Note
;;  Platform Compatibility Notes
;;  Enabling Brief Mode
;;  Cygwin 2.x Users
;;
;; Based CRiSP version info:
;;  CRiSP mode from XEmacs: revision 1.34 at 1998/08/11 21:18:53.
;;
;;   CRiSP mode was created on 01 Mar 1996 by
;;   "Gary D. Foster <gfoster@suzieq.ml.org>"
;;

;;; History:

;;
;; Starting from 31 May 2001 I started modifying XEmacs CRiSP mode
;; emulator for my own use on Linux, in order to get a similar
;; finger-feel like the MS-DOS-based Brief editor which I had been
;; using for ~10 years at that time.  Those days around 2001 the
;; MS-DOS backward compatibility was getting worse and worse for
;; every MS-Windows release and which forced me to find a free
;; replacement for Brief.  Then I found the XEmacs+CRiSP mode that
;; seems to be the best candidate at that moment.  It implemented at
;; least the basic Brief editor functionality but far from a complete
;; emulator.  Therefore I started doing modifications and adding
;; missing Brief features on a per-need basis.
;;
;; After 17+ years of intermittent development (as the release of
;; version 5.80), the code is almost completely rewritten compare to
;; the original XEmacs CRiSP mode code.
;;
;; The original Emacs version of "crisp.el" is now obsolete but still
;; (temporarily) exists in the Emacs source code repository at
;; "<emacs_git_repo>/src/lisp/obsolete/crisp.el".  In case of it being
;; removed from Emacs source tree someday, I moved the whole
;; development history from Emacs source into ELPA repository at
;; "<elpa>/packages/crisp".  To check the whole development history
;; log you need to clone the ELPA git repository, then go to
;; sub-directory "packages/crisp" and type the command
;; "git log --follow crisp.el".  Notice the "--follow" argument,
;; if lacking of it git will only give you the history "after" the
;; directory structure changed, which was introduced when moving
;; CRiSP from Emacs source repository to ELPA.
;;

;;; Brief Extension:

;;
;; This Brief mode extends a lot of functionalities that the original
;; Brief editor don't have.  For example, the original Brief <ctrl>-<->
;; (C--) kills current buffer; however, when prefixed with <ctrl>-u
;; (C-u) the <ctrl>-<-> (C--) command will restore the just killed
;; buffer.  This is convenient as sometimes user accidentally hit C--
;; for some reason and lost the buffer.  Another simple example is the
;; original Brief command <alt>-f (M-f) which shows the file name of
;; the current buffer; while prefixed with C-u it will also copy the
;; file name into the clipboard.  The following sections list some of
;; the major extensions of this extended Brief mode.
;;
;; * Visual mode, line truncation and hidden texts:
;;   (config option: `brief-linecmd-respect-visual')
;;
;; In the original DOS Brief editor, press <home> key once brings the
;; cursor to the beginning of the current line; a second <home> key to
;; the top of the window and the third <home> to the beginning of the
;; file.  In Emacs it's a bit different; it has `toggle-truncate-lines'
;; to wrap long lines and `visual-line-mode' to do smart word-warp.
;; This Brief mode emulator thus adapted to Emacs accordingly: if long
;; lines are wrapped, the first <home> keystroke brings the cursor to
;; the beginning of the "visual line"; a second <home> then brings the
;; cursor to the beginning of the "physical line".  The third <home> to
;; the top of the window and finally the fourth <home> to the beginning
;; of the file.  When `toggle-truncate-lines' is enabled and
;; `visual-line-mode' is off, Brief mode then behaves the same as DOS
;; Brief.  Similar behavior applies to the <end> key, as well as many
;; other keyboard commands like clipboard copy/paste functions.
;;
;; This further leads to an user-customizable option:
;; `brief-linecmd-respect-visual'.  This customized variable determines
;; if a user would like to have line commands (like line-copy, line-cut,
;; line-deletion...) by default respect visual mode or not.  That
;; variable is by default nil and thus line commands will by default
;; operate on "physical" line(s) instead of "visual" lines.  They
;; can be overridden by a common prefix C-u or a negative prefix
;; number.
;;
;; See the comments of the function `brief-is-visual-operation' for
;; more detail.
;;
;;  Basically, the rules are:
;;
;;   1. `brief-linecmd-respect-visual' determines the current default
;;      mode, either visual or physical line mode.
;;   2. Either a common prefix C-u (\\[universal-argument]) or negative
;;      prefix reverse the current mode; aka,
;;      (not `brief-linecmd-respect-visual').
;;
;;  Example:
;;
;; Try editing a text using line deletion command [M-d] on some very
;; long lines a few times longer than the window width (or shrink
;; window size), or an org-mode document with long lines. Now either
;; turn on `visual-line-mode' or enable line wrapping using
;; `toggle-truncate-lines'.  Assuming `brief-linecmd-respect-visual'
;; is kept default value 'nil' which means physical line operations
;; are expected by default, now try the following key combinations:
;;
;;   1. [M-d]               : delete one "physical" line.
;;   2. [C-u] [M-d]         : delete one "visual" line.
;;   3. [C-4] [M-d]         : delete four "physical" lines as it's
;;                            prefixed with integer '4'.
;;   4. [C-u] [-] [4] [M-d] : delete four "visual" lines as it's
;;                            prefixed with integer '-4'.
;;
;; On the other hand, cursor movement always respects visual mode
;; unless prefixed with C-u or negative prefix number.
;;
;; * Fast line number computation cache:
;;   (config option: `brief-replace-emacs-func:line-number-at-pos')
;;
;; Brief editor window operation relies heavily on cursor position to
;; determine which window is going to be operated.  As the original
;; Emacs line number computation is too slow (especially when a lot
;; of texts are hidden within a window) so a line counting caching
;; mechanism is implemented.  By default, the original Emacs function
;; `line-number-at-pos' is replaced by `brief-fast-line-number-at-pos'
;; if Emacs version is older than 27 where native line numbering is
;; not yet supported.  There is actually a custom variable named
;; `brief-replace-emacs-func:line-number-at-pos' defined near the end
;; of this file.  It controls if the `line-number-at-pos' is going
;; to be replaced or not, regardless of the Brief mode is on or off.
;;
;; It is recommended to keep the default value for fast line number
;; calculation which helps Linum mode as well, otherwise, you might
;; experience big slow down when switching windows using Brief window
;; commands (i.e. [F1] [<arrow>] keys).
;;
;; * Huge clipboard/Xselection texts:
;;   (config option: `brief-enable-postpone-selection')
;;
;; In the following context and code the term Xselection and clipboard
;; are used interchangeably and refer to the same thing - the XWindow
;; selection (mainly "primary" and "clipboard", "secondary" is not
;; used) or the Windows clipboard.  This emulator tries to sync kill-
;; ring with Xselection even in terminal mode (Xterm).
;;
;; This Brief mode also implements an asynchronous mechanism to
;; prevent the selected texts from immediately going into the
;; Xselection, as all (most) *NIX GUI program currently do -- any
;; marked/selected text got copied into Xselection with no delay.
;;
;; This helps to boost performance a lot especially when huge texts
;; (say, 500Mb texts) are being selected.  There is no point to keep
;; copying texts into clipboard before user completed the text
;; selection.  When the user tries to enlarge the huge selection area,
;; copying texts into clipboard could take very long and the system
;; seems to stop responding.  The auto-repeated cursor movement key
;; commands are then accumulated into the key queue and make things
;; worse.  Our Brief mode does not immediately copy texts into
;; Xselection until the cursor movement key is released, which usually
;; means a user has completed (say, at least temporarily) the
;; selection.  Unfortunately, this key-releasing event cannot be seen
;; within Emacs Lisp so we workaround this by detecting if the keyboard
;; is idle or not. Functions are called asynchronously by the idle
;; timer.
;;
;; There is also another important reason for doing so, on Xservers
;; running on MS Windows with MS office running in the background,
;; this problem is fatal and usually caused a system crash in the end.
;; With this selection postponement, we prevented that.  For more
;; detail description about the MS problem, please search the comments
;; in the following source code containing "Xselection Postponement:".
;; The solution is not perfect but works for most cases.
;;
;; A custom option `brief-enable-postpone-selection' is defined to
;; control if the selection postponement is enabled or not.  By
;; default, it's turned on.
;;
;; * External Xselection helper programs `xsel' and `xclip':
;;   (config option: `brief-use-external-clipboard-when-possible')
;;
;; In order to further boost the performance when a user selects huge
;; texts, external Xselection helper programs are used, although it
;; increased the complexity of the asynchronous event handling.  The
;; main reason is that the internal Emacs Xselection code is
;; un-interruptible and not quit-able.  When Emacs tries to get/set
;; Xselection, the operation is blocking and Emacs is not responding
;; to any key before the operation completes.
;;
;; For example, when a user selects some huge texts using arrow keys
;; and paused a short period of time then try to continue enlarging/
;; shrinking the selection area using arrow keys, the previous Emacs
;; Xselection get/set operation is usually not completed yet (due to
;; the size of huge text).  Therefore the attempt to continue
;; enlarging/shrinking texts will be blocked and user will experience
;; no response until Emacs completed the Xselection operation.  What's
;; more, those continued arrow keys then in turn queued in the Xwindow
;; system.  When Emacs eventually resume responding, the selection
;; area will be changed accordingly to some earlier keys in the queue
;; that the user typed a few seconds ago.  This kind of non-real-time
;; response usually confuse the user and sometimes cause problems.
;;
;; To workaround this we implemented our own non-blocking and quit-able
;; Xselection get/set functions, with the help of external X utilities
;; like `xsel' or `xclip'.  Therefore user needs to install `xsel' or
;; `xclip' in the *NIX system to get the benefit.  If neither xsel nor
;; xclip is installed, it will fall back using the Emacs internal
;; blocking Xselection code.  With our non-blocking implementation,
;; when a user tries to enlarge/shrink/quit a huge text selection
;; while the Xselection gets/sets operation is in-progress, the
;; unfinished operation will be interrupted immediately.  A user gets
;; an immediate response.
;;
;; A custom variable `brief-use-external-clipboard-when-possible' (by
;; default 't) controls if this Brief mode would invoke external helper
;; or not.  For further detail consult the help message for function
;; `brief-external-set-selection' and `brief-external-get-selection'.
;; There are also a few other configuration options controlling if
;; the progress is showing in the message area or not:
;;    `brief-show-external-clipboard-recv-progress'
;;    `brief-show-external-clipboard-send-progress' ...
;; With these two options, the user can see the progress when big
;; clipboard data are transferring.
;;
;; Also, notice that for terminal mode Emacs under X running Brief mode
;; the external helpers are always invoked in order to copy/paste texts
;; with any other windowed mode applications.
;;
;; * Fast cursor movement:
;;
;; What I like most about the old DOS Brief editor is its super fast
;; cursor movement.  On a lot of occasions, I didn't even need to do
;; page-up or page-down.  Once you get used to the cursor speed you
;; really don't want to return to the normal speed.  Unfortunately,
;; this cannot be done in Emacs.  However, there is an alternative way
;; to achieve similar effect -- setting X keyboard autorepeat rate
;; using the command line `xset' X utility:
;;
;;  # xset r rate 255 100
;;
;; Although it's still not as fast as DOS Brief editor, it's at least
;; closer.  Also, notice that this will affect the global X window
;; environment.
;;

;;; Key Binding Compatibility Note:

;; Notice that the original brief commands <Keypad-'+'> (copy line) and
;; <Keypad-'-'> (cut line) are now duplicated into <Ctrl>-<insert>
;; (copy line) and <Shift>-<delete> (cut line).  This is mainly for
;; keyboards and notebooks without a keypad.
;;
;; As both <Alt>-n (M-n) and <Alt>-p (M-p) are so widely used in
;; various Emacs modes so in the Brief mode [M-n] and [M-p] are
;; assigned "weakly". This means any major mode can override the
;; assignment of [M-n] and [M-p].  In Brief mode, [M-n] is used for
;; switching to next buffer while [M--] for the previous buffer.  Both
;; of them are frequently used commands, therefore, we also weakly
;; assigned [M-+] and [M-=] for [M-n] and [M-_] for [M--].  However,
;; for [M-p] (brief-prit) we do not provide other key combinations for
;; the same key.
;;
;; For line marking commands, as Emacs regions always start/end at
;; cursor so the Brief mode region commands <Alt>-<L> <arrow> ([M-L]
;; prefixed arrow keys) behave a bit differently from the original
;; Brief ones (unless I implemented it using the secondary Xselection,
;; which could lead to more issues).  This Brief mode does not
;; immediately mark the current line until the followed arrow key user
;; typed.  Also, the 'paste' command key <insert> also insert keys
;; starting from the cursor position instead of the beginning of
;; current line.  On the other hand, 'copy line', 'cut line' and
;; 'delete line' commands work on the beginning of current line like
;; the original Brief editor did.
;;
;; The Brief 3.1 <Ctrl>-<X> (C-x) key for "write all and exit brief"
;; is disabled as it's a prefix key used frequently in various Emacs
;; commands.  This is similarly for <Ctrl>-<U> (C-u), the Brief 'redo'
;; command.  To perform a redo during a sequence of undos (using
;; <Alt>-<U>s (M-u)), just hit any arrow key then the 'undo' command
;; will start redo .  The <Esc> key in Brief 3.1 was for command
;; cancellation; however in Emacs it's replaced by <Ctrl>-<G> (C-g)
;; , the standard `keyboard-quit' command.  Usually, pressing three
;; continuous <Esc> has the same effect.
;;
;; For search and replace commands, originally the case-sensitivity
;; toggling key was <Ctrl>-<F5> (C-f5); in this Brief mode it's rebind
;; to key sequence <Ctrl>-<X> <F5> (C-x f5).  The original <Ctrl>-<F5>
;; is used for forward searching the "current" word at cursor, which
;; is a more frequently used command.  The original <Ctrl>-<F6> (C-f6)
;; command for toggling regular expression search is also now used for
;; replacing "current" word at cursor and the regular expression/simple
;; string toggling is rebinded to <Ctrl>-<X> <F6> (C-x f6).
;;
;; For a complete keybinding list, check the following source code
;; commented with "Brief mode key bindings".
;;

;;; Platform Compatibility Notes:

;; This brief mode is mainly tested for X window system, as there are
;; too many combinations of versions (EmacsVers x {OSes} x {XServers})
;; need to be tested but I don't have all environments (and time).
;; Therefore, I mainly tested recent Emacs versions on Linux machines
;; on a daily use basis.  For other combinations listed below they
;; are not heavily tested; usually tested when they are been used.
;;
;; Test Matrix (note: all 64-bit systems):
;;
;; On Linux: (including native X11 Emacs, terminal mode Emacs on xterm
;;            and Linux X11 Emacs running on VcXsrv (1.19.3.4+) under
;;            Win10):
;;      Emacs 23.3.1, 24.4.50.2, 25.2.2, 26.0.50, 26.1 and 27.0.50.
;;      Mainly focusing on Emacs 25 ~ 27.
;;      For Emacs23 and 24, not all the functionality behaves the same
;;        as in Emacs 25 ~ 27 (ex. search/replace in region/rectangle).
;; On Cygwin:
;;      Emacs 25.3.1, 26.1.
;; On Win10:
;;      Emacs 25.1.1 (x86_64-w64-mingw32).
;; On WSL (Windows Subsystem for Linux):
;;      Emacs 24.5.1, terminal mode and on VcXsrv (1.19.3.4+).
;;

;;; Enabling Brief Mode:

;; Enable this package by putting "(require 'brief)" in your .emacs
;; and "(brief-mode 1)" to enable it.  If you want to toggle it later
;; interactively just use "M-x brief-mode" to toggle it on or off.
;; Notice that "M-x" may be overridden if `brief-override-meta-x' is
;; set to non-nil (default value) when you enable brief mode.  In this
;; case use function key #10 "F10 brief-mode" to toggle it off.

;; The default keybindings for Brief mode override the M-x key to exit
;; the editor.  If you don't like this functionality, you can prevent
;; this behavior (or redefine it dynamically) by customizing the value
;; of `brief-override-meta-x' either in your .emacs or interactively.
;; The default setting is t, which means that M-x will by default
;; by default run `save-buffers-kill-emacs' instead of the command
;; `execute-extended-command'.

;; It is recommended to load the Ibuffer package "(require 'ibuffer)"
;; before or after loading Brief mode.  "M-b" will invoke ibuffer
;; to show buffer menu if ibuffer package presents, otherwise it by
;; default runs `buffer-menu'.

;; This package will automatically load the "scroll-lock.el" package if
;; you put "(setq brief-load-scroll-lock t)" in your .emacs before
;; loading this package.  If this feature is enabled, it will bind
;; "<Scroll_Lock>" to the scroll-lock mode toggle.  The scroll-lock
;; package provides the scroll-locking feature in Brief.

;; Finally, if you want to change the string displayed in the modeline
;; when this mode is in effect, override the definition of
;; `brief-mode-mode-line-string' in your .emacs.  The default value is
;; " Brief" which may be a bit lengthy if you have a lot of things
;; being displayed there.

;; All these overrides should go *before* the (require 'brief)
;; statement.

;;; Cygwin 2.x Users:

;; When the default "mintty" application is used to lunch Cygwin, if
;; you would like to run Emacs with Brief mode enabled in mintty's
;; special terminal mode, some mintty window settings need to be
;; changed:
;;
;; 1. Right-click in the Cygwin "mintty" window on the title bar
;; 2. Choose "Options"
;; 3. At the left sided pane choose "Keys" category
;; 4. Disable (uncheck) the following shortcuts:
;;
;;      Copy and Paste (Ctrl/Shift+Ins)
;;      Switch window (Ctrl+[Shift+]Tab)
;;      Zoom (Ctrl+plus/minus/zero)
;;      Alt+Fn shortcuts
;;      Ctrl+Shift+letter shortcuts
;;
;; 5. Click 'Apply' and 'Save'

;;; Code:

(eval-when-compile
  (require 'cl)
  ;; Quiet byte-compiler about argument number changes due to advice functions,
  ;; as well as other warnings that's known to be not important.
  (setq byte-compile-warnings
        '(not
          ;; For backward compatibility's sake, obsolete functions and
          ;; unresolved function (like w32 ones) might be used
          obsolete
          unresolved
          ;; Some advised function causing arguments count change, ignore them
          redefine))
  (if (version< emacs-version "25.0")
      (setq byte-compile-warnings
            (append byte-compile-warnings '(free-vars callargs)))))
;;(eval-when-compile (require 'cl-lib))
;; local variables
;; Relys on `cua-rectangle-mark-mode' to perform rectangle operations.
(require 'cua-base)
(require 'cua-rect)
;; "replace" package is Emacs26 only, for `query-replace', it's autoloaded
;; so we don't need to "require" it explicitly, otherwise it will cause
;; backward compatibility issues.
;;(require 'replace)

(defconst brief-version "5.87"
  "The version of this Brief emulator.")

;;
;; Backward compatibility and inter-platform operative compatibility:
;;

(defvar brief-is-cygwin nil
  "Check if we're currently running under Cygwin.")

;; This value don't change so we only need to check at load/eval time.
(setq brief-is-cygwin
      (or (eq system-type 'cygwin)
          (and (file-exists-p "/dev/clipboard")
               (file-exists-p "/dev/windows")
               (file-exists-p "/proc/version")
               (string= (with-temp-buffer
                          (insert-file-contents "/proc/version" nil 0 10)
                          (buffer-string))
                        "CYGWIN_NT-"))))

(defvar brief-selection-op-legacy nil)

(eval-when (compile load eval)

  (defmacro brief-is-x ()
    `(eq (framep (selected-frame)) 'x))

  (defmacro brief-is-terminal ()
    `(eq (framep (selected-frame)) 't))

  (defmacro brief-is-winnt ()
    `(or (eq system-type 'windows-nt) ;; Is there a terminal mode Emacs for winnt?
         (eq (framep (selected-frame)) 'w32)   ;; Win64 Emacs still return w32
         (eq (framep (selected-frame)) 'w64))) ;; for future, just in case

  (defmacro brief-input-pending-p ()
    (if (version< emacs-version "24.0")
        `(input-pending-p)
      `(input-pending-p t)))

  ;; Define macros for all cases, byte-compilation, interpreting, or loading.
  (if (and (version< emacs-version "25.1")
           ;; in case some backward compatibility layer already loaded
           (not (fboundp 'save-mark-and-excursion)))
      (defmacro save-mark-and-excursion (&rest body)
        "A backward compatibility macro for Emacs version below 25.1.
This macro behaves exactly like `save-excursion' before Emacs 25.1.
After Emacs 25.1 `save-excursion' no longer save mark and point."
        `(save-excursion ,@body)))

  ;; Emacs <= v25
  (unless (boundp 'inhibit-message)
    (defvar inhibit-message nil))

  (unless (fboundp 'process-live-p)
    ;; Copy&paste from Emacs26 for older Emacs
    (defun process-live-p (process)
      "Returns non-nil if PROCESS is alive.
A process is considered alive if its status is `run', `open',
`listen', `connect' or `stop'.  Value is nil if PROCESS is not a
process."
      (and (processp process)
           (memq (process-status process)
                 '(run open listen connect stop)))))

  ;; Backward compatibility functions for Emacs23
  (when (version< emacs-version "24.0")

    (if (and (not (fboundp 'x-get-selection-value))
             (fboundp 'x-get-selection))
      (defun x-get-selection-value ()
        (x-get-selection 'PRIMARY)))

    (unless (fboundp 'file-equal-p)
      ;; Code copy&pasted from Emacs26 files.el for Emacs-23 backward compatibility
      (defun file-equal-p (file1 file2)
        "Return non-nil if files FILE1 and FILE2 name the same file.
If FILE1 or FILE2 does not exist, the return value is unspecified."
        (let ((handler (or (find-file-name-handler file1 'file-equal-p)
                           (find-file-name-handler file2 'file-equal-p))))
          (if handler
              (funcall handler 'file-equal-p file1 file2)
            (let (f1-attr f2-attr)
              (and (setq f1-attr (file-attributes (file-truename file1)))
                   (setq f2-attr (file-attributes (file-truename file2)))
                   (equal f1-attr f2-attr)))))))

    (unless (fboundp 'read-char-choice)
      (defun read-char-choice (prompt chars &optional inhibit-keyboard-quit)
        (let ((inhibit-quit inhibit-keyboard-quit)
              c)
          (while (not (member
                       (setq c (read-char prompt))
                       chars)))
          c))))

  (if (version< emacs-version "24.0")
      ;; a wrapper function to ignore arguments
      (defmacro bookmark-jump-wrapper (bmk func _regionp)
        `(bookmark-jump ,bmk ,func))
    (defmacro bookmark-jump-wrapper (bmk func regionp)
      `(bookmark-jump ,bmk ,func ,regionp)))

  ;; Selection/clipboard related functions and variables
  (when (version< emacs-version "25.1")

    ;;(unless (boundp 'saved-region-selection) ;; Legacy code for XEmacs
    ;;  (defvar saved-region-selection nil))

    (unless (boundp 'gui--last-selected-text-primary)
      (if (boundp 'x-last-selected-text-primary)
          (defvaralias 'gui--last-selected-text-primary
            'x-last-selected-text-primary)
        (defvar gui--last-selected-text-primary nil)))

    (unless (boundp 'gui--last-selected-text-clipboard)
      (if (boundp 'x-last-selected-text-clipboard)
          (defvaralias 'gui--last-selected-text-clipboard
            'x-last-selected-text-clipboard)
      (defvar gui--last-selected-text-clipboard nil)))

    ;; Also try to prevent cases that someone write his own compatibility codes
    (if (brief-is-winnt)
        (progn
          (unless (fboundp 'gui-get-selection)
            (setq brief-selection-op-legacy t)
            (defalias 'gui-get-selection 'w32-get-clipboard-data))
          (unless (fboundp 'gui-set-selection)
            (defun gui-set-selection (_type data)
              (w32-set-selection data)))
          (unless (fboundp 'gui-backend-get-selection)
            (defun gui-backend-get-selection (_selection-symbol _target-type)
              (w32-get-clipboard-data)))
          (unless (fboundp 'gui-backend-set-selection)
            (defun gui-backend-set-selection (_selection value)
              (w32-set-selection value))))

      (unless (fboundp 'gui-get-selection)
        (setq brief-selection-op-legacy t)
        (defalias 'gui-get-selection 'x-get-selection))
      (unless (fboundp 'gui-set-selection)
        (defalias 'gui-set-selection 'x-set-selection))
      (unless (fboundp 'gui-backend-get-selection)
        (defun gui-backend-get-selection (selection-symbol target-type)
          (x-get-selection selection-symbol target-type)))
      (unless (fboundp 'gui-backend-set-selection)
        (defun gui-backend-set-selection (selection value)
          (x-set-selection selection value))))))

(eval-when-compile
  ;; Backward compatibility for Emacs versions without `defvar-local', which
  ;; might not have `make-variable-buffer-local' defined so use
  ;; `make-local-variable' here.
  (unless (fboundp 'defvar-local)
    (defmacro defvar-local (var val &optional doc)
      `(progn
         (defvar ,var ,val ,doc)
         (make-local-variable ',var))))

  (unless (fboundp 'window-body-width) ;; Emacs23
    (defmacro window-body-width ()
      `(window-width))))

;; Helper functions and macros
(defun brief-current-time ()
  "Epoch time in floating point."
  (let ((ct (current-time)))
    (+ (logior (lsh (car ct) 16)
               (cadr ct))
       (* 1e-6 (caddr ct)))))

(defvar brief-calibration-value 0.0
  "Latest calibration value of UI performance.
This value is a measurement of the performance when running Brief in a
slow remote connection.")

(defvar brief-calibration-count 0
  "Number of worst case calibration done so far.")

(defvar brief-calibration-sum 0.0
  "The sum of worst case calibration values measured so far.")

(defvar brief-calibration-ref 0.0
  "Average worst case calibration value of UI performance.
This value an average of `brief-calibration-value' measured so far.
A lot of timeout values are adjusted accordingly to the slowdown
referencing this value.")

(defvar brief-slowdown-factor 1.0
  "A slowdown measurement according to the calibration.
This value is an empirical obtained thru various experiments.")

(defun brief-calibration ()
  "Estimate the UI performance reference values.
Notice that this function sometimes works only if called interactively.
Also, under terminal mode it can't actually get the slowdown."
  (interactive)
  ;; Yes, these can be done in a single setq, but this make things clearer.
  (setq brief-calibration-value
        (- (- (brief-current-time)
              (progn
                ;; TODO: how to obtain the slowdown in terminal mode?
                (redisplay t)
                (brief-current-time)))))
  ;; Record only the worst case. In a lot of cases it takes very short
  ;; period of time to calculate the value.
  (when (> brief-calibration-value
           brief-calibration-ref)
    (setq brief-calibration-sum
          (+ brief-calibration-sum
             brief-calibration-value))
    (setq brief-calibration-count
          (1+ brief-calibration-count))
    (setq brief-calibration-ref
          (/ brief-calibration-sum brief-calibration-count))
    ;; Purely empirical and conjecture values.
    (cond
     ((< brief-calibration-ref 0.1)
      (setq brief-slowdown-factor 1.0))
     ((< brief-calibration-ref 0.2)
      (setq brief-slowdown-factor 2.0))
     ((< brief-calibration-ref 0.3)
      (setq brief-slowdown-factor 3.0))
     ;; Very slow systems
     ((< brief-calibration-ref 0.4)
      (setq brief-slowdown-factor 4.0))
     ((< brief-calibration-ref 0.5)
      (setq brief-slowdown-factor 5.0))
     ((< brief-calibration-ref 0.6)
      (setq brief-slowdown-factor 6.0))
     ((< brief-calibration-ref 0.7)
      (setq brief-slowdown-factor 7.0))
     ;; Extremely slow systems, I wonder if anyone would work in such kind
     ;; of environment.
     (t (setq brief-slowdown-factor 8.0)))))

;;(defvar brief-debugging t) ;; enable debugging here
(eval-when (compile load eval)
  (if (boundp 'brief-debugging)
      (defun brief-dbg-message (&rest args)
        (let ((inhibit-message t)
              (msg (apply 'format args)))
          ;;    (apply 'message args)
          (message "%s %s" (format-time-string "[%H:%M:%S.%3N]" (current-time))
                   msg)))
    (defmacro brief-dbg-message (&rest _))))

(defmacro brief-rectangle-active ()
  "Compatibility macro to test for an active rectangle."
  ;; For earlier versions of Emacs:
  ;;   * Line region active: (region-active-p) = t and mark-active = t
  ;;   * Rectangle active: (region-active-p) = nil and mark-active = t
  ;; (not (region-active-p))
  ;;
  ;; For XEmacs:
  ;;   * Line region active: mouse-track-rectangle-p = nil and mark-active = t
  ;;   * Rectangle active:   mouse-track-rectangle-p = t   and mark-active = t
  `cua--rectangle)

(defun brief-use-region ()
  "In Brief this means either a line region or rectangle is active."
  (or (ignore-errors
        ;; In Emacs<24 sometimes this will invoke `region-end' and cause
        ;; error: "The mark is not set now, so there is no region"
        (use-region-p))
      ;; [2018-02-01 Thu] If we don't include `brief-rectangle-active'
      ;; here, in some rare cases the `use-region-p' will return NIL
      ;; while `brief-rectangle-active' remains non-NIL.  For example, it
      ;; once happened when: (1) marking a rectangle (2) use self-inserting
      ;; commands (3) undo -- at this moment the rectangle will disappear
      ;; and `deactivate-mark-hook' will be executed.  During this period
      ;; of time, any function in the hook will experienced `use-region-p'
      ;; be NIL while `brief-rectangle-active' be non-NIL.
      (brief-rectangle-active)))

(defun brief-region-beginning ()
  "Safe version of `region-beginning' to prevent error signaling."
  (and (brief-use-region)
       (region-beginning)))

(defun brief-region-end ()
  "Safe version of `region-end' to prevent error signaling."
  (and (brief-use-region)
       (region-end)))

(defmacro brief-line-region-active ()
  "Compatibility function to test for an active line region."
  ;; (region-active-p) return t for either rectangle or region
  `(and (region-active-p)
        (not cua--rectangle)))

(defmacro brief-keep-rectangle-unchanged ()
  "Keep rectangle unchanged after cursor commands."
  ;; This prevents CUA post-command handler resize rectangle
  ;; after we changed the `point'
  `(setq cua--buffer-and-point-before-command nil))

(defmacro brief-xor (a b)
  "A macro for exclusive or (xor a b), side effects prevented."
  `(let ((A ,a)  ;; expand only once, prevent side effect
         (B ,b))
     (and (or A B)
          (not (and A B)))))

;;; Customization

(defgroup brief nil
  "Emulator for Brief key bindings."
  :prefix "brief-"
  :group 'emulations)

(defvar brief-mode-original-keymap (current-global-map)
  "The original keymap before Brief emulation mode remaps anything.
This keymap is restored when Brief emulation mode is disabled.")

(defvar brief-global-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map brief-mode-original-keymap)
    map)
  "Brief global keymap for Brief emulation mode.
All the bindings are assigned here instead of directly into the
inherited current global keymap.  In this way we can have brief mode
key assignments placed in the least priority place in order to respect
other minor modes.")

;;[2015-04-13 15:42:21 +0800] I found setting its parent will slow down
;; the system, especially if brief-prefix-F1's parent is set to
;; `brief-global-mode-map', when openning the menu items (shift-f10)
;; it will take "extremely" long to open. Seems that it will lengthen
;; the key searching route a lot.
;;(defvar brief-prefix-meta-l
;;  (let ((map (make-sparse-keymap)))
;;    (set-keymap-parent map `brief-global-mode-map')
;;    map)
;;  "Prefix key M-l for Brief emulation mode.")
(defvar brief-prefix-meta-l (make-sparse-keymap)
  "Prefix key M-l for Brief emulation mode.")

(defvar brief-prefix-F1 (make-sparse-keymap)
  "Prefix key F1 for Brief emulation mode.")

(defvar brief-prefix-F2 (make-sparse-keymap)
  "Prefix key F2 for Brief emulation mode.")

(defvar brief-prefix-F3 (make-sparse-keymap)
  "Prefix key F3 for Brief emulation mode.")

(defvar brief-prefix-F4 (make-sparse-keymap)
  "Prefix key F4 for Brief emulation mode.")

;;
;; Customized Settings
;;

;;;###autoload
(defcustom brief-mode nil
  "Track status of Brief emulation mode.
A value of nil means Brief mode is not enabled.  A value of t
indicates Brief mode is enabled.

Setting this variable directly does not take effect;
use either M-x customize or the function `brief-mode'."
  :set        (lambda (_symbol value) (brief-mode (if value t nil)))
  :initialize 'custom-initialize-default
  :require    'brief
  :type       'boolean
  :group      'brief)

(defcustom brief-search-replace-using-regexp t
  "Determine if search & replace commands using regular expression or string.
This is a buffer local variable with default value 't, which means
regular expression is used for search & replace commands by default."
  :type  'boolean)

(make-variable-buffer-local 'brief-search-replace-using-regexp)

(defcustom brief-mode-mode-line-string " Brief"
  "String to display in the mode line when Brief emulation mode is enabled."
  :type  'string)

(defcustom brief-override-meta-x t
  "Controls overriding the normal Emacs M-x key binding in this Brief emulator.
Normally the Brief emulator rebinds M-x to `save-buffers-exit-emacs',
and provides the usual M-x functionality on the F10 key.  If this
variable is non-nil, M-x will exit Emacs."
  :type  'boolean)

(defcustom brief-enable-less-frequent-keys t
  "Enable less frequently used Brief keys that Emacs native commands are using.
By disabling this flag Emacs native key commands like C-e and C-d are
kept unaltered.  This works only on Brief mode load time. Changing this
variable at run-time has no effect."
  :type  'boolean)

(defcustom brief-warn-meta-x-moved t
  "Show help message when `brief-override-meta-x' is non-nil.
New user might be wondering where the original <M-x> key has gone,
so this shows a message on Brief mode start to notify that that key
combination has been moved to '<f10>' key."
  :type  'boolean)

(defcustom brief-query-exit-emacs t
  "Ask if user really want to exit Emacs, when `brief-override-meta-x' is t.
This option is useful for people who has already been accustomed to
treat M-x as `execute-extended-command' \\[execute-extended-command].  This gives user
the second chance without quitting Emacs immediately and accidentally."
  :type  'boolean)

(defcustom brief-load-scroll-lock nil
  "Controls loading of the Scroll Lock in the Brief emulator.
Its default behavior is to load and enable the Scroll Lock minor mode
package when enabling the Brief emulator.

If this variable is nil when you start the Brief emulator, it
does not load the scroll-lock package."
  :type  'boolean)

(defcustom brief-load-hook nil
  "Hooks to run after loading the Brief emulator package."
  :type  'hook)

;; Some X systems use 'PRIMARY while some use 'CLIPBOARD.
;; Enabling both can ensure data sync but night become slow when
;; clipboard data is huge.
(defcustom brief-select-enable-primary t
  "On XWinodws enable Brief to use 'PRIMARY selection.
For Windows NT systems we always use 'CLIPBOARD and this option has
no effect."
  :type  'boolean)

(defcustom brief-select-enable-clipboard nil
  "On XWindows enable Brief to use 'CLIPBOARD selection.
For Windows NT systems we always use 'CLIPBOARD and this option has
no effect."
  :type  'boolean)

;;
;; Xselection get/set Postponement
;;

(defun brief-set:brief-enable-postpone-selection (sym val)
  "Enable/disable the postponement of setting Xselection.
For more detail check `brief-enable-postpone-selection'."
  (when brief-mode
    ;; Forward references, available only after brief mode is loaded
    (brief-resume-gui-set-selection)
    (if val
        (brief-enable-clipboard-postponement)

      (brief-disable-clipboard-postponement)))
  (set sym val))

(defcustom brief-enable-postpone-selection t
  "Flag for enabling/disabling of the postponement of setting Xselection.

When region/rectangle is being marked, Emacs actively copies marked
region/rectangle into Xselection and behaves like any other X
applications.  However this is sometimes too excessive if the marked
region is huge: a tiny change of the selection requires a huge update
of the Xselection which could take very long.  Moreover, when doing
this under Xservers on Microsoft Windows with any MS-office application
running, it could cause problems if there are a lot of opened Xwindows,
say, 20 Emacs Xwindow frames opened.

For more detail explanation, see the comments in the source code before
the definition of the variable `brief-is-gui-set-selection-postponed'.

Notice that to change the setting of this value, `custom-set-variables'
need to be used or only the value of is changed but it won't have any
real effect until `brief-mode' changed."
  :type  'boolean
  :set   'brief-set:brief-enable-postpone-selection)

(defcustom brief-debounce-keys-microsoft-office nil
;; Workaround MS-office issue, for detail explanation, see the comments
;; in the source code before the definition of the variable
;; `brief-is-gui-set-selection-postponed'.
  "Debounce commands setting Xselection when MS office application is running.
Keyboard commands that \"sets\" Xselection (e.g. marking region) could
be problematic.  This problem only happens when running Emacs on
Xservers under MS-Windows operating system, with *any* of the Microsoft
Office applications running.

This Brief mode already try to prevent this problem by postponing Xselection
copying if the customized variable `brief-enable-postpone-selection' is
set to non-NIL.  However, in case this problem persists on some slower
machines this option is provided to allow users workaround it.

Currently debouncing only implemented in `brief-kill-line' (\\[brief-kill-line]),
`brief-delete-entire-line'(\\[brief-delete-entire-line]) and `brief-copy-line'(\\[brief-copy-line])."
  :type  'boolean)

;;
;; External Xselection (clipboard) helper function related
;;

(defcustom brief-force-set-external-selection t
  "When setting current kill data, always set external selection.
This is useful when multiple editors are sharing the external clipboard."
  :type  'boolean)

(defcustom brief-external-xclipboard-timeout 5
  ;; (/ most-positive-fixnum 10)  ;; will be multiplied by slowdown factor
  "Timeout seconds for Brief to invoke external xclipboard helper program."
  :type  'number)

(defcustom brief-use-external-clipboard-when-possible t
  "Use external clipboard program helper to read clipboard whenever possible.
Emacs internal clipboard function has certain limitations, especially
when processing huge clipboard contents.  With this flag enabled Brief
will always use external clipboard program like 'xclip' to read
clipboard data.  Notice when Brief is running in terminal mode it
always use external clipboard program no matter if this flag is on
or off."
  :type  'boolean)

(defcustom brief-show-external-clipboard-recv-progress t
  "Show progress when reading data from external clipboard helper program.
This is useful when the clipboard data is very large.  Without this
option there might be no visual indication about the (in)completion
of clipboard reading.  By enabling this option the user can see the
progress."
  :type  'boolean)

(defcustom brief-show-external-clipboard-send-progress t
  "Show progress when writing data to external clipboard helper program.
This is useful when the clipboard data is very large.  Without this
option there might be no visual indication about the (in)completion
of clipboard writing.  By enabling this option the user can see the
progress."
  :type  'boolean)

(defcustom brief-external-clipboard-coding-system 'utf-8 ;; 'buffer
  "Customized coding system for external clipboard, default UTF-8.
When set to 'buffer, it will apply current `buffer-file-coding-system'.
This sounds safe but actually it might still cause issues.  For example,
when current buffer is of coding system `utf-8-with-signature-unix'.
When copy and paste strings it sometimes will insert BOM (byte order
mark) into the beginning of the pasted text.

Set this variable to any other fixed coding system symbol like 'utf-16le
is also possible, but might also lead to some unexpected result if the
coding system does not match your buffer.  For a complete list of
available coding system symbols, check the completion list (press
\\[minibuffer-complete]) when running `set-buffer-file-coding-system'
(\\[set-buffer-file-coding-system])"
  :type  'symbol)

(defcustom brief-cygwin-use-clipboard-dev t
  "When running under Cygwin use /dev/clipboard to speed things up.
Forking a subprocess is expensive in Cygwin.  Running external helper
could cause a lot of delay comparing to its virtual clipboard device."
  :type  'boolean)

(defcustom brief-giveup-clipboard-backup-if-huge t
  "Give up backing up original clipboard data if it's huge.
Usually when brief is about to change external clipboard data it will
backup its original data so that if user cancelled current operation
it could restore clipboard.  However, the data size of current clipboard
is not known so it's possible that we are backing up a very huge data.
With this option enabled we can skip backing up to prevent Emacs waits
long."
  :type  'boolean)

(defcustom brief-giveup-clipboard-backup-timeout 1.5
  "Timeout value to give up backing up clipboard data if it's huge.
This works only if `brief-giveup-clipboard-backup-if-huge' is enabled.
Note that there is no direct way to detect the size of current clipboard
contents without first reading it in first.  Therefore we use a general
timeout value instead."
  :type  'number)

(defcustom brief-giveup-clipboard-backup-message t
  "Show visual message if backup timeout reached and we're giving up backup.
This works only if `brief-giveup-clipboard-backup-if-huge' is enabled."
  :type  'boolean)

(defvar brief-xclipboard-args nil
  "Brief internal variable for external Xselection helper.")
;; format: (get-arg
;;          set-arg
;;          select-arg
;;          primary clipboard secondary
;;          (must-have-args-for-get...))

(defvar brief-xclipboard-cmd nil
  "Brief internal variable for external Xselection helper.")

(defconst brief-in-favor-of-xsel-default nil
  "Default value of the `brief-in-favor-of-xsel'.")

(defun brief-xclipboard-cmd-search ()
  "Search for external Xclipboard helper program.
Find either 'xsel' or 'xclip', if both exists in favor of 'xsel' when
`brief-in-favor-of-xsel' is non-nil.  If none-exists only Emacs
internal Xselection functions will be used and terminal mode won't
react to Xselection change.
However, care need to be taken that in some Linux environment, using
'xsel' sometimes produce some extra bytes when getting data from the
Xclipboard."
  (let* ((xsel  (executable-find "xsel"))
         (xclip (executable-find "xclip"))
         ;; Forward reference to brief-in-favor-of-xsel, if
         ;; not yet defined, use its default
         (xc    (if (or (and (boundp 'brief-in-favor-of-xsel)
                             brief-in-favor-of-xsel)
                        brief-in-favor-of-xsel-default)
                    (or xsel xclip)
                  (or xclip xsel))))
    (setq brief-xclipboard-cmd xc)
    (cond
     ((string= xc xsel)
      (setq brief-xclipboard-args
            '("-o"
              "-i"
              "-n" ;; nil
              "-p"      "-b"        "-s"
              nil)))
     ((string= xc xclip)
      (setq brief-xclipboard-args
            '("-o"
              nil
              "-selection"
              "primary" "clipboard" "secondary"
              ("-l" " 1")))))
    xc))

(defun brief-set:brief-in-favor-of-xsel (sym val)
  "Reset `brief-xclipboard-cmd' and `brief-xclipboard-args' so that
it won't stick on 'xclip'."
  (set sym val)
  ;; Reset and search again
  (setq brief-xclipboard-cmd nil
        brief-xclipboard-args nil)
  (brief-xclipboard-cmd-search))

(defcustom brief-in-favor-of-xsel brief-in-favor-of-xsel-default
  "When both 'xclip' and 'xsel' exist, choose 'xsel'.
This is an emperical value.  My original personal experiemnts shows
that 'xsel' seems to respond faster than 'xclip'.  However, on Ubuntu
18, the associated 'xsel' is buggy and tend to return extra garbage at
the end of the pasted data, sigh.  Considering the wide acceptance of
Ubuntu, this force me to set the default setting to use 'xclip'
instead.
For Ubuntu 18 users that would like to use 'xsel' and set this option
as non-nil, please rebuild the 'xsel' from author's git repository:
\"https://github.com/kfish/xsel.git\" and checkout at least commit id
\"9bfc13d\"."
  :type  'boolean
  :set   'brief-set:brief-in-favor-of-xsel)

;;
;; Miscellaneous behavioral configurations
;;

(defcustom brief-shorter-bookmark-jump-key t
  "Swap M-j with M-# key for simpler keystroke for bookmark jump.
The original key combination of jumping to a bookmark is M-j followed
by a digit key, while setting a bookmark is M-# (# is the digit key)
which is a shorter key combination.  However, in real cases jumping
bookmarks happens much more frequently than setting bookmarks,
therefore a shorter key sequence is sometimes more desired.  Setting
this flag non-NIL will achieve this goal."
  :type  'boolean)

(defcustom brief-open-file-use-dialog-when-possible nil
  "Use the file dialog box X window manager provided when possible."
  :type  'boolean)

;; This option works only if `brief-open-file-use-dialog-when-possible' is t,
;;  it also seems not working on some older GTK+ systems. But anyway,
;;  it doesn't cause any problems to be either 't or 'nil.
(defcustom brief-open-file-show-hidden-files t
  "Show hidden files in the file dialog box."
  :type  'boolean)

(defcustom brief-search-recenter-vertically t
  "Recenter cursor vertically when doing search or replacement.
By recentering the cursor, context above/below are shown."
  :type  'boolean)

(defcustom brief-search-recenter-horizontally t
  "Recenter cursor horizontally to the right when doing search or replacement.
By recentering the cursor to the right, context left to the cursor
are shown."
  :type  'boolean)

(defcustom brief-group-undo-replacement t
  "Group undos for search&replace in a (rectangle) region."
  :type  'boolean)

(defcustom brief-search-fake-region-mark t ;nil
  "Mark a fake region when doing search within it.
If we use the same background color as other normal region, it looks
as if our marked region is still there when cursor moved.  Normally
marked region changed according to our cursor."
  :type  'boolean)

(defface brief-fake-region-face
  '((default :inherit region))  ;; 'secondary-selection
  "The face (color) of fake region when `brief-search-fake-region-mark' is t."
  :group 'brief)

(defcustom brief-after-search-hook nil
  "Hook for user defined search behavior.
This hook gets called after each brief search operation."
  :type  'hook)

(defcustom brief-linecmd-respect-visual nil
  "Set this to t if line commands must respect visual/truncation mode.
Line-oriented commands like \"line deletion\" (\\[brief-delete-entire-line]), \"delete till end
of line\" (\\[brief-delete-end-of-line]), \"copy line\" (\\[brief-copy-line]) and \"cut line\" (\\[brief-kill-line]),
usually operate on the whole physical line regardless of current
visual/truncation mode.  With this option enabled, it *will* operate
only on the visual part of current line, *unless* that command is
prefixed (\\[universal-argument]).  Similarly, when visual/truncation mode is turned on,
prefixed (\\[universal-argument]) line commands *will* only operate on visual part of
the line."
  :type  'boolean)

(defcustom brief-skip-buffers '("TAGS")
  "Define the buffer names that Brief don't want to switch to."
  :type  'sexp)

(defcustom brief-apply-slowdown-factor  t
  "Apply the calibrated slowdown factor to all related timeout values.
Under a lot of circumstances the calibrated slowdown factor is too
small.  However, it is not known that if there could be environments
that make this slowdown factor too high and make Brief performance
poor.  If that's the case, set this value to nil to prevent applying
the slowdown factor."
  :type  'boolean)

(defcustom brief-init-multi-cursor-cmd  t
  "Initialize multiple-cursors package for Brief commands.
If this option is set to NIL, when multiple-cursors mode is ON, user
will need to choose if a command is to be executed for each cursor or
just run-once for all cursors and multiple-cursors package will then
register user's selection.  With this option set non-NIL, the choice
of all Brief mode commands are set initially without the need of
user's attention."
  :type  'boolean)

(defcustom brief-turn-off-scroll-bar-mode  nil
  "Turn off scroll-bar mode to save more window area for texts.
As far as I knew quite a few old Brief users love its thin borders.  With
this value set to non-NIL Brief mode turn-off `scroll-bar-mode' which
saves the horizontal scroll bar for one more column of texts."
  :type  'boolean)

;;
;; End of customization variables
;;

(defun brief-slowdown-factor ()
  "Get the calibrated slowdown factor or keep it as 1.0.
If `brief-apply-slowdown-factor' is non-NIL, return the calibrated
slowdown factor; otherwise, return 1.0."
  (or (and brief-apply-slowdown-factor
           brief-slowdown-factor)
      1.0))

;; The cut and paste routines are different between XEmacs and Emacs
;; so we need to set up aliases for the functions.

(defalias 'brief-set-clipboard
  (if (fboundp 'clipboard-kill-ring-save)
      'clipboard-kill-ring-save
    'copy-primary-selection))

(defalias 'brief-kill-region
  (if (fboundp 'clipboard-kill-region)
      'clipboard-kill-region
    'kill-primary-selection))

(defalias 'brief-yank-clipboard
  (if (fboundp 'clipboard-yank)
      'clipboard-yank
    'yank-clipboard-selection))

(defvar brief-last-search-action-forward 't)
;;(defvar brief-query-replace-to-history nil)
;;(defvar query-replace-from-history-variable nil)
;;(defvar query-replace-to-history-variable nil)
(defvar brief-last-query-replace-forward 't)

(defvar brief-orig-query-replace-from-history-variable nil)
(defvar brief-orig-query-replace-to-history-variable nil)
(defvar brief-query-replace-from-history nil)
(defvar brief-query-replace-to-history nil)
;;(defvar c-basic-offset nil)
;;(defvar brief-c-tabs-always-indent nil)
;;(defvar brief-c-insert-tab-function nil)

;; TODO: [2016-05-10 Tue] Should we distinguish the following two?
;; Shouldn't we use the same one?
(defvar brief-search-last nil)

(defvar brief-query-replace-last nil)

;;;;[2011-04-14 Thu 13:53] rem 1
;;(defvar brief-global-case-fold-search nil) ;; default nil : case-sensitive
(defvar brief-get-current-word nil)
(defvar brief-search-history nil)

(defvar brief--kbd-macro-seq nil
  "Internal variable for saving paused keyboard macro")

(defvar-local brief--search-overlay nil
  "A buffer local overlay which records the current search selection.
It is deleted when the search ends or region deactivated.")

;;
;; Brief key commands
;;

(defun brief-buffer-list-window ()
  "Display buffer list.
If `ibuffer' package is loaded it will invoke `ibuffer', otherwise
it calls `buffer-menu' instead."
  (interactive)
  ;; Doing calibration here won't work, the measured delay is very low here.
  ;; (call-interactively 'brief-calibration)
  (if (fboundp #'ibuffer-make-column-filename) ;; prefered buffer mode
      (let ((search-str (ibuffer-make-column-filename (current-buffer) nil))
            (pos -1))
        (if (zerop (length search-str))
            (setq search-str (buffer-name)))
        (ibuffer nil)
        (redisplay) ;; sometimes if we don't do this the texts in buffer won't be ready
        (when search-str
          (save-excursion
            (goto-char (point-min))
            (ignore-errors
              (setq pos (search-forward search-str))))
          (when (/= pos -1)
            (goto-char pos)
            (beginning-of-line)
            (recenter)))
        ;; Sometimes the mark got activated, probably due to earlier marking
        ;; operation in the buffer.
        (deactivate-mark))
    (call-interactively 'buffer-menu)))

(defvar brief-latest-killed-buffer-info nil
  "The info of the most recently killed file buffer to be restored.
If user accidentally killed a file buffer, it can be recovered
accordingly.
Information is a list of:

  buffer file name
  point
  buffer-undo-list
  major mode
  current visual row
  linum-mode (optional, only if linum-mode package is loaded)
  nlinum-mode (optional, only if nlinum-mode package is loaded)
  display-line-numbers-mode (optional, only if supported)
  ;;TODO: font/fontsize/scaled-size/ruler-mode/user-defined-hook
")

(defun brief-kill-current-buffer ()
  "Kill current buffer, or restore latest killed file buffer if prefixed.
When prefix argument presents (\\[universal-argument]) it will try to restore the latest
killed buffer if it's a file buffer, otherwise it warns the user and
do nothing. For non-prefixed calls, it will invoke `kill-buffer' which
usually in turn ask user to save the changes if current buffer was
modified."
  (interactive)
  (if current-prefix-arg

      ;; Restore latest killed buffer
      (let* ((filename (pop brief-latest-killed-buffer-info))
             (buf (and filename
                       (find-file filename)))
             (inhibit-message nil)
             item)
        (if (null filename)
            (message "No killed file buffer to restore")
          (if (null buf)
              (progn
                (setq brief-latest-killed-buffer-info nil)
                (message "Fail restoring killed buffer %S" filename))
            (with-current-buffer buf
              (setq inhibit-message t)
              ;; restore point position
              (goto-char (pop brief-latest-killed-buffer-info))
              ;; restore undo list
              (setq buffer-undo-list (pop brief-latest-killed-buffer-info))
              ;; restore major mode
              (call-interactively (pop brief-latest-killed-buffer-info))
              ;; restore cursor position relative to window
              (and (setq item (pop brief-latest-killed-buffer-info))
                   (fboundp #'undo-window-pos)
                   (apply #'undo-window-pos item nil))
              ;; restore `linum-mode' or `nlinum-mode' if packages loaded
              (and (setq item (pop brief-latest-killed-buffer-info))
                   (boundp 'linum-mode)
                   ;; Use `call-interactively' instead of calling `linum-mode'
                   ;; directly in case `linum-mode' package is not loaded
                   (call-interactively 'linum-mode))
              (and (setq item (pop brief-latest-killed-buffer-info))
                   (boundp 'nlinum-mode)
                   (call-interactively 'nlinum-mode))
              (and (setq item (pop brief-latest-killed-buffer-info))
                   (boundp 'display-line-numbers-mode)
                   (call-interactively 'display-line-numbers-mode)))
            (setq inhibit-message nil)
            (message "Killed buffer %S restored" filename))))

    ;; Save buffer info before killing it for later restoring it
    (let ((latest-killed-buffer-info nil))
      (when buffer-file-name
        (setq latest-killed-buffer-info
              (list major-mode
                    buffer-undo-list
                    (point)
                    buffer-file-name))
        (push (if (fboundp #'undo-window-pos)
                  (brief-current-row-visual))
              latest-killed-buffer-info)
        ;; Run-time detection of `linum-mode', `nlinum-mode' and
        ;; `display-line-numbers-mode'
        (push (if (boundp 'linum-mode)
                  linum-mode)
              latest-killed-buffer-info)
        (push (if (boundp 'nlinum-mode)
                  nlinum-mode)
             latest-killed-buffer-info)
        (push (if (boundp 'display-line-numbers-mode)
                  display-line-numbers-mode)
              latest-killed-buffer-info)
        (setq latest-killed-buffer-info
              (nreverse latest-killed-buffer-info))
        (message
         "To restore killed buffer, prefix buffer kill command [C-u C--] to restore it."))
      (kill-buffer (current-buffer))
      (and latest-killed-buffer-info
           (setq brief-latest-killed-buffer-info
                 latest-killed-buffer-info)))))

(defun brief-delete-region (start end)
  "Delete texts without going into kill-ring or clipboard."
  (delete-region start end))

(defun brief-delete-char (count)
  (delete-char count))

(defun brief-forward-1-char-noerror ()
  "Forward one char and ignore \"end-of-buffer\" error."
  (ignore-errors
      (forward-char 1))
  (point))

(defun brief-rectangle-mode ()
  "Start marking rectangle region."
  (interactive)
  ;; TODO: support native rectangle mode
  (cua-set-rectangle-mark))

;; [2016-04-20 Wed] support multiple cursors
(defun brief-multiple-cursor-in-action ()
  "Check if we are currently running a command on a fake cursor."
  (and (boundp 'mc--executing-command-for-fake-cursor)
       mc--executing-command-for-fake-cursor))

;; [2016-05-24 17:06:41 +0800] support multiple-cursors mode
(defun brief-multiple-cursor-mode-is-on ()
  "Check if we are currently in multiple-cursor mode.
This check differs from `brief-multiple-cursor-in-action' since this
function returns t no matter if we are currently at a real cursor or
fake cursor."
  (and (boundp 'multiple-cursors-mode)
       multiple-cursors-mode))

;; 06/01/2011 ins 5 for Emacs, use kill-whole-line.
;; <2011-06-09 Thu 14:13> removed since it will enter kill-ring

;; 07/03/2007 ins 3 for emacs
;; (define-key brief-global-mode-map [(meta d)]
;;   (lambda ()
;;     (interactive)
;;     (beginning-of-line) (kill-line)))

(defun brief-find-file ()
  "Use file dialog or minibuffer to open a file according to user's preference."
  (interactive)
  (if brief-open-file-use-dialog-when-possible
      (let ((last-nonmenu-event '(t))
            (use-dialog-box t)
            (use-file-dialog t)
            ;; This option seems not working on some old GTK+ systems
            (x-gtk-show-hidden-files brief-open-file-show-hidden-files))
        (find-file (read-file-name
                    (format "Brief: open file in %S" default-directory)
                    nil default-directory nil)))
    (call-interactively 'find-file)))

(defun brief-current-filename ()
  "Dispaly file name of current buffer if it has one.
When prefixed (\\[universal-argument]) it will put current buffer file name into clipboard."
  (interactive)
  (if (not buffer-file-name)
      (message "Current buffer does not have a file name")
    (if (null current-prefix-arg)
        (message "%S" buffer-file-name)
      (brief-copy-region-into-clipboard buffer-file-name)
      (message "%S copied into clipboard"
               buffer-file-name))))

(defun brief-version (&optional arg)
  "Version number of the Brief emulator package.
If ARG is non-NIL, insert results at point."
  (interactive "P")
  (let ((foo (concat "Brief mode version " brief-version)))
    (if arg
        (insert (message foo))
      (message foo))))

;; For Emacs in case mouse-track-rectangle-p not defined

;; <2011-06-02 Thu 17:03> ins func, derived from pc-mode.el
(defun brief-set-mark-here-if-not-active ()
  "Sets the mark at point if it is not active."
  (if (not (brief-use-region))
      ;;(push-mark (point) t)
      (set-mark (point))))

(defun brief-call-list-interactively (alist)
  (if (not (equal alist nil))
      (progn
        (call-interactively (car alist))
        (brief-call-list-interactively (cdr alist)))))

(defun brief-call-mark-line-up-with-key (key)
  (brief-call-list-interactively
   (cons 'brief-mark-line-up (list (key-binding key)))))

(defun brief-call-mark-line-down-with-key (key)
  (brief-call-list-interactively
   (cons 'brief-mark-line-down (list (key-binding key)))))

(defun brief-mark-line-down-with-meta-l ()
  "Double M-L"
  (interactive)
  (brief-call-list-interactively (list 'brief-mark-line-down)))

(defmacro brief-meta-l-key (updown key)
  "Define M-L associated key function and map in `brief-prefix-meta-l'.
UPDOWN is either 'up' or 'down' (with no (') quote) indicating the
direction of the key.
A key function name `brief-call-mark-line-[up|down]-with-<key>' is
created and mapped into `brief-prefix-meta-l'"
  (let* ((dir     (symbol-name updown))
         (keydesc (key-description key))
         (keyfunc (intern (concat "brief-mark-line-" dir "-with-" keydesc))))
    `(progn
       (defun ,keyfunc ()
         ,(concat "Mark line " dir " with " keydesc " key")
         (interactive)
         (,(intern (concat "brief-call-mark-line-" dir "-with-key"))
          ,key))
       (define-key brief-prefix-meta-l ,key ',keyfunc))))

;;
;; Brief bookmarks
;;

(defcustom brief-bookmark-nearby 256
  "Define the distance (chars) of two bookmarks that brief considered nearby."
  :type  'number)

(defvar brief-inhibit-bookmark-try-switch-frame-window nil
  "An internal flag to prevent bookmark jumps switching frame and window.")

(defun brief-frame-list ()
  "Return frame-list with current frame the first.
When running in terminal mode, exclude root frame as it's incorrect for brief
mode to access root frame windows."
  (remove
   (and (bound-and-true-p server-process)
        (car (last (frame-list)))) ;; root frame
   (cons (selected-frame) ;; let current frame be the 1st frame to test
         (remove (selected-frame) (frame-list)))))

(defun brief-bookmark-try-switch-frame-window (bookmark)
  "Try to find a window containing BOOKMARK then jump to the frame and window.
If found, return 't; if not found, jump to the first frame/window
containing the buffer and return symbol 'frame.  If no such buffer/
window/frame exists, keep current frame/window and return NIL.

  This can be used to advice all bookmark jumping functions. For
example, add the following into .emacs:

  (defun bookmark-jump-restoring-frame-window (bookmark &rest args)
    (brief-bookmark-try-switch-frame-window bookmark))

  (advice-add 'bookmark-jump
              :before 'bookmark-jump-restoring-frame-window)
  (advice-add 'bookmark-jump-other-window
              :before 'bookmark-jump-restoring-frame-window)"

  (unless brief-inhibit-bookmark-try-switch-frame-window
    (let ((fname (ignore-errors ;; prevent non-existing bookmark error
                   (bookmark-get-filename bookmark)))
          bname ;; buffer name, no file (e.g. *scratch* buffer)
          (currf (selected-frame))
          (foundframe nil)
          (foundwin nil)
          (result nil)
          bpos
          buf bfname
          tmp)

      (and fname
           (not (file-exists-p (file-truename fname)))
           (setq bname
                 (or (and (setq tmp
                                (cdr (assoc
                                      'buffer-name
                                      (cdr (bookmark-get-bookmark bookmark)))))
                          (stringp tmp)
                          tmp)
                     (and ;;(car
                      ;; (last (car (bookmark-get-bookmark-record bookmark))))
                      (listp (setq tmp
                                   (bookmark-get-bookmark-record bookmark)))
                      (setq tmp (car tmp))
                      (listp (setq tmp (last tmp)))
                      (car tmp)))
                 fname nil))
      (when (or fname bname)
        (catch 'found
          (dolist (f (brief-frame-list))
            (select-frame f)
            (dolist (w (window-list))
              (and (setq buf (window-buffer w))
                   (or
                    (and fname ;; bookmark is in a filed buffer
                         (setq bfname (buffer-file-name buf))
                         (file-equal-p bfname fname))
                    (and bname ;; bookmark is in a buffer with no file
                         (string= bname (buffer-name buf))))
                   ;; Record the first found frame/win
                   (if foundframe
                       t
                     (setq foundframe f
                           foundwin w))
                   ;; Check if bookmark position is already
                   ;; displayed in the window
                   (ignore-errors
                     (setq bpos (bookmark-get-position bookmark))
                     (when (and (>= bpos (window-start w))
                                (<= bpos (window-end w t)))
                       (select-frame-set-input-focus f)
                       (raise-frame f)
                       (select-window w)
                       (throw 'found (setq result t)))))))
          ;; Did not found a window containing bookmark, jump
          ;; to the first window containing the buffer if there is
          (if foundwin
              (ignore-errors
                (select-frame-set-input-focus foundframe)
                (raise-frame foundframe)
                (select-window foundwin)
                (setq result 'frame))
            ;; Not found, back to original frame
            (select-frame currf))))

      result)))

(defun brief-bookmark-do-jump (bookmark)
  "Jump to Brief bookmark #0~#9.  ARG is a char within '0'~'9'."
  (setq bookmark (char-to-string bookmark))
  ;; If not prefixed, try to switch to a existing window that continaing the
  ;; bookmarked file/buffer in order to keep current window/buffer intact.
  (and (boundp 'bookmarks-already-loaded)
       (fboundp 'bookmark-maybe-load-default-file)
       (or bookmarks-already-loaded (bookmark-maybe-load-default-file)))

  ;; Prefixed jump (C-u) won't switch frame/buffer.
  (let ((brief-inhibit-bookmark-try-switch-frame-window current-prefix-arg))
    (brief-bookmark-try-switch-frame-window bookmark)

    (condition-case nil
        (progn
          (bookmark-jump-wrapper bookmark
                                 'switch-to-buffer
                                 (brief-use-region))
          (if (fboundp 'bmkp-light-bookmark)
              (bmkp-light-bookmark (bookmark-get-bookmark bookmark)))
          (message "Jump to bookmark %S" bookmark))
      (error (message (format "Bookmark %S not existed." bookmark))))))

(defun brief-bookmark-do-set (arg) ;; 06/02/'08 ins 1 func
  "Set Brief bookmark #0~#9.  ARG is a char within '0'~'9'."
  (if (minibufferp)
      (error "Can't set bookmark in a minibuffer")
    (setq arg (char-to-string arg))
    (let* ((argn  (string-to-number arg))
           (reg   (+ #36rBRIEF argn)) ;; make it unique
           (temp  (+ #36rTEMP argn))
           (temp2 (+ #36rTEMP2 argn))
           (orig  (get-register reg))
           mark1 mark2
           bmk)
      (ignore-errors
        ;; Try to save current bookmark into REG register
        (save-mark-and-excursion
          (save-window-excursion
            (point-to-register temp)
            (setq mark1 (or (and current-prefix-arg
                                 orig
                                 (jump-to-register reg))
                            (get-register temp)))
            (setq mark2 (ignore-errors
                          (bookmark-jump arg)
                          (point-to-register temp2)))
            (unless
                ;; (equal marker1 marker2)  ;; Do not overwrite REG we're
                ;;                          ;;  bookmarking the same position
                (and (markerp mark1) ;; Do not overwrite REG if two bookmarks
                     ;;              ;;  are nearby, in case we accidentally
                     (markerp mark2) ;;  moved a little bit left, right, up
                     ;;              ;;  or down and mark.
                     (eq (marker-buffer mark1) (marker-buffer mark2))
                     (<= (abs (- (marker-position mark1)
                                 (marker-position mark2)))
                         brief-bookmark-nearby))
              (set-register reg (get-register temp2))))))

      ;; If prefixed by C-u, restore bookmark.
      ;; This help restoring the original bookmark.
      (if (and current-prefix-arg orig)
          (ignore-errors
            (set-register temp orig)
            (save-mark-and-excursion
              (save-window-excursion
                (jump-to-register temp 't)
                (bookmark-delete arg)
                (bookmark-set arg)))
            (message (concat "Backup bookmark #" arg " restored")))

        (bookmark-delete arg)
        (bookmark-set arg)
        (setq bmk (bookmark-get-bookmark arg))
        ;; Support `bookmark+-1' from EmacsWiki
        (and (fboundp 'bmkp-make-bookmark-savable)
             (bmkp-make-bookmark-savable bmk))
        (and (fboundp 'bmkp-light-bookmark)
             (bmkp-light-bookmark bmk))
        (message (concat "Brief bookmark #" arg " set")))

      ;; Now really jump to the bookmark
      (let ((current-prefix-arg nil))
        (brief-bookmark-do-jump (string-to-char arg))))))

(defun brief-bookmark-jump-set (bookmark) ;; 06/02/'08 ins 1 func
  "Jump to bookmark 0~9 if `brief-shorter-bookmark-jump-key' is t.
Otherwise, set the bookmark at cursor.

When performing bookmark set, prefixed with (\\[universal-argument]), it tries to restore
the previous bookmark if there is one.  This is useful when user
miss-typed a bookmark-set command somewhere but forgot where it
originally was.  When the bookmark is successfully restored, again
restoring the bookmark will bring it back to its new location (where
user might just miss-typed).

When performing jump, prefixed with (\\[universal-argument]) will prevent it from
switching window/frame and will search bookmark only within current
window and frame.  See `brief-bookmark-try-switch-frame-window' for
more detail."
    (if brief-shorter-bookmark-jump-key
        (brief-bookmark-do-jump bookmark)
      (brief-bookmark-do-set bookmark)))

(defun brief-bookmark-set-jump (bookmark)
  "Set bookmark 0-9 if `brief-shorter-bookmark-jump-key' is t (default).
Otherwise, jump to it.
When prefixed it won't switch current frame or buffer."
  (interactive (list (read-char-choice
                      (concat
                       (if brief-shorter-bookmark-jump-key
                           (if current-prefix-arg "Try restoring" "Set")
                         "Jump to")
                       " bookmark 0-9 ")
                      '(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?\r ?\e))))

  (if (or (= bookmark ?\r)  ;; press return
          (= bookmark ?\e)) ;; press escape
      (keyboard-quit)
    (if brief-shorter-bookmark-jump-key
        (brief-bookmark-do-set bookmark)
      (brief-bookmark-do-jump bookmark))))

(defun brief-bookmark-jump-set-0 ()
  "Jump/set bookmark '0'."
  (interactive)
  (brief-bookmark-jump-set ?0))

(defun brief-bookmark-jump-set-1 ()
  "Jump/set bookmark '1'."
  (interactive)
  (brief-bookmark-jump-set ?1))

(defun brief-bookmark-jump-set-2 ()
  "Jump/set bookmark '2'."
  (interactive)
  (brief-bookmark-jump-set ?2))

(defun brief-bookmark-jump-set-3 ()
  "Jump/set bookmark '3'."
  (interactive)
  (brief-bookmark-jump-set ?3))

(defun brief-bookmark-jump-set-4 ()
  "Jump/set bookmark '4'."
  (interactive)
  (brief-bookmark-jump-set ?4))

(defun brief-bookmark-jump-set-5 ()
  "Jump/set bookmark '5'."
  (interactive)
  (brief-bookmark-jump-set ?5))

(defun brief-bookmark-jump-set-6 ()
  "Jump/set bookmark '6'."
  (interactive)
  (brief-bookmark-jump-set ?6))

(defun brief-bookmark-jump-set-7 ()
  "Jump/set bookmark '7'."
  (interactive)
  (brief-bookmark-jump-set ?7))

(defun brief-bookmark-jump-set-8 ()
  "Jump/set bookmark '8'."
  (interactive)
  (brief-bookmark-jump-set ?8))

(defun brief-bookmark-jump-set-9 ()
  "Jump/set bookmark '9'."
  (interactive)
  (brief-bookmark-jump-set ?9))

;;
;; Compilation buffer with its associated frame and window
;;

(defvar brief-compilation-frame-win nil
  "Dedicated frame and window of the user assigned compilation buffer.")

(defun brief-view-compilation-output ()
  "Try to find the compilation frame and window.
If not found, return NIL.  If found, return non-NIL and jump to the
first frame/window containing '*compilation*' buffer, or any buffer in
`compilation-mode', also set them as the dedicated frame/window for
the next run.

When prefixed with \\[universal-argument], it search only in current frame for a window
containing a compilation buffer.  If it found one, it will assign that
window and frame as the dedicated ones so that the next run it always
try to pop the assigned frame and window containing the compilation
buffer.  When the prefix is a number, the search is restricted only
for '*compilation*' buffer.  Other compilation buffers like lisp
compilation won't be counted in."
  (interactive)
  (if (and current-prefix-arg ;; pure `universal-argument'
           (equal current-prefix-arg '(4)))
      (if (catch 'found
            (dolist (w (cons (selected-window) ;; selected window preferred
                             (remove (selected-window) (window-list))))
              (when (eq (with-current-buffer (window-buffer w)
                          major-mode)
                        'compilation-mode)
                (setq brief-compilation-frame-win
                      (cons (selected-frame) w))
                (throw 'found t)))
            nil)
          (message "Dedicated compilation buffer frame/window set.")
        (message "No compilation window found in this frame."))

    (if (eq major-mode 'compilation-mode)
        ;; Already in compilation buffer, just go to previous error
        (call-interactively 'compilation-previous-error)

      ;; Search for a proper compilation buffer/frame/window
      (let ((buf (get-buffer "*compilation*"))
            (currf (selected-frame))
            (foundframe nil)
            (foundwin nil)
            (strict (numberp current-prefix-arg))
            wb f w)
        ;; Check if the user assigned compilation-buffer dedicated
        ;; frame and window are still alive as they was
        (if (and brief-compilation-frame-win
                 (setq f (car brief-compilation-frame-win))
                 (setq w (cdr brief-compilation-frame-win))
                 (framep f)
                 (frame-live-p f)
                 (windowp w)
                 (window-live-p w)
                 (eq (with-current-buffer (window-buffer w)
                       major-mode)
                     'compilation-mode)
                 (or (not strict)
                     (and strict
                          (eq (window-buffer w) buf))))
            ;; Still in good condition, use it
            (setq foundframe f
                  foundwin   w)

          ;; No dedicated f/w or not in a good condition, search next
          (setq brief-compilation-frame-win nil)
          (catch 'found
            (dolist (f (brief-frame-list))
              (select-frame f)
              (dolist (w (or (and (eq currf f) ;; in current frame
                                  ;; selected window preferred
                                  (cons (selected-window)
                                        (remove (selected-window)
                                                (window-list))))
                             (window-list)))
                (setq wb (window-buffer w))
                (when (or (and buf (eq wb buf))
                          (and (not strict)
                               (eq (with-current-buffer wb major-mode)
                                   'compilation-mode)))
                  ;; Record the first found frame/win
                  (if foundframe
                      t
                    (setq foundframe f
                          foundwin   w))
                  (if (eq wb buf)
                      (throw 'found foundwin)))))
            ;; Not found, switch back to the original frame
            (select-frame currf)))

        (if foundwin
            (progn
              ;; Found it, switch to that frame and window, also set
              ;; that as dedicated one unless user assigned otherwise
              (setq brief-compilation-frame-win
                    (cons foundframe foundwin))
              (select-frame-set-input-focus foundframe)
              (raise-frame foundframe)
              (select-window foundwin)
              (when (boundp 'compilation-mode-map)
                ;; Extend compilation-mode for brief style keys
                ;;(define-key compilation-mode-map [(control p)]
                ;;  'compilation-previous-error)
                (define-key compilation-mode-map [(control n)]
                  'compilation-next-error)))

          ;; Not found, search buffer list if not strict search.
          (if (not strict)
              (catch 'found
                (dolist (b (buffer-list))
                  (if (eq (with-current-buffer b major-mode)
                          'compilation-mode)
                    (throw 'found (setq buf b))))))
          (if (null buf)
              (message (concat "No " (if strict
                                         "*compilation*"
                                       "compilation") " buffer found."))
            ;; Open buffer in a split window
            (if (= (length (window-list)) 1)
                (split-window-vertically))
            (other-window 1)
            (switch-to-buffer buf)))

        (or foundwin buf)))))

;;
;; Window locating functions
;;
(defun brief-switch-to-window (window)
  "Switch to the specified window in the current frame."
  (interactive)
  (if window
      (let (;;(top (selected-window))
            (curr (next-window))
            (count 1))
        (catch 'break
          ;; Repeat no more than the number of exinsting windows
          (dolist (l (window-list))
            (if (eq window curr)
                (throw 'break nil))
            (incf count 1)
            (setf curr (next-window curr))))
        (if (eq window curr)
            (other-window count)))))

;; Window:
;; (x0,y0)*------------+
;;        |            |
;;        |        c   |  c:cursor position
;;        |            |
;;        +------------* (x1,y1)

(defun brief-is-crlf (c)
  (or (= c #x0a)   ;; end of line 'LF'
      (= c #x0d))) ;; end of line 'CR'

;;(defun brief-text-scaled-width (width)
;;  "Compute scaled text width according to current font scaling.
;;Convert a width of char units into a text-scaled char width units,
;;Ex. `window-hscroll'. The return value is rounded up to the closest
;;integer."
;;  (let ((def (default-font-width)))
;;    (/ (+ (* width (frame-char-width))
;;          (lsh def -1))
;;       def)))

;; No need to round up because the remainder of the division means the character
;; is still partially displayed, therefore we can't consider hscroll scrolled to
;; that character, but its previous one.
;; A 'truncation' instead of 'round up' is more correct.
(defun brief-text-scaled-width (width)
  "Compute scaled text width according to current font scaling.
Convert a width of char units into a text-scaled char width units,
Ex. `window-hscroll'."
  (if (fboundp #'default-font-width)
      (/ (* width (frame-char-width)) (default-font-width))
    ;; For Emacs version<=24. A not exact value but close to.
    (round (/ width
              (or (and (boundp 'text-scale-mode-remapping)
                       (caddr text-scale-mode-remapping))
                  1)))))

(defun brief-text-unscaled-width (width)
  "Reverse operation of `brief-text-scaled-width'.
Convert a width of text-scaled char unit back to units of
`frame-char-width'."
  (if (fboundp #'default-font-width)
      (/ (* width (default-font-width)) (frame-char-width))
    ;; For Emacs version<=24. A not exact value but close to.
    (round (* width
              (or (and (boundp 'text-scale-mode-remapping)
                       (caddr text-scale-mode-remapping))
                  1)))))

(defun brief-text-scaled-char-width ()
  "Text scaled char width."
  (if (fboundp #'default-font-width)
      (default-font-width)
    (round (* (frame-char-width)
              (or (and (boundp 'text-scale-mode-remapping)
                       (caddr text-scale-mode-remapping))
                  1)))))

(defun brief-text-scaled-char-height ()
  "Text scaled char height."
  (if (fboundp #'default-font-height)
      (default-font-height)
    (round (* (frame-char-height)
              (or (and (boundp 'text-scale-mode-remapping)
                       (caddr text-scale-mode-remapping))
                  1)))))

(defun brief-text-unscaled-current-column ()
  "Non-text-scaled version of `current-column'."
  (brief-text-unscaled-width (current-column)))

(defun brief-current-column-visual () ;; base:0
  "Compute the relative column number of cursor for current window.
Supports all 3 modes: line truncation, line wrapping and visual line
mode, as well as hidden texts."
  (save-excursion
    (let* ((p       (point))
           (c       (following-char))
           ;; Notice that we must use `current-column' instead of `point' here,
           ;; otherwise abbreviated texts (like the hyperlink texts in org-mode)
           ;; will have incorrect value
           (currcol (current-column))
           (begcol  (progn (beginning-of-visual-line) (current-column)))
           ;; [2017-04-19 Wed] `window-hscroll' not working well on
           ;; `text-scale-mode', also does `ruler-mode' (tested Emacs "25.1.50.16").
           ;; This is a workaround but still not able to calculate the
           ;; *exact* hscroll value.
           ;;(text-scale-mode-amount 0) ;; try to temporarily disable
           ;;                           ;; `text-scale-mode', no use
           (hscroll
            ;;(or (and (string= emacs-version "25.1.50.16") ; also emacs 25.2.2
            ;;(round (/ (window-hscroll) ;; Not exact [2017-05-23 Tue]
            ;;          (or (and (boundp 'text-scale-mode-remapping)
            ;;                   (caddr text-scale-mode-remapping))
            ;;              1)))
                    (brief-text-scaled-width (window-hscroll)))
           (x       (- currcol begcol hscroll))) ;; hscroll will be nonzero
      ;;                                         ;;  only if in truncation mode
      (if (not (brief-is-crlf c))
          (if (not (minibufferp))
              x
            ;; Minibuffer usually contain a prefix string, count it in
            (if (/= begcol ;; not in the 1st minibuffer visual line?
                   (progn
                     (beginning-of-line) (current-column)))
                x
              (+ x begcol)))
        ;; It's possible that we "were" at the end of abbreviated texts (like
        ;; the '...' in compressed org mode)
        (end-of-line) ;; `end-of-visual-line' will bypass the trailing '...'
        ;;            ;; so we try `end-of-line' first
        (if (= p (point))
            x  ;; We "were" at the left side of '...', so do we are "now",
          ;;   ;; so just return it
          ;; We "were" at the right side of '...', must be at `end-of-visual-line'
          ;;(if (/= p (progn (end-of-visual-line) (point)))
          ;;    ;; we 'were' neither at `end-of-line' nor at
          ;;    ;; `end-of-visual-line', where could we be?
          ;;    (error "Assertion failed: Unexpected case!!"))
          (+ 3 (- (current-column) ;; We "are" now at `end-of-line'
                  ;;               ;; since '...' take 3 characters
                  begcol hscroll)))))))

(defun brief-move-to-column (arg)
  "Move to column ARG by considering the presence of hidden texts."
  (if (and (= 3 (- arg (move-to-column arg))) ;; (length "...") = 3
           (brief-is-crlf (following-char)))
      ;;
      (let ((p1 (point)))
        (end-of-visual-line) ;; We're now at here
        (if (not (brief-is-crlf (following-char)))
            (goto-char p1)))))

;;
;; Brief Fast Line/Row Counting and relative window position calculation
;;

(defun brief-count-physical-lines (start end)
  ;; Modified from `count-lines' of simple.el.
  ;; It's different from `count-lines' as it will return 0 if both START and END
  ;; are at the same line; `count-lines' will return 1 on such cases.
  ;; [2017-11-23 17:56:33 +0800]
  ;; Notice we can't use `count-lines' directly here since it sometimes returns 1
  ;; more than expected, we have to search if there is really an EOL there.
  "Return number of lines between START and END.
START point is included while END point is excluded. Thus when
START=END it always return 0.  This function returns the number of
newlines between them."
  (save-excursion
    (save-restriction
      (let* ((done 0))
        (narrow-to-region start end)
        (goto-char (point-min))
        ;; Follow how `count-lines' do it
        (if (eq selective-display t)
            (save-match-data
              (while (re-search-forward "[\n\C-m]" nil t 64)
                (setq done (+ 64 done)))
              (while (re-search-forward "[\n\C-m]" nil t 1)
                (incf done))
              done)
          (setq done
                (- (buffer-size) (forward-line (buffer-size))))
          ;; Reverse the side effect caused by the above line
          (if (and (/= start end)
                   (not (bolp)))
              (1- done)
            done))))))

(defconst brief-flcbs 32768 ; 65536
  "Fast line counting block size (flcbs).")

;; Each item in this cache list denotes line numbers within this [start,end) block
;; (including START, excluding END)
(defvar-local brief-fast-line-number-list nil
  "A list (cache) to store number of lines within each `brief-flcbs' chars block.")

;;(defun brief-fast-line-number-at-pos (&optional pos)
;;  "Much faster version for replacing `line-number-at-pos'.
;;This function will also update cache `brief-fast-line-number-list'."
;;  ;; replace `line-number-at-pos'
;;  (let* ((p (or pos (point)))
;;         (q (/ p brief-flcbs))
;;         (r (% p brief-flcbs))
;;         (s (min (length brief-fast-line-number-list)
;;                 q))
;;         (l brief-fast-line-number-list)
;;         (lines 0))
;;    (cl-loop for i from 0 to (1- s) do
;;             (setq lines (+ lines (car l)))
;;             (setq l (cdr l)))
;;    (setq lines (+ lines (brief-count-physical-lines (1+ (* brief-flcbs s)) p)))
;;    ;; Append into `brief-fast-line-number-list'
;;    (when (< (length brief-fast-line-number-list) q)
;;      (setq brief-fast-line-number-list (nreverse brief-fast-line-number-list))
;;      (cl-loop for i from (1+ (* brief-flcbs s)) to p by brief-flcbs do
;;               (when (<= (+ i brief-flcbs -1) (point-max))
;;                 (push (brief-count-physical-lines i (+ i brief-flcbs -1))
;;                       ;; [start,end), not including end point
;;                       brief-fast-line-number-list)))
;;      (setq brief-fast-line-number-list (nreverse brief-fast-line-number-list)))
;;    (1+ lines))) ; base 1

(defun brief-fast-line-number-at-pos (&optional pos)
  "Much faster version for replacing `line-number-at-pos'.
This function will also update cache `brief-fast-line-number-list'."
  ;; replace `line-number-at-pos'
  (let* ((p (or pos (point)))            ;; pos
         (l brief-fast-line-number-list) ;; cache list
         (lines 0)                       ;; line number
         (rev nil)                       ;; reversed?
         (c 1)                           ;; curr line-counted pos
         i                               ;; item/index
         (n brief-flcbs))                ;; next
    ;; Use as many cached line numbers as possible
    (while (and (<= n p) ;; pos not reached yet
                l) ;; list not exhausted
      (setq lines (+ lines (car l)) ;; accumulate cached line numbers
            l     (cdr l)
            c     n
            n     (+ n brief-flcbs)))
    ;; Either cache exhausted or p reached/passed
    ;;  * cache exhausted: l = nil, which means n <= p still true
    ;;  * pos reached: n > p, l could be either nil or not
    (if (setq rev (<= n p))
        ;; Cache exhausted, preparing appending items into cache.
        ;; In order to prevent `append', reverse the list and pushing
        (setq brief-fast-line-number-list
              (nreverse brief-fast-line-number-list)))
    ;; Push into reversed `brief-fast-line-number-list' if cache miss
    (while (<= n p)
      (setq i     (brief-count-physical-lines c n) ;; [c,n)
            lines (+ lines i)                      ;;   [c,n)
            c     n                                ;;     [c,n)
            n     (+ n brief-flcbs))
      (push i     brief-fast-line-number-list))
    (if rev
        (setq brief-fast-line-number-list
              (nreverse brief-fast-line-number-list)))
    ;; Calculate the remaining lines within a `brief-flcbs', base 1
    (if (< c p)
        (+ 1 lines (brief-count-physical-lines c p)) ;; [c,p)
      (1+ lines))))

(defun brief-trim-fast-line-number-list (beg end)
  "Trim off cached line number counting list.
Any text change invalidates the list from the point of editing.
This save us the time to change line numbers for all the cached
blocks after the editing point.  This function is meant to be
placed in `before-change-functions' so we try to finish it as
soon as possible in order not to cause any delay when editing."

  ;;(assert (>= beg end))

  (let ((len (safe-length brief-fast-line-number-list))
        (q   (/ (min beg end) brief-flcbs)))
    (when (> len q)
      (setq brief-fast-line-number-list
            (nbutlast brief-fast-line-number-list (- len q)))))
  t)

;;(add-hook 'before-change-functions 'brief-trim-fast-line-number-list)

(defun brief-fast-count-physical-lines (start end)
  "Return number of lines between START and END.
This is the number of newlines between them."
  ;;    |<-brief-flcbs->|<-brief-flcbs->|         |<-brief-flcbs->|
  ;; ...|---------------|-----s---------|--...----|------e--------|---...
  ;;                          ^         sq        eq     ^
  (let* ((s (min start end))
         (e (max start end))
         (sq (/ (+ s brief-flcbs -1) brief-flcbs)) ;; sq always >= 1
         (eq (/ e brief-flcbs))
         (c  (- eq sq))
         (lines 1)
         ;;verify
         l)
    (if (>= sq eq)
        (brief-count-physical-lines s e)
      (if (< (safe-length brief-fast-line-number-list) eq)
          (- (brief-fast-line-number-at-pos e)
             (brief-fast-line-number-at-pos (max (1- s) 1)))
        (setq lines (+ (brief-count-physical-lines s (max (1- (* sq brief-flcbs))
                                                          1))
                       (brief-count-physical-lines (* eq brief-flcbs) e))

              l (last brief-fast-line-number-list
                      (- (safe-length brief-fast-line-number-list) sq)))
        ;;(cl-loop for i from sq to (1- eq) by 1 do
        ;;       (setq lines (+ lines (car l))
        ;;             l (cdr l)))
        (while (> c 0)
          (decf c)
          (setq lines (+ lines (car l))
                l (cdr l)))
        ;; ;; Verification
        ;; (setq verify (- (brief-fast-line-number-at-pos e)
        ;;                 (brief-fast-line-number-at-pos (max (1- s) 1))))
        ;; (when (/= lines verify)
        ;;   (message "error %d /= %d" lines verify)
        ;;   (setq lines verify))
        lines))))

;;(defun brief-fast-line-number-at-pos (&optional pos)
;;  "Much faster version for replacing `line-number-at-pos'."
;;  (1+ (brief-fast-count-physical-lines 1 (or pos (point))))) ; base 1

(defun brief-current-row-visual () ;; base:0
  "Compute the relative row number of cursor for current window.
Supports all 3 modes: line truncation, line wrapping and visual line mode."
  (save-excursion
    (let* ((point0 (point))
           ;;(line0 (line-number-at-pos))
           ;;(linum-mode nil) ;; TODO: line number mode can make big org mode
           ;;                 ;; file becomes very very slow, but it seems no
           ;;                 ;; use to disable it this way
           ;;line
           (column0 (current-column))
           (count 0)
           (disable-local-cursor-tracking '(t)))
      (goto-char (window-start))
      (while (or (and (< (point) point0)
                      ;;(< (setq line (line-number-at-pos)) line0)
                      (> (brief-fast-count-physical-lines (point) point0) 0))

                 (and (zerop (brief-fast-count-physical-lines point0 (point)))
                      ;;(= line line0)
                      (< (current-column) column0)))
        ;;(ignore-errors
        ;;  ;; When `eobp' is t, error occurs and do not increase count
        ;;  ;; Using `forward-line' will fail on visual line mode. Only
        ;;  ;; `next-line' can we move to the correct visual line, especially
        ;;  ;; when we enabled line wrapping or visual line mode, but in some
        ;;  ;; cases it will cause the cursor moving visible
        ;;  (call-interactively 'next-line)
        ;;  (setq count (1+ count))))
        (vertical-motion 1)
        (setq count (1+ count)))
      (if (or (and (> (point) point0)
                   (> (brief-fast-count-physical-lines point0 (point))
                      0)) ;;(> line line0)
              (and (zerop ;; (= line line0)
                    (brief-fast-count-physical-lines point0 (point)))
                   (> (current-column) column0)))
          (1- count)
        count))))

(defun brief-window-pos-delta ()
  "Get the Emacs on XWindow server internal error value of window position.
This error happened when running Cygwin/X, VcXsrv in combination with
some Emacs versions along with some font settings.  Not sure what
combination could cause this but anyway it happens so do this
calculation here.
  Sometimes the (left,top) position reported by `window-inside-edges'
is not the same as the window found by `window-at' (left,top),
especially if customized fontsize is used. This function tries to find
out the internal delta value of the current window."
  (let* ((curr (selected-window))
         (wie  (window-inside-edges))
         (left (car wie))
         (top  (cadr wie))
         (xdelta 0)
         (ydelta 0))
    ;; Increment at most 3 on X and 2 on Y directions, at the size of a
    ;; block cursor.
    (and (not (eq curr (window-at (+ xdelta left) (+ ydelta top))))
         (incf xdelta) ;; x+1
         (not (eq curr (window-at (+ xdelta left) (+ ydelta top))))
         (incf ydelta) ;; y+1
         (not (eq curr (window-at (+ xdelta left) (+ ydelta top))))
         (incf xdelta) ;; x+2 !
         (not (eq curr (window-at (+ xdelta left) (+ ydelta top))))
         (incf ydelta) ;; y+2 !
         (not (eq curr (window-at (+ xdelta left) (+ ydelta top))))
         (incf xdelta) ;; x+3 ?!
         (not (eq curr (window-at (+ xdelta left) (+ ydelta top))))
         (error ;; No, not trying y+3, it's an error
          "Either this window system or this code has a big problem"))
    (list xdelta ydelta)))

(defun brief-window-cursor-xpos ()  ;; x position at cursor
  "Get the relative column number of cursor position comparing to `window-start'."
  (+ (car (window-inside-edges))
     (brief-current-column-visual)
     (car (brief-window-pos-delta))))

(defun brief-window-cursor-xpos-pixel ()
  "Get the relative X pixel coordinate of cursor comparing to `window-start'."
  (+ (car (window-inside-pixel-edges))
     (* (+ (brief-current-column-visual) (car (brief-window-pos-delta))
           )
        ;;(or (and (boundp 'text-scale-mode-remapping)
        ;;         (caddr text-scale-mode-remapping))
        ;;    1)
        ;;(frame-char-width)
        (brief-text-scaled-char-width))))

(defun brief-window-cursor-ypos ()
  "Get the relative line number of cursor position comparing to `window-start'."
  (+ (cadr (window-inside-edges))
     (brief-current-row-visual)
     (cadr (brief-window-pos-delta))))

(defun brief-window-cursor-ypos-pixel ()
  "Get the relative Y pixel coordinate of cursor comparing to `window-start'."
  (+ (cadr (window-inside-pixel-edges))
     (* (+ (brief-current-row-visual) (cadr (brief-window-pos-delta))
           )
        ;;(or (and (boundp 'text-scale-mode-remapping)
        ;;         (caddr text-scale-mode-remapping))
        ;;    1)
        ;;(frame-char-height)
        (brief-text-scaled-char-height))))

(defun brief-upside-window ()
  "Get window located above current window.
If more than one window there, choose the window located right above
the cursor."
  (let* ((edges (window-edges))
         ;;(x0 (brief-window-cursor-xpos))
         (x0 (round (/ (* 1.0 (brief-window-cursor-xpos-pixel))
                       (frame-char-width)))) ; consider font rescaling
         (y0 (cadr edges))
         (curr (selected-window))
         (win curr))
    (while (and win (>= y0 0) (eq win curr))
      (setq y0 (1- y0)
            win (window-at x0 y0)))
    win))

(defun brief-downside-window ()
  "Get window located below current window.
If more than one window there, choose the window located right below
the cursor."
  (let* ((edges (window-edges))
         ;;(x0 (brief-window-cursor-xpos))
         (x0 (round (/ (* 1.0 (brief-window-cursor-xpos-pixel))
                       (frame-char-width)))) ; consider font rescaling
         (y1 (cadddr edges))
         (curr (selected-window))
         (win curr))
    (while (and win (eq win curr))
      (setq win (window-at x0 y1)
            y1 (1+ y1)))
    (if (eq win (minibuffer-window))
        (and (> (minibuffer-depth) 0)
             (minibuffer-window-active-p (minibuffer-window))
             win)
      win)))

(defun brief-leftside-window ()
  "Get window located left side of current window.
If more than one window there, choose the window pointed leftward
from cursor."
  (let* ((edges (window-edges))
         (x0 (car edges))
         ;;(yy (brief-window-cursor-ypos))
         (yy (round (/ (* 1.0 (brief-window-cursor-ypos-pixel))
                       (frame-char-height)))) ; consider font scaling
         (curr (selected-window))
         (win curr))
    (while (and win (>= x0 0) (eq win curr))
      (setq win (window-at x0 yy)
            ;; workaround for hidden lines like org-mode, but it's incorrect
            ;; win (or win (window-at x0 y0))
            x0 (1- x0)))
    win))

(defun brief-rightside-window ()
  "Get window located right side of current window.
If more than one window there, choose the window pointed rightward
from cursor."
  (let* ((edges (window-edges))
         (x1 (caddr edges))
         ;;(yy (brief-window-cursor-ypos))
         (yy (round (/ (* 1.0 (brief-window-cursor-ypos-pixel))
                       (frame-char-height)))) ; consider font scaling
         (curr (selected-window))
         (win curr))
    (while (and win (eq win curr))
      (setq win (window-at x1 yy)
            ;; workaround for hidden lines like org-mode, but it's incorrect
            ;; win (or win (window-at x1 y0))
            x1 (1+ x1)))
    win))

(defun brief-switch-window-up ()
  "Switch current window to the one above current cursor position."
  (interactive)
  (brief-switch-to-window (brief-upside-window)))

(defun brief-switch-window-down ()
  "Switch current window to the one below current cursor position."
  (interactive)
  (brief-switch-to-window (brief-downside-window)))

(defun brief-switch-window-left ()
  "Switch current window to the one left to current cursor position."
  (interactive)
  (brief-switch-to-window (brief-leftside-window)))

(defun brief-switch-window-right ()
  "Switch current window to the one right to current cursor position."
  (interactive)
  (brief-switch-to-window (brief-rightside-window)))

(defun brief-split-window-up ()
  "Split window vertically to be the upper neighbor of current window."
  (interactive)
  (split-window-vertically)
  (other-window 1))

(defun brief-split-window-left ()
  "Split window horizontally to be the left neighbor of current window."
  (interactive)
  (split-window-horizontally)
  (other-window 1))

(defun brief-delete-window-up ()
  (interactive)
  (delete-window (brief-upside-window)))

(defun brief-delete-window-down ()
  (interactive)
  (delete-window (brief-downside-window)))

(defun brief-delete-window-left ()
  (interactive)
  (delete-window (brief-leftside-window)))

(defun brief-delete-window-right ()
  (interactive)
  (delete-window (brief-rightside-window)))

(defun brief-delete-current-window ()
  (interactive)
  (delete-window)
  (other-window -1))

;;
;; Brief cursor commands
;;

(defun brief-forward-physical-line (arg)
  "Move forward a physical line instead of visual line."
  (let ((col (brief-current-column-visual)))
    (forward-line arg)
    (brief-move-to-column
     (max col
          (or (and (consp temporary-goal-column)
                   (truncate
                    (+ (car temporary-goal-column)
                       (cdr temporary-goal-column))))
              temporary-goal-column)))))

(defun brief-previous-physical-line (arg)
  "Move cursor vertically up (a) physical line(s)"
  (interactive "^p")
  (setq this-command 'previous-line)
  (brief-forward-physical-line (- arg)))

(defun brief-next-physical-line (arg)
  "Move cursor vertically down (a) physical line(s)"
  (interactive "^p")
  (setq this-command 'next-line)
  (brief-forward-physical-line arg))

(defun brief-previous-line (arg)
  "Move cursor vertically up (a) visual line(s).
When prefixed with a number, it move up several visual lines.
Negative prefix number move up several physical lines instead.  If the
prefix is simply a \\[universal-argument], it move up one physical line."
  (interactive "^p")
  ;; In order to keep `temporary-goal-column' unchanged, we need to set
  ;; current command as 'previous-line here. (Refer to `line-move-visual'.)
  (setq this-command 'previous-line)
  (if (brief-rectangle-active)
      (call-interactively 'cua-resize-rectangle-up)
    (if (equal current-prefix-arg '(4)) ;; only C-u without numbers
          (brief-previous-physical-line 1)
      (if (< arg 0)
          (brief-forward-physical-line arg)
        (call-interactively 'previous-line)))))

(defun brief-next-line (arg)
  "Move cursor vertically down (a) visual line(s).
When prefixed with a number, it move down several visual lines.
Negative prefix number move down several physical lines instead.
If the prefix is simply a \\[universal-argument], it move down one physical line."
  (interactive "^p")
  ;; In order to keep `temporary-goal-column' unchanged, we need to set
  ;; current command as 'next-line here. (Refer to `line-move-visual'.)
  (setq this-command 'next-line)
  (if (brief-rectangle-active)
      (call-interactively 'cua-resize-rectangle-down)
    (if (equal current-prefix-arg '(4)) ;; only C-u without numbers
        (brief-next-physical-line 1)
      (if (< arg 0)
          (brief-forward-physical-line (- arg))
        (call-interactively 'next-line)))))


;;; Xselection Postponement:

;; Postpone changing Xselection till keyboard is idle for a while.
;;
;; This prevents message flooding on some X server systems that runs an
;; application broadcasting messages whenever clipboard changes.

;; A sample environment is a remote Linux machine running X11 Emacs displayed on
;; Xservers (VcXsrv, CygwinX, ... etc) running under Windows 10, with any of
;; Microsoft Office running (eg. MS Outlook, which usually sit in the background
;; for emails).
;;
;; With MS office application running, any change of the clipboard will cause
;; MS office application to broadcast 'WM_MSO_BROADCASTCHANGE' messages.  If
;; there are, say, 25 Xwindows on screen (for example, remote Emacs showing
;; 25 frames as VcXsrv/CygwinX windows), this broadcasted message will flood
;; into VcXsrv/CygwinX server message loop and cause problems.  Here are the
;; details I find it the hard way, after a long investigateion (notice that
;; Xserver here refers to Xservers running on MsWindows, say VcXsrv):
;;
;;   1. Usually a Emacs keyboard command is executed right after Xserver
;;      received the 'WM_KEYDOWN' message(s) of that command.  This happens
;;      *BEFORE* the 'WM_KEYUP' message(s) of that keyboard command.
;;   2. If that Emacs keyboard command changes the clipboard, this will
;;      immediately make MS office application fire 'WM_MSO_BROADCASTCHANGE'
;;      message broadcasted into all existing Windows, including all windows
;;      opened by Xserver.
;;   3. Now these messages goes into Xserver's message loop, it usually take
;;      a while to digest this message, this period sometimes exceed the
;;      keyboard auto-repeat time period and therefore that Emacs keyboard
;;      command start to repeat again and again.
;;   4. These auto-repeated keyboard commands changes the clipboard again
;;      and again and cause a lot more broadcasted messages and flood into the
;;      message loop of Xserver, causing infinite loop.
;;   5. The 'WM_KEYUP' message of that keyboard command thus never got a chance
;;      to be processed and thus this keyboard command repeats forever.
;;
;; An example of this is the region marking commands.  In Brief mode we press
;; [M-l] and '[up]/[down]'s to enlarge/shrink the marked region.  A common
;; behavior of *NIX terminals and editors are that they copy the selection
;; texts into clipboard in realtime as the selected texts area changes.  Emacs
;; region marking also do the same.  Thus marking region initiates the above
;; message flood.  The last up/down key will take effect and repeats forever.
;; Each time the up/down key command ran it resized the region, changed the
;; clipboard and make the flood grew bigger.  Eventually 'WM_KEYUP' message of
;; that up/down key is postponed forever.  What we can see on the screen is the
;; marked region grows bigger and bigger till the beginning/end of the edited
;; file is reached.
;;
;; Since Emacs and Xserver are now not responding, the only ways to stop it
;; are:
;;  1. Wait till the beginning/end of file is reached.  However this could take
;;     very long depend on the file size.
;;  2. Terminate Emacs by force, by login into this Linux machine with another
;;     machine and terminate it.
;;  3. Close all MS office applications.
;;
;; Sometimes if we waited too long the message flood will grow so big that all
;; CPUs runs in 100% and the host Windows system hangs (message queues of
;; Xserver and all windows will grow and eat up system memory fast).  To make
;; system respond again we need to unplug the network cable for a while (if
;; Emacs is running on a remote machine) or even reboot the host Windows machine.

;; To workaround this problem, we need to detect the KEY-UP message.  If system
;; do commands only *after* key-up, we won't have this problem as the key won't
;; be automatically repeated before all the messages in the message queue got
;; digested.  Unfortunately there is no way to detect KEY-UP message in eLisp.
;; Therefore, we need to emulate the key-up using idle timer.  When idle timer
;; expires it means all keys are already up for a while.  The workaround provided
;; here is that we postponed the copy of data into Xselection till a certain
;; period of keyboard idle-time.

;; However, without the exact detection of the KEY-UP, this problem is still
;; potentially happening if the Xserver host machine is either too slow or too
;; heavily loaded, while our idle time setting may still not be not long enough.
;; For those systems we also provides another mean to workaround this: turn on
;; the custom option `brief-debounce-keys-microsoft-office'.  It will debounce
;; the key commands if they are happening too fast: `brief-copy-line',
;; `brief-kill-line' and `brief-delete-entire-line'.



;; [2017-01-13 16:39:37 +0800]
;; Add mark-activation/deactivation hooks to prevent keep modifying Xselection
;; on marking region.  Whatever make clipboard change will cause Microsoft
;; office to broadcast WM_MSO_BROADCASTCHANGE to *ALL* openned windows which
;; could flood X server if a lot of X windows are currently openned.  This even
;; cause VcXsrv to drop important message like WM_KEYDOWN/WM_KEYUP and cause the
;; region marking operation goes forever, until network disconnect or all MS
;; Office applications closed.
;; (this issue take me almost a week to track down, using Visual Studio Spy++).

;; TODO 1: merge this hook operation with the
;;         `brief-region-backup-clipboard-selection' hook function
;; TODO 2: with this, there should be no need to disable clipboard backup
;;         for multiple-cursor mode?
(defvar-local brief-is-gui-set-selection-postponed nil)

;; ;;The following will produce error:
;; ;; apply: Wrong number of arguments:
;; ;; (lambda nil (setq brief-is-gui-set-selection-postponed t)), 3 [12 times]
;; ;;This should be an Emacs advice-add internal issue.
;; ;;
;; (defun brief-postpone-gui-set-selection (orig-func &rest args)
;;   (unless brief-is-gui-set-selection-postponed
;;     (apply orig-func args)))
;;
;; (advice-add 'gui-set-selection :around #'brief-postpone-gui-set-selection)
;; (advice-remove 'gui-set-selection #'brief-postpone-gui-set-selection)

;;
;; Use idle-timer to detect key-released, whenever key is pressed,
;; idle-timer never runs
;;
(defvar-local brief-postpone-gui-selection-idle-timer  nil
  "The per-buffer postponed `gui-selection' timer.")

(defvar-local brief-postpone-idle-timer-buffer         nil
  "This variable records in which buffer do we starts the idle timer.
User might switch to another buffer before idle timer starts.  Therefore
when idle timer starts it shouldn't do anything if buffer changes")

(defvar brief-postpone-gui-selection-idle-period 0.4)

(defun brief-cancel-postpone-gui-selection-timer ()
  (brief-dbg-message "brief-cancel-postpone-gui-selection-timer")
  (unless window-system
    ;; We could have two frames with one frame in terminal mode while another
    ;; in window-system, this prevents the window-system frame removed the
    ;; terminal frame's hook, since `post-command-hook' is global.  Unless we
    ;; can put it buffer-local but Emacs manual already says it's risky to put
    ;; that as file-local so I wonder it might be risky as well to do that
    ;; buffer-local.
    ;; TODO: verify this, confirm `post-command-hook' risky as buffer-local.
    (remove-hook 'post-command-hook
                 #'brief-terminal-mode-activate-gui-selection-idle-timer))
  (and brief-postpone-gui-selection-idle-timer
       (cancel-timer brief-postpone-gui-selection-idle-timer))
  (setq brief-postpone-gui-selection-idle-timer nil))

;;
;; Copy into clipboard after key-up, emulated by idle timer.
;;

(defvar-local brief-copy-to-clipboard-on-keyup-data nil
  "Region/rectangle data to be copied to clipboard when all keys are released.")

(defvar-local brief-keep-postpone-gui-selection-timer nil
  "A flag to prevent timer killed by deactivating mark.
This flag is used by `brief-copy-to-clipboard-on-keyup' and
`brief-resume-gui-set-selection'")

;; [2017-10-25 Wed] When debouncing is on, MS office is ON with lots of
;; VcXsrv windows, 0.01 always debouncing, 0.05 often debouncing, 0.1 sometimes
;; debouncing and 0.2 seems not. 0.15 rarely need debouncing but sometimes do.
;; Hence we use a more conservative value 0.25 here.
(defvar brief-postpone-copy-to-clipboard-idle-period 0.25
  "Keyboard idle period for postponing copying data into clipboard.
This emulates the 'key up' event. We copy things into clipboard only when
keyboard is idle.")

(defvar brief-postponed-mark-selection-copy-completed nil
  "Brief internal info to identify if the previous clipboard copy was
done.  If we try to do `brief-copy-line' (\\[brief-copy-line]) right after this we don't
need to copy the whole clipboard again otherwise when clipboard data
is huge it's just a waste of time.")

(defun brief-copy-to-clipboard-on-keyup (&optional thetext)
  "Postpone `brief-copy-region-into-clipboard' till key-up.
The 'key-up' is actually emulated by running an idle timer."
  (brief-dbg-message "enter brief-copy-to-clipboard-on-keyup")

  (if (not brief-enable-postpone-selection)

      ;; Don't wait till key-up (idle)
      (brief-copy-region-into-clipboard (or thetext
                                            (car kill-ring)
                                            "")
                                        'quitable)

    ;; Wait till key-up (idle)
    (unless (and brief-postponed-mark-selection-copy-completed
                 (equal (list (region-beginning) (region-end))
                        brief-postponed-mark-selection-copy-completed))
      (setq brief-copy-to-clipboard-on-keyup-data
            (or thetext
                (car kill-ring)
                ""))
      (setq brief-keep-postpone-gui-selection-timer t)
      ;; Run not too soon after key-up detected, otherwise MS office
      ;; WM_MSO_BROADCASTCHANGE broadcat message will still cause problems.
      (brief-activate-postpone-gui-selection-timer
       brief-postpone-copy-to-clipboard-idle-period))

    (setq brief-postponed-mark-selection-copy-completed nil))

  (brief-dbg-message "leave brief-copy-to-clipboard-on-keyup"))

(defvar brief-postponed-running nil)
(defvar brief-is-external-interruptible nil)
(defvar-local brief-postponed-clipboard-ran nil)
(defvar-local brief-external-set-selection-interrupted nil)

(defun brief-run-postponed-gui-set-selection ()
  "The timer postponed gui-set-selection is run here."
  ;; Emulate the X-window clipboard behavior that clipboard content is being set
  ;; as the marked region grows/shrinks
  (brief-dbg-message "enter brief-run-postponed-gui-set-selection")

  ;; Postponed gui-set-selection ran, wait till next key-stroke (that will
  ;; invoke `gui-set-selection' if we're marking a region) to activate it.
  (brief-cancel-postpone-gui-selection-timer)

  (setq brief-postponed-mark-selection-copy-completed nil)

  (let ((brief-is-gui-set-selection-postponed nil) ;; prevent reenter
        (brief-postponed-running t)
        ;; By default we use 'PRIMARY only, and disable 'CLIPBOARD (For VcXsrv,
        ;; the option "Clipboard may use PRIMARY selection" in its context menu
        ;; need to be turned ON).  These values are used by `gui-select-text' and
        ;; `cua-copy-region'.
        (select-enable-primary   (and (not (brief-is-winnt))
                                      brief-select-enable-primary))
        (select-enable-clipboard (or (brief-is-winnt)
                                     brief-select-enable-clipboard))
        ;; Hide global `kill-ring' here otherwise `cua-copy-region' will
        ;; insert the temp selection into kill-ring.
        kill-ring
        kill-ring-yank-pointer) ; also hide `kill-ring-yank-pointer'

    (if (null brief-copy-to-clipboard-on-keyup-data)

        ;; Postponed due to region marking
        (if (not (brief-use-region))
            ;; We are expecting user is marking a region here
            (if (or
                 ;; Just start marking `cua-set-mark' (C-space)
                 (and (region-active-p)
                      (eq (region-beginning) (region-end)))

                 ;; `string-rectangle' in action, no need to copy clipboard.
                 (and (minibufferp)
                      (or (and (boundp 'rectangle--string-preview-state)
                               rectangle--string-preview-state)
                          (and (boundp 'rectangle--string-preview-window)
                               rectangle--string-preview-window)))

                 ;; User switch to another buffer before postponed timer starts.
                 (not (eq (current-buffer) brief-postpone-idle-timer-buffer))

                 ;; TODO: could there be other cases?
                 ;; Note:
                 ;;  Test ok: if user switch to menu bar before timer starts
                 )

                t ;; do nothing

              (if (and (version< emacs-version "24.0")
                       (minibufferp))
                  ;; Known case, for Emacs23 when try searching in a region.
                  t  ;; ignore and do nothing,

                ;; This should not happen, but sometimes it does.  According to
                ;; the comments in `region-active-p':
                ;; > FIXME: Somehow we sometimes end up with mark-active non-nil
                ;; > but without the mark being set (e.g. bug#17324).  We really
                ;; > should fix that problem, but in the mean time, let's make
                ;; > sure we don't say the region is active when there's no mark.
                ;; We probably having the same issue here so we fire a warning
                ;; message here.
                (message (concat
                          "Brief: expecting marked region"
                          (and (minibufferp)
                               ;; There could be some other similar situation like
                               ;; `string-rectangle' so we notify user about that.
                               " (currently in minibuffer)")))))

          ;; User is currently marking a region
          (unless brief-postponed-clipboard-ran
            ;; First run after mark activated?
            (setq brief-postponed-clipboard-ran t)
            ;; backup current clipboard since we're going to change it
            (brief-backup-clipboard-selection))

          (let ((brief-is-external-interruptible 'interruptible)
                (regbeg (region-beginning))
                (regend (region-end)))
            ;; This function will invoke `gui-set-selection', then copy
            ;; data into `kill-ring', later `brief-copy-region-into-clipboard'
            ;; will use it.
            ;;(call-interactively 'cua-copy-region) ;; allow rectangle
            (cua-copy-region nil) ;; allow rectangle

            ;; If we was using external clipboard set and got interrupted,
            ;; there is no need to copy that again.
            (unless brief-external-set-selection-interrupted
              ;; The following will activate `gui-set-selection' function
              ;; `gui-select-text' -> `gui-set-selection'.  In terminal mode if
              ;; we don't set `brief-is-gui-set-selection-postponed' nil it will
              ;; got postponed again.
              (brief-copy-region-into-clipboard nil 'interruptible))

            (unless brief-external-set-selection-interrupted
              ;; Region completely copied into external selection, mark it.
              (setq brief-postponed-mark-selection-copy-completed
                    (list regbeg regend)))))

      ;; Postponed due to clipboard copy command like `brief-copy-line' and
      ;; `brief-kill-line'.  These commands should not be interrupted but can
      ;; be forced quitting with C-g.
      (condition-case err
          (progn
            ;; `brief-copy-region-into-clipboard' will invoke `gui-select-text'
            ;; which checks if our text is identical with the previous one.  If
            ;; yes it will not set selection to external clipboard.  However,
            ;; When sharing clipboard with other editor (or other Emacs) the
            ;; external clipboard might already changed but this Emacs is not
            ;; going to know that.  If we are currently trying to copy again
            ;; the same string to another editor we will fail.  Therefore the
            ;; customized variable `brief-force-set-external-selection' decides
            ;; if we're overwriting the external clipboard without comparing to
            ;; our previous copy or not.
            (when (and select-enable-primary
                       brief-force-set-external-selection)
              (setq gui--last-selected-text-primary nil))
            (when (and select-enable-clipboard
                       brief-force-set-external-selection)
              (setq gui--last-selected-text-clipboard nil))

            (brief-copy-region-into-clipboard
             brief-copy-to-clipboard-on-keyup-data 'quitable)

            (setq brief-copy-to-clipboard-on-keyup-data nil))

        ;; If user quit, reset data or it will confuse our next run, then
        ;; propagate the quit signal.
        (quit (progn
                (brief-dbg-message
                 "User quit from brief-run-postponed-gui-set-selection")
                (setq brief-copy-to-clipboard-on-keyup-data nil)
                (signal 'quit nil)))

        ;; Catch all other kinds of errors
        (error (progn
                 (setq brief-copy-to-clipboard-on-keyup-data nil)
                 (error (or (and (stringp (cdr err))
                                 (cdr err))
                            (format "Error:%S" err))))))))

  ;; In terminal mode `gui-set-selection' run only once at the 1st idle,
  ;; we need to use pre/post-command-hook to activate idle timer again if
  ;; key-stroke happens.
  (when (and (not window-system)
             brief-is-gui-set-selection-postponed)
    (add-hook 'post-command-hook
              #'brief-terminal-mode-activate-gui-selection-idle-timer))
  (brief-dbg-message "leave brief-run-postponed-gui-set-selection"))

(defun brief-activate-postpone-gui-selection-timer (&optional idle-period)
  (setq idle-period
        (or idle-period brief-postpone-gui-selection-idle-period))
  ;; Instead of preventing creating multiple idle timers, we restart the timer
  ;; to prevent continuous activation. Continuous activation means we're not
  ;; idle for long enough.
  (when brief-postpone-gui-selection-idle-timer
    ;;(brief-cancel-postpone-gui-selection-timer)
    (cancel-timer brief-postpone-gui-selection-idle-timer)
    (setq brief-postpone-gui-selection-idle-timer nil))

  (setq brief-postpone-gui-selection-idle-timer
        ;; Notice that letting this timer auto-repeat itself is a bit dangerous
        ;; if for some reason the deactivate-mark hook is not run (e.g. quit?)
        ;; -- the timer function will keep repeating whenever idle occurs.
        (run-with-idle-timer (* idle-period (brief-slowdown-factor))
                             nil
                             #'brief-run-postponed-gui-set-selection))

  (setq brief-postpone-idle-timer-buffer (current-buffer))
  ;; Region/mark changed, reset completion flag
  (setq brief-postponed-mark-selection-copy-completed nil))

(defun brief-terminal-mode-activate-gui-selection-idle-timer ()
  ;; For emacsclient we could have two frames with one on window-system while
  ;; the other on terminal, as `post-command-hook' is global for both frames
  ;; we must remove the hook only if we're running in terminal mode.
  (when (and (not window-system)
             brief-is-gui-set-selection-postponed)
    (brief-activate-postpone-gui-selection-timer)
    (remove-hook 'post-command-hook
                 #'brief-terminal-mode-activate-gui-selection-idle-timer))
  t)

(defun brief-enable-clipboard-postponement ()
  (add-hook 'activate-mark-hook
            'brief-postpone-gui-set-selection 't)
  (add-hook 'deactivate-mark-hook
            'brief-resume-gui-set-selection))

(defun brief-disable-clipboard-postponement ()
  (remove-hook 'activate-mark-hook
               'brief-postpone-gui-set-selection)
  (remove-hook 'deactivate-mark-hook
               'brief-resume-gui-set-selection)
  ;; In case any terminal mode frame in work
  (remove-hook 'post-command-hook
               'brief-terminal-mode-activate-gui-selection-idle-timer))

;;
;; External Xselection selection get/set helper using programs like 'xsel' or
;; 'xclip'.  This also allows terminal mode programs running under X able to
;; interact with Xselection.  These functions are mainly for *NIX systems;
;; native win32/win64 systems cannot use them.
;;
;; 'xsel' is in favor when 'xsel' and 'xclip' are both available.  Accodring to
;; my personal experience 'xsel' responds faster.  However, the default value
;; was set to 'xclip' due to Ubuntu 18.  Check the documentation string for the
;; custom variable `brief-in-favor-of-xsel'.
;;
;; The implemetation is both non-blocking and quittable.  When huge clipboard
;; data is transfering between Emacs and Xselection, it could take very long
;; using Emacs native functions which is not even interruptible or quit-able
;; (C-g).  The native implementation always copy the full content of selection
;; into Xselection whenever region size changed, this is also very inefficient
;; and slow when huge texts are selected.  In this implementation when region
;; size changed the previous unfinished Emacs selection <-> Xselection
;; transferring operation will be interrupted and terminated so it's more
;; efficient.  User won't be blocked and waiting there without any visible
;; response and feel like Emacs hangs.

(defun brief-xclipboard-get-type-arg (type)
  (or (and (eq (or type 'PRIMARY) 'PRIMARY)
           (nth 3 brief-xclipboard-args))
      (and (eq type 'CLIPBOARD)
           (nth 4 brief-xclipboard-args))
      (and (eq type 'SECONDARY)
           (nth 5 brief-xclipboard-args))
      (error "Invalid TYPE")))

;; All the variables and functions defined as brief--xxx are all used
;; internally by Brief, mainly for external process related functions.
(defvar brief--prev-external-bytes-received 0
  "Brief internal variable to store previously received bytes count.")

(defvar brief--external-bytes-received 0
  "Brief internal variable to store received bytes count.")

(defvar brief--backing-up-clipboard nil
  "Brief internal flag to indicate we're currently backing up external clipboard.")

(defun brief--external-clipboard-filter (proc string)
  "Brief internal function to filter external clipboard helper program data."
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      ;;(message "<string> %S:%S" (process-mark proc) (length string))
      ;; Insert the text, advancing the process marker.
      (goto-char (process-mark proc))
      (insert string)
      (setq brief--external-bytes-received
            (+ brief--external-bytes-received
               (string-bytes string)))
      (when (and brief-show-external-clipboard-recv-progress
                 (> (- brief--external-bytes-received
                       brief--prev-external-bytes-received)
                    (/ 8388608  ;; 8M (empirical)
                       (exp (- (brief-slowdown-factor) 1.0)))))
        ;; More messages, slower the receiving
        (message "* Receiving data from Xselection : %d bytes ..."
                 brief--external-bytes-received)
        (setq brief--prev-external-bytes-received
              brief--external-bytes-received))
      (set-marker (process-mark proc) (point)))))

(defvar brief-external-process-status-changed 0
  "A brief internal variable for Emacs <= v24 to detect process done.
This is used in `brief-external-get-selection'")

(defun brief--external-clipboard-sentinel (_proc _event)
  "Brief internal function, discard process status message string.
Also indicate the status change of the external helper process.  For
Emacs <= v24 this is required before getting all the output of the
external helper process."
  (incf brief-external-process-status-changed)
  ;;(message "brief--external-clipboard-sentinel %S %S %d"
  ;;         (process-exit-status proc) event
  ;;         brief-external-process-status-changed)
  t)

(defun brief-external-clipboard-process-coding-system ()
  "Prepare coding system before running external clipboard program.
The return value will be applied to `default-process-coding-system'
before creating new process."
  ;; Leave the decision of coding-system to the user.  When
  ;; user is editing a buffer, the buffer coding should already
  ;; set to the correct encoding or things might display
  ;; incorrectly.  The sender/receiver of the clipboard should
  ;; use the same coding system to be able to transfer the
  ;; correct clipboard data.
  (let ((coding (if (eq brief-external-clipboard-coding-system
                        'buffer)
                    buffer-file-coding-system
                  brief-external-clipboard-coding-system)))
    (cons coding coding)))

(defun brief-external-get-selection (&optional type mode)
  "Helper function for `brief-gui-get-selection' using an external program.
TYPE is clipboard type, can be either 'PRIMARY, 'CLIPBOARD or 'SECONDARY.
MODE is either 'interrupt (default) or 'timeout.  With 'interrupt MODE
user is able to break this function by the 'quit signal (\\[keyboard-quit]).

This function does not support native Win32/Win64; but it does support
Cygwin.  For Cygwin if the customized option `brief-cygwin-use-clipboard-dev'
is set to non-NIL (default value 't'), it will use Cygwin's '/dev/clipboard'
virtual device instead and which is much faster then forking a helper
program."

  (let ((currbuf (current-buffer)) ;; we are about to switch to a temp buffer
        (result nil)
        (coding-system (brief-external-clipboard-process-coding-system)))

    (if (and brief-is-cygwin
             brief-cygwin-use-clipboard-dev)
        ;; Cygwin is so slow on forking process so we prevent external
        ;; process and use /dev/clipboard if configured to do so.
        (with-temp-buffer
          (save-match-data
            (insert-file-contents "/dev/clipboard") ;; nil nil nil nil)
            (goto-char (point-min))
            ;; Convert from DOS line-ending to UNIX line-ending, or
            ;; MAC line endings according to current coding system
            (let ((encoding (coding-system-eol-type buffer-file-coding-system))
                  eol)
              (unless (= encoding 1) ;; DOS encoding
                (setq eol
                      (if (= 2 encoding) ;; Mac encoding
                          "\r"
                        ;; Default UNIX encoding
                        "\n"))
                (while (search-forward "\r\n" nil t)
                  (replace-match eol))))
            ;;(message "dbg: read from /dev/clipboard")
            (setq result (buffer-string))))

      ;; non-Cygwin environment
      (if (or (and (null brief-xclipboard-cmd) ;; no external helper
                   (null (brief-xclipboard-cmd-search)))
              ;; For pure terminal we don't have shared clipboard, unless someone
              ;; write a Cygwin-like /dev/clipboard ;)
              (string= "" (or (getenv "DISPLAY") ""))) ;; no display setting

          ;; No external helper available, use native backend functions
          (setq result
                (gui-backend-get-selection type 'STRING))

        ;; Running external helper program
        (with-temp-buffer
          (let*
              ((buf (current-buffer))
               ;;(process-connection-type nil) ; pipe
               (msgbuf (get-buffer "*Messages*"))
               ;; Set/Create a standalone STDERR buffer to be different from
               ;; current buffer in order to prevent process status string
               ;; being injected into stdout.
               (stderr (or msgbuf
                           (get-buffer-create
                            "*brief-external-clipboard-stderr*")))

               (default-process-coding-system coding-system)

               (proc (if (not (fboundp 'make-process))
                         ;; Emacs <= v24
                         (let ((proc
                                (apply
                                 'start-process
                                 (nconc
                                  (list brief-xclipboard-cmd
                                        (current-buffer)
                                        brief-xclipboard-cmd
                                        (car brief-xclipboard-args))
                                  ;; selection arg
                                  (and (nth 2 brief-xclipboard-args)
                                       (list (nth 2 brief-xclipboard-args)))
                                  (list (brief-xclipboard-get-type-arg type))
                                  ;; must-have args
                                  (and (nth 6 brief-xclipboard-args)
                                       (nth 6 brief-xclipboard-args))))))
                           (set-process-filter
                            proc
                            'brief--external-clipboard-filter)
                           (set-process-sentinel
                            proc
                            'brief--external-clipboard-sentinel)
                           (set-process-query-on-exit-flag proc nil)
                           proc)

                       ;; Emacs >= v25
                       (make-process
                        :name "brief-xclip"
                        :buffer buf
                        :command (nconc
                                  (list brief-xclipboard-cmd
                                        (car brief-xclipboard-args))
                                  ;; selection arg
                                  (and (nth 2 brief-xclipboard-args)
                                       (list (nth 2 brief-xclipboard-args)))
                                  (list (brief-xclipboard-get-type-arg type))
                                  ;; must-have args
                                  (and (nth 6 brief-xclipboard-args)
                                       (nth 6 brief-xclipboard-args)))
                        :connection-type 'pipe
                        :noquery t
                        ;; Prevent extern process status message get into the buffer
                        :filter   'brief--external-clipboard-filter
                        :sentinel 'brief--external-clipboard-sentinel
                        :stderr   stderr)))

               ;; Prevent message if already inhibited or in minibuffer
               (inhibit-message (or inhibit-message
                                    (minibufferp currbuf)))
               (garbage-collection-messages t)
               (count 0)
               (start-wait-time (brief-current-time))
               (clipboard-timeout
                (case (or mode 'interrupt) ;; default 'interrupt mode
                  ('timeout   (* brief-external-xclipboard-timeout
                                 (brief-slowdown-factor)))
                  ;; Emacs23: A `most-positive-fixnum' will cause immediate
                  ;; timeout, so divide it by 2.
                  ('interrupt (/ most-positive-fixnum 2))
                  (otherwise
                   (error
                    "Brief: unknown mode %S for brief-external-get-selection"
                    mode))))
               (timeout nil))

            (unless (eq msgbuf stderr)
              ;; Empty the stderr buffer to remove old contents in case
              ;; we later need to parse the information there (e.g.
              ;; "Can't display" and get the outdated information accidentally.
              (with-current-buffer stderr
                (erase-buffer)))

            (setq brief--prev-external-bytes-received 0)
            (setq brief--external-bytes-received 0)
            (setq brief-external-process-status-changed 0)
            ;;(set-process-query-on-exit-flag proc nil)
            ;;(set-process-sentinel proc 'brief--external-clipboard-sentinel)

            ;; Force timeout for big clipboard data, if in 'timeout mode
            (if (with-timeout
                    (clipboard-timeout
                     ;;(message "@@ Timeout in buffer %S!!" (current-buffer))
                     (setq timeout t)
                     (delete-process proc)
                     nil)
                  (condition-case err
                      (catch 'break
                        (progn
                          ;; Wait till process ends
                          (while (process-live-p proc)
                            ;;(sit-for 0.2) ;; extremely slow
                            (accept-process-output proc
                                                   (* 0.01
                                                      (brief-slowdown-factor)))
                            (incf count)

                            (when (zerop (logand count #xf))
                              ;; 0.01 * 15 = 0.15 second each iteration.
                              ;; We need to actually calculate the elapsed time
                              ;; to conclude a timeout.
                              ;;
                              ;; If we're just trying to backup clipboard in
                              ;; order to be nice to other APs, give up if it's
                              ;; too big, or if the external xclipboard program
                              ;; respond too slow: wait no more than
                              ;; `brief-giveup-clipboard-backup-timeout'
                              ;; seconds. By default it's 1.0 second.
                              (when
                                  (and brief-giveup-clipboard-backup-if-huge
                                       brief--backing-up-clipboard
                                       (> (- (brief-current-time)
                                             start-wait-time)
                                          (* brief-giveup-clipboard-backup-timeout
                                             (brief-slowdown-factor))))
                                (when brief-giveup-clipboard-backup-message
                                  (message
                                   "* Xselection data too big, giving up backup")
                                  (sit-for (* 0.01 (brief-slowdown-factor))))
                                (delete-process proc)
                                (throw 'break 'giveup))

                              (when brief-show-external-clipboard-recv-progress
                                ;; ;; As `brief--external-bytes-received' is
                                ;; ;; always read as zero here we can only use
                                ;; ;; a counter to estimate the progress.
                                ;; (message
                                ;;  "Receiving data from Xselection : %d iterations ..."
                                ;;  (truncate (/ count (brief-slowdown-factor))))
                                ;; ;; This `sit-for' allows message buffer updating
                                (sit-for (* 0.01 (brief-slowdown-factor))))))

                          (if (not (fboundp 'make-process))
                              ;; Wait till process sentinel reached which means
                              ;; the process status changed for some reason.
                              ;; Usually for process exited.  Only Emacs<=24
                              ;; require this.  If we enable this in Emacs26,
                              ;; it sometimes cause problems.
                              (while
                                  (and (= 0 brief-external-process-status-changed)
                                       (not (and
                                             brief-giveup-clipboard-backup-if-huge
                                             brief--backing-up-clipboard
                                             (> (- (brief-current-time)
                                                   start-wait-time)
                                                brief-giveup-clipboard-backup-timeout))))
                                (accept-process-output proc)
                                (sit-for 0.01)))

                          ;; Show total bytes received if user had waited long
                          ;; enough to see the earlier recv message.
                          (when (and brief-show-external-clipboard-recv-progress
                                     (> brief--external-bytes-received 0))
                            (message
                             "Total %d bytes received from Xselection in %.3f seconds"
                             brief--external-bytes-received
                             (- (brief-current-time) start-wait-time)))))

                    ;; User quit
                    (quit
                     (progn
                       (if (process-live-p proc)
                           (delete-process proc))
                       (if brief-show-external-clipboard-recv-progress
                           (message "* Quit receiving data from Xselection"))
                       (signal 'quit nil)))

                    ;; Catch all other kinds of errors
                    (error
                     (progn
                       (if (process-live-p proc)
                           (delete-process proc))
                       (if brief-show-external-clipboard-recv-progress
                           (message
                            "* Error %S occurs when receiving Xselection"
                            err))
                       ;; propagate the error up
                       (error (or (and (stringp (cdr err))
                                       (cdr err))
                                  (format "Error:%S" err))))))
                  ;; Everything fine, go get the `buffer-string'
                  t)

                ;; Process done.
                ;; We *won't* get any data (will get empty string, after
                ;; some experiments) if 'giveup was thrown during receiving.
                (setq result (buffer-string))

              ;; Process timeout.
              (if timeout
                  (message "Error: timeout running process %S" proc)
                ;; TODO: try to understand why it fails.  May need to get the
                ;; `process-exit-status' and parse the STDERR buffer to show
                ;; some error message.  In this case we can't use MSGBUF to
                ;; be the STDERR in this function.
                'TODO))

            ;; In case any unhandled interruption here that leaves
            ;; an useless idle proc
            (when (process-live-p proc)
              ;; Unhandled error, but no abort here since we still can
              ;; create a new process next time.
              (message "Warning: Process %S is still alive, killed it."
                       proc)
              (delete-process proc))

            ;; Kill stderr buf, seems not necessary, anyway, do it for safe
            (unless (eq stderr msgbuf)
              (let ((kill-buffer-query-functions nil)
                    ;;(delq 'process-kill-buffer-query-function
                    ;;      kill-buffer-query-functions)
                    (kill-buffer-hook nil))
                ;;(message "@@ Killing stderr %S" stderr) ;;DBG
                (kill-buffer stderr))))))

      ;; Finally, return the result
      (case type
        ('PRIMARY
         (setq gui--last-selected-text-primary result))
        ('CLIPBOARD
         (setq gui--last-selected-text-clipboard result))
        (otherwise
         ;; don't shoot an error since we can still work
         (message "Unsupported TYPE '%S' for `brief-external-get-selection'" type)))
      result)))

;;(defvar brief-external-sending 0) ;;DBG
(defconst brief-external-send-blocksize  65536 ;; (* 2 65536)
  ;; The old value (* 2 65536) make the send progress x32 times slower!
  "Maximum block size when sending string to external Xselection.
The block size is counted in 'characters' with the current buffer
encoding, not in bytes.")

(defvar brief-external-set-selection-prev-proc nil
  "Internal variable to store the lingering external helper process.
This sometimes happens maybe due to some temporarily system burst load
that make the external process postponed a bit.  Usually it will complete
later so we won't kill it in the end of `brief-external-set-selection'.
Usually before the next `brief-external-set-selection' run the process
will complete its job in the background.  In case it didn't we then
kill it as our new operation is going to overwrite it.")

(defun brief-kill-lingering-external-set-helper (&optional msg)
  "Kill lingering Xselection set process.
See `brief-external-set-selection-prev-proc' for more detail."
  (when (and brief-external-set-selection-prev-proc
             (process-live-p brief-external-set-selection-prev-proc))
    (unless msg
      (message "Warning: lingering process %S is still alive, killed it."
               brief-external-set-selection-prev-proc))
    (delete-process brief-external-set-selection-prev-proc))
  (setq brief-external-set-selection-prev-proc nil)
  t)

(defun brief-external-set-selection (type data)
  "Helper function for `brief-gui-set-selection' using an external helper program.

This function is non-blocking if external helper program is available,
unlike the default Emacs behavior that is usually blocking and not
interruptible.  If data is huge this blocking behavior could take very
long and the whole system respond time would be reduced a lot.

For Cygwin launching an external program is expensive so we use the
cygwin specific \"/dev/clipboard\" if user set the custom variable
`brief-cygwin-use-clipboard-dev' to be non-NIL.

When external helper program is launched, this non-blocking function
can be interrupted in two ways:

When marking a region, it's in \"interruptible\" mode; any keystroke
will interrupt current in-progress clipboard copying.  When performing
a `brief-copy-line' (\\[brief-copy-line]) it's in \"quittable\" mode where we
can only use `keyboard-quit' (\\[keyboard-quit]) command to interrupt it.

This function has an internal mode: `brief-is-external-interruptible'
which defines how this function can be interrupted.  If this variable
is not set this function is by default blocking and it will return
only when data are completely sent to the helper program.

This function does not support native Win32/Win64."

  ;; Notice that this function is a replacement for `gui-set-selection' which
  ;; has no mode.  Therefore the actual mode relies on the global flag
  ;; `brief-is-external-interruptible' which is controlled by function
  ;; `brief-run-postponed-gui-set-selection', which again calls
  ;; `brief-copy-region-into-clipboard' that sets this variable.
  ;;
  ;; Terminal support, we don't do `gui-get-selection' here to compare
  ;; existing clipboard, in order to reduce overheads because we are
  ;; invoking external program to process clipboards.
  (if (and brief-is-cygwin
           brief-cygwin-use-clipboard-dev)
      ;; Cygwin only
      (and (or (eq type 'PRIMARY)
               (eq type 'CLIPBOARD))
           (if (boundp 'gui--last-selected-text-primary)
               (not (or (eq gui--last-selected-text-primary data)
                        (string= gui--last-selected-text-primary data)))
             t)
           ;;(message "dbg: write to /dev/clipboard")
           (let ((encoding (coding-system-eol-type buffer-file-coding-system))
                 eol)
             (unless (= 1 encoding) ;; already dos encoding
               (setq eol
                     (if (= 2 encoding) ;; mac encoding
                         "\r"
                       ;; default unix encoding
                       "\n"))
               (with-temp-buffer
                 ;; Convert to DOS line-ending
                 (insert data)
                 (goto-char (point-min))
                 (while (search-forward eol nil t)
                   (if (/= (char-before (1- (point))) ?\r)
                       ;; ?\r already there? don't convert
                       (replace-match "\r\n")))
                 (write-region (buffer-string)
                               nil "/dev/clipboard" nil 'nomsg)))))
    ;; Non-cygwin
    (if (or (and (null brief-xclipboard-cmd) ;; No external helper available
                 (null (brief-xclipboard-cmd-search)))
            (string= "" (or (getenv "DISPLAY") ""))) ;; no display setting
        ;; Use native backend functions
        (gui-backend-set-selection type data)

      ;; Run external helper
      (when (if (boundp 'gui--last-selected-text-primary)
                (and (or (eq type 'PRIMARY)
                         (eq type 'CLIPBOARD))
                     (not (or (eq gui--last-selected-text-primary data)
                              (string= gui--last-selected-text-primary data))))
              t)

        (case type
          ('PRIMARY
           (setq gui--last-selected-text-primary data))
          ('CLIPBOARD
           (setq gui--last-selected-text-clipboard data))
          (otherwise
           (message "Unsupported TYPE for `brief-external-set-selection'")))

        ;; Delete the lingering process as we're going to overwrite the
        ;; Xselection which it has been lingering on.
        (brief-kill-lingering-external-set-helper
         brief-show-external-clipboard-send-progress)

        (let* ((process-connection-type nil) ; pipe
               (default-process-coding-system ;;'(utf-8 . utf-8))
                 (brief-external-clipboard-process-coding-system))
               (proc (apply
                      'start-process
                      (nconc (list
                              brief-xclipboard-cmd nil
                              ;;"timeout"
                              ;;(number-to-string brief-external-xclipboard-timeout)
                              brief-xclipboard-cmd)
                             ;; input arg
                             (and (cadr brief-xclipboard-args)
                                  (list (cadr brief-xclipboard-args)))
                             ;; selection arg
                             (and (nth 2 brief-xclipboard-args)
                                  (list (nth 2 brief-xclipboard-args)))
                             (list
                              (brief-xclipboard-get-type-arg type))
                             ;; must have args only needed for 'get'
                             ;;(last brief-xclipboard-args
                             ;;      (- (length brief-xclipboard-args) 6))
                             )))
               ;; inhibit message if already did or in minibuffer
               (inhibit-message  (or inhibit-message (minibufferp)))
               (count 0)
               (datalen (length data))
               (databytes (string-bytes data))
               (databeg 0)
               (sentbytes 0)
               (sendstr "")
               (blocksize  (truncate (/ brief-external-send-blocksize
                                        (brief-slowdown-factor))))
               (dataend blocksize)
               (start-wait-time (brief-current-time)))

          (set-process-query-on-exit-flag proc nil)
          ;;(message "*proc %S starts*" proc) ;;DBG
          (condition-case err
              ;; Catch 'quit signal or proc will stay alive after quit
              (progn
                ;; Send pieces during idle, till user inputs something
                (while (and
                        (case brief-is-external-interruptible
                          ;; Interruptible mode
                          ('interruptible
                           ;; any input event breaks the loop
                           (not (brief-input-pending-p)))
                          ;; Quitable mode
                          ('quitable
                           ;; continue, but check 'quit signal
                           ;;(if
                           (brief-input-pending-p) ;; checking timer
                           ;;    (accept-process-output nil 0.05))
                           t)
                          ;; Continuous mode, no break
                          (otherwise
                           t))
                        (< databeg datalen)
                        (process-live-p proc))

                  (setq sendstr
                        (substring-no-properties data
                                                 databeg
                                                 (min dataend datalen)))
                  (setq sentbytes
                        (+ sentbytes (string-bytes sendstr)))
                  (process-send-string proc sendstr)

                  (when (zerop (logand count 255))
                    ;; Notice that this `sit-for' MUST be placed *BEFORE*
                    ;; showing the message, otherwise the `process-live-p'
                    ;; will think the PROC to be still running even if
                    ;; it's already exited or aborted.
                    ;;(accept-process-output)
                    (sit-for 0.01 nil) ;; cannot be zero
                    (and brief-show-external-clipboard-send-progress
                         (process-live-p proc)
                         (message "* Sent %d bytes to Xselection"
                                  sentbytes)))

                  (when (and brief-is-external-interruptible
                             (zerop (logand count 255)))
                    ;; Allow Emacs updating its internal status, or
                    ;; receive the 'quit signal.
                    (accept-process-output)
                    ;;(sit-for 0.01 t)
                    )
                  (incf count)
                  (setq databeg dataend
                        dataend (+ dataend blocksize)))

                (if (process-live-p proc)
                    (progn
                      (if (and brief-show-external-clipboard-send-progress
                               (> databeg 0))
                          (if (>= databeg datalen)
                              (message
                               "Complete sending %d bytes to Xselection in %.3f seconds"
                               databytes
                               (- (brief-current-time) start-wait-time))
                            (message "* Interrupted sending to Xselection")))
                      (process-send-eof proc))

                  ;; TODO: Try to understand why process ends.  Showing error
                  ;; messages here is not favored as it will keep showing if
                  ;; it's a persistent error (e.g. some X display server failure
                  ;; or incorrect "DISPLAY" environment variable.); unless we
                  ;; limit the times that the error messages shown.  If the
                  ;; display server come back to live again we should restart
                  ;; to show that error message in case it again failed.
                  ;; Another possible reason is that it crashed or been killed
                  ;; by other process.
                  'TODO)

                (if (>= dataend datalen)
                    ;; Completed sending
                    (progn
                      (setq brief-external-set-selection-interrupted nil)
                      data)
                  ;; Interrupted by user input
                  (if (process-live-p proc)
                      (delete-process proc))
                  (setq brief-external-set-selection-interrupted t)
                  nil))

            ;; Kill proc in case of 'quit signal
            (quit (progn
                    (process-send-eof proc)
                    (if (process-live-p proc)
                        (delete-process proc))
                    (if brief-show-external-clipboard-send-progress
                        (progn
                          (message "* Quit sending data to Xselection")
                          (brief-dbg-message "  (quit sending in %S mode)"
                                             brief-is-external-interruptible)))

                    (setq brief-external-set-selection-interrupted 'quit)
                    ;; Propagate quit signal forwards
                    (signal 'quit nil)))

            ;; Catch all other kinds of errors
            (error (progn
                     (process-send-eof proc)
                     (if (process-live-p proc)
                         (delete-process proc))
                     (if brief-show-external-clipboard-send-progress
                         (message
                          "* Error %S occurs when sending to Xselection" err))

                     (setq brief-external-set-selection-interrupted 'error)
                     ;; propagate the error up
                     (error (or (and (stringp (cdr err))
                                     (cdr err))
                                (format "Error:%S" err))))))

          ;; If in any case the process might not complete its job here, we don't
          ;; need to wait as we have already sent it all our data.  Neither can
          ;; we kill it since it might still be processing the data we sent in
          ;; the background.
          (when (process-live-p proc)
            (setq brief-external-set-selection-prev-proc proc)))))))

(defun brief-gui-select-text (text)
  "Override the default `gui-select-text' system function (select.el).
Conditionally set `gui--last-selected-text-primary' and
`gui--last-selected-text-clipboard'.  If our previous external clipboard
invoking was interrupted, clear them."
  (let ((select-enable-primary   (and (not (brief-is-winnt))
                                      brief-select-enable-primary))
        (select-enable-clipboard (or (brief-is-winnt)
                                     brief-select-enable-clipboard)))
    (setq brief-external-set-selection-interrupted nil)
    (when select-enable-primary
      (gui-set-selection 'PRIMARY text)
      (setq gui--last-selected-text-primary
            (if brief-external-set-selection-interrupted
                nil
              text)))
    (when select-enable-clipboard
      (setq saved-region-selection text)
      (gui-set-selection 'CLIPBOARD text)
      (setq gui--last-selected-text-clipboard
            (if brief-external-set-selection-interrupted
                nil
              text)))))

(defvar brief-previous-clipboard-selection nil
  "Backup Xselection before start marking texts.
The region/rectangle marking operation could later be canceled.  We
therefore need to backup first otherwise Xselection will be immediately
replaced by the marking texts; when operation canceled we will not be
able to restore it back if we have no backups.")

;; The core modification that prevent Windows X server failure
;; due to too much flooding message as clipboard change
(if (not (fboundp 'advice-add))
    (progn
      (defvar brief-ad-gui-get-selection-reenter nil)
      (defvar brief-ad-gui-set-selection-reenter nil)
      (if brief-selection-op-legacy
          (defadvice x-get-selection (around brief-advice-gui-get-selection
                                             (&optional type data-type)
                                             disable activate compile)
            (if (or window-system
                    brief-ad-gui-get-selection-reenter)
                ad-do-it
              (let ((brief-ad-gui-get-selection-reenter t))
                (brief-external-get-selection (or type 'PRIMARY)))))

        (defadvice gui-get-selection (around brief-advice-gui-get-selection
                                             (&optional type data-type)
                                             disable activate compile)
          ;; [2017-07-14 Fri] When clipboard data is huge,
          ;; `gui-backend-get-selection', which was implemented as
          ;; `x-get-selection-internal', will stop responding.
          (if (or window-system
                  brief-ad-gui-get-selection-reenter)
              ad-do-it
            (let ((brief-ad-gui-get-selection-reenter t))
              (brief-external-get-selection (or type 'PRIMARY))))))

      (if brief-selection-op-legacy
          (defadvice x-set-selection (around brief-advice-gui-set-selection
                                             (type data) disable activate compile)
            (if (or (eq type 'SECONDARY)
                    brief-ad-gui-set-selection-reenter)
                ad-do-it
              (let ((brief-ad-gui-set-selection-reenter t))
                (if brief-is-gui-set-selection-postponed
                    (brief-activate-postpone-gui-selection-timer)
                  (unless (brief-multiple-cursor-in-action)
                    (if (and (brief-is-x)
                             (not brief-use-external-clipboard-when-possible))
                        ad-do-it
                      (brief-external-set-selection type data)))))))

        (defadvice gui-set-selection (around brief-advice-gui-set-selection
                                             (type data)
                                             disable activate compile)
          (if (or (eq type 'SECONDARY)
                  brief-ad-gui-set-selection-reenter)
              ad-do-it
            (let ((brief-ad-gui-set-selection-reenter t))
              (unless (and brief--search-overlay
                           (overlay-start brief--search-overlay))
                ;; Bypass `gui-set-selection' due to searching

                (if brief-is-gui-set-selection-postponed
                    ;; Activate timer to start postponing gui-set-selection.
                    ;; In terminal mode this will not run.
                    (brief-activate-postpone-gui-selection-timer)
                  (unless (brief-multiple-cursor-in-action)
                    (if (or (brief-is-winnt)
                            (and
                             (brief-is-x)
                             ;; [2017-12-12 Tue] The following is no longer true.
                             ;; Cannot reproduce it any longer, maybe a glitch
                             ;; during the development?
                             ;; [2017-07-13 Thu] When running in X11, the
                             ;; function `x-own-selection-internal' will fail
                             ;; if the data is longer than 262040 bytes.  This
                             ;; bug is caught thru many experiments.
                             ;; When data is long, use external helper program.
                             ;;(< (length data) 262041)
                             (not brief-use-external-clipboard-when-possible)))
                        ad-do-it
                      (brief-external-set-selection type data))))))))))
  ;;
  ;; `advice-add' defined
  ;;
  ;; When external helper is enabled but neither 'xsel' nor 'xclip' is
  ;; installed, reenter will occur.
  (defvar brief-gui-get-selection-reentry nil
    "An internal variable to prevent advised function reenter.")

  (defun brief-gui-get-selection (orig-func &optional type &rest args)
    "Brief's advice replacement for `gui-get-selection'."
    ;; [2017-07-14 Fri] When clipboard data is huge, `gui-backend-get-selection'
    ;; which was implemented as `x-get-selection-internal' will stop responding.
    (if brief-gui-get-selection-reentry
        (apply orig-func type args)
      (let ((brief-gui-get-selection-reentry t))
        (if (or (and (brief-is-x)
                     brief-use-external-clipboard-when-possible)
                (brief-is-terminal))
            (brief-external-get-selection (or type 'PRIMARY))
          (or (if (and (brief-is-winnt)
                       ;; On Win32/Win64 we by default use 'CLIPBOARD
                       (eq (or type 'CLIPBOARD)
                           'CLIPBOARD))
                  ;; TODO: revise this for Win32/64 Emacs newer than v23
                  (or (w32-get-clipboard-data)
                      (w32--get-selection)))
              (apply orig-func type args))))))

  ;;(advice-remove 'gui-get-selection 'brief-gui-get-selection)
  ;;(advice-add 'gui-get-selection :around 'brief-gui-get-selection)

;;(defvar brief-gui-set-debouncer
;;  "Debouncing info for brief-gui-set-selection due to MS Office message flood.
;;Format: (time selection-data)"
;;    nil)
;;

  (defvar brief-gui-set-selection-reentry nil
    "An internal variable to prevent advised function reenter.")

  ;; The core modification that prevent Windows X server failure
  ;; due to too much flooding message as clipboard change caused by
  ;; Microsoft Office
  (defun brief-gui-set-selection (orig-func &rest args)
    "Brief's advice replacement for `gui-set-selection'."
    (if brief-gui-set-selection-reentry
        (progn
          (brief-dbg-message "Reenter brief-gui-set-selection, call orig func.")
          (apply orig-func args))
      (brief-dbg-message "enter brief-gui-set-selection")
      (let ((type (car args))
            ;;(data (cadr args))
            (brief-gui-set-selection-reentry t))
        (if (eq type 'SECONDARY)
            (apply orig-func args)
          (unless (and brief--search-overlay
                       (overlay-start brief--search-overlay))
            ;; Bypass `gui-set-selection' due to searching
            (if brief-is-gui-set-selection-postponed
                ;; Activate timer to start postponing gui-set-selection
                ;; (in terminal mode this will not run)
                (brief-activate-postpone-gui-selection-timer)

              ;;(deactivate-mark) ;; this will cause reenter as this will invoke
              ;;                  ;; `gui-set-selection'
              (unless (brief-multiple-cursor-in-action)
                (if (or (brief-is-winnt)
                        (and
                         (brief-is-x)
                         ;; [2017-12-12 Tue] The following is no longer true.
                         ;; Cannot reproduce it any longer, maybe a glitch
                         ;; during the development?
                         ;; [2017-07-13 Thu] When running in X11, the function
                         ;; `x-own-selection-internal' will fail if the data
                         ;; is longer than 262040 bytes.  This bug is caught
                         ;; by many experiments.
                         ;; When data is long, use external helper program.
                         ;; TODO: verify if this bug persists for emacs version
                         ;; > 26.0.50. if bug persists, fix it.
                         ;;(< (length data) 262041)
                         (not brief-use-external-clipboard-when-possible)))
                    (apply orig-func args)
                  (apply #'brief-external-set-selection args)))))))
      (brief-dbg-message "leave brief-gui-set-selection"))))

;;
;; Xselection/Windows-clipboard backward compatibility functions for older Emacs
;;

;;(when (brief-is-winnt) ;; TODO: fix this here, also use gui-xxxx function here
;;  (defalias 'x-selection-exists-p 'w32-selection-exists-p)
;;  (defvar x-last-selected-text nil)
;;  (defun x-get-selection ()
;;    (setq x-last-selected-text (w32-get-clipboard-data)))
;;  (defun x-get-selection-value ()
;;    (w32-get-clipboard-data)))

;; A shared local variable between `brief-selection-exists-p' and
;; `brief-get-selection-value' to prevent `brief-yank' invoking
;; `gui-get-selection' twice in terminal mode.
;; When clipboard data is big, this invokes external program twice,
;; which is very inefficient.
(defvar-local brief-terminal-getclip-when-check-existence nil)

;; No need to check the existence, as it works only in the very beginning.
;; Once Xselection is set once, this should always returns true. Even if it's
;; empty, just read it back and compare with empty string.
;;(defun brief-selection-exists-p ()
;;  (if (brief-is-x)
;;      (x-selection-exists-p 'PRIMARY)
;;    (if (brief-is-terminal)
;;        (not (zerop (length
;;                     (or (setq brief-terminal-getclip-when-check-existence
;;                               (gui-get-selection 'PRIMARY))
;;                         ""))))
;;      (and (brief-is-winnt)
;;           (w32-selection-exists-p 'CLIPBOARD)))))

(defvar brief-last-selected-text nil) ;; for w32 environment
(defun brief-last-selected-text ()
  "Local function called only by these backward compatibility functions."
  (if (brief-is-x)
      gui--last-selected-text-primary
    (and (brief-is-winnt)
         brief-last-selected-text)))

(defun brief-get-selection-value ()
  "Legacy function called by `brief-yank' for older Emacs only."
  (let ((result nil))
    (if (or (brief-is-x)
            (brief-is-terminal))
        (if (fboundp #'gui-get-selection)
            ;; Here we choose primary only since we consider primary is in sync
            ;; with clipboard when editing with Brief mode
            (if brief-terminal-getclip-when-check-existence
                ;; We just get it using external program, use it, don't invoke
                ;; external program again
                (setq result brief-terminal-getclip-when-check-existence
                      brief-terminal-getclip-when-check-existence nil)
              (setq result (gui-get-selection 'PRIMARY)))
          (setq result (x-get-selection-value)))
      (and (brief-is-winnt)
           (let ((clipdata (or (w32-get-clipboard-data)
                               (w32--get-selection))))
             (setq result
                   (and clipdata
                        (setq brief-last-selected-text clipdata))))))
    result))

(defun brief-get-selection ()
  "Local function called by `brief-get-clipboard-selection' only."
  (if (brief-is-x)
      (x-get-selection)
    (and (brief-is-winnt)
         (let ((clipdata (or (w32-get-clipboard-data)
                             (w32--get-selection))))
           (and clipdata
                (setq brief-last-selected-text clipdata))))))

;;(if (fboundp #'w32-selection-exists-p)
;;    (defun brief-get-clipboard-selection ()
;;      "Function to get system clipboard text on X or MS-WIN systems."
;;      (and (brief-is-winnt)
;;           (w32-selection-exists-p 'CLIPBOARD)
;;           (w32--get-selection)))
(defun brief-get-clipboard-selection ()
  "Function to get system clipboard text on X or MS-WIN systems."
  ;;(or (and (brief-is-x)
  ;;         (x-selection-exists-p 'PRIMARY)
  ;;         ;;(x-get-selection brief-X-selection-target)
  ;;         (x-get-selection-value))
  ;;    (and (brief-is-winnt)
  ;;         (x-selection-exists-p 'CLIPBOARD)
  ;;         (or (x-get-selection-value)
  ;;             x-last-selected-text))))
  (if (brief-is-winnt)
      (and (w32-selection-exists-p 'CLIPBOARD)
           (or (brief-get-selection)
               brief-last-selected-text))
    ;;(if (brief-is-x)
    ;;    (and (x-selection-exists-p 'PRIMARY)
    ;;         ;;(x-get-selection brief-X-selection-target)
    ;;         (x-get-selection-value)))
    ;; [2017-01-19 Thu] The following now works for both terminal and X
    (gui-get-selection)))

;; This will sometimes make `wg-restore-window' fails. I modified
;; `wg-restore-window' and add a advice function in init.el to prevent this.
;;(add-hook 'activate-mark-hook 'brief-region-backup-clipboard-selection)

;;
;; Xselection backup and restore
;;
;; Whenever mark is activated, for either rectangle or region, we need to
;; backup Xselection before it got changed, in case user later would like
;; to cancel current operation.
;;

(defun brief-copy-region-into-clipboard (&optional thetext interruptible)
  "Copy current region or THETEXT into X clipboard then return it."
  ;;(if window-system ;; <2011-09-19 Mon 15:01> prevent run this on pure terminals
  (unless (brief-multiple-cursor-mode-is-on)
    ;; [2016-05-24 17:06:41 +0800] support multiple-cursors mode
    ;;(and (boundp 'multiple-cursors-mode)
    ;;     multiple-cursors-mode)
    (let ((text
           ;;(or (and (boundp 'cua--last-killed-rectangle)
           ;;         cua--last-killed-rectangle
           ;;         (car cua--last-killed-rectangle))
           ;;    (buffer-substring-no-properties
           ;;     (region-beginning) (region-end)))
           (or thetext
               ;; [2017-01-25 Wed] must not use (current-kill 0)
               ;; otherwise the kill-ring will grow.
               (car kill-ring)
               ""))
          (brief-is-gui-set-selection-postponed
           nil) ; this function won't work if this flag is t
          (brief-is-external-interruptible interruptible)
          ;; Some system use 'PRIMARY, some use 'CLIPBOARD
          (x-select-enable-primary 't)
          (x-select-enable-clipboard 'nil)
          ;; Emacs >= 25.1
          (select-enable-primary   (and (not (brief-is-winnt))
                                        brief-select-enable-primary))
          (select-enable-clipboard (or (brief-is-winnt)
                                       brief-select-enable-clipboard)))

      (unless (zerop (length text))
        ;;(x-set-selection brief-X-selection-target text)
        (if (fboundp #'gui-select-text)
            (gui-select-text text) ;; -> `brief-gui-select-text'
          (x-select-text text));;)
        ;;(setq x-select-enable-primary t)
        ;;        (x-set-selection 'PRIMARY text)
        ;;(setq x-select-enable-clipboard t)
        ;;        (x-set-selection 'CLIPBOARD text)
        ;;(x-set-selection 'SECONDARY text)
        ;;(x-select-text text)
        ;;(set-register 'brief text)
        ))))

(defun brief-backup-clipboard-selection ()
  "Force backup clipboard data before selection changed.
If user cancelled the selection, we can restore it."
  (let ((brief-is-gui-set-selection-postponed nil)
        (brief--backing-up-clipboard t))
    (setq brief-previous-clipboard-selection
          (brief-get-clipboard-selection))))

(defun brief-region-backup-clipboard-selection ()
  ;; [2016-04-15 Fri] If we're marking rectangle and do editing (say, inserting
  ;; chars for all column), we'll `cua-set-rectangle-mark' repeatedly called
  ;; before region deactivated, and therefore corrupt the clipboard backup.
  ;; Therefore we backup only when region is not yet activated.
  (unless (or (region-active-p) ;; For Emacs23 we need to test `region-active-p' first
              (brief-use-region)
              (brief-multiple-cursor-in-action))
    (brief-backup-clipboard-selection))
  t)

(defun brief-no-longer-need-restore-clipboard ()
  "Destroy Xselection backup data.
We do this either if we completed the backup or no need to keep it."
  (setq brief-previous-clipboard-selection nil))

(defun brief-restore-clipboard-selection ()
  "Restore just backed up clipboard data if there is one.
Return the restored text if previously backed up data successfully
restored, otherwise NIL."
  (let ((brief-is-gui-set-selection-postponed nil)
        (text brief-previous-clipboard-selection))
    (and ;;(brief-use-region) ;; Don't check this, see comments in
     ;;                       ;; `brief-use-region'. When that happens this
     ;;                       ;; TEXT will not be restored.
         text
         (progn
           (brief-copy-region-into-clipboard text)
           (brief-no-longer-need-restore-clipboard)
           text))))

;;
;; Mark activate/deactivate hook for Xselection postpone/backup/restore
;;
;;(defvar brief-garbage-collection-message-stack nil)

(defun brief-postpone-gui-set-selection ()
  "Hook function for `activate-mark-hook' to activate Xselection postponment."
  (unless brief-is-gui-set-selection-postponed
    ;; When marking a CUA rectangle, any self-inserting command will re-activate
    ;; the marking, so this function will be called again without any
    ;; `deactivate-mark-hook' called in between.  Therefore we need to test
    ;; if `brief-is-gui-set-selection-postponed' is already true.
    (setq brief-is-gui-set-selection-postponed t)
    (setq brief-postponed-clipboard-ran nil)
    (brief-activate-postpone-gui-selection-timer))
  ;; When marking a very big region, I would like to see why system is pausing
  ;; intermittently.
  ;; I found it usually because of the garbage collection.
  ;;
  ;; This is not very good because quit signal sometimes not popping this flag
  ;; out, only for debugging.
  ;;(push garbage-collection-messages brief-garbage-collection-message-stack)
  ;;(setq garbage-collection-messages t)
  t)

(defun brief-resume-gui-set-selection ()
  "Hook function for `deactivate-mark-hook' to deactivate Xselection postponment."
  ;;(message "@ brief-resume-gui-set-selection")
  (if brief-keep-postpone-gui-selection-timer
      ;; If `brief-copy-to-clipboard-on-keyup' was called, we *MUST* not cancel
      ;; idle timer as that copy was postponed but need to be done on idle time.
      (setq brief-keep-postpone-gui-selection-timer nil)
    (brief-cancel-postpone-gui-selection-timer))
  (setq brief-is-gui-set-selection-postponed nil)
  ;; Restore backup if not done yet.
  (brief-restore-clipboard-selection)
  (setq brief-postponed-clipboard-ran nil)
  ;;(setq garbage-collection-messages
  ;;      (pop brief-garbage-collection-message-stack))
  t)

;;(add-hook 'activate-mark-hook   'brief-postpone-gui-set-selection 't)
;;(add-hook 'deactivate-mark-hook 'brief-resume-gui-set-selection)

;;
;; Various advice functions
;;
;; So far I have not found a good way to eliminate all these necessary
;; advice functions.  Some of the hook functions are not invoked when
;; a command is canceled or interrupted.  Once there is a good way to
;; do this these advice functions will be removed.

;; [2017-07-10 Mon] Even Emacs 23.3.1 still having `activate-mark-hook' called.
;; (defadvice cua-set-rectangle-mark (before brief-cua-set-rectangle-mark ()
;;                                           activate compile)
;;   ;; [2017-07-10 Mon] The following comments was written on 2015-05-13 commit
;;   ;; 5579b35d, but seems not valid. My local tests shows even for Emacs
;;   ;; 24.4.50, `activate-mark-hook' still got invoked.
;;   ;; [2015-05-13]
;;   ;; cua-set-rectangle-mark will not invoke activate-mark-hook so we do it
;;   ;; using advice
;;   ;; TODO: is there a better way without using advice?
;;   (brief-region-backup-clipboard-selection)))

;; When undo restored the cua rectangle, we need to backup the clipboard or the
;; same issue happens
;; (defadvice cua--rect-undo-handler (around brief-cua--rect-undo-handler
;;                                           activate compile)
;;   (let ((backup-clipboard (use-region-p)))
;;     (or backup-clipboard
;;         (brief-region-backup-clipboard-selection))
;;     ad-do-it
;;     (or backup-clipboard
;;         (brief-region-backup-clipboard-selection))))

(defun brief-reset-for-command-cancellation (&rest _)
  "Restore selection and reset internal variables due to command cancellation."
  (brief-restore-clipboard-selection)
  (and (brief-use-region) (deactivate-mark))
  (brief-cancel-postpone-gui-selection-timer)
  (setq brief-postponed-mark-selection-copy-completed nil))

(defun brief-reset-for-keyboard-quit (&rest _)
  "Reset Brief internal state for `keyboard-quit' command."
  (brief-reset-for-command-cancellation)
  ;; reset keyboard macro definition
  (setq brief--kbd-macro-seq nil))

(defun brief-cua-rectangle-undo-helper ()
  ;; `cua--rect-undo-handler' relies on this function to restore rectangle,
  ;; so we do advice here
  (and cua--restored-rectangle
       (brief-region-backup-clipboard-selection)))

;; TODO: Is there a better way then defining an advice for restoring clipboard?

;; Note that `deactivate-mark-hook' is not adequate as in many situation
;; involving command cancellation, that hook never got invoked.
(eval-when (compile eval load)
  (unless (fboundp #'advice-add)
    ;; Initially we disable our advises but hooked, till Brief mode is enabled
    (if (fboundp 'cua--rectangle-post-command)
        (defadvice cua--rectangle-post-command
            (before brief-cua--rectangle-post-command () disable activate compile)
          (brief-cua-rectangle-undo-helper)))

    ;; Starting from Emacs23 the CUA mode is builtin.
    (defadvice cua-cancel (before brief-cua-cancel () disable activate compile)
      (brief-reset-for-command-cancellation))

    (defadvice keyboard-quit
        (before brief-keyboard-quit () disable activate compile)
      (brief-reset-for-keyboard-quit))

    (defadvice keyboard-escape-quit
        (before brief-keyboard-escape-quit () disable activate compile)
      (brief-reset-for-command-cancellation))

    (when (fboundp 'cua-clear-rectangle-mark)
      (defadvice cua-clear-rectangle-mark
          (before brief-clear-rectangle-mark () disable activate compile)
        "Extend `cua-clear-rectangle-mark' by a Xselection backup.
The implementation of `cua-clear-rectangle-mark' does not invoke the
`deactivate-mark-hook' thus we need to do that ourselves."
        (brief-reset-for-command-cancellation)))

    (when (fboundp 'cua-close-rectangle)
      (defadvice cua-close-rectangle
          (before brief-close-rectangle () disable activate compile)
        "Extend `cua-close-rectangle' by a Xselection backup.
The implementation of `cua-close-rectangle' does not invoke the
`deactivate-mark-hook' thus we need to do that ourselves."
        (brief-reset-for-command-cancellation)))))

;;
;; Various Brief mode interactive commands
;;

(defun brief-scan-rectangle (text)
  "Scan text and set its yank properties as rectangle if it is a rectangle."
  ;; Refer to function `rectangle--extract-region' for rectangle
  (let* ((strs nil)
         (width nil)
         (widtheq nil)
         (eol 0)
         (start 0))
    (while (and (setq eol (string-match "\n" text start))
                (or width (setq width (- eol start)))
                (setq widtheq (= width (- eol start))))
      (setq strs (cons (substring-no-properties text start eol) strs)
            start (1+ eol)))
    ;; Rectangles should not have the ending EOL
    (if (and widtheq
               (= (- (length text) start) width))
      (put-text-property 0 (length text) 'yank-handler
                         `(rectangle--insert-for-yank
                           ,(nreverse (cons (substring-no-properties
                                             text start
                                             (length text)) strs))
                           t)
                         text)))
  text)

(defun brief-yank ()
  "Yank kill-ring/Xselection or rectangle into current text."
  (interactive "*")
  (let* ((interprogram-cut-function nil)
         (interprogram-paste-function nil))

    ;; If the previous `brief-copy-line' has not yet completed (keyup timeout
    ;; not reached yet), then flush the pending operation which must be
    ;; uninterruptible.
    (if brief-copy-to-clipboard-on-keyup-data
        (brief-run-postponed-gui-set-selection))

    (unless (brief-multiple-cursor-mode-is-on)
      ;; For multiple cursors mode we probably want different texts to be
      ;; yanked on different cursors so grabbing a shared Xselection is not
      ;; correct here.
      (let ((clip-select (or (brief-restore-clipboard-selection)
                             (if (and (fboundp #'gui-get-selection)
                                      (not (brief-is-winnt)))
                                 ;; Here we don't invoke `gui-selection-value'
                                 ;; as it favors 'CLIPBOARD over 'PRIMARY.
                                 ;; If user just switch from using 'CLIPBOARD
                                 ;; to 'PRIMARY, we might get the old data
                                 ;; in the 'CLIPBOARD.
                                 (gui-get-selection
                                  (or (and brief-select-enable-primary
                                           'PRIMARY)
                                      (and brief-select-enable-clipboard
                                           'CLIPBOARD)))
                               ;; For older Emacs and WinNT
                               (brief-get-selection-value))))
            (deactivate-mark nil))
        (unless (or (zerop (length clip-select))
                    ;;(message "/clip-select=%S" clip-select) ;;dbg
                    ;;(message "\\kill-ring=%S" (car kill-ring)) ;;dbg
                    (string= clip-select ; is it the same as clipboard ?
                             (or (car  kill-ring) ""))
                    (string= clip-select (or (cadr kill-ring) "")))
          ;; The below is too slow, when kill-ring is huge, so comment it out
          ;;(member clip-select kill-ring)) ;; is it already in kill-ring ?
          (save-mark-and-excursion  ; Sync with kill-ring
            ;;(message "kill-new %S" clip-select) ;;dbg
            (ignore-errors
              ;; on EmacsW32 it once failed to call `menu-bar-update-yank-menu'
              (kill-new
               ;; Scan if it's a rectangle
               (brief-scan-rectangle clip-select)))))))
    (let ((inhibit-message t))
      (save-mark-and-excursion
        ;; TODO: use 'delete-selection property instead
        ;; (see 'delsel.el and delete-selection-mode)
        ;; eg. (put 'brief-yank 'delete-selection ...)
        (when (brief-use-region)
          (and (brief-rectangle-active)
               (goto-char (cua--rectangle-top))
               (move-to-column (cua--rectangle-left)))
          (cua-delete-region))
        (call-interactively 'cua-paste)))
    (brief-no-longer-need-restore-clipboard)))

;; 04/15/'08 ins function
(defun brief-print ()
  "Print buffer or region."
  (interactive)
  (call-interactively (if (brief-use-region)
                          'print-region
                        'print-buffer)))

(defun brief-buffer-read-only-toggle ()
  ;; 06/21/2005 ins function
  "Toggle buffer read only status ON/OFF."
  (interactive)
  (if (fboundp 'read-only-mode) ;; above Emacs23
      (if buffer-read-only
          (read-only-mode -1)
        (read-only-mode 1))
    ;;(if (and (buffer-modified-p) (not buffer-read-only))
    ;;    ;;if it's modified, it must not be read-only
    ;;    ;; - [2013-01-31 10:06:48 +0800] false assumption, buffers created by
    ;;    ;; '*Find*' will be such case.
    ;;    (message
    ;; "Buffer modified, cannot set to read-only. Please save the buffer first.")
    (setq buffer-read-only (not buffer-read-only)))

  (force-mode-line-update) ;; 05/07/2008 ins 1
  (if buffer-read-only
      (message "Buffer set to read-only")
    (message
     "Buffer set to read-write, careful when modifying read-only files!")))

(defun brief-mark-line (arg)
  "Set mark at the end of the line.
ARG behaves the same as `beginning-of-line'."
  (interactive "p")
  (let (newmark)
    (save-excursion
      ;; 06/17/'05 rem 1 ins 1
      ;; 04/15/'08 rem 1 unrem 1, a region always starts from the
      ;;           top-of-mark-stack to '(point)'
      ;;    (end-of-line arg)
      (beginning-of-line arg)
      (setq newmark (point)))
    ;;(push-mark newmark t t)
    (set-mark newmark)))

;; 04/15/2008 ins function, modified from brief-mark-line
(defun brief-mark-line-up (arg)
  "Set mark at the end of the line.
ARG behaves the same as `end-of-line'."
  ;; TODO: take care of visual line mode
  (interactive "p")
  (let ((inhibit-message t)
        newmark)
    (cua-set-mark)
    ;;(message nil)
    (save-excursion
      (end-of-line arg)
      (setq newmark (1+ (point))))  ;; include EOL
    ;;(push-mark newmark t t)
    (set-mark newmark)))

;; 04/15/2008 ins function, modified from brief-mark-line
(defun brief-mark-line-down (arg)
  "Set mark at the begin of the line.
ARG behaves the same as `beginning-of-line'."
  ;; TODO: take care of visual line mode
  (interactive "p")
  (let ((inhibit-message t)
        newmark)
    (cua-set-mark)
    ;;(message nil)
    (save-excursion
      (beginning-of-line arg)
      (setq newmark (point)))
    ;;(push-mark newmark t t)
    (set-mark newmark)))

;; ;; 04/15/'08 add function for `brief-kill-line'
(unless (fboundp 'move-to-column)
  (defun move-to-column (column
                         &optional insert-white) ;; [06/12/2008] ins &optional
    "Goto column number in current line.
This is a backward compatibility function for older Emacs versions."
    (let ((i 0) (max-column 0))
      (save-excursion
        (end-of-line)
        (setq max-column (current-column)))
      (beginning-of-line)
      (while (and (< i column) (< (current-column) column))
        (if (= (current-column) max-column)
            (progn
              (if (not insert-white)    ;; [06/12/2008] ins 2
                  (setq i (1- column))  ; set exit while condition
                (insert " ") ;; fill with spaces
                (setq max-column (1+ max-column))))
          (brief-forward-1-char-noerror))
        (setq i (1+ i))))))

;;
;; Line Oriented Commands
;;

(defvar brief-delete-entire-line-debounce nil
  "MSoffice debouncing variable for `brief-delete-entire-line' command.")

(defun brief-is-visual-operation ()
  "Test if Brief need to respect visual mode or not.
This function implements the following truth table, according to
current command prefix and the value of `brief-linecmd-respect-visual'.
The basic logic are:
 1. `brief-linecmd-respect-visual' determines the current default mode,
    either visual or physical line mode.
 2. Either a common prefix \\[universal-argument] or negative prefix
    reverse the current mode; aka, ~`brief-linecmd-respect-visual'.

         Prefix  |  brief-linecmd- || result:
         command |  respect-visual || visual?
        ---------+-----------------||--------
            F    |        F        ||   F
            F    |        T        ||   T
          '(4)   |        F        ||   T
          '(4)   |        T        ||   F
        ---------+-----------------||--------
           #n    |        F        ||   F
           #n    |        T        ||   T
          -#n    |        F        ||   T
          -#n    |        T        ||   F

Therefore, it's basically an XOR operation between {common prefix \\[universal-argument]
or negative prefix} and {`brief-linecmd-respect-visual'}."

    (brief-xor (and current-prefix-arg
                    (or (and (numberp current-prefix-arg)
                             (< current-prefix-arg 0))
                        (equal current-prefix-arg '(4))))
               brief-linecmd-respect-visual))

(defun brief-move-to-column-visual (vcol)
  "Move to visual column VCOL but not exceeding EOL."
  (let* (p
         (vend (save-excursion
                 (end-of-visual-line) ;; move to current visual line end
                 (setq p (brief-current-column-visual))
                 (if (not (zerop p))
                     p
                   ;;(left-char 1) ;; Emacs23 have no `left-char'
                   (backward-char 1)
                   (brief-current-column-visual)))))
    ;;Emacs23 have no `right-char'
    ;;(right-char (- (min vcol vend) (brief-current-column-visual)))
    (forward-char (- (min vcol vend) (brief-current-column-visual)))))

(defun brief-delete-entire-line (arg)
  ;; <2011-06-09 Thu 14:14> for Emacs, use 'delete' as name since it does not
  ;; go into kill-ring
  "Delete entire (visual) line(s) where cursor located.

When a region is selected, the marked region is deleted without going
into the kill-ring or clipboard.  Otherwise, it operates only on
current line, which in turn considering the setting of visual mode,
hidden texts and command prefix.

When marked region is not present and `brief-linecmd-respect-visual'
is non-nil, this command respect visual mode (line truncation, and
hidden texts) so it only deletes the visible part of current line
unless prefixed with a \\[universal-argument] which told this command to do delete
the entire current line (or lines if hidden texts present).

On the contrary, when `brief-linecmd-respect-visual' is nil (default),
this command deletes the entire line unless prefixed with a \\[universal-argument] which
told this command to respect visual mode."
  (interactive "*p")

  (let ((this-command this-command)
        (last-command last-command)
        (cmd-time (brief-current-time))
        (debounce (list (point) (brief-use-region))))
    (if (and brief-debounce-keys-microsoft-office
             (not executing-kbd-macro)
             brief-delete-entire-line-debounce
             (eq this-command last-command)
             (< (- cmd-time
                   (car brief-delete-entire-line-debounce))
                (* 0.02 (brief-slowdown-factor)))
             (equal (list (point) (brief-use-region))
                    (cadr brief-delete-entire-line-debounce)))
        ;; Debouncing for M$ Office weird behavior, otherwise this command will
        ;; be repeatedly invoked and cause infinite loop
        nil
      (if (brief-use-region)
          (brief-delete arg)
        (let* ((visual (brief-is-visual-operation)) ;; delete only visual part
               (column (brief-current-column-visual)) ;; (current-column)
               p1 p2)
          (if (equal current-prefix-arg '(4))
              (setq arg 1)) ;; When prefixed with C-u, ARG=4 so fix it
          (brief-delete-region
           ;; Use `move-beginning-of-line' or `beginning-of-visual-line' to
           ;; bypass hidden texts '...' if we're at the end of it '...'. Current
           ;; version of `beginning-of-visual-line' move to the very beginning
           ;; of current line.  However, in order to prevent later version Emacs
           ;; changing `beginning-of-visual-line' to behaves like
           ;; `end-of-visual-line' (move to end of visual screen width end
           ;; instead of the real line end), I choose `move-beginning-of-line'.
           (setq p1 (if visual
                        (beginning-of-visual-line 1)
                      (move-beginning-of-line 1)))
           (if (= p1 (progn
                       (if visual
                           (vertical-motion (abs arg))
                         (move-beginning-of-line (1+ (abs arg))))
                       (setq p2 (point)))) ;; are we at `eobp' ?
               (progn (move-end-of-line 1) (point))
             p2))
          (if (/= p1 p2)
              (brief-move-to-column-visual column)))))
    (setq brief-delete-entire-line-debounce
          (list cmd-time debounce))))

(defvar brief-kill-line-debounce nil)

;; Control variables used in my "cursor undo" package, to be released.
(defvar enable-cursor-tracking)
(defvar disable-local-cursor-tracking)

(defun brief-kill-line (arg)
  "Kill/cut (visual) line(s) into kill-ring and clipboard.
Marks from point to end of the current line (honoring prefix arguments),
copies the region to the kill ring and clipboard, and then deletes it."
  (interactive "*p")

  (let ((interprogram-cut-function nil)
        (interprogram-paste-function nil)
        (old-column (brief-current-column-visual)) ;; (current-column)
        (prev-point (point))
        (enable-cursor-tracking nil)
        (this-command this-command)
        (last-command last-command)
        (visual (brief-is-visual-operation))
        ;;(curr-prefix-arg current-prefix-arg)
        ;; Debouncing
        (cmd-time (brief-current-time)))
    (if (and brief-debounce-keys-microsoft-office
             (not executing-kbd-macro)
             brief-kill-line-debounce
             (eq this-command last-command)
             (< (- cmd-time (car brief-kill-line-debounce)) 0.3)
             (= (point) (cadr brief-kill-line-debounce)))
        ;; Debouncing for M$ Office weird behavior, this command will be
        ;; repeatedly invoked and cause infinite loop
        (brief-dbg-message (format "deboucing %S:%S"
                                   this-command brief-kill-line-debounce))

      (if (brief-use-region)
          (let ((inhibit-message t)) ; preventing `cua-set-mark' saying "Mark Set"
            (call-interactively 'cua-cut-region)
            ;;(brief-copy-region-into-clipboard)
            (brief-copy-to-clipboard-on-keyup))

        (save-mark-and-excursion
          ;;(cua-set-mark)
          ;;(message nil) ; clear the "Mark Set" message due to cua-set-mark
          (set-mark (if visual
                        (save-excursion
                          ;; There could be more than one lines hidden
                          ;; in this visual line.
                          (beginning-of-visual-line 1))
                      (line-beginning-position)))
          ;;(ignore-errors ; prevent "end of buffer" error
          ;;  ;;(next-line arg)
          ;;  ;;(forward-line 1) does not work for org-mode hidden lines
          ;;  ;; Use call-interactively to prevent compilation warning message
          ;;  ;; like "Warning: `next-line' is for interactive use only; use
          ;;  ;; `forward-line' instead."
          ;;  (call-interactively 'next-line))
          ;;(vertical-motion 1)
          ;;(if (> (brief-fast-count-physical-lines prev-point (point)) 0)
          ;;    (beginning-of-line))
          (ignore-errors ; prevent `end-of-buffer' error
            ;;(let ((current-prefix-arg
            ;;       (if (and curr-prefix-arg
            ;;                (not (equal curr-prefix-arg '(4))))
            ;;           curr-prefix-arg ;; other prefix than C-u
            ;;         (if visual nil
            ;;           '(4)))))
            ;;  (brief-next-line)) ; honor `current-prefix-arg', will jump a
            ;;                    ;;physical line if prefixed on line wrapping
            (if visual
                (vertical-motion (abs arg))
              (move-beginning-of-line (1+ (abs arg))))
            (if (or (not visual)
                    (= (brief-fast-count-physical-lines prev-point (point))
                       arg)) ; physical line(s) moved
                (beginning-of-line) ; goto column 1 of current physical line
              (beginning-of-visual-line)))

          (call-interactively 'brief-kill-region)
          (brief-copy-to-clipboard-on-keyup)) ;;(brief-copy-region-into-clipboard)
        ;;(move-to-column old-column 't)
        (brief-move-to-column-visual old-column)))

    (and brief-debounce-keys-microsoft-office
         (not executing-kbd-macro)
         (setq brief-kill-line-debounce (list cmd-time (point))))))

(defvar brief-copy-line-debounce '(0 (0 0 0)))

(defun brief-copy-line (arg)
  "Copy (visual) line(s) into kill-ring and clipboard.

When a region is selected, the marked region is copy into kill-ring
and clipboard, the mark is then deactivated.

When marked region is not present and `brief-linecmd-respect-visual'
is non-nil, this command respect visual mode (line truncation, and
hidden texts) so it only copy the visible part of current line unless
prefixed with a \\[universal-argument] which told this command to copy the entire
physical line (or lines if hidden texts present).

On the contrary, when `brief-linecmd-respect-visual' is nil (default),
this command copy the entire line(s) unless prefixed with a \\[universal-argument] which
told this command to respect visual mode and copy only visual line(s).

To copy exactly 4 lines use C-4 as prefix instead of a single \\[universal-argument]."
  (interactive "p")
  (let ((fdebounce
         ;; `flet' was obsoleted since 24.3, while older emacs don't have
         ;; `cl-flet'.  For backward compatibility's sake we use lambda
         ;; function here.
         (lambda () (list (point)
                          (brief-region-beginning)
                          (brief-region-end)))))
    (if (and brief-debounce-keys-microsoft-office
             (not executing-kbd-macro)
             brief-copy-line-debounce
             (eq this-command last-command)
             (< (- (brief-current-time) (car brief-copy-line-debounce)) 0.3)
             (equal (funcall fdebounce) (cadr brief-copy-line-debounce)))
        ;; Debouncing for MS Office clipboard behavior, this command will be
        ;; repeatedly invoked and cause infinite loop
        (progn
          ;;(setq brief-copy-line-debounce
          ;;  (cons (brief-current-time) (cadr brief-copy-line-debounce)))
          (and brief-debounce-keys-microsoft-office
               (not executing-kbd-macro)
               (setq brief-copy-line-debounce
                     (list (brief-current-time) (funcall fdebounce)))))

      (let* ((interprogram-cut-function nil)
             (interprogram-paste-function nil)
             (prev-point (point))
             (inhibit-message t) ; preventing `cua-set-mark' saying "Mark Set"
             ;;(curr-prefix-arg current-prefix-arg)
             (is-physical-line (not (brief-is-visual-operation)))
             ;; make `current-prefix-arg' a local variable, override the original
             (current-prefix-arg current-prefix-arg))
        ;;    (when (brief-multiple-cursor-mode-is-on)
        ;;      (redisplay)) ;; [2017-03-13 Mon] sometimes `use-region-p'
        ;;                   ;; supposed to be 't but is nil in multiple cursor
        ;;                   ;; mode, not sure why so try `redisplay' here
        (and brief-debounce-keys-microsoft-office
             (not executing-kbd-macro)
             (setq brief-copy-line-debounce
                   (list (brief-current-time)
                         (funcall fdebounce))))
        (brief-dbg-message
         (format "%S:%S:dbc:%S"
                 (- (brief-current-time) (car brief-copy-line-debounce))
                 (< (- (brief-current-time) (car brief-copy-line-debounce)) 0.5)
                 brief-copy-line-debounce))
        (if (brief-use-region)
            ;; Copy region/rectangle
            (progn
              (cua-copy-region nil) ;; in case using rectangle
              ;;(copy-region-as-kill (mark) (point) 'region)
              (brief-no-longer-need-restore-clipboard)
              (brief-copy-to-clipboard-on-keyup))
          ;; Copy line(s)
          (save-mark-and-excursion
            ;;(cua-set-mark)
            ;;(message nil) ; clear the annoying "Mark Set" message from cua-set-mark
            ;;(push-mark (line-beginning-position) t t)
            (let (this-command ;; prevent var `this/last-command' got changed
                  last-command)
              (if (equal current-prefix-arg '(4))
                  (setq arg 1)) ;; When prefixed with C-u, ARG=4 so fix it
              (set-mark (if is-physical-line
                            (line-beginning-position)
                          (save-excursion
                            ;; There could be more than one lines abbreviated
                            ;; in this visual line.
                            (beginning-of-visual-line 1))))
              (ignore-errors ; prevent `end-of-buffer' error
                ;; (next-line arg)
                ;; (forward-line 1) does not work for org-mode hidden lines
                ;;(brief-next-line) ; honor `current-prefix-arg', will jump a
                ;;                  ;;physical line if prefixed on line wrapping
                (if is-physical-line
                    (move-beginning-of-line (1+ (abs arg)))
                  (vertical-motion (abs arg)))
                (if (or is-physical-line
                        (= ;; calculate physical line moved, was
                           ;; (- (line-number) curr-line)
                           (brief-fast-count-physical-lines prev-point (point))
                           ;;(or (and (null curr-prefix-arg) 1) ; no prefix
                           ;;    (and (listp curr-prefix-arg)
                           ;;         (car curr-prefix-arg)) ;; C-<#num> prefixed
                           ;;    curr-prefix-arg)
                           arg)) ; physical line(s) moved
                    (beginning-of-line) ; goto column 1 of current physical line
                  ;; Wrap long lines
                  ;; [2017-03-03 Fri] The earlier `brief-next-line' already
                  ;; honored `current-prefix-arg' so we don't need the following
                  ;; block of code anymore
                  ;; (if (not (equal curr-prefix-arg '(4))) ; long line wrapped,
                  ;;     ;; if a single 'C-u' prefix was pressed we copy the
                  ;;     ;; while line instead of the visual line. To copy exact
                  ;;     ;; 4 visual lines use 'C-4' as prefix instead of a single
                  ;;     ;; 'C-u'
                  ;;     (beginning-of-visual-line)
                  ;;   (end-of-line) ; no prefix argument, we want the whole line
                  ;;   ;; instead of visual line
                  ;;   (right-char))
                  (beginning-of-visual-line))))
            ;; Set the overidden prefix arg to nil so that the following
            ;; `cua-copy-region' won't be prefixed
            (setq current-prefix-arg nil)
            (cua-copy-region nil) ; this will insert region into `kill-ring'
            ;;(copy-region-as-kill (mark) (point) 'region) ; this will insert
            ;;                                      ;; region into `kill-ring'
            (brief-no-longer-need-restore-clipboard)
            (brief-copy-to-clipboard-on-keyup))))

      ;; Clear the region after the operation is complete
      ;; XEmacs does this automatically, Emacs doesn't.
      (if (fboundp 'deactivate-mark)
          (deactivate-mark)))))

;;
;; Page down/Page up
;;

(defvar brief-fixed-cursor-scroll-column)
(defvar brief-fixed-cursor-scroll-line)

(make-local-variable 'brief-fixed-cursor-scroll-column)
(make-local-variable 'brief-fixed-cursor-scroll-line)

(defmacro smooth-scroll-mode-activate ()
  "Older Emacs need `smooth-scroll-mode' package to be able to scroll smoothly."
  `(and (boundp 'smooth-scroll-mode)
        smooth-scroll-mode))

(defun brief-discard-input ()
  "Discard inputs when doing a time consuming operation.
If not doing so, the autorepeat key settings might cause that command to
start again.  For example, page-down and page-up commands."
  (or defining-kbd-macro ;; discard input will make macro definition fail
      (discard-input)))

;; Modified from the obsoleted pc-mode.el
(defun brief-fixed-cursor-scroll-down-internal (arg)
  "Acts like scroll-down, but leaves cursor fixed relative to window.
Just like previous-line and forward-line, this function should not be
used in programs."
  (if (and arg (< arg 0))
      (brief-fixed-cursor-scroll-up-internal (- arg))
    (or (or (eq last-command 'brief-fixed-cursor-page-down)
            (eq last-command 'brief-fixed-cursor-page-up))
        (and (setq brief-fixed-cursor-scroll-column
                   (brief-current-column-visual))
             (setq brief-fixed-cursor-scroll-line
                   (brief-current-row-visual))))
    (let ((scroll-by (* (or arg 1)
                        (1- (- (window-height)
                               next-screen-context-lines)))))
      (condition-case nil
          ;; <2011-06-08 Wed 18:48> modified for smooth-scroll
          (if (= 1 (window-start))
              (goto-char 1)
            (if (smooth-scroll-mode-activate)
                (smooth-scroll/orig-scroll-down scroll-by)
              (scroll-down scroll-by))
            (move-to-window-line brief-fixed-cursor-scroll-line)
            (brief-move-to-column (+ (current-column)
                                     brief-fixed-cursor-scroll-column))
            (and (= brief-fixed-cursor-scroll-column (1- (window-width)))
                 (null (eolp))
                 (forward-char -1)))
        (beginning-of-buffer
         (progn (goto-char (point-min)) (message "Beginning of buffer")))
        (end-of-buffer
         (progn (goto-char (point-max)) (message "End of buffer")))
        (error nil))
      ;; scroll up/down is slow so for complicated file the queued pgup/pgdown
      ;; keys will take a lot of time to process, so discard the inputs
      (brief-discard-input))))

(defun brief-fixed-cursor-scroll-up-internal (arg)
  "Acts like scroll-up, but leaves cursor fixed relative to window.
Just like previous-line and forward-line, this function should not
be used in programs."
  (if (and arg (< arg 0))
      (brief-fixed-cursor-scroll-down-internal (- arg))
    (or (or (eq last-command 'brief-fixed-cursor-page-down)
            (eq last-command 'brief-fixed-cursor-page-up))
        (and (setq brief-fixed-cursor-scroll-column
                   (brief-current-column-visual))
             (setq brief-fixed-cursor-scroll-line
                   (brief-current-row-visual))))
    (let ((scroll-by (* (or arg 1)
                        (1- (- (window-height)
                               next-screen-context-lines)))))
      (condition-case nil
          (progn ;; <2011-06-08 Wed 18:48> modified for smooth-scroll
            (if (smooth-scroll-mode-activate)
                (smooth-scroll/orig-scroll-up scroll-by)
              (scroll-up scroll-by))
            (if (not (eobp)) (move-to-window-line
                              brief-fixed-cursor-scroll-line))
            (brief-move-to-column (+ (current-column)
                                     brief-fixed-cursor-scroll-column))
            (and (= brief-fixed-cursor-scroll-column (1- (window-width)))
                 (null (eolp))
                 (forward-char -1)))
        (beginning-of-buffer
         (progn (goto-char (point-min)) (message "Beginning of buffer")))
        (end-of-buffer
         (progn (goto-char (point-max)) (message "End of buffer")))
        (error nil))
      (brief-discard-input))))

;; <2011-06-02 Thu 12:05> ins 2 functions
(defun brief-fixed-cursor-page-down (arg)
  "Scroll up a full page by going down to the next page.
The page size is determined by the window size.  With a prefix it
scroll several pages.  Negative prefix scrolls page in the reverse
direction."
  (interactive "^p")
  (if (brief-rectangle-active)
      (call-interactively 'cua-resize-rectangle-page-down)
    (brief-fixed-cursor-scroll-up-internal arg)))

(defun brief-fixed-cursor-page-up (arg)
  "Scroll down a full page by going up to the previous page.
The page size is determined by the window size.  With a prefix it
scroll several pages.  Negative prefix scrolls page in the reverse
direction."
  (interactive "^p")
  (if (brief-rectangle-active)
      (call-interactively 'cua-resize-rectangle-page-up)
    (brief-fixed-cursor-scroll-down-internal arg)))

(defconst brief-recenter-horizontal-positions '(middle left right))

(defvar-local brief-recenter-horizontal-last-op nil
  "Indicates the last recenter operation performed.
Possible values: `left', `middle', `right', integer or float numbers.
It can also be nil, which means the first value in
`brief-recenter-horizontal-positions'.")

(defun brief-recenter-horizontally (&optional arg)
  "Recenter horizontally according to ARG.
ARG is a integer or list of a integer."
  (set-window-hscroll
   (selected-window)
   (or (and (null arg)
            (- (brief-text-unscaled-current-column)
               (/ (window-body-width) 2)))
       (and (listp arg) (not (null arg)) (car arg))
       (and (integerp arg) arg)
       (window-hscroll))))

(defun brief-recenter-horizontally-left ()
  (brief-recenter-horizontally (brief-text-unscaled-current-column)))

(defun brief-recenter-horizontally-right ()
  (brief-recenter-horizontally (max 0 (- (brief-text-unscaled-current-column)
                                         (window-body-width)))))

(defun brief-recenter-left-right (arg)
  "Recenter cursor horizontally, like `recenter-top-bottom' does.
Reference to `recenter-top-bottom'.  Note that it won't do anything
if cursor position is currently at the beginning of a line."
  (interactive "P")
  (cond
   (arg (brief-recenter-horizontally arg)) ; Always respect ARG.
   (t
    (message "Recentering horizontally to the %S."
             (setq brief-recenter-horizontal-last-op
                   (if (eq this-command last-command)
                       (car
                        (or (cdr
                             (member
                              brief-recenter-horizontal-last-op
                              brief-recenter-horizontal-positions))
                            brief-recenter-horizontal-positions))
                     (car brief-recenter-horizontal-positions))))
    (cond ((eq brief-recenter-horizontal-last-op 'middle)
           (brief-recenter-horizontally))

          ((eq brief-recenter-horizontal-last-op 'left)
           (brief-recenter-horizontally-left))

          ((eq brief-recenter-horizontal-last-op 'right)
           (brief-recenter-horizontally-right))

          ((integerp brief-recenter-horizontal-last-op)
           (brief-recenter-horizontally brief-recenter-horizontal-last-op))

          ((floatp brief-recenter-horizontal-last-op)
           (brief-recenter-horizontally
            (round (* brief-recenter-horizontal-last-op
                      (window-body-width)))))))))

(defun brief-recenter ()
  "Recenter cursor to show contexts around.
If `brief-search-recenter-vertically' is t, this function will
recenter cursor vertically to show contexts above/below.
If `brief-search-recenter-horizontally' is t and line truncation is
ON, this will also recenter horizontally to the right side to show
contexts left to the cursor."
  (and brief-search-recenter-vertically
       (not (window-minibuffer-p)) ;; no re-center in minibuffer
       (recenter))
  (and brief-search-recenter-horizontally
       (or visual-line-mode
           (not truncate-lines)
           (brief-recenter-horizontally-right))))

;;
;; Search
;;

(defun brief-toggle-search-case-sensitivity ()
  "Toggle case sensitivity for search or replace commands in current buffer.
This function toggles buffer-local variable `case-fold-search'.
Notice that this will also affect replacement operation.  When `case-replace'
is non-nil the replacement transfers the case pattern.  For more details on the
case-pattern-transfer behavior please refer to `query-replace'."
  (interactive)
  (setf case-fold-search (not case-fold-search))
  (message (concat "Toggle search in this buffer to case-"
                   (if case-fold-search "in") "sensitive.")))

(defun brief-toggle-search-replace-regexp ()
  "Toggle search & replace commands using regular expression or string.
Buffer-local variable `brief-search-replace-using-regexp' will be toggled."
  (interactive)
  (message (concat "Toggle search & replace using "
                   (if (setq brief-search-replace-using-regexp
                             (not brief-search-replace-using-regexp))
                       "regular expression"
                     "simple string"))))

;; Search/Query&replace command type test functions
(defun brief-is-prefix-command (cmd)
  "Check if CMD is a prefix command."
  (member cmd '(universal-argument      ; C-u
                digit-argument)))       ; C-<digits>

(defun brief-is-search-forward-command (cmd)
  "Check if CMD is a forward search command."
  (or (member cmd '(brief-search-forward
                    brief-search-forward-currword
                    brief-repeat-search-forward))
      (and (eq cmd 'brief-repeat-search)
           brief-last-search-action-forward)))

(defun brief-is-search-backward-command (cmd)
  "Check if CMD is a backward search command."
  (or (member cmd '(brief-search-backward
                    brief-search-backward-currword
                    brief-repeat-search-backward))
      (and (eq cmd 'brief-repeat-search)
           (not brief-last-search-action-forward))))

(defun brief-is-search-command (cmd)
  "Check if CMD is a search command."
  (or (eq cmd 'brief-repeat-search)
      (brief-is-search-forward-command cmd)
      (brief-is-search-backward-command cmd)
      (eq cmd 'brief-toggle-search-case-sensitivity)))


(defun brief-is-query-replace-forward-command (cmd)
  "Check if CMD is a query replace forward command."
  (or (member cmd '(brief-query-replace-forward
                    brief-query-replace-forward-currword
                    brief-repeat-query-replace-forward))
      (and (eq cmd 'brief-repeat-query-replace)
           brief-last-query-replace-forward)))

(defun brief-is-query-replace-backward-command (cmd)
  "Check if CMD is a query replace backward command."
  (or (member cmd '(brief-query-replace-backward
                    brief-query-replace-backward-currword
                    brief-repeat-query-replace-backward))
      (and (eq cmd 'brief-repeat-query-replace)
           (not brief-last-query-replace-forward))))

(defun brief-is-query-replace-command (cmd)
  "Check if CMD is a query replace command."
  (or (eq cmd 'brief-repeat-query-replace)
      (brief-is-query-replace-forward-command cmd)
      (brief-is-query-replace-backward-command cmd)))

(defvar brief-hold-overlay nil
  "A dynamically scoped temporarily variable to allow region deactivation
without affecting the overylay.  Used by `brief-search-replace'.")

(defun brief-delete-search-overlay ()
  "Delete the search overlay when region deactivated."
  (and (overlayp brief--search-overlay)
       (not brief-hold-overlay)
       (eq (overlay-buffer brief--search-overlay) (current-buffer))
       (delete-overlay brief--search-overlay))
  t)

;;(add-hook 'deactivate-mark-hook 'brief-delete-search-overlay)

;;(defvar-local brief-last-query-replace-region nil)
(defvar-local brief-last-search-region nil ; (is-rect is-region count/beg end)
  "This local variable records the last region/rectangle used in search.")

(defun brief-undo (&optional arg)
  "Wrapper for the default `undo' for brief specific operations.
If we're searching in a region, this undo will restore the region."
  (interactive "P")
  ;; TODO: undo everything in the region (check `undo' then restore
  ;;       the region. (Find text "Undo in region" in `undo' source)
  (let () ;; (was-replace nil)
    (if (not (or (brief-is-search-command last-command)
                 (brief-is-prefix-command last-command)
                 ;;(setq was-replace
                       (brief-is-query-replace-command last-command)));;)
        ;; It was not a search or query&replace command, do usual undo
        (undo arg)

      ;; It was a search or query&replace command

      ;;(when (and was-replace
      ;;           brief-last-query-replace-region)
      ;;  (deactivate-mark)
      ;;  (goto-char (car brief-last-query-replace-region))
      ;;  (set-mark (mark t))
      ;;  (goto-char (cadr brief-last-query-replace-region))
      ;;  (setq brief-last-query-replace-region nil))
      (setq brief-last-search-region nil) ; (is-rect is-region count/beg end)
      (if (and (overlayp brief--search-overlay)
               (eq (overlay-buffer brief--search-overlay) (current-buffer))
               (overlay-start brief--search-overlay)
               (overlay-end   brief--search-overlay))
          ;; Line region?
          (progn
            ;; restore region
            (if (brief-is-search-forward-command last-command)
                (progn
                  (set-mark (overlay-end brief--search-overlay))
                  (goto-char (overlay-start brief--search-overlay)))
              ;; was a backward search
              (set-mark (overlay-start brief--search-overlay))
              (goto-char (overlay-end brief--search-overlay)))
            (delete-overlay brief--search-overlay))
        ;; Rectangle?
        (if (brief-rectangle-active)
            (progn
              (brief-keep-rectangle-unchanged)
              (goto-char (cua--rectangle-top))
              (move-to-column (cua--rectangle-left)))
          ;; Neither line region nor rectangle, old undo
          (undo arg))))))

(defun brief-reset-new-search ()
  "Reset search and delete the overlay if not continuing searching.
For example, moving cursor around should reshape the region."
  (unless (or (brief-is-search-command this-command)
              (brief-is-prefix-command this-command)
              (eq this-command 'brief-undo))
    (setq brief-last-search-region nil)
    (brief-delete-search-overlay)
    (remove-hook 'pre-command-hook 'brief-reset-new-search))
  t)

(defvar-local brief-search-failed nil)

(defun brief-search-complete ()
  "Completion function for search."
  (and (not (minibuffer-window-active-p (selected-window)))
       (message "Search completed.")
       (brief-recenter))
  (setq brief-search-failed nil))

(defun brief-search-failed ()
  "Failure function for search."
  (when (and (or (car brief-last-search-region) ;; region/rectangle search ended
                 (cadr brief-last-search-region))
             brief-search-failed)
    ;; In order not to deactivate user's region so easily, for the first failure,
    ;; we only warns; for the second failure we then deactivate the mark.  In
    ;; this way if user insist on searching with the same pattern, the next
    ;; search will go beyond the region and continue globally.
    (setq brief-last-search-region nil)
    (deactivate-mark))
  (and (not (minibuffer-window-active-p (selected-window)))
       (message "Pattern not found."))
  (setq brief-search-failed t))

;;
;; Forward Search
;;
(defun brief-search-forward-rectangle-pattern (pattern
                                               &optional regend noerror count)
  "Search PATTERN forwards within current rectangle.
If REGEND presents, the forward search does not extend beyond of REGEND.
When repeat searching within a rectangle, the cursor will temporarily
stopped at the searched point if found.  If we really want to set cursor
there, press \\[keyboard-quit] to cancel the rectangle."
  (setq count (or count 1))
  (let* ((left (cua--rectangle-left))
         (width (1+ (- (cua--rectangle-right) left)))
         (lineend (save-excursion ;; end point of the rectangle at
                    ;;            ;; current line
                    (move-to-column (cua--rectangle-right))
                    (min (1+ (point))
                         (line-end-position))))
         (cnt 0)
         (result (catch 'found
                   (save-excursion
                     ;;(goto-char (cua--rectangle-top))
                     ;;(move-to-column left)
                     ;;(setq lineend (+ (point) width))
                     (while (< (point) regend)
                       (if (funcall
                            (if brief-search-replace-using-regexp
                                #'search-forward-regexp
                              #'search-forward)
                            pattern
                            (min lineend
                                 (line-end-position)
                                 (+ (point) width))
                            noerror 1)
                           (and (incf cnt)
                                (= cnt count)
                                (throw 'found (point)))
                         (forward-line 1)
                         (move-to-column left)
                         (setq lineend (+ (point) width))))
                   nil))))
    (and result
         (progn
           (brief-keep-rectangle-unchanged)
           (goto-char result)))))

;;(defvar brief-last-search-prefix-arg nil)

;; Two local variables to record latest search begin/end for horizontal
;; recentering.
(defvar-local brief-last-search-begin  nil)
(defvar-local brief-last-search-end    nil)

(defun brief-search-forward-pattern (pattern &optional noerror count)
  "Search forwards for a PATTERN.
The search is limited in the currently marked (rectangle) region.
Cursor will jump the match position only if the search is successful.
The cursor jump is only temporarily when searching in a (rectangle)
region.  To settle the cursor there, cancel the (rectangle) region."
  (brief-dbg-message "enter brief-search-forward-pattern")
  (let* ((point      (point))
         (is-rect    (brief-rectangle-active))
         (is-region  (or (brief-use-region)
                         ;; previous search was a region but happen to end up
                         ;; having region size reduced to zero after the search
                         ;; (the end point of region is exactly the end of the
                         ;; search pattern)
                         (and (cadr brief-last-search-region)
                              (eq (region-beginning) (region-end)))))
         (is-overlay (and brief-search-fake-region-mark
                          (overlayp brief--search-overlay)))
         (reg-start  (or (and is-overlay
                              (overlay-start brief--search-overlay))
                         (and is-rect
                              (cua--rectangle-top))
                         (and is-region
                              (region-beginning))))
         (reg-end    (or (and is-overlay
                              (overlay-end brief--search-overlay))
                         (and is-rect
                              (cua--rectangle-bot))
                         (and is-region
                              (region-end))))
         (new-search (and (or is-rect is-region)
                          (brief-is-search-command this-command)
                          (not (brief-is-search-command last-command))))
         (result     nil))

    (when new-search
      ;; Reset `brief-search-failed' for a new search
      (setq brief-search-failed nil)
      (add-hook 'pre-command-hook 'brief-reset-new-search))

    ;; Activate the search overlay for a region if it's not activated
    (and is-region
         (not is-rect)
         new-search
         brief-search-fake-region-mark
         (if brief--search-overlay
             (when (null (overlay-start brief--search-overlay))
               (move-overlay brief--search-overlay reg-start reg-end))
           ;; Create buffer specific overlay
           (setq brief--search-overlay
                 (make-overlay (point-min) (point-min)))
           (delete-overlay brief--search-overlay)
           (overlay-put brief--search-overlay 'face 'brief-fake-region-face)
           (move-overlay brief--search-overlay reg-start reg-end)))

    ;; Check if we're continuing a search within a region
    (if (brief-is-search-forward-command last-command)
        ;; Check if we're continuing a search within a region/rectangle
        (if (pop brief-last-search-region)
            ;; Previous search was in a rectangle
            ;; (is-rect| point count ignore)
            (progn
              ;;(setq count (1+ (or (cadr brief-last-search-region) 1)))
              (goto-char (pop brief-last-search-region))) ;; previous point
          (when (pop brief-last-search-region)
            ;; Previous search was in a region
            ;; (is-rect| is-region| beg end)
            (pop brief-last-search-region) ;; drop BEG
            (goto-char (pop brief-last-search-region)) ;; END
            ;; previous forward search stopped at the end of the pattern
            (set-mark (point)) ; mark backwards
            (goto-char point))))

    ;; [2017-06-21 Wed] When a rectangle is at the same line, either REG-START
    ;; and REG-END, or [2017-06-27 Tue] REG-END and `cua--rect-start-position'
    ;; will be equal.  We therefore need to adjust REG-END otherwise the search
    ;; will fail.
    (if (and is-rect
             (or (= reg-start reg-end)
                 (= reg-end (cua--rect-start-position))))
        (save-excursion
          ;; rect at the same line
          (move-to-column (cua--rectangle-right))
          (setq reg-end (min (1+ (point))
                             (point-max)))))

    ;; DONE [2017-06-22 Thu]: when repeatedly performing a failure search in a
    ;;       region/rectangle means user is trying to expand current selection
    ;;       region/rectangle via search, we need to go back to
    ;;       `search-forward-regexp' to resize current selection (hint: keep
    ;;       typing shift-f5 3 times when region is active and no result found)
    ;; Note: [2014-09-15 Mon 11:36] when prefix command "ctrl-u" presents, it
    ;; will force the search not limited in the region
    ;;(setq brief-last-search-prefix-arg current-prefix-arg)
    (save-mark-and-excursion
      ;; When region active, search from the beginning of the region
      (if new-search
          (if is-rect
              ;; If new search begins, goto top of rectangle
              (progn
                (goto-char (cua--rectangle-top))
                (move-to-column (cua--rectangle-left)))
            (if is-region
                (unless (eq reg-start (point))
                  ;; We are marking forwards, change it backwards; or the
                  ;; region will end at the cursor and thus stop further
                  ;; region-searching.
                  ;;(push-mark (mark t) t t)
                  (cua-exchange-point-and-mark nil)))))

      (setq result
            (funcall (or (and (brief-rectangle-active)
                              #'brief-search-forward-rectangle-pattern)
                         (if brief-search-replace-using-regexp
                             #'search-forward-regexp
                           #'search-forward))
                     pattern
                     reg-end
                     noerror count))
      (setq point (point))) ;; save the point we find the pattern

    ;; If the initial search failed, restore the region right away
    ;; TODO: [2017-06-28 11:33:55 +0800]
    ;;       Since we now use `save-mark-and-excursion' do we still need this??
    ;;(when (and new-search
    ;;           is-region
    ;;           (not is-rect)
    ;;           (not result)
    ;;           ;; was not marking backwards
    ;;           (not (eq reg-start (point))))
    ;;  ;;(pop-mark) ;; this will deactivate overlay as well
    ;;  ;;(set-mark (mark t))
    ;;  (cua-exchange-point-and-mark nil))

    (setq brief-last-search-region
          (if is-rect
              (list is-rect point count reg-end)
            (list is-rect is-region reg-start reg-end)))
    (setq brief-last-search-begin (match-beginning 0)
          brief-last-search-end   (match-end 0))

    (and result
         (goto-char result)
         (progn
           ;;(and is-region
           ;;     (= (region-beginning)
           ;;        (region-end)) ;; region reduced to zero now
           ;;     (progn ;; prevent region keep extending on next search
           ;;       (setq brief-last-search-region nil)
           ;;       (deactivate-mark)))
           (ignore-errors (run-hook-with-args 'brief-after-search-hook result))
           result))))

;; added function 02/09/2006
(defun brief-search-forward (arg)
  "Search string/regexp forwards."
  (interactive
   (list
    (if (brief-multiple-cursor-in-action)
        brief-search-last
      (read-string "Search forwards: " nil 'brief-search-history nil))))
  ;;[2011-04-14 Thu 13:53] rem 1;
  ;; (setq case-fold-search brief-global-case-fold-search)
  (if (and (string= "" arg) ;; no input, search last if exists
           (car brief-search-history))
      (setq arg (car brief-search-history)))
  (if (prog1 (brief-search-forward-pattern arg t)
        ;;(prog1 (setq brief-search-forward-last
        ;;             (car minibuffer-history)) ;; 04/03/2008 rem 1 ins 1
        (setq brief-search-last arg)
        ;;(defalias 'brief-last-search-function 'brief-search-forward)
        (setq brief-last-search-action-forward t))
      (brief-search-complete)
    (brief-search-failed)
    nil))

;; added function 02/10/2006
(defun brief-search-forward-currword (arg)
  "Search forwards for the word that cursor is focusing on."
  (interactive
   (list
    (let ((default (or (current-word) "")))
      (if (brief-multiple-cursor-in-action)
          (and (string= default "")
               default)
        (read-string "Search forwards: "
                     default 'brief-search-history default)))))
  ;;[2011-04-14 Thu 13:53] rem 1;
  ;; (setq case-fold-search brief-global-case-fold-search)
  (if (and (string= "" arg) ;; no input, search last if exists
           (car brief-search-history))
      (setq arg (car brief-search-history)))
  (if (prog1 (brief-search-forward-pattern arg t)
        ;;(prog1 (setq brief-search-forward-last
        ;;       (car minibuffer-history)) ;; 04/03/2008 rem 1 ins 1
        (setq brief-search-last arg)
        ;;(defalias 'brief-last-search-function 'brief-search-forward-currword)
        (setq brief-last-search-action-forward t))
      (brief-search-complete)
    (brief-search-failed)
    nil))

;;
;; Backward Search
;;

(defun brief-search-backward-rectangle-pattern (pattern
                                                &optional regstart noerror count)
  "Search PATTERN backwards within current rectangle.
If REGSTART presents, the backward search does not go ahead of REGSTART.
When repeat searching within a rectangle, the cursor will temporarily
stopped at the searched point if found.  If we really want to set
cursor there, press \\[keyboard-quit] to cancel the rectangle."
  (setq count (or count 1))
  (let* ((lastline 0)
         (right (1+ (cua--rectangle-right))) ;; 1+: including the rightmost char
         (width (- right (cua--rectangle-left)))
         (linestart (save-excursion ;; start point of the rectangle
                      ;;            ;; at that line
                      (move-to-column (cua--rectangle-left))
                      (max (1- (point))
                           (line-beginning-position))))
         (cnt 0)
         (result (catch 'found
                   (save-excursion
                     (while (> (point) regstart)
                       (if (funcall
                            (if brief-search-replace-using-regexp
                                #'search-backward-regexp
                              #'search-backward)
                            pattern
                            (max linestart
                                 (line-beginning-position)
                                 (- (point) width))
                            noerror 1)
                           (and (incf cnt)
                                (= cnt count)
                                (throw 'found (point)))
                         (if (= lastline
                                (progn (forward-line -1)
                                       (setq lastline (point))))
                             (throw 'found nil)) ; Top of file
                         (move-to-column right)
                         (setq linestart (- (point) width))))
                     nil))))
    (and result
         (progn
           (brief-keep-rectangle-unchanged)
           (goto-char result)))))

(defun brief-search-backward-pattern (pattern &optional noerror count)
  "Search backwards for a PATTERN.
The search is limited in the currently marked (rectangle) region.
Cursor will jump the match position only if the search is successful.
The cursor jump is only temporarily when searching in a (rectangle)
region.  To settle the cursor there, cancel the (rectangle) region."

  (let* ((point      (point))
         (is-rect    (brief-rectangle-active))
         (is-region  (or (brief-use-region)
                         ;; previous search was a region but happen to end up
                         ;; having region size reduced to zero after the search
                         ;; (the end point of region is exactly the end of the
                         ;; search pattern)
                         (and (cadr brief-last-search-region)
                              (eq (region-beginning) (region-end)))))
         (is-overlay (and brief-search-fake-region-mark
                          (overlayp brief--search-overlay)))
         (reg-start  (or (and is-overlay
                              (overlay-start brief--search-overlay))
                         (and is-rect
                              (cua--rectangle-top))
                         (and is-region
                              (region-beginning))))
         (reg-end    (or (and is-overlay
                              (overlay-end brief--search-overlay))
                         (and is-rect
                              (cua--rectangle-bot))
                         (and is-region
                              (region-end))))
         (new-search (and (or is-rect is-region)
                          (brief-is-search-command this-command)
                          (not (brief-is-search-command last-command))))
         (result     nil))

    (when new-search
      ;; Reset `brief-search-failed' for a new search
      (setq brief-search-failed nil)
      (add-hook 'pre-command-hook 'brief-reset-new-search))

    ;; Activate the search overlay for a region if it's not activated
    (and is-region
         (not is-rect)
         new-search
         brief-search-fake-region-mark
         (if brief--search-overlay
             (when (null (overlay-start brief--search-overlay))
               (move-overlay brief--search-overlay reg-start reg-end))
           ;; Create buffer specific overlay
           (setq brief--search-overlay
                 (make-overlay (point-min) (point-min)))
           (delete-overlay brief--search-overlay)
           (overlay-put brief--search-overlay 'face 'brief-fake-region-face)
           (move-overlay brief--search-overlay reg-start reg-end)))

    ;; Check if we're continuing a search within a region
    (if (brief-is-search-forward-command last-command)
        ;; Check if we're continuing a search within a region/rectangle
        (if (pop brief-last-search-region)
            ;; Previous search was in a rectangle
            ;; (is-rect| point count ignore)
            (progn
              ;;(setq count (1+ (or (cadr brief-last-search-region) 1)))
              (goto-char (pop brief-last-search-region))) ;; previous point
          (when (pop brief-last-search-region)
            ;; Previous search was in a region
            ;; (is-rect| is-region| beg end)
            (goto-char (pop brief-last-search-region)) ; BEG
            ;; previous backward search stopped at the beginning of the pattern
            (set-mark (point)) ; mark forwards
            (goto-char point))))

    ;; [2017-06-21 Wed] When a rectangle is at the same line, either REG-START
    ;; and REG-END, or [2017-06-27 Tue] REG-END and `cua--rect-start-position'
    ;; will be equal.  We therefore need to adjust REG-START otherwise the search
    ;; will fail.
    (if (and is-rect
             (or (= reg-start reg-end)
                 (= reg-end (cua--rect-start-position))))
        (save-excursion
          ;; rect at the same line
          (move-to-column (cua--rectangle-left))
          (setq reg-start (point))))

    (save-mark-and-excursion
      ;; When region active, search from the end of the region
      (if new-search
          ;; If new search begins, goto end of rectangle
          (if is-rect
              ;; If new search begins, goto end of rectangle
              (progn
                (goto-char (cua--rectangle-bot))
                ;; need to plus one or we'll miss the pattern if it's at the
                ;; end of rectangle
                (move-to-column (1+ (cua--rectangle-right))))
            (if is-region
                (unless (eq reg-end (point))
                  ;; We are marking backwards, change it forwards; or the
                  ;; region will end at the cursor and thus stop further
                  ;; region-searching.
                  ;;(push-mark (mark t) t t)
                  (cua-exchange-point-and-mark nil)))))

      (setq result
            (funcall (or (and (brief-rectangle-active)
                              #'brief-search-backward-rectangle-pattern)
                         (if brief-search-replace-using-regexp
                             #'search-backward-regexp
                           #'search-backward))
                     pattern
                     reg-start
                     noerror count))
      (setq point (point))) ;; save the point we find the pattern

    ;; If the initial search failed, restore the region right away
    ;; TODO: [2017-06-28 11:35:28 +0800]
    ;;     After we used `save-mark-and-excursion', do we still need this??
    ;;(when (and new-search
    ;;           is-region
    ;;           (not is-rect)
    ;;           (not result)
    ;;           ;; was not marking forwards
    ;;           (not (eq reg-end (point))))
    ;;  (pop-mark) ;; this will deactivate overlay as well
    ;;  (set-mark (mark t)))

    (setq brief-last-search-region
          (if is-rect
              (list is-rect point count reg-end)
            (list is-rect is-region reg-start reg-end)))
    (setq brief-last-search-begin (match-beginning 0)
          brief-last-search-end   (match-end 0))

    (and result
         (goto-char result)
         (progn
           ;;(and is-region
           ;;     (= (region-beginning)
           ;;        (region-end)) ;; region reduced to zero now
           ;;     (progn ;; prevent region keep extending on next search
           ;;       (setq brief-last-search-region nil)
           ;;       (deactivate-mark)))
           (ignore-errors (run-hook-with-args 'brief-after-search-hook result))
           result))))

;; added function 02/09/2006
(defun brief-search-backward (arg)
  "Search string/regexp backwards."
  (interactive
   (list
    (if (brief-multiple-cursor-in-action)
        brief-search-last
      (read-string "Search backwards: " nil 'brief-search-history nil))))
  ;;[2011-04-14 Thu 13:53] rem 1
  ;; (setq case-fold-search brief-global-case-fold-search)
  (if (and (string= "" arg) ;; no input, search last if exists
           (car brief-search-history))
      (setq arg (car brief-search-history)))
  (if (prog1 (brief-search-backward-pattern arg t)
        ;;(prog1 (setq brief-search-backward-last
        ;;             (car minibuffer-history)) ;; 04/03/2008 rem 1 ins 1
        (prog1 (setq brief-search-last arg)
          ;;(defalias 'brief-last-search-function 'brief-search-backward)
          (setq brief-last-search-action-forward nil)))
      (brief-search-complete)
    (brief-search-failed)
    nil))

;; added function 02/10/2006
(defun brief-search-backward-currword (arg)
  "Search backwards for the word that the cursor is focusing on."
  (interactive
   (list
    (let ((default (or (current-word) "")))
      (if (brief-multiple-cursor-in-action)
          (and (string= default "")
               default)
        (read-string "Search backwards: "
                     default 'brief-search-history default)))))
  ;;[2011-04-14 Thu 13:53] rem 1
  ;; (setq case-fold-search brief-global-case-fold-search)
  (if (and (string= "" arg) ;; no input, search last if exists
           (car brief-search-history))
      (setq arg (car brief-search-history)))
  (if (prog1 (brief-search-backward-pattern arg t)
        ;;(prog1 (setq brief-search-backward-last
        ;;             (car minibuffer-history)) ;; 04/03/2008 rem 1 ins 1
        (setq brief-search-last arg)
        ;;(defalias 'brief-last-search-function 'brief-search-backward-currword)
        (setq brief-last-search-action-forward nil))
      (brief-search-complete)
    (brief-search-failed)
    nil))

;; added 03/09/2006
(defun brief-repeat-search ()
  "Repeat the most recent forward/backward search command.
Notice that if the latest search command is prefixed with a prefix
command (\\[universal-argument]), it is also repeated."
  (interactive "")
  (let* ((current-prefix-arg current-prefix-arg))
    (if brief-search-last
        (if brief-last-search-action-forward
            (brief-search-forward brief-search-last)
          (brief-search-backward brief-search-last))
      (call-interactively (if brief-last-search-action-forward
                              'brief-search-forward
                            'brief-search-backward)))))

(defun brief-repeat-search-forward ()
  "Repeat the latest search operation but change direction forwards."
  (interactive "")
  (setq brief-last-search-action-forward t)
  (brief-repeat-search))

(defun brief-repeat-search-backward ()
  "Repeat the latest search operation but change direction backwards."
  (interactive "")
  (setq brief-last-search-action-forward nil)
  (brief-repeat-search))

;;
;; Query & Replace
;;

(defvar brief-query-replace-automatic-keys nil)
(defvar brief-query-replace-quit-keys nil)

(eval-after-load 'replace
  '(progn
     (map-keymap (lambda (ev bind)
                   (if (or (eq bind 'automatic)
                           (eq bind 'automatic-all))
                       (push ev brief-query-replace-automatic-keys)))
                 query-replace-map)
     (map-keymap (lambda (ev bind)
                   (if (eq bind 'exit)
                       (push ev brief-query-replace-quit-keys)))
                 query-replace-map)))

(defun brief-query-replace-rectangle (pattern to
                                             &optional _delimited start end)
  "Backward compatibility function for Emacs version < 25.1.
Replace string/regexp PATTERN in a rectangle."
  ;;(save-excursion ;; `apply-on-rectangle' already did this
  (let* ((message-list nil)
         (replace-count 0)
         (result
          (catch 'break
            (apply-on-rectangle
             (lambda (startcol _endcol pattern to)
               (move-to-column startcol)
               (let ((last-key (aref (this-command-keys)
                                     (1- (length (this-command-keys))))))
                 (if (memq last-key brief-query-replace-quit-keys)
                     (throw 'break nil)
                   (funcall (or
                             (and
                              (memq last-key
                                    brief-query-replace-automatic-keys)
                              (if brief-search-replace-using-regexp
                                  #'replace-regexp
                                #'replace-string)) ; No query, go through all lines
                             (if brief-search-replace-using-regexp
                                 #'query-replace-regexp
                               #'query-replace)) ;; Query one by one
                            pattern to nil
                            (point)
                            (min (line-end-position)
                                 ;; Not safe so comment it out, since cua
                                 ;; rectangle might not be compatible with
                                 ;; simple rect.
                                 ;;(+ 1 (point) (- endcol startcol)))))))
                                 (+ 1 (point) (- (cua--rectangle-right)
                                                 (cua--rectangle-left)))))
                   ;; For later calculating how many "Replaced #n occurrence(s)"
                   (push (current-message) message-list))))
             start end pattern to))))
    (dolist (m message-list)
      ;; TODO: if `preform-replace' changed the following text message,
      ;; the following need to be changed as well.
      ;; TODO: also parse the remaining "(skip ...)" in the message string
      (and (string-match "Replaced \\([0-9]+\\) occurrence" m)
           (setq replace-count (+ replace-count (string-to-number
                                                 (match-string 1 m))))))
    (message "Replaced %d occurrence%s"
             replace-count
             (if (= replace-count 1) "" "s"))
    result))

(defun brief-query-replace-pattern (pattern to-string)
  "Backward compatibility function for Emacs version < 25.1."
  (let ((reg-start (or (and (brief-rectangle-active)
                            (cua--rectangle-top))
                       (and (brief-use-region)
                            (region-beginning))))
        (reg-end   (or (and (brief-rectangle-active)
                            (cua--rectangle-bot))
                       (and (brief-use-region)
                            (region-end))))
        ;;(undo-len  (and brief-group-undo-replacement
        ;;                (length buffer-undo-list)))
        ;;undo-before
        )
    ;;(and
    (assert (and (equal pattern (car brief-query-replace-from-history))
                 (equal to-string (car brief-query-replace-to-history))))
    (funcall (or (and ;;(brief-use-region)
                  (brief-rectangle-active)
                  #'brief-query-replace-rectangle)
                 (if brief-search-replace-using-regexp
                     #'query-replace-regexp
                   #'query-replace))
             pattern
             to-string
             nil reg-start reg-end)
    ;;(brief-recenter)) ;; recenter after replacements is a bit weird so comment
    ;;                  ;; it out.
    ;;(when brief-group-undo-replacement
    ;;  (setq undo-before (last buffer-undo-list undo-len))
    ;;  (if (car undo-before) ;; delimit the original undo list with 'nil'
    ;;      (push nil undo-before))
    ;;  (setq buffer-undo-list
    ;;        (append ;; `nconc' seems to cause unknown problems here, some
    ;;                ;; pointers must be corrupted
    ;;         '(nil)
    ;;         (delq nil ;; remove the undo boundary in the undo-after list
    ;;               (butlast buffer-undo-list undo-len))
    ;;         undo-before)))
    ))

;;(defalias query-replace brief-query-replace)
(defun brief-query-replace (&optional pattern to)
  "Query and replace a PATTERN.

If a marked (rectangle) region is active, the search and replacement
will be restricted within the (rectangle) region.
The PATTERN could either be a regular expression, or a simple string,
depending on the internal variable `brief-search-replace-using-regexp'
toggled by `brief-toggle-search-replace-regexp' (\\[brief-toggle-search-replace-regexp])."
  (interactive "*")
  (let* ((point       (point))
         (is-rect     (brief-rectangle-active))
         ;;(use-empty-active-region nil)
         (is-region   (brief-use-region))
         (reg-start   (or (and is-rect
                               (cua--rectangle-top))
                          (and is-region
                               (region-beginning))))
         (reg-end     (or (and is-rect
                               (cua--rectangle-bot))
                          (and is-region
                               (region-end))))
         ;; Reference to `cua--deactivate-rectangle' for the format
         ;; of `cua--last-rectangle'
         (rect        (and is-rect
                           (cons (current-buffer)
                                 (cons (point)
                                       cua--rectangle))))
         ;;(backward    (< (prefix-numeric-value current-prefix-arg) 0))
         (undo-len    (and brief-group-undo-replacement
                           (length buffer-undo-list)))
         (undo-before nil)
         ;; query&replace might change these, if `undo' was done
         (last-command last-command)
         (this-command this-command)
         (brief-hold-overlay t)
         (justquit    nil)
         (lasterr     nil)
         (result      nil))

    (when is-region
      ;;(setq brief-last-query-replace-region (list (mark) point))
      (push-mark (mark t) t t)
      (and (not is-rect)
           brief-search-fake-region-mark
           (if brief--search-overlay
               (when (null (overlay-start brief--search-overlay))
                 (move-overlay brief--search-overlay reg-start reg-end))
             ;; Create buffer specific overlay
             (setq brief--search-overlay
                   (make-overlay (point-min) (point-min)))
             (delete-overlay brief--search-overlay)
             (overlay-put brief--search-overlay 'face 'brief-fake-region-face)
             (move-overlay brief--search-overlay reg-start reg-end))))

    (save-mark-and-excursion
      (condition-case err
          ;; Catch 'quit signal
          (progn
            (when is-rect
              ;; Always move to corner 3 first (bottom/right)
              ;; (refernece to `cua--rectangle-set-corners'
              (goto-char (cua--rectangle-top))
              (move-to-column (cua--rectangle-left))
              (set-mark (point))
              (setq reg-start (point))
              (goto-char (cua--rectangle-bot))
              ;; Need to plus one or we'll miss the pattern, if it's
              ;; right at the right-bottom end of the rectangle.
              ;; Is this `query-replace' bug when operating in rectangle?
              (move-to-column (1+ (cua--rectangle-right)))
              (setq reg-end (point)))
            (setq result
                  (if (not (version< emacs-version "25.1"))
                      ;; Emacs 25.1 and above
                      (if (and pattern to)
                          (funcall (if brief-search-replace-using-regexp
                                       #'query-replace-regexp
                                     #'query-replace)
                                   pattern to nil
                                   reg-start reg-end
                                   (> 0 (prefix-numeric-value
                                         current-prefix-arg))
                                   is-rect)
                        (call-interactively
                         (if brief-search-replace-using-regexp
                             'query-replace-regexp
                           'query-replace)))
                    ;; Emacs version below 25.1, no backward replacement
                    ;; supported here.
                    (let ((default
                            (car (symbol-value
                                  query-replace-from-history-variable))))
                      (unless
                          (or (and pattern to)
                              (string=
                               "" ;; no input
                               (read-string (format "Query replace: ")
                                            default
                                            query-replace-from-history-variable
                                            default)))
                        (setq default
                              (car (symbol-value
                                    query-replace-to-history-variable)))
                        (read-string
                         (format "Query replace %s with: "
                                 (car (symbol-value
                                       query-replace-from-history-variable)))
                         default
                         query-replace-to-history-variable
                         ;; If user input empty string, then he is trying to
                         ;; do query and delete.
                         ""))
                      (brief-query-replace-pattern
                       (car (symbol-value query-replace-from-history-variable))
                       (car (symbol-value query-replace-to-history-variable)))
                      ;; Remove the just-pushed empty string if in the
                      ;; `query-replace-from-history'
                      (if (string= ""
                                   (car (symbol-value
                                         query-replace-from-history-variable)))
                          (set query-replace-from-history-variable
                               (cdr (symbol-value
                                     query-replace-from-history-variable))))))))

        ;; Catch error and continue so that the overlay will be removed correctly
        (error      (setq lasterr (list 'error err)))
        (user-error (setq lasterr (list 'user-error err)))

        ;; Take care of 'quit signal
        ;; Quit signal will cancel the (rectangle) region operation and keep
        ;; cursor at the current position
        (quit (progn
                (setq justquit t
                      point (point))))))

    ;; Restore the (rectangle) region if not quit
    (when is-region
      (pop-mark)
      ;; This `pop-mark' won't deactivate the overlay as `brief-hold-overlay'
      ;; is still 't, in case we immediately reactivate the region which
      ;; cause the region background flickers (turn off and immediately on).

      (if justquit ;; quit?
          (goto-char point)
        ;; Reactivate mark if not quitting
        (set-mark (mark t)))

      ;; If we don't set `deactivate-mark' as nil in the end, we
      ;; will need this. Or if we did 'undo' during `query-replace',
      ;; we will also need to restore rectangle here.
      ;; To detect if we just 'undid', test if `this-command' is 'mode-exited.
      (when (and is-rect
                 (eq this-command 'mode-exited)) ;; undo?
        ;; Reactivate rectangle
        (setq cua--last-rectangle rect)
        (cua--activate-rectangle) ;;(cua-set-rectangle-mark t)
        (brief-keep-rectangle-unchanged))

      ;; Remove overlay after the region is re-activated, to prevent
      ;; region background flicker
      (setq brief-hold-overlay nil)
      (brief-delete-search-overlay))

    ;; Group undo records in a replacement, remove boundaries within the list
    (when brief-group-undo-replacement
      (setq undo-before (last buffer-undo-list undo-len))
      (if (car undo-before) ;; delimit the original undo list with 'nil'
          (push nil undo-before))
      (setq buffer-undo-list
            (append ;; `nconc' seems to cause unknown problems here, some
             ;;     ;; pointers must be corrupted
             '(nil)
             (delq nil ;; remove the undo boundary in the undo-after list
                   (butlast buffer-undo-list undo-len))
             undo-before)))

    ;; In case of any error or user error, signal it now
    (and lasterr
         (signal (car lasterr) (cdr lasterr)))

    ;; prevent existing mark from being deactivated
    (setq deactivate-mark nil)
    result))

(defun brief-query-replace-forward ()
  "Query and replace pattern forwards.
Perform `brief-query-replace' in forward direction."
  (interactive "*")
  (setq brief-last-query-replace-forward t)
  (call-interactively 'brief-query-replace))

(defun brief-query-replace-backward ()
  "Query and replace pattern backwards.
Perform `brief-query-replace' in backward direction."
  (interactive "*")
  (let ((current-prefix-arg '-))
    (setq brief-last-query-replace-forward nil)
    (brief-query-replace current-prefix-arg)))

(defun brief-query-prompt ()
  (concat "Query replace"
          (if (< (prefix-numeric-value current-prefix-arg) 0)
              " backward" " word")
          (if (brief-use-region) " in region" "")))

(defun brief-repeat-query-replace ()
  "Repeat the latest query and replacement operation."
  (interactive "*")
  (if (eq (symbol-value query-replace-from-history-variable) nil)
      ;; no previous 'from', make query for both 'from' and 'to'
      (call-interactively 'brief-query-replace)
    (if (eq (symbol-value query-replace-to-history-variable) nil)
        ;; no previous 'to', make query for 'to'
        (query-replace-read-to
         (car (symbol-value query-replace-from-history-variable))
         (brief-query-prompt) t))
    (brief-query-replace
     (car (symbol-value query-replace-from-history-variable))
     (car (symbol-value query-replace-to-history-variable)))
    ;;(setf deactivate-mark nil)
    ))

(defun brief-repeat-query-replace-forward ()
  "Repeat the latest query and replacement operation forwards."
  (interactive "*")
  (let ((current-prefix-arg nil))
    (setq brief-last-query-replace-forward t)
    (call-interactively 'brief-repeat-query-replace)))

(defun brief-repeat-query-replace-backward ()
  "Repeat the latest query and replacement operation backwards."
  (interactive "*")
  (let ((current-prefix-arg '-))
    (setq brief-last-query-replace-forward nil)
    (call-interactively 'brief-repeat-query-replace)))

(defun brief-query-replace-forward-currword ()
  "Query and replace current word in forward direction."
  (interactive "*")
  ;; Don't set `brief-last-query-replace-forward' as 't in this function as this
  ;; function is also called by `brief-query-replace-backward-currword'. Set
  ;; that variable by `brief-query-replace' function.
  (let ((default (current-word)))
    (if (or (null default)
            (string= "" default))
        (call-interactively 'brief-query-replace)
      ;; push current word into 'from' history if it's not in the head
      (unless (string= default
                       (car (symbol-value query-replace-from-history-variable)))
        (set query-replace-from-history-variable
             (cons default (symbol-value query-replace-from-history-variable))))
      (query-replace-read-to default (brief-query-prompt) t)
      (brief-query-replace
       (car (symbol-value query-replace-from-history-variable))
       (car (symbol-value query-replace-to-history-variable))))))

(defun brief-query-replace-backward-currword ()
  "Query and replace current word in backward direction."
  (interactive "*")
  (let ((current-prefix-arg '-))
    (call-interactively 'brief-query-replace-forward-currword)))

(defun brief-delete (arg)
  "Delete marked region/rectangle or delete a char."
  (interactive "*p")
  (if (brief-use-region)
      ;; <2011-06-02 Thu 15:24> Fix for Emacs 23
      (progn
        (if (brief-line-region-active) ;; for Emacs
            (call-interactively 'cua-delete-region)
          (if (fboundp 'cua-delete-rectangle) ;; it does not exist in Emacs 24.3
              (call-interactively 'cua-delete-rectangle)
            (call-interactively 'cua-delete-region)))
        (brief-restore-clipboard-selection))
    (brief-delete-char arg)))

(defun brief-forward-word (&optional arg)
  "New implementation of `forward-word' which won't be that greedy."
  (interactive "^p")
  (ignore-errors ;; prevent begin of buffer or end of buffer
    (if (< arg 0)
        (let ((currsyn (char-syntax (char-before)))
              (iswhite (char-syntax ?\t )))
          (ignore-errors
            (loop for i from arg to -1 by 1 do
                  (while (= (char-syntax (char-before)) iswhite)
                    (backward-char))
                  (while (= (char-syntax (char-before)) currsyn)
                    (backward-char)))))
      (let ((currsyn (char-syntax (char-after)))
            (iswhite (char-syntax ?\t )))
        (ignore-errors
          (loop for i downfrom (or arg 1) to 1 by 1 do
                (while (= (char-syntax (char-after)) currsyn)
                  (forward-char))
                (while (= (char-syntax (char-after)) iswhite)
                  (forward-char)))))))
  t)

(defun brief-backward-word (&optional arg)
  "New implementation of `backward-word' which is not as greedy."
  (interactive "^p")
  (brief-forward-word (- (or arg 1))))

;; <2011-06-14 Tue 16:51> modified from XEmacs simple.el 'kill-word'
(defun brief-delete-word (&optional count)
  "Delete characters forward until encountering the end of a word.
With optional argument COUNT, do this that many times."
  (interactive "*p")
  (brief-delete-region (point) (save-excursion
                                 (brief-forward-word count) (point))))

;; <2011-06-14 Tue 18:05> modified from XEmacs simple.el 'backward-kill-word'
(defun brief-backward-delete-word (&optional count)
  "Delete characters backward until encountering the end of a word.
With argument, do this that many times."
  (interactive "*p")
  (brief-delete-word (- (or count 1))))

(defun brief-delete-end-of-line ()
  "Delete characters till end of current (visual) line(s).

The deleted texts does not go into the kill-ring or clipboard.  If
that is expected use the cut-line command (\\[brief-kill-line]) instead.  Mark
the texts then do \\[brief-kill-line].

When `brief-linecmd-respect-visual' is non-nil, this command respect
visual mode, which means it delete texts till end of visual line,
unless prefixed with \\[universal-argument] which will delete texts till the end
of a physical line.

On the otherhand, when `brief-linecmd-respect-visual' is nil, this
command delete texts till the end of a physical line unless prefixed
with \\[universal-argument] which will delete texts till the end of visual line."
  (interactive "*")
  (brief-delete-region
   (point)
   (if (and (or (null truncate-lines)
                visual-line-mode)
            (brief-is-visual-operation))
       (progn (end-of-visual-line 1) (point))
     (progn (move-end-of-line 1)
            (point))))) ;; bypass the hidden texts ('...') if there is

;; [06/02/'08] rem 2 func, ins 1 func.
;; No more checking for is-cc-mode. Tab always refer to indent region when
;; region is active
;;(defun brief-is-cc-mode () ;; [07/11/2007] add
;;  (or (string= mode-name "C")
;;      (string= mode-name "C++")
;;      (string= mode-name "C++/l")))

(defun brief-indent-tab (arg)
  "Indent the region if marked, otherwise insert a normal TAB character.
When in minibuffer it will do completion unless prefixed with \\[universal-argument]."
  (interactive "*P")
  (if (and (window-minibuffer-p)
           (null arg))
      ;;(call-interactively 'minibuffer-complete)
      (completion-at-point)
    (if (brief-use-region)
        (progn
          (call-interactively 'indent-region)
          (brief-restore-clipboard-selection))
      (indent-for-tab-command))))

(defun brief-goto-xy (x y)  ;; [06/12/2008] ins 1 func
  (goto-char (point-min))
  ;; (goto-line y)      ;; <2011-06-09 Thu 14:56> for emacs, according to the
  (forward-line (1- y)) ;; help page of 'goto-line' : "not to use 'goto-line'
  ;;                    ;; directly"
  (move-to-column x))

;; TODO: rewrite this ugly function, remove `brief-goto-xy'
(defun brief-indent-buffer ()
  "Indent and untabify the whole buffer."
  (interactive "*")
  (let ((x (current-column))
        (y (line-number-at-pos))
        reg-min reg-min-x reg-min-y
        reg-max reg-max-x reg-max-y)
    (message "Indenting & untabifying ...")
    (if (brief-use-region)
        (progn
          (setq reg-min (region-beginning) reg-max (region-end))
          (goto-char reg-min)
          (setq reg-min-x (current-column) reg-min-y (line-number-at-pos))
          (goto-char reg-max)
          (setq reg-max-x (current-column) reg-max-y (line-number-at-pos))
          (indent-region reg-min reg-max nil)
          (brief-goto-xy reg-min-x reg-min-y)
          (setq reg-min (point))
          (brief-goto-xy reg-max-x reg-max-y)
          (setq reg-max (point))
          (untabify reg-min reg-max)
          (if (= y reg-min-y)
              (brief-goto-xy reg-max-x reg-max-y)
            (brief-goto-xy reg-min-x reg-min-y))
          (push-mark (point) t t))
      (indent-region (point-min) (point-max) nil)
      (untabify (point-min) (point-max)))
    (message ;; <2011-06-09 Thu 14:56> add done message
     "Indenting & untabifying ... done")
    (brief-goto-xy x y)))

;; ;; <2010-07-21 Wed 11:25> add
;; (defun is-gdb-doing-trace ()
;;   (if (boundp 'gdb-arrow-extent)
;;       (eval '(and (not (equal gdb-arrow-extent nil))
;;                   (equal (extent-object gdb-arrow-extent) (current-buffer))))
;;     nil))

;;
;; Brief mode macro commands
;;

(defun brief-define-macro ()
  "Start defining a keyboard macro. Press another (\\[brief-define-macro]) to end defining."
  (interactive)
  (if defining-kbd-macro
      (progn
        (call-interactively 'end-kbd-macro)
        (if brief--kbd-macro-seq
            (setq last-kbd-macro
                  (vconcat brief--kbd-macro-seq last-kbd-macro)))
        (setq brief--kbd-macro-seq nil))
    (if brief--kbd-macro-seq
        (message "Use <Shift>-<F7> to continue a paused macro")
      (call-interactively 'start-kbd-macro)
      (setq brief--kbd-macro-seq []))))

(defun brief-toggle-pause-kbd-macro ()
  "Toggle to pause/continue keyboard macro recording."
  (interactive)
  (if defining-kbd-macro
      (let ((inhibit-message t))
        (call-interactively 'end-kbd-macro)
        (setq inhibit-message nil)
        (message "Pause recording keyboard macro")
        (setq brief--kbd-macro-seq
              (vconcat brief--kbd-macro-seq last-kbd-macro)))
    (if brief--kbd-macro-seq
        (let ((inhibit-message t))
          (call-interactively 'start-kbd-macro)
          (setq inhibit-message nil)
          (message "Resume recording keyboard macro"))
      (error "Brief: not defining keyboard macro"))))

;; <2010-07-21 Wed 11:57> added
(defun brief-call-last-kbd-macro ()
  "Run the latest defined keyboard macro."
  (interactive)
  ;; (if (is-gdb-doing-trace)
  ;;     (progn (switch-to-buffer-other-window
  ;;             (eval 'current-gdb-buffer)) ;; <2010-07-22 Thu 11:23> ins 1
  ;;            (call-interactively 'gdb-next))
  (let ((oldlen (length buffer-undo-list))
        newhead item count)
    (call-interactively 'call-last-kbd-macro)
    ;; Trim off undo boundaries inserted in the last macro
    (setq count (- (length buffer-undo-list) oldlen))
    (while (> count 0)
      (setq item (pop buffer-undo-list))
      (or (null item)
          (push item newhead))
      (setq count (1- count)))
    (setq buffer-undo-list (append (nreverse newhead) buffer-undo-list))
    (or (car buffer-undo-list)
        (push nil buffer-undo-list))))

(defun brief-save-kbd-macro (arg)
  "Save last keyboard macro into a file."
  (interactive (or (and last-kbd-macro
                        (call-interactively
                         (lambda (filename)
                           (interactive
                            "GSave current keyboard macro to file : ")
                           (list filename))))
                   (error
                    "Keyboard macro not defined yet, press <f7> to define one.")))
  (if (eq t ;; is a directory, i.e. no filename inputed
          (car (file-attributes arg)))
      (error "No file name specified.")
    (with-temp-buffer
      (insert-kbd-macro (intern ""))
      (ignore-errors
        (write-file arg t)
        t))))

(defun brief-load-kbd-macro (arg)
  "Load keyboard macro from a file."
  (interactive "fLoading keyboard macro file : ")
  (save-current-buffer
    (and (find-file arg "*.kbm")
         (or (eval-buffer) t)
         (kill-buffer)
         (message (format "Keyboard macro file \"%s\" loaded" arg))
         (setq brief--kbd-macro-seq nil))))

;; [06/17/2008] commented out, no need, <alt>-<left> and <alt>-<right> servers
;; for left/right paren searching.
;; ;; added, modified from 'simple.el'
;; (defun brief-matching-open ()
;;   "Move cursor to the beginning of the sexp before point."
;;   (interactive)
;;   (and (> (point) (1+ (point-min)))
;;        blink-matching-paren
;;        ;; Verify an even number of quoting characters precede the close.
;;        (= 1 (logand 1 (- (point)
;;                          (save-excursion
;;                            (forward-char -1)
;;                            (skip-syntax-backward "/\\")
;;                            (point)))))
;;        (let* ((oldpos (point))
;;               (blinkpos)
;;               (mismatch))
;;          (save-restriction
;;            (if blink-matching-paren-distance
;;                (narrow-to-region
;;                 (max (point-min)
;;                      (- (point) blink-matching-paren-distance))
;;                 oldpos))
;;            (condition-case ()
;;                (let ((parse-sexp-ignore-comments
;;                       (and parse-sexp-ignore-comments
;;                            (not blink-matching-paren-dont-ignore-comments))))
;;                  (setq blinkpos (scan-sexps oldpos -1)))
;;              (error nil)))
;;          (and blinkpos
;;               (/= (char-syntax (char-after blinkpos))
;;                   ?\$)
;;               (setq mismatch
;;                     (or (null (matching-paren (char-after blinkpos)))
;;                         (/= (char-after (1- oldpos))
;;                             (matching-paren (char-after blinkpos))))))
;;          (if mismatch (setq blinkpos nil))
;;          (if blinkpos
;;              ;; Don't log messages about paren matching.
;;              (let () ;;(message-log-max) [07/11/2007] remove from varlist
;;                (goto-char blinkpos)
;;                (if (pos-visible-in-window-p)
;;                    (and blink-matching-paren-on-screen
;;                         (sit-for blink-matching-delay))
;;                  (goto-char blinkpos)))
;;            (cond (mismatch
;;                   (message "Mismatched parentheses"))
;;                  ((not blink-matching-paren-distance)
;;                   (message "Unmatched parenthesis")))))))

(defvar brief-last-last-command nil
  "The previous value of `last-command'.")

(defvar brief-last-3rd-command nil
  "The previous value of `brief-last-last-command'.")

(defun brief-home ()
  "Move the cursor (point) to the home (top) of a line, window or buffer.

When `visual-line-mode' is nil and `truncate-lines' is non-nil, the
first \\[brief-home] moves point to the beginning of current line.
The second consecutive \\[brief-home] moves point to top of the window.
The thrid consecutive \\[brief-home] moves point to the beginning of the buffer.

When `visual-line-mode' is non-nil or `truncate-lines' is nil, the first
\\[brief-home] goes to the beginning of visible line and the second \\[brief-home]
goes to the physical beginning of line.  Consecutive 3rd and 4th \\[brief-home]s
then goes to the top of screen and beginning of buffer."
  (interactive "^")
  (let ((home4  (eq brief-last-3rd-command  'brief-home))
        (home3  (eq brief-last-last-command 'brief-home))
        (home2  (eq last-command            'brief-home)))
    (cond
     ;; 4th press
     ((and home4 home3 home2)
      (goto-char (point-min)))
     ;; 3rd press
     ((and home3 home2)
      (if truncate-lines
          (goto-char (point-min))
        (let ((p1 (save-excursion (beginning-of-line) (point)))
              (p2 (window-start)))
          (goto-char (if (/= p1 p2)
                         (min p1 p2)
                       (point-min))))))
     ;; 2nd press
     (home2
      (if truncate-lines
          (move-to-window-line 0)
        (let ((p1 (save-excursion (beginning-of-line) (point)))
              (p2 (window-start)))
          (goto-char (if (/= (point) (max p1 p2))
                         (max p1 p2)
                       (min p1 p2)))))) ;; do 3rd press home
     ;; 1st press
     (t
      (beginning-of-visual-line))))

  (setq brief-last-3rd-command brief-last-last-command
        brief-last-last-command last-command))

;; ;; <2011-06-02 Thu 17:03> ins func
;; (defun brief-shift-home (arg)
;;   "\"Home\" the point, the way Brief would do it, with marker activated.
;; The first use moves point to beginning of the line.  Second
;; consecutive use moves point to beginning of the screen.  Third
;; consecutive use moves point to the beginning of the buffer."
;;   (interactive "p")
;;   (brief-set-mark-here-if-not-active)
;;   (call-interactively 'brief-home (list arg)))

(defun brief-window-end ()
  "Get the point of the window end."
  (let ((we (window-end)))
    (if (/= (point-max) we)
        (1- we)
      we)))

(defun brief-end-of-visual-line ()
  "Move to the visual end of current line."
  (let* ((p1 (save-excursion
               (end-of-visual-line)
               (point)))
         (c1 (char-after p1))
         (p2 (line-end-position))) ;; `end-of-line' of course is at crlf
    (goto-char (if (and (/= p1 p2)
                        ;; Check if we're at the abbreviated text '...'
                        (not (brief-is-crlf c1)))
                   (1- p1)
                 ;; `end-of-visual-line' on line wrapping will have cursor
                 ;; at the beginning of next line
                 p1))))

(defun brief-end ()
  "Move the cursor (point) to the end (bottom) of a line, window or buffer.

When `visual-line-mode' is nil and `truncate-lines' is non-nil, the
first \\[brief-end] moves point to end of current line.
The second consecutive \\[brief-end] moves point to bottom of the window.
The third consecutive \\[brief-end] moves point to the end of the buffer.

When `visual-line-mode' is non-nil or `truncate-lines' is nil, the first
\\[brief-end] goes to the end of visible line and the second \\[brief-end] then goes to
the physical end of line.  Consecutive 3rd and 4th \\[brief-end]s goes to the
bottom of screen and end of buffer."

  (interactive "^")
  (let ((end4  (eq brief-last-3rd-command  'brief-end))
        (end3  (eq brief-last-last-command 'brief-end))
        (end2  (eq last-command            'brief-end))
        p1 p2 c1)
    (cond
     ;; 4th press
     ((and end4 end3 end2)
      (goto-char (point-max)))
     ;; 3rd press
     ((and end3 end2)
      (if truncate-lines
          (goto-char (point-max))
        (setq p1 (save-excursion (move-end-of-line 1) (point))
              p2 (brief-window-end))
        (goto-char (if (/= p1 p2)
                       (max p1 p2)
                     (point-max)))))
     ;; 2rd press
     (end2
      (if truncate-lines
          ;;(goto-char (brief-window-end))
          (progn
            (move-to-window-line -1)
            (move-end-of-line 1))
        (setq p1 (save-excursion
                   ;; if we're at the abbreviated text '...' we will need to
                   ;; go beyond that.
                   (move-end-of-line 1) (point))
              p2 (brief-window-end))
        (goto-char (if (/= (point) (min p1 p2))
                       (min p1 p2)
                     (max p1 p2)))))
     ;; 1st press
     (t
      (if truncate-lines
          (move-end-of-line 1)
        (brief-end-of-visual-line)))))

  (setq brief-last-3rd-command brief-last-last-command
        brief-last-last-command last-command))

(defun brief-move-to-window-line-0 ()
  "Goto the first character of this window."
  (interactive "^")
  (move-to-window-line 0))

(defun brief-mark-move-to-window-line-0 ()
  (interactive) ;; <2011-06-02 Thu 17:40>
  (brief-set-mark-here-if-not-active)
  (move-to-window-line 0))

(defun brief-move-to-window-line-end ()
  (interactive "^")
  (move-to-window-line -1))

(defun brief-mark-move-to-window-line-end () ;; <2011-06-02 Thu 17:40>
  (interactive)
  (brief-set-mark-here-if-not-active)
  (move-to-window-line -1))

;;(defun brief-shift-end (arg)
;;  "\"End\" the point, the way Brief would do it, with marker activated.
;;The first use moves point to end of the line.  Second
;;consecutive use moves point to the end of the screen.  Third
;;consecutive use moves point to the end of the buffer."
;;  (interactive "p")
;;  (brief-set-mark-here-if-not-active)
;;  (call-interactively 'brief-end (list arg)))

(defun brief-is-unwanted-buffer (bufname)
  "Check if BUF is a hidden buffer or match any customized unwanted buffer."
  (or
   ;; Skip hidden buffers
   (and (>= (length bufname) 2)
        (string= (substring bufname 0 2) " *"))
   ;; Skip any customized buffer
   (member bufname  brief-skip-buffers)))

(defun brief-unbury-buffer ()
  "Go back one buffer in the global buffer list."
  (interactive)
  (let ((startbuf (car (buffer-list))))
    (while (progn ;; 09/12/2005 ins while loop
             (switch-to-buffer (car (last (buffer-list))))
             (and (not (eq startbuf (current-buffer)))
                  ;; prevent go back to the same buffer if no other
                  ;; valid buffer exists
                  (brief-is-unwanted-buffer (buffer-name)))))))

(defun brief-previous-buffer ()
  "In selected window switch to previous buffer.
Notice that this function will skip any buffer name that listed in
`brief-skip-buffers'.  When prefixed, it bury current buffer in a
global buffer list instead of a per-window/frame buffer list."
  (interactive)
  (let ((curr     (current-buffer))
        (fskipbuf (if current-prefix-arg
                      'bury-buffer
                    (or (and (fboundp 'switch-to-prev-buffer)
                             'switch-to-prev-buffer)
                        'previous-buffer))))
    (while (progn
             (call-interactively fskipbuf)
             (and (brief-is-unwanted-buffer (buffer-name))
                  ;; Break if loop back to original buffer
                  (not (eq curr (current-buffer))))))))

(defun brief-next-buffer ()
  "In selected window switch to next buffer.
Notice that this function will skip any buffer name that listed in
`brief-skip-buffers'.  When prefixed, it un-bury current buffer in a
global buffer list instead of a per-window/frame buffer list."
  (interactive)
  (if current-prefix-arg
      (call-interactively 'brief-unbury-buffer)
    (let ((curr (current-buffer)))
      (while (progn
               (call-interactively
                (or (and (fboundp 'switch-to-next-buffer)
                         'switch-to-next-buffer)
                    'next-buffer))
               (and (brief-is-unwanted-buffer (buffer-name))
                    ;; Break if loop back to original buffer
                    (not (eq curr (current-buffer)))))))))

(defun brief-color-message (text &optional color)
  "Same as `message' but with specified foreground COLOR.
COLOR could be either a string like \"red\" or an existing face
like 'font-lock-warning-face."
  (or color
      (setq color "white"))
  (message "%s"
           (propertize text 'face
                       (or (and (stringp color)
                                `(:foreground ,color))
                           color))))

(defun brief-meta-x-wrapper ()
  "Wrapper function to conditionally override the normal M-x bindings.
When `brief-override-meta-x' is non-nil, M-x will exit Emacs (the
normal Brief binding) and when it is nil M-x will run
`execute-extended-command' (the normal Emacs binding).

It also works for emacs client/server.  For editing with emacs-client,
When `brief-override-meta-x' it ends emacs-client editing.

Notice that the original `execute-extended-command' was moved to \\[execute-extended-command]
when M-x is overridden by Brief mode.
Also notice that the keybinding \\[save-buffers-kill-emacs] is a shortcut
key of `save-buffers-kill-emacs' to bypass all these checks."
  (interactive)
  (if brief-override-meta-x

      (if (bound-and-true-p server-process)

          ;; Emacs Client/Server mode editing

          (if (bound-and-true-p server-clients)
              ;; Client connected

              ;; <2011-09-20 Tue 10:40> modified from `server-edit' and
              ;; `server-switch-buffer' (server.el.gz) to deal with the
              ;; special case the invoking emacs-client without giving
              ;; a file.
              (if (or (not server-process)
                      (memq (process-status server-process) '(signal exit)))
                  (server-mode 1)

                (let ((next-buffer (server-done))
                      (rest server-clients))
                  (if next-buffer
                      (apply 'server-switch-buffer next-buffer)
                    (while (and rest (not next-buffer))
                      (let ((proc (car rest)))
                        ;; Only look at frameless clients, or those in the
                        ;; selected frame.
                        (when (or (not (process-get proc 'frame))
                                  (eq (process-get proc 'frame)
                                      (selected-frame)))
                          (setq next-buffer
                                (car (process-get proc 'buffers))))
                        (setq rest (cdr rest))))
                    (and next-buffer
                         (server-switch-buffer next-buffer))
                    (unless (or next-buffer (window-dedicated-p
                                             (selected-window)))
                      ;; just in case
                      (if (= 1 (length (frame-list)))
                          (when
                              (progn
                                (discard-input)
                                (and
                                 (y-or-n-p
                                  "No buffers remain to edit, quit server? ")
                                 (yes-or-no-p "Really quit server? ")))
                            (server-force-stop)
                            (kill-emacs))
                        (delete-frame (selected-frame) t))))))

            ;; No client connected
            (if (progn
                  (discard-input)
                  (and
                   (y-or-n-p
                    "Running as server but no client connected, exit? ")
                   (yes-or-no-p "Really quit server? ")))
                (save-buffers-kill-emacs)
              (message "Keep running server-mode")))

        ;; Normal Emacs exit path

        (if (or (not brief-query-exit-emacs)
                (let ((use-dialog-box nil))
                  (discard-input)
                  (brief-color-message
                   "Brief: really exit Emacs ? (y or n) "
                   'minibuffer-prompt)
                  (y-or-n-p "Brief: really exit Emacs ? ")))
            (save-buffers-kill-emacs)
          (message "Brief: cancel exiting Emacs")))

    ;; not `brief-override-meta-x'
    (call-interactively 'execute-extended-command)))

(defun brief-save-buffer (filename)
  "Save whole buffer or a region, a rectangle.
The interactive code part (read file name, check overwrite) is copied
from `write-file'."
  (interactive
   (list (if (brief-use-region)
             (read-file-name "Write region to file: " default-directory
                             (expand-file-name
                              (file-name-nondirectory (buffer-name))
                              default-directory)
                             nil nil))))
  (if (not (brief-use-region))
      (save-buffer)
    (or (null filename) (string-equal filename "")
        (progn
          ;; If arg is just a directory,
          ;; use the default file name, but in that directory.
          (if (file-directory-p filename)
              (setq filename (concat (file-name-as-directory filename)
                                     (file-name-nondirectory
                                      (or buffer-file-name (buffer-name))))))
          (and buffer-file-name
               (string-equal (expand-file-name filename)
                             (expand-file-name buffer-file-name))
               (error "Cannot overwrite current buffer file with a region."))
          (and (file-exists-p filename)
               ;; NS does its own confirm dialog.
               (not (and (eq (framep-on-display) 'ns)
                         (listp last-nonmenu-event)
                         use-dialog-box))
               (or (y-or-n-p
                    (format "File `%s' exists; overwrite? " filename))
                   (error "Canceled"))) ))
    (if (brief-rectangle-active)
        ;; Write rectangle line by line, append white spaces at the end
        ;; if not long enough
        (let ((regionstr "")
              width
              str)
          (apply-on-rectangle
           (lambda (startcol endcol)
             (setq endcol (cua--rectangle-right)
                   startcol (cua--rectangle-left)
                   width (+ 1 (- endcol startcol))
                   str (buffer-substring-no-properties
                        (min (line-end-position) (+ startcol (point)))
                        (min (line-end-position) (+ 1 endcol (point))))
                   regionstr (concat regionstr
                                     str
                                     (if (< (length str) width)
                                         (spaces-string
                                          (- width (length str))))
                                     "\n")))
           (region-beginning) (region-end))
          (write-region regionstr nil filename))
      ;; Write lines region
      (write-region (region-beginning) (region-end) filename))
    (deactivate-mark)))

;;
;; Miscellaneous, infrequently used commands
;;

(defun brief-scroll-up-one-line ()
  "Scroll one line up."
  (interactive)
  (scroll-up 1))

(defun brief-scroll-down-one-line ()
  "Scroll one line down."
  (interactive)
  (scroll-up -1))

;; Emacs v23 does not support this
;;(define-minor-mode brief-auto-backup-mode
;;  "Whether auto-backup is done globally."
;;  :global t
;;  :variable auto-save-default)

(defun brief-toggle-auto-backup ()
  "Toggle auto-backup on or off."
  (interactive)
  (message "Turn Emacs auto-backup %s"
           (if (setq auto-save-default (not auto-save-default))
               "ON" "OFF")))

(defun brief-end-of-window ()
  "Goto the last visible character of window."
  (interactive "^")
  (move-to-window-line -1)
  (brief-end-of-visual-line))

(defalias 'brief-beginning-of-file    #'beginning-of-buffer)
(defalias 'brief-end-of-file          #'end-of-buffer)

(defun brief-open-new-line-next ()
  "Open a new line right below the current line and go there.
Unlike [return] key, this command does not split current line."
  (interactive)
  (move-end-of-line 1)
  (newline))

;;==============================================================================
;;
;;; Brief mode key bindings
;;

(defmacro brief-key (key def)
  "Define key in `brief-global-mode-map'."
  `(define-key brief-global-mode-map ,key ,def))

;; Brief window commands: F1-<arrow> F2-<arrow> F3-<arrow> F4-<arrow>

;; Defining F1 keymap bindings
(brief-key                  [(f1)]  brief-prefix-F1)

(define-key brief-prefix-F1 [(up)]         'brief-switch-window-up)
(define-key brief-prefix-F1 [(down)]       'brief-switch-window-down)
(define-key brief-prefix-F1 [(left)]       'brief-switch-window-left)
(define-key brief-prefix-F1 [(right)]      'brief-switch-window-right)

;; Defining F2 keymap bindings
;; TODO: enlarge/shrink according to current window position and layouts
(brief-key                  [(f2)]  brief-prefix-F2)

(define-key brief-prefix-F2 [(down)]       'enlarge-window)
(define-key brief-prefix-F2 [(left)]       'shrink-window-horizontally)
(define-key brief-prefix-F2 [(right)]      'enlarge-window-horizontally)
(define-key brief-prefix-F2 [(up)]         'shrink-window)

;; Defining F3 keymap bindings

(brief-key                  [(f3)]  brief-prefix-F3)

(define-key brief-prefix-F3 [(down)]       'split-window-vertically)
(define-key brief-prefix-F3 [(up)]         'brief-split-window-up)
(define-key brief-prefix-F3 [(right)]      'split-window-horizontally)
(define-key brief-prefix-F3 [(left)]       'brief-split-window-left)

;; Defining F4 keymap bindings
(brief-key                  [(f4)]  brief-prefix-F4)

(define-key brief-prefix-F4 [(up)]         'brief-delete-window-up)
(define-key brief-prefix-F4 [(down)]       'brief-delete-window-down)
(define-key brief-prefix-F4 [(left)]       'brief-delete-window-left)
(define-key brief-prefix-F4 [(right)]      'brief-delete-window-right)


(brief-key [(control f4)]           'brief-delete-current-window)

;; Search commands

;; "shift"   key means "repeat"
;; "meta"    key means "backward"
;; "control" key means "forward/current"

(brief-key               [(f5)]     'brief-search-forward)
(brief-key          [(meta f5)]     'brief-search-backward)

(brief-key         [(shift f5)]     'brief-repeat-search)
(brief-key [(control shift f5)]     'brief-repeat-search-forward)
(brief-key    [(meta shift f5)]     'brief-repeat-search-backward)
;; 02/10/2005 ins 2, Search forward/backward, default current word
;; "control" key here means "current", as "f5" without "meta" already means
;; "forward"
(brief-key       [(control f5)]     'brief-search-forward-currword)
(brief-key  [(meta control f5)]     'brief-search-backward-currword)

(brief-key    [(control x)(f5)]     'brief-toggle-search-case-sensitivity)

;; Replace commands

;; 04/03/'08 rem 1 ins 1
(brief-key               [(f6)]     'brief-query-replace-forward)
(brief-key          [(meta f6)]     'brief-query-replace-backward)

(brief-key         [(shift f6)]     'brief-repeat-query-replace)
(brief-key [(control shift f6)]     'brief-repeat-query-replace-forward)
(brief-key    [(meta shift f6)]     'brief-repeat-query-replace-backward)

(brief-key       [(control f6)]     'brief-query-replace-forward-currword)
(brief-key  [(meta control f6)]     'brief-query-replace-backward-currword)

(brief-key    [(control x)(f6)]     'brief-toggle-search-replace-regexp)

(brief-key [(control s)]            'isearch-forward)
(brief-key [(meta s)]               'isearch-backward)

;; File/Buffer commands

(brief-key [(meta e)]               'brief-find-file)
(brief-key [(meta f)]               'brief-current-filename)

(brief-key [(f9)]                   'find-file)
(brief-key [(meta f9)]              'load-library)

(brief-key [(meta o)]               'write-file)
(brief-key [(meta r)]               'insert-file)

(brief-key [(f10)]                  'execute-extended-command)
(brief-key [(meta f10)]             'compile)
(brief-key [(shift f10)]            'menu-bar-open)
(brief-key [(control p)]            'brief-view-compilation-output)

(brief-key [(meta b)]               'brief-buffer-list-window)
;; added function brief-indent-tab
;; <2012-01-03 Tue 16:29> !!! This key-binding is too strong for Info mode !!!
;;   Info mode use "\t" instead of <tab>.
;;(brief-key [(tab)]                  'brief-indent-tab)
;; <2012-01-03 Tue 16:29> This weak binding will enable <TAB> in Info mode.
;; [2012-02-24 14:35:58 +0800] This seems to work only after Emacs 24,
;;  Emacs 23 won't work.
(brief-key "\t"                     'brief-indent-tab)
;; 06/02/2008 Original function can be replaced by 'ctrl-q' <tab>,
;;   change this key combination to brief-indent-buffer
(brief-key [(control meta tab)]     'brief-indent-buffer)
;; 06/21/2005 ins 1 08/10/2005 change to meta-f11 for Fedora4
(brief-key [(meta f11)]             'brief-buffer-read-only-toggle)
(brief-key [(meta p)]               'brief-print) ;; 04/15/'08 ins 1

;;(brief-key [(control ?-)]          'kill-buffer)
(brief-key [(control ?-)]           'brief-kill-current-buffer)
(brief-key [(control kp-subtract)]  'kill-buffer)

(brief-key [(meta ?-)]              'brief-previous-buffer)
(brief-key [(meta ?_)]              'brief-previous-buffer)

(brief-key [(meta n)]               'brief-next-buffer)
;; 2009/12/1 add since meta-n (next) is a so widely used key combination
;;   with meta-p (prev)
(brief-key [(meta ?+)]              'brief-next-buffer)
(brief-key [(meta ?=)]              'brief-next-buffer)

(brief-key [(meta w)]               'brief-save-buffer)

(brief-key [(meta x)]               'brief-meta-x-wrapper)
;;(brief-key [(control x)]            'save-buffers-kill-emacs) ;; Emacs prefix
(brief-key [(control meta shift x)] 'save-buffers-kill-emacs)
(brief-key [(control meta shift X)] 'save-buffers-kill-emacs)

(brief-key [(meta h)]               'help)

;; Macro commands

;; modify start-kbd-macro to brief-define-macro
(brief-key [(f7)]                   'brief-define-macro)
(brief-key [(shift f7)]             'brief-toggle-pause-kbd-macro)

(brief-key [(f8)]                   'brief-call-last-kbd-macro)

(brief-key [(meta f7)]              'brief-load-kbd-macro)
(brief-key [(meta f8)]              'brief-save-kbd-macro)

;; Region / Xselection(Clipboard) commands

(brief-key [(kp-add)]               'brief-copy-line)
(brief-key [(kp-subtract)]          'brief-kill-line)
;; just to cover all the bases (GNU Emacs, for instance)

;; <2011-06-10 Fri 14:35> for Emacs X clipboard
(brief-key [(insert)]               'brief-yank)
(brief-key [(insertchar)]           'brief-yank)

;; Preventing the need for a keypad; for notebooks and a lot of new keyboards
(brief-key [(control insert)]       'brief-copy-line)
(brief-key [(shift delete)]         'brief-kill-line)

;;
;; Defining meta-L keymaps
;;

(brief-key [(meta l)]               brief-prefix-meta-l)
(brief-key [(meta L)]               brief-prefix-meta-l)

;; marking upwards
(brief-meta-l-key up   [(up)])            ;; 'brief-mark-line-up-with-<up>
(brief-meta-l-key up   [(left)])          ;; 'brief-mark-line-up-with-<left>
(brief-meta-l-key up   [(home)])          ;; 'brief-mark-line-up-with-<home>
(brief-meta-l-key up   [(prior)])         ;; 'brief-mark-line-up-with-<prior>

(brief-meta-l-key up   [(control up)])    ;; 'brief-mark-line-up-with-<C-up>
(brief-meta-l-key up   [(control left)])  ;; 'brief-mark-line-up-with-<C-left>
(brief-meta-l-key up   [(control home)])  ;; 'brief-mark-line-up-with-<C-home>
(brief-meta-l-key up   [(control prior)]) ;; 'brief-mark-line-up-with-<C-prior>

;; marking downwards
(brief-meta-l-key down [(down)])          ;; 'brief-mark-line-down-with-<down>
(brief-meta-l-key down [(right)])         ;; 'brief-mark-line-down-with-<right>
(brief-meta-l-key down [(end)])           ;; 'brief-mark-line-down-with-<end>
(brief-meta-l-key down [(next)])          ;; 'brief-mark-line-down-with-<next>

(brief-meta-l-key down [(control down)])  ;; 'brief-mark-line-down-with-<C-down>
(brief-meta-l-key down [(control right)]) ;; 'brief-mark-line-down-with-<C-right>
(brief-meta-l-key down [(control end)])   ;; 'brief-mark-line-down-with-<C-end>
(brief-meta-l-key down [(control next)])  ;; 'brief-mark-line-down-with-<C-next>

;; 04/28/2008 ins
(define-key brief-prefix-meta-l [(meta l)]   'brief-mark-line-down-with-meta-l)

(brief-key [(meta m)]               'set-mark-command)

;; Rectangle open/close/clear
(brief-key [(meta c)]               'cua-set-rectangle-mark)

(brief-key [(meta u)]               'brief-undo)
;;(brief-key [(control u)]            'undo) ;; a prefix key
(brief-key [(kp-multiply)]          'brief-undo)

;; General editing commands

(brief-key [(meta d)]               'brief-delete-entire-line)
(brief-key [(meta k)]               'brief-delete-end-of-line)

;;(brief-key [(control ?])]          'brief-matching-open)

;; <2011-06-30 Thu 15:04> rename 'kill' to 'delete'
;; <2012-10-31 Wed 16:00> remove brief-delete-word due to the brief-forward-word
;;   fix that it's now identical to brief-delete-word
(brief-key [(meta backspace)]       'brief-delete-word)
;; <2011-06-14 Tue 18:06> Add brief-backward-delete-word
(brief-key [(control backspace)]    'brief-backward-delete-word)

(brief-key [(delete)]               'brief-delete)
(brief-key [(shift backspace)]      'backward-kill-word)

(brief-key [(meta i)]               'overwrite-mode)

;; Bookmark commands

(brief-key [(meta ?0)]              'brief-bookmark-jump-set-0)
(brief-key [(meta ?1)]              'brief-bookmark-jump-set-1)
(brief-key [(meta ?2)]              'brief-bookmark-jump-set-2)
(brief-key [(meta ?3)]              'brief-bookmark-jump-set-3)
(brief-key [(meta ?4)]              'brief-bookmark-jump-set-4)
(brief-key [(meta ?5)]              'brief-bookmark-jump-set-5)
(brief-key [(meta ?6)]              'brief-bookmark-jump-set-6)
(brief-key [(meta ?7)]              'brief-bookmark-jump-set-7)
(brief-key [(meta ?8)]              'brief-bookmark-jump-set-8)
(brief-key [(meta ?9)]              'brief-bookmark-jump-set-9)

(brief-key [(meta j)]               'brief-bookmark-set-jump)

;; Cursor commands

(brief-key [(meta left)]            'backward-sexp)
(brief-key [(meta right)]           'forward-sexp)

(brief-key [(control left)]         'brief-backward-word)
(brief-key [(control right)]        'brief-forward-word)

(brief-key [(home)]                 'brief-home)
(brief-key [(control home)]         'brief-move-to-window-line-0)
(brief-key [(control shift home)]   'brief-mark-move-to-window-line-0)

(brief-key [(meta home)]            'beginning-of-line)
(brief-key [(end)]                  'brief-end)
(brief-key [(control end)]          'brief-move-to-window-line-end)
(brief-key [(control shift end)]    'brief-mark-move-to-window-line-end)
(brief-key [(meta end)]             'end-of-line)

(brief-key [prior]                  'brief-fixed-cursor-page-up)
(brief-key [next]                   'brief-fixed-cursor-page-down)

(brief-key [(meta up)]              'brief-previous-physical-line)
(brief-key [(meta down)]            'brief-next-physical-line)

(brief-key [(meta g)]               'goto-line)

;;(brief-key [up]                     'brief-previous-line)
;;(brief-key [down]                   'brief-next-line)
(brief-key [remap previous-line]    'brief-previous-line)
(brief-key [remap next-line]        'brief-next-line)

(brief-key [(control shift l)]      'brief-recenter-left-right)

;;
;; Miscellaneous infrequently used Brief keys
;;

(defun brief-enable-less-frequent-keys ()
  (when brief-enable-less-frequent-keys
    ;; Change Emacs native key binding for infrequently used commands

    (brief-key [(meta v)]               'brief-version)
    (define-key cua--cua-keys-keymap [(meta v)] 'brief-version)

    (brief-key [(control e)]            'brief-scroll-up-one-line)
    (brief-key [(control d)]            'brief-scroll-down-one-line)

    (brief-key [(control w)]            'brief-toggle-auto-backup)

    (brief-key [(control prior)]        'brief-beginning-of-file)
    (brief-key [(control next)]         'brief-end-of-file)
    ;;(brief-meta-l-key up   [(control prior)]) ;;already defined
    ;;(brief-meta-l-key down [(control next)])  ;;already defined

    (brief-key [(meta home)]            'brief-move-to-window-line-0)
    (brief-key [(meta end)]             'brief-end-of-window)
    (brief-meta-l-key up   [(meta home)])     ; brief-mark-line-up-with-<M-home>
    (brief-meta-l-key down [(meta end)])      ; brief-mark-line-down-with-<M-end>

    (brief-key [(control return)]       'brief-open-new-line-next)
    (define-key cua-global-keymap [(control return)] 'brief-open-new-line-next)

    (brief-key [(meta z)]               'eshell)))

;;==============================================================================
;;
;; Brief mode definitions
;;

(defvar brief--prev-brief-mode nil
  "Brief internal variable to store previous brief mode's status.")

(defun brief-mode-setter (arg)
  "Set function for the global `brief-mode' variable.
Also set internal variable `brief--prev-brief-mode'."
  (setq brief--prev-brief-mode brief-mode
        brief-mode arg))

;; If our global map is named as `brief-mode-map' instead of the current
;; `brief-global-mode-map'; we will need to define an empty mode map to
;; override its minor-mode association as `define-minor-mode' by default
;; automatically finds symbol named "brief-mode-map" if it exists and is
;; a keymap.
;;(defvar brief-local-map nil
;;  "Brief dummy empty local map.")

(defvar brief-backup-select-enable-clipboard
  (and (boundp 'select-enable-clipboard) select-enable-clipboard)
  "(Backup variable for Win32/Win64 only)")

(defvar brief-backup-select-enable-primary
  (and (boundp 'select-enable-primary) select-enable-primary)
  "(Backup variable for Win32/Win64 only)")

(defvar brief-backup-cua-mode cua-mode
  "Backup variable for `cua-mode', before enabling Brief mode.")

(defvar brief-backup-scroll-bar-mode scroll-bar-mode
  "Backup variable for `scroll-bar-mode', before enabling Brief mode.")

;;;###autoload
(define-minor-mode brief-mode
  "Enable/Disable/Toggle Brief emulation mode.
With a prefix argument ARG, enable Brief mode if ARG is positive,
and disable it otherwise.  If called from Lisp, enable the mode
if ARG is omitted or nil.  If called interactively without ARG,
toggle brief-mode."
  ;;:keymap brief-local-map
  ;;:keymap brief-global-mode-map       ; we can't use `brief-global-mode-map'
                                        ; here otherwise it will be enabled
                                        ; as a minor-mode keymap and will get
                                        ; searched ahead of `special-mode's
                                        ; like `ibuffer-mode', `help-mode'
                                        ; , `info-mode' ... etc.  Keys like
                                        ; <enter>, <tab> or 'q' defined in
                                        ; those special modes will fail since
                                        ; `brief-global-mode-map' is searched
                                        ; first.
  :lighter brief-mode-mode-line-string
  ;;
  ;; Enabling either of the following (:variable, :global) will cause problems
  ;; ("M-: brief-mode <enter>" will fail on <enter> key, guess it's because we're
  ;; using `use-global-map'.  However, without them `brief-mode' will be buffer
  ;; local and will not showing 't even if `brief-mode' is currently on.  Check
  ;; `global-brief-mode' instead to make sure brief mode is ON or OFF.
  ;; [2017-06-07 18:09:47 +0800] After removed the `use-global-map' these two
  ;; variables becomes consistent; however, it seems less efficient since restoring
  ;; a session will invoke `brief-mode' fucntion for each buffer. However, this
  ;; could be a `session-restore' issue since it does not differentiate global
  ;; mode from local modes.
  ;; [2017-06-13 11:34:47 +0800] The above mentioned problem is because of the
  ;; :keymap.  When we defined a keymap with `brief-mode-map', `define-minor-mode'
  ;; will associate this minor mode (no matter it's named `global-brief-mode' or
  ;; `brief-mode') to that keymap in the `minor-mode-alist'.  When the mode variable
  ;; is ON, the keymap in the active `minor-mode-alist' is searched when translating
  ;; a key.  This will make those keys in `special-mode' failed (`ibuffer-mode',
  ;; `help-mode', and `info-mode', minibuffer... ). As `special-mode' is also a
  ;; minor mode but not presented in the `minor-mode-alist' so our keymap take
  ;; precedence and fail `special-mode' keys.
  ;;:variable global-brief-mode
  ;;:global t

  ;; Define my global `brief-mode' variable and a setter function to prevent
  ;; `define-minor-mode' define a buffer-local `brief-mode' associated with
  ;; this minor-mode.
  :variable (brief-mode . brief-mode-setter)

  (unless (eq brief--prev-brief-mode brief-mode)
    (if (not brief-mode)
        ;;
        ;; Disable brief mode
        ;;
        (progn
          ;; restore cua-mode
          (cua-mode (if brief-backup-cua-mode 1 -1))
          ;; restore scroll-bar-mode if customized to do so
          (if brief-turn-off-scroll-bar-mode
              (scroll-bar-mode (if brief-backup-scroll-bar-mode 1 -1)))

          (if (and (brief-is-winnt)
                   (boundp 'select-enable-clipboard))
              ;; Respect user's original selection, whatever reason it was
              (setq select-enable-clipboard brief-backup-select-enable-clipboard
                    select-enable-primary   brief-backup-select-enable-primary))
          ;; These gui-get/set-selection changes are also required for
          ;; win32/win64 systems
          (if (fboundp 'advice-add)
              (if brief-selection-op-legacy
                  (progn
                    (advice-remove 'x-get-selection 'brief-gui-get-selection)
                    (advice-remove 'x-set-selection 'brief-gui-set-selection)
                    (advice-remove 'x-select-text   'brief-gui-select-text))
                ;; newer emacs
                (advice-remove 'gui-get-selection 'brief-gui-get-selection)
                (advice-remove 'gui-set-selection 'brief-gui-set-selection)
                (advice-remove 'gui-select-text   'brief-gui-select-text))

            (if brief-selection-op-legacy
                (progn
                  ;; older emacs don't have gui-get/set-selection
                  (ad-disable-advice 'x-get-selection
                                     'around 'brief-advice-gui-get-selection)
                  (ad-disable-advice 'x-set-selection
                                     'around 'brief-advice-gui-set-selection)
                  (ad-activate 'x-get-selection)
                  (ad-activate 'x-set-selection))
              ;; `advice-add' not present, but `gui-get-selection' exists,
              ;; this combination is not likely to happen, just in case.
              (ad-disable-advice 'gui-get-selection
                                 'around 'brief-advice-gui-get-selection)
              (ad-disable-advice 'gui-set-selection
                                 'around 'brief-advice-gui-set-selection)
              (ad-activate 'gui-get-selection)
              (ad-activate 'gui-set-selection)))

          (if (fboundp 'advice-add)
              (progn
                (advice-remove 'cua--rectangle-post-command
                               'brief-cua--rectangle-post-command)
                (advice-remove 'cua-cancel
                               'brief-reset-for-command-cancellation)
                (advice-remove 'keyboard-quit
                               'brief-reset-for-keyboard-quit)
                (advice-remove 'keyboard-escape-quit
                               'brief-reset-for-command-cancellation)
                (advice-remove 'cua-clear-rectangle-mark
                               'brief-reset-for-command-cancellation)
                (advice-remove 'cua-close-rectangle
                               'brief-reset-for-command-cancellation))
            (when (fboundp 'cua--rectangle-post-command)
              (ad-disable-advice 'cua--rectangle-post-command
                                 'before 'brief-cua--rectangle-post-command)
              (ad-activate 'cua--rectangle-post-command))
            (ad-disable-advice 'cua-cancel
                               'before 'brief-cua-cancel)
            (ad-activate 'cua-cancel)
            (ad-disable-advice 'keyboard-quit
                               'before 'brief-keyboard-quit)
            (ad-activate 'keyboard-quit)
            (ad-disable-advice 'keyboard-escape-quit
                               'before 'brief-keyboard-escape-quit)
            (ad-activate 'keyboard-escape-quit)
            (when (fboundp 'cua-clear-rectangle-mark)
              (ad-disable-advice 'cua-clear-rectangle-mark
                                 'before 'brief-clear-rectangle-mark)
              (ad-activate 'cua-clear-rectangle-mark))
            (when (fboundp 'cua-close-rectangle)
              (ad-disable-advice 'cua-close-rectangle
                                 'before 'brief-close-rectangle)
              (ad-activate 'cua-close-rectangle)))

          (remove-hook 'activate-mark-hook
                       'brief-region-backup-clipboard-selection)
          (remove-hook 'activate-mark-hook
                       'brief-postpone-gui-set-selection)
          (remove-hook 'deactivate-mark-hook
                       'brief-resume-gui-set-selection)
          (remove-hook 'before-change-functions
                       'brief-trim-fast-line-number-list)
          (remove-hook 'deactivate-mark-hook
                       'brief-delete-search-overlay)

          (use-global-map brief-mode-original-keymap)

          (setq query-replace-from-history-variable
                brief-orig-query-replace-from-history-variable)
          (setq query-replace-to-history-variable
                brief-orig-query-replace-to-history-variable)

          (if (version< emacs-version "24.0")
              (setq brief--prev-brief-mode nil)))
      ;;
      ;; Enable brief mode
      ;;

      ;; Need to enable `cua-mode' for rectangle operation
      (setq brief-backup-cua-mode cua-mode)
      (cua-mode 1)
      ;; Disable `scroll-bar-mode' if customized to do so
      (when brief-turn-off-scroll-bar-mode
        (setq brief-backup-scroll-bar-mode scroll-bar-mode)
        (scroll-bar-mode -1))

      (if brief-enable-postpone-selection
          (brief-enable-clipboard-postponement))

      (if (and (brief-is-winnt)
               (boundp 'select-enable-clipboard))
          ;; On Windows only 'CLIPBOARD works, 'PRIMARY does not work
          (setq select-enable-clipboard t
                select-enable-primary   nil))
      (if (fboundp 'advice-add)
          ;; Emacs >= 24
          (if brief-selection-op-legacy
              (progn
                (advice-add 'x-get-selection
                            :around   'brief-gui-get-selection)
                (advice-add 'x-set-selection
                            :around   'brief-gui-set-selection)
                (advice-add 'x-select-text
                            :override 'brief-gui-select-text))
            ;; Emacs >= 25
            (advice-add 'gui-get-selection
                        :around   'brief-gui-get-selection)
            (advice-add 'gui-set-selection
                        :around   'brief-gui-set-selection)
            (advice-add 'gui-select-text
                        :override 'brief-gui-select-text))
        (if brief-selection-op-legacy
            ;; Older emacs
            (progn
              (ad-enable-advice 'x-get-selection
                                'around 'brief-advice-gui-get-selection)
              (ad-enable-advice 'x-set-selection
                                'around 'brief-advice-gui-set-selection)
              (ad-activate 'x-get-selection)
              (ad-activate 'x-set-selection))
          ;; `advice-add' not present, but `gui-get-selection' exists
          ;; not likely to happen, just in case.
          (ad-enable-advice 'gui-get-selection
                            'around 'brief-advice-gui-get-selection)
          (ad-enable-advice 'gui-set-selection
                            'around 'brief-advice-gui-set-selection)
          (ad-activate 'gui-get-selection)
          (ad-activate 'gui-set-selection)))

      (if (fboundp 'advice-add)
          (progn
            (advice-add 'cua--rectangle-post-command
                        :before   'brief-cua-rectangle-undo-helper)
            (advice-add 'cua-cancel
                        :before   'brief-reset-for-command-cancellation)
            (advice-add 'keyboard-quit
                        :before   'brief-reset-for-keyboard-quit)
            (advice-add 'keyboard-escape-quit
                        :before   'brief-reset-for-command-cancellation)
            (advice-add 'cua-clear-rectangle-mark
                        :before   'brief-reset-for-command-cancellation)
            (advice-add 'cua-close-rectangle
                        :before   'brief-reset-for-command-cancellation))
        (when (fboundp 'cua--rectangle-post-command)
          (ad-enable-advice 'cua--rectangle-post-command
                            'before 'brief-cua--rectangle-post-command)
          (ad-activate 'cua--rectangle-post-command))
        (ad-enable-advice 'cua-cancel
                          'before 'brief-cua-cancel)
        (ad-activate 'cua-cancel)
        (ad-enable-advice 'keyboard-quit
                          'before 'brief-keyboard-quit)
        (ad-activate 'keyboard-quit)
        (ad-enable-advice 'keyboard-escape-quit
                          'before 'brief-keyboard-escape-quit)
        (ad-activate 'keyboard-escape-quit)
        (when (fboundp 'cua-clear-rectangle-mark)
          (ad-enable-advice 'cua-clear-rectangle-mark
                            'before 'brief-clear-rectangle-mark)
          (ad-activate 'cua-clear-rectangle-mark))
        (when (fboundp 'cua-close-rectangle)
          (ad-enable-advice 'cua-close-rectangle
                            'before 'brief-close-rectangle)
          (ad-activate 'cua-close-rectangle)))

      ;; This will sometimes make `wg-restore-window' fails. I modified
      ;; `wg-restore-window' and add a advice function in init.el to prevent this
      (add-hook 'activate-mark-hook
                'brief-region-backup-clipboard-selection)
      (when brief-enable-postpone-selection
        (add-hook 'activate-mark-hook
                  'brief-postpone-gui-set-selection 't)
        (add-hook 'deactivate-mark-hook
                  'brief-resume-gui-set-selection))
      (add-hook 'deactivate-mark-hook
                'brief-delete-search-overlay)

      (use-global-map brief-global-mode-map)

      (brief-enable-less-frequent-keys)

      ;; Force `transient-mark-mode' in Emacs, so that the marking routines work
      ;; as expected.  If the user turns off transient mark mode, most things
      ;; will still work fine but some region/rectangle marking functions won't
      ;; work quite as nicely.

      (if (fboundp 'transient-mark-mode)
          (transient-mark-mode 1))

      (if brief-load-scroll-lock
          (require 'scroll-lock))
      (if (featurep 'scroll-lock)
          (define-key brief-global-mode-map [(Scroll_Lock)] 'scroll-lock-mode))

      (add-hook 'before-change-functions 'brief-trim-fast-line-number-list)
      ;; ;; On Windows we can only interact with CLIPBOARD but not PRIMARY
      ;; (if (eq window-system 'w32)
      ;;     (setq brief-X-selection-target 'CLIPBOARD))
      ;; (run-hooks 'brief-load-hook)
      ;; (message "@@ Brief mode enabled")
      (setq brief-orig-query-replace-from-history-variable
            query-replace-from-history-variable)
      (setq brief-orig-query-replace-to-history-variable
            query-replace-to-history-variable)
      (setq query-replace-from-history-variable
            'brief-query-replace-from-history)
      (setq query-replace-to-history-variable
            'brief-query-replace-to-history)

      ;; Search for Xclipboard helper
      (brief-xclipboard-cmd-search)

      (and brief-override-meta-x
           brief-warn-meta-x-moved
           (brief-color-message
            "Brief: The original <M-x> key now moved to function key <f10>."
            'minibuffer-prompt))
      (if (version< emacs-version "24.0")
          (setq brief--prev-brief-mode t))))

  ;; calibrate current system UI performance
  (call-interactively 'brief-calibration))

;; Interaction with other packages.

(eval-after-load 'cua-base
  '(progn
     (put 'brief-home 'CUA 'move)
     (put 'brief-end  'CUA 'move)
     ;; [2016-04-01 18:20:22 +0800] Fix CUA C-v behavior which is not consistent
     ;; with my brief-yank All CUA keymaps starts from `cua--keymap-alist', which
     ;; exists in `emulation-mode-map-alists'. In `cua--keymap-alist', the
     ;; `cua--cua-keys-keymap' was listed near the head so it got higher priority.

     (when (and (fboundp 'cua-paste)
                (boundp 'cua--cua-keys-keymap))
       (define-key cua--cua-keys-keymap [remap yank] 'brief-yank)
       (define-key cua--cua-keys-keymap [(control v)] 'brief-yank))))

;; Support multiple-cursors package

(eval-after-load 'multiple-cursors
  '(progn
     (message "Brief mode adjusted for multiple-cursors package.")
     (defvar mc/cmds-to-run-for-all)
     (defvar mc/cmds-to-run-once)
     ;; Check if `brief-previous-clipboard-selection' is listed
     ;; in `mc/cursor-specific-vars', if not, add it in.
     ;; This make copy&paste works under multiple-cursors mode.
     (if (and (boundp 'mc/cursor-specific-vars)
              (not (member 'brief-previous-clipboard-selection
                           mc/cursor-specific-vars)))
         (push 'brief-previous-clipboard-selection
               mc/cursor-specific-vars))

     ;; Setup `mc/cmds-to-run-for-all' and `mc/cmds-to-run-once'
     ;; for all Brief keys binding commands.
     (when brief-init-multi-cursor-cmd
       (message
"Brief mode initialized run-all and run-once commands for multiple-cursors.")
       ;; Run for all
       (delete-dups
        (nconc mc/cmds-to-run-for-all
               '(brief-backward-delete-word
                 brief-backward-word
                 brief-copy-line
                 brief-delete
                 brief-delete-word
                 brief-end
                 brief-forward-word
                 brief-home
                 brief-next-line
                 brief-previous-line
                 brief-undo
                 brief-yank
                 brief-delete-end-of-line
                 brief-delete-entire-line
                 brief-indent-tab
                 ;;brief-shift-tab
                 brief-kill-line
                 brief-query-replace
                 brief-query-replace-forward
                 brief-query-replace-backward
                 brief-repeat-query-replace
                 brief-repeat-query-replace-forward
                 brief-repeat-query-replace-backward
                 brief-query-replace-forward-currword
                 brief-query-replace-backward-currword
                 brief-search-backward
                 brief-search-backward-currword
                 brief-repeat-search-backward
                 brief-search-forward
                 brief-search-forward-currword
                 brief-repeat-search-forward
                 brief-fixed-cursor-page-up
                 brief-fixed-cursor-page-down
                 brief-call-last-kbd-macro
                 brief-open-new-line-next
                 brief-mark-line-up-with-<up>
                 brief-mark-line-up-with-<left>
                 brief-mark-line-up-with-<home>
                 brief-mark-line-up-with-<prior>
                 brief-mark-line-down-with-<down>
                 brief-mark-line-down-with-<right>
                 brief-mark-line-down-with-<end>
                 brief-mark-line-down-with-<next>
                 brief-mark-line-up-with-<C-up>
                 brief-mark-line-up-with-<C-left>
                 brief-mark-line-down-with-<C-down>
                 brief-mark-line-down-with-<C-right>)))

       ;; Run once
       (delete-dups
        (nconc mc/cmds-to-run-once
               '(brief-buffer-list-window
                 brief-save-buffer
                 brief-indent-buffer
                 brief-meta-x-wrapper
                 brief-find-file
                 brief-current-filename
                 brief-move-to-window-line-0
                 brief-mark-move-to-window-line-0
                 brief-move-to-window-line-end
                 brief-mark-move-to-window-line-end
                 brief-kill-current-buffer
                 brief-buffer-read-only-toggle
                 brief-toggle-search-case-sensitivity
                 brief-toggle-search-replace-regexp
                 brief-print
                 brief-search-again
                 brief-search-forward-currword
                 brief-switch-window-up
                 brief-switch-window-down
                 brief-switch-window-left
                 brief-switch-window-right
                 brief-split-window-up
                 brief-split-window-left
                 brief-delete-window-up
                 brief-delete-window-down
                 brief-delete-window-left
                 brief-delete-window-right
                 brief-delete-current-window
                 brief-unbury-buffer
                 brief-previous-buffer
                 brief-next-buffer
                 brief-bookmark-set-jump
                 brief-bookmark-jump-set
                 brief-recenter-left-right
                 brief-load-kbd-macro
                 brief-save-kbd-macro
                 brief-toggle-pause-kbd-macro
                 brief-scroll-up-one-line
                 brief-scroll-down-one-line
                 brief-toggle-auto-backup
                 brief-beginning-of-file
                 brief-end-of-file
                 brief-end-of-window
                 brief-bookmark-jump-set-0
                 brief-bookmark-jump-set-1
                 brief-bookmark-jump-set-2
                 brief-bookmark-jump-set-3
                 brief-bookmark-jump-set-4
                 brief-bookmark-jump-set-5
                 brief-bookmark-jump-set-6
                 brief-bookmark-jump-set-7
                 brief-bookmark-jump-set-8
                 brief-bookmark-jump-set-9
                 brief-bookmark-set-jump
                 brief-mark-line-up-with-<C-home>
                 brief-mark-line-up-with-<C-prior>
                 brief-mark-line-down-with-<C-end>
                 brief-mark-line-down-with-<C-next>
                 brief-end-of-window
                 brief-mark-line-up-with-<M-home>
                 brief-mark-line-down-with-<M-end>
                 brief-version))))

     ;; Non-brief commands, but mapped in brief keymap

     (defun brief-setup-multicurs-cmds ()
       (when (and brief-mode
                  brief-init-multi-cursor-cmd)
         ;; Extend run-all
         (delete-dups
          (nconc mc/cmds-to-run-for-all
                 '(beginning-of-line
                   end-of-line
                   forward-sexp
                   backward-sexp)))

         ;; Extend run-once
         (delete-dups
          (nconc mc/cmds-to-run-once
                 '(find-file
                   load-library
                   write-file
                   insert-file
                   compile
                   kill-buffer
                   enlarge-window
                   shrink-window-horizontally
                   enlarge-window-horizontally
                   shrink-window
                   split-window-vertically
                   split-window-horizontally
                   save-buffers-kill-emacs
                   overwrite-mode
                   goto-line
                   help
                   eshell)))))

     (add-hook 'brief-mode-mode 'brief-setup-multicurs-cmds)))

;;
;; Overriding `line-number-at-pos'
;;

(eval-when (compile load eval)
  (when (version< emacs-version "24.0")
    ;;(message "Defining advice function `brief-override-line-number-at-pos'")
    ;; Initially we disable it till brief mode is enabled
    (defadvice line-number-at-pos
        (around brief-override-line-number-at-pos (&optional pos)
                disable compile activate)
      ;; Forward reference here
      (if brief-replace-emacs-func:line-number-at-pos
          (brief-fast-line-number-at-pos pos)
        ad-do-it))))

(defun brief-set:brief-replace-emacs-func:line-number-at-pos (sym val)
  "Method 'set' of variable `brief-replace-emacs-func:line-number-at-pos'.
This function dynamically override `line-number-at-pos' according to
the VAL.  Notice this works only if functions like `custom-set-variables'
are used to set this custom variable."
  (if val
      (if (fboundp #'advice-add)
          (progn
            (advice-add 'line-number-at-pos
                        :override #'brief-fast-line-number-at-pos)
            (message
             "Overriding `line-number-at-pos' with Brief's fast version."))
        (ad-enable-advice 'line-number-at-pos
                          'around 'brief-override-line-number-at-pos)
        (ad-activate 'line-number-at-pos)
        (message "Advice `line-number-at-pos' with Brief's fast version."))
    ;; Iterate through the buffer list and clean each buffer's line number cache
    (dolist (b (buffer-list))
      (with-current-buffer b
        (setq brief-fast-line-number-list nil)))
    (if (fboundp 'advice-remove)
        (progn
          (advice-remove 'line-number-at-pos #'brief-fast-line-number-at-pos)
          (message
           "Restoring `line-number-at-pos' to Emacs's default version."))
      (ad-disable-advice 'line-number-at-pos
                         'around 'brief-override-line-number-at-pos)
      (ad-activate 'line-number-at-pos)
      (message
       "Disable advicing `line-number-at-pos' return to Emacs's default.")))
  (set sym val))

(defcustom brief-replace-emacs-func:line-number-at-pos
  ;;(and (version< emacs-version "26.1")
  ;;     (not (boundp 'display-line-numbers)))
  ;; Although Emacs 26.1 supports native line numbering, for relative window
  ;; position calculation line numbers are still heavily in use so keep this
  ;; feature ON by default.
  t
  "Replace Emacs's `line-number-at-pos' with `brief-fast-line-number-at-pos'.
Use advice function to replace the system default `line-number-at-pos'
which is slow on very big files, especially when using `linum-mode'.
Notice that this replacement is global once you loaded brief mode, no
matter if brief-mode is enabled or not."
  :type  'boolean
  :set   'brief-set:brief-replace-emacs-func:line-number-at-pos)

(eval-when (compile eval load)
  '(when brief-replace-emacs-func:line-number-at-pos
     ;; Global replacement, no matter if Brief mode is enabled or not.
     ;; Notice that it dynamically overrides the `line-number-at-pos' function
     ;; according to `brief-replace-emacs-func:line-number-at-pos'.
     ;; `custom-set-variables' must be used to change this custom value.
     ;; Normal `setq', `set' and `set-default' will only change the value but
     ;; will not change the replacement status of `line-number-at-pos'.
     (if (fboundp 'advice-add)
         ;; Its :set method already do this; here is just in case the user
         ;; using `setq' to set the value.
         (advice-add 'line-number-at-pos
                     :override #'brief-fast-line-number-at-pos)
       (ad-enable-advice 'line-number-at-pos
                         'around 'brief-override-line-number-at-pos)
       (ad-activate 'line-number-at-pos))))

;;;###autoload
(defun brief-easy-start ()
  "Emulate Brief by changing less favored Emacs settings for programmers.
Before enabling brief mode this sets the following:
 1) No line wrapping by setting `truncate-lines' 't.
 2) No jumppy scrolling in both vertical and horizontal directions.
 3) Smaller borders.
This function is used by the quick launcher 'b' script."
  (interactive)
  (setq-default truncate-lines t)  ;; disable line wrapping
  ;;(setq-default global-visual-line-mode t)
  (setq scroll-step 1              ;; set vertical scroll not jumppy
        scroll-conservatively 101)
  (setq hscroll-margin 1           ;; set horizontal scroll not jumppy
        hscroll-step 1)
  (scroll-bar-mode -1)             ;; small border without scroll bar
  (brief-mode 1))

(run-hooks 'brief-load-hook)

(provide 'brief)

;;; brief.el ends here
