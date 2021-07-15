;;; gnugo-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "gnugo" "gnugo.el" (0 0 0 0))
;;; Generated autoloads from gnugo.el

(autoload 'gnugo "gnugo" "\
Run gnugo in a buffer, or resume a game in progress.
If there is already a game in progress you may resume it instead
of starting a new one.  Prefix arg means skip the game-in-progress
check and start a new game straight away.

Before starting, Emacs queries you for additional command-line
options (Emacs supplies \"--mode gtp --quiet\" automatically).

Note that specifying \"--infile FILENAME\" (or, \"-l FILENAME\")
silently clobbers certain other options, such as \"--color\".
For details, see info node `(gnugo) Invoking GNU Go'.

\\<gnugo-board-mode-map>
To play, use \\[gnugo-move] to place a stone or \\[gnugo-pass] to pass.
See `gnugo-board-mode' for a full list of commands.

\(fn &optional NEW-GAME)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "gnugo" '("gnugo")))

;;;***

;;;### (autoloads nil "gnugo-frolic" "gnugo-frolic.el" (0 0 0 0))
;;; Generated autoloads from gnugo-frolic.el

(autoload 'gnugo-frolic-in-the-leaves "gnugo-frolic" "\
Display the game tree in a *GNUGO Frolic* buffer.
This looks something like:

  1 B  --  E7    E7    E7    E7
  2 W  --  K10   K10   K10   K10
  3 B  --  E2    E2    E2    E2
  4 W  --  J3    J3    J3    J3
  5 B  --  A6    A6    A6    A6
  6 W  --  C9    C9    C9    C9
           │
           ├─────┬─────┐
           │     │     │
  7 B  --  H7   !B8    C8    C8
                       │
                       ├─────┐
                       │     │
  8 W  --  D9    D9    D9    E9
  9 B  --              H8    H8
 10 W  --              PASS  PASS
 11 B  --              H5    PASS
 12 W  --              PASS
 13 B  --             *PASS

with 0, 1, ... N (in this case N is 3) in the header line
to indicate the branches.  Branch 0 is the \"main line\".
Point (* in this example) indicates the current position,
\"!\" indicates comment properties (e.g., B8, branch 1),
and moves not actually on the game tree (e.g., E7, branch 3)
are dimmed.  Type \\[describe-mode] in that buffer for details.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "gnugo-frolic" '("gnugo-")))

;;;***

;;;### (autoloads nil "gnugo-imgen" "gnugo-imgen.el" (0 0 0 0))
;;; Generated autoloads from gnugo-imgen.el

(autoload 'gnugo-imgen-create-xpms "gnugo-imgen" "\
Return a list of XPM images suitable for BOARD-SIZE.
The size and style of the images are determined by
`gnugo-imgen-sizing-function' (rounded down to an even number)
and `gnugo-imgen-style', respectively.  See `gnugo-xpms'.

The returned list is cached; see also `gnugo-imgen-clear-cache'.

\(fn BOARD-SIZE)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "gnugo-imgen" '("gnugo-imgen-")))

;;;***

;;;### (autoloads nil nil ("gnugo-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; gnugo-autoloads.el ends here
