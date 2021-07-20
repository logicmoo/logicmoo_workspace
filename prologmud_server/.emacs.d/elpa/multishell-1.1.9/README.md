multishell.el
=============

Facilitate use of multiple local and remote Emacs shell buffers.

Multishell is available via Emacs package manager, [in ELPA](https://elpa.gnu.org/packages/multishell.html). Install "multishell" from the `M-x package-list-packages` listing.

I use the emacs shell a *lot*, including separate shells for separate
project, and more shells for access to remote systems (which I do a lot, as
a systems administrator). On top of emacs' powerful shell and tramp
facilities, use a `multishell` (customization-activated) key binding to:

* Get to the input point from wherever you are in a shell buffer,
  ... or to any of your shell buffers, from anywhere inside emacs.

* Use universal arguments to launch and choose among alternate shell buffers,
  ... and change which is the current default.

* Easily restart disconnected shells, or shells from prior sessions
  ... the latter from Emacs builtin savehist minibuf history persistence

* Append a path to a new shell name to launch a shell in that directory,
  ... and use a path with Emacs tramp syntax to launch a remote shell -
  for example:

  * `#root/sudo:root@localhost:/etc` for a buffer named "#root" with a
    root shell starting in /etc.

  * `/ssh:example.net:/` for a shell buffer in / on example.net.
    The buffer will be named "*example.net*".

  * `#ex/ssh:example.net|sudo:root@example.net:/etc` for a root shell
    starting in /etc on example.net named "*#ex*".

  * `interior/ssh:gateway.corp.com|ssh:interior.corp.com:` to go via
    gateway.corp.com to your homedir on interior.corp.com.  The buffer
    will be named "*interior*". You could append a sudo hop, and so on.

* Thanks to tramp, file visits from the shell will seamlessly be on the
  host where the shell is running, in the auspices of the target account.

See the `multishell-pop-to-shell` docstring (in
[multishell.el](multishell.el)) for details, and
[getting-to-a-shell.md](getting-to-a-shell.md) for the nitty-gritty
decision tree that determines where the keybinding according to the various
conditions.

Customize-group `multishell' to select and activate a keybinding and set
various behaviors. Customize-group `savehist' to preserve buffer
names/paths across emacs restarts.

Please use
[the multishell repository](https://github.com/kenmanheimer/EmacsMultishell)
issue tracker to report problems, suggestions, etc.

See the [multishell.el](multishell.el) file commentary for a change log and
Todo list.
