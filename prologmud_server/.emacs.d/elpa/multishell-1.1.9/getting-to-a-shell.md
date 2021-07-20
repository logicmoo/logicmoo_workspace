Multishell enables you to get to the input prompt in the shell you want
with as few keystrokes as possible.

* One keybinding, unmodified, gets you to the your current default shell, if
  not in a shell, or to the input prompt of the current shell, if you're in
  one.

* Use the universal argument to select a specific shell buffer, wherever
  point happens to be residing. Enter an empty line to the prompt to go to
  your current default shell, or use completing read to go to a shell from
  your multishell history, or start a new shell at the path you specify -
  including remote paths, using tramp syntax. (See the
  multishell-pop-to-shell docstring in [multishell.el](multishell.el) for
  details.)

* Use a doubled universal argument to set the shell you choose to be the
  current default. (The prompt will indicate that mode with a "<==".)

Here's the decision tree:

* No universal argument - use:

  * From-buffer is shell buffer: use from-buffer current name/path
    - if shell/connection is stopped, restart/reconnect
    - if not at input prompt, go there
  * From-buffer is not shell buffer: 
    - Go to multishell-primary-name current name/path, creating or
      restarting and/or reconnecting if that shell is not currently running.

* Universal argument provided - use:

  - No name is specified - use current multishell-primary-name path
  * Name is specified - use named buffer (creating if not already present):
    * Path is also specified:
      - shell is running - ignore new path
      - shell will be started or restarted - use new path
    * No path is specified:
      - Name has history: use path from history
      - Name has no history: use path that target buffer already has or inherits
    - If the universal argument is doubled, set the selected shell as the
      default one, going forwards.
