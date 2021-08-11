---+ Controlling colored output on the terminal

By default, SWI-Prolog will use colored output on the commandline based on
library(ansi_term) if `current_output` is connected to a terminal.  This is
detected by the `tty` property of streams (see stream_property/2).  Coloring
can be controlled in two ways

  - It can be disabled using this in your personal initialization file (see
    below).

    ==
    :- set_prolog_flag(color_term, false).
    ==

  - Colors can be changed by message type (see print_message/2) using the
    hook message_property/2.



@see PlInitialisation.txt for editing your personal init file.