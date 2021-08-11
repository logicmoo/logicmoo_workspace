---+ Can I compile my whole project inc. kernel to C?

...for instance if I wanted to target a PalmPilot.

Not with SWI-Prolog. SWI-Prolog however is entirely written in C, so if
you can compile C on the target platform you can run SWI-Prolog
programs, or ... Almost. SWI-Prolog requires a 32- or 64-bit flat
address space and the PalmPilot doesn't have that :-(

Anyway, there is GNU-Prolog that can generate native code on some
platforms and its predecessor Wamcc that translates Prolog to C. For
translating small programs these may be valid options.

There are also various Prolog systems written in Java and/or compiling
to Java VM code. Some are quite good. Most are merely toys. 
