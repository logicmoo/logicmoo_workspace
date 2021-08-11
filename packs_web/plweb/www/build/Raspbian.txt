# Building SWI-Prolog for the Raspberry Pi

Building SWI-Prolog for the [Raspberry Pi](https://www.raspberrypi.org/)
using [Raspbian](https://www.raspbian.org/) follows the [Debian](<Debian.txt>)
installation instructions.  There are some specific issues:

  - `libunwind` is not available for Raspbian. You can simply ignore
  this dependency.
  - You need *|gcc 4.8 or later|*.  SWI-Prolog uses 64-bit atomic
  instructions for managing the _generation_ of the database for
  implementing the _logical update semantics_.  *|gcc 4.8 and later|*
  implement 64-bit atomic instructions for all ARM models based on
  a kernel helper for the single core Pi-1 and CPU atomic instructions
  for the SMP models.
