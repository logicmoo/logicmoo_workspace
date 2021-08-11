---+ SWI Prolog modules

These are some libraries and modules to enable some useful capabilities
from SWI Prolog.

    $ plosc (v0.1, [[tarball][<plosc-0.1.tar.gz>]]) :
      Sends Open Sound Control (OSC) messages (requires liblo).

    $ plmidi (v0.1, [[tarball][<plmidi-0.1.tar.gz>]]) :
      Sends MIDI messages (Mac OS X only).

    $ plrand (v0.3, [[tarball][<plrand-0.3.tar.gz>]]) :
      Better random number generation, with a high quality, long
		period generator (L'Ecuyer's RngStream), support for jumping
		ahead in the stream of random numbers, better control of
      generator state and some interesting distributions including
      Normal, Gamma, Inverse Gamma, Beta, Dirichlet, Dirichlet Process,
		Student's t, Stable, Zeta, Poisson, Binomial etc.
		UPDATED: see ChangeLog for new features in v0.3.

    $ plml (v0.91, [[tarball][<plml-0.91.tar.gz>]]) :
      Manages one or more Matlab engines running locally or remotely,
      and provides access to Matlab functions and graphics. Primary
      access is through an evaluator (a bit like is/2) that understands
      a Prolog term language for representing Matlab expressions.

@author Samer Abdallah

