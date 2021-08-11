---+ Why did SWI-Prolog move to GIT?

The SWI-Prolog project moved from CVS to GIT because

    * CVS started to become too slow
    * CVS doesn't support staging very well. I like frequent commits to
    synchronise between machines, commit work that is known to be
    incomplete, etc. For GIT this is the natural model, for CVS it is
    just too cumbersome.
    * GIT allows for offline working.
    * GIT checkouts are totally stand alone, making local branches much
    easier as well as providing security against system failure, etc. 

Sofar, we are very happy with this move. Recent system failures have
indeed proved that the distributed approach allows to restoring the
central repository from the development systems easily.

---++ Why GIT instead of SVN, Mercurial, Bazaar, ...?

SVN is a traditional central SCM.  We wanted a distributed SCM to accommodate
a more flexible and robust development model.  Mercurial and Bazaar would have
been serious options.  GIT was brought to my attention by Markus Triska.  A
brief study on its working made me confident that it is a simple, robust and
open SCM.  The only drawback was relatively poor support of Windows clients,
but CVS server emulation could deal with that.  In the meanwhile, SWI-Prolog
uses GIT modules that are (were?) not supported by the CVS bridge, but
luckily the Windows clients have evolved to a usable level.

