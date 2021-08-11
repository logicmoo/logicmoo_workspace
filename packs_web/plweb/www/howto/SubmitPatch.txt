---+ How to submit patches?

This page describes how to submit patches if you find a bug or missing
functionality. There are several options, which are described in
decreasing preference.

---++ Using GitHub pull requests (new, preferred)

As of May 1, 2014, the primary repository of SWI-Prolog is at
[GitHub](https://github.com/SWI-Prolog).

GitHub has a
[good description](https://help.github.com/articles/using-pull-requests)
of submitting patches using _pull requests_.

Note that quite a few of the remarks below also apply to using GitHub, notably
those about dealing with packages that are represented as _git submodules_.

### A note about GitHub forks

You cannot _fork_ on GitHub and _next_ clone.  If you try to do so, all submodules are resolved against _your_ GitHub account and thus you must clone _all_ repositories from the SWI-Prolog organization.  Much better is to _clone_ and _then_ _fork_:

```
git clone https://github.com/SWI-Prolog/swipl-devel.git
git submodule update --init
```

Now you have a complete working source tree you can compile and modify.  If you want to create a _pull request_ against one of the git repos,  _now fork it on github if you did not already do so_, go to the top directory of the repo on your machine and run (you can also use ``https://``)

```
git remote add myfork git@github.com:me/repo.git
```

Now, after creating a _topic branch_, push it to github using

```
git push myfork mytopic:mytopic
```


---++ Submit as GIT patch-sets (old, still good)

Download the system as described in [[Accessing SWI-Prolog source via GIT][</git.html>]].  First we describe patches to the core system.  Patches
to packages that are distributed as git submodules are described below.

  1. Make sure git knows who you are.  You only need to do this once
  for your machine.

  ==
  % git config --global user.name "The Great Fixer"
  % git config --global user.email fixer@bugs.com
  ==

  2. Switch to a branch.  This makes it easy to distinguish between the
  central version and yours.

  ==
  % git checkout -b fixes
  ==

  3. Edit your files, making the necessary changes.  Test your changes.
  Make sure you changed nothing unwanted using

  ==
  % git diff
  ==

  If you are happy, commit them using the command below.  Please add a
  sensible story that explains what has been added, fixed, ....  If you
  want the change to appear in the release notes, make the comment start
  with a word in capitals, followed by a colon (:).  Typically, this is
  =|ADDED:|=, =|FIXED:|=, =|PORT:|=.  The set is not fixed, but try to
  reuse old keywords.

  ==
  % git commit -a
  ==

  4. Create a patch-set.  The command below creates a file for each commit
  between the master branch and your fixes branch.  It creates files 0001-*,
  0002-*, etc.  Send these files to bugs@swi-prolog.org, *one* file per
  mail.  The commit comment is part of these files, which should be enough
  to explain what you did why.

  ==
  % git format-patch master
  ==

---+++ How about submitting patches to a package?

This is slightly more complicated because packages are git submodules and
submodules are `not on a branch'.  Therefore, you need some more preparation.
Go to the directory holding the package you want to patch.  Then use these
commands to turn this into a normal repository.  After that you can follow
the same steps as above.

  ==
  % git checkout master
  % git pull
  ==

---+++ Synchronizing

At some point you may want to synchronize   with the upstream version.  You
do this by switching branches, and update as usual:

  ==
  % git checkout master
  % git pull
  % git submodule update
  ==

Now, there are some options that may apply.

  $ All changes were accepted :
  Simply discard your fixes branch using

    ==
    % git branch -D fixes
    ==

  $ No changes are applied upstream (yet) :
  Go to your fixes branch and _rebase_ it:

    ==
    % git checkout fixes
    % git rebase master
    ==

  $ Some changes were accepted, others not and you want to keep using them :
  Move your branch, recreate it and selectively pick the commits you want
  to keep.  You can find the hashes of the commits using =|git log|=.
  The sequence is:

    ==
    % git branch -m fixes tmp
    % git checkout -b fixes
    % git cherry-pick <hash1>
    % git cherry-pick <hash2>
    % ...
    % git branch -D tmp
    ==

---+++ Do's and Don'ts

  $ Use small commits :
  Do not put multiple changes into the same commit.  The ideal commit
  establishes exactly one fix or addition.

  $ Amend commits :
  Often, you'll find that the commit you just created is wrong or
  incomplete.  As long as your commit is only known locally, use
  the command below to fix the last commit rather than creating
  two commits.

  ==
  % git commit -a --amend
  ==

---++ As simple git patches (2nd option)

Just checkout the version using git and use the command below to create
a patch file.

  ==
  % git diff > my-patches
  ==

This is less ideal because:

  1. There is no author in the GIT log.  It can be very useful to
  know who did what when and you claim your work.

  2. There is no comment.  Please add a comment to the mail about
  why and what.

---++ As classical patches (3rd option)

You can also keep a copy and use the diff command to produce a classical
patch file as below.  diff should be available on virtually any Unix system
and there are many ports for Windows.  Use the unified (=|-u|=) style to
include context.

  ==
  % cp somefile somefile.orig
  <edit, test, etc>
  % diff -u somefile.orig somefile > my-patches
  ==

This is worse than using =|git diff|= because git diff includes information
on the exact version that is patched, which can help us if there are other
conflicting patches.  In addition to comments on the why and what, please
indicate the version you patched.


---++ Send complete new files (please don't)

Please do not send complete files.  They are hard to integrate.  If you
really must, at least accompany them with a clear description of what you
changed and which version of the file you changed.  Change as little as
possible.
