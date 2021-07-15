[![License GPL 3](https://img.shields.io/badge/license-GPL_3-green.svg)](http://www.gnu.org/copyleft/gpl.html)
[![MELPA](http://melpa.org/packages/vc-hgcmd-badge.svg)](http://melpa.org/#/vc-hgcmd)
[![MELPA Stable](http://stable.melpa.org/packages/vc-hgcmd-badge.svg)](http://stable.melpa.org/#/vc-hgcmd)

# emacs-vc-hgcmd

Emacs VC backend to work with mercurial repositories through [hg command server](https://www.mercurial-scm.org/wiki/CommandServer)

## Advantage over `vc-hg`

The main advantage compared to `vc-hg` is speed.

Because communicating with single `hg` process over pipe is much faster than starting separate `hg` process for each command.

### Other improvements and differences

#### Graph log

Graph log is used by default when viewing branch or root log.

#### Conflict status for a file

Files with unresolved merge conflicts have appropriate status in `vc-dir`.
Also you can use `vc-find-conflicted-file` to find next file with unresolved merge conflict.

Files with resolved merge conflicts have extra file info in `vc-dir`.

#### hg summary as `vc-dir` extra headers

`hg summary` command gives useful information about commit, update and phase states.

#### Current branch is displayed on mode line.

Mode-line format is not customizable yet.

#### Amend and close branch commits

While editing commit message you can toggle `--amend` and `--close-branch` flags.

#### Merge branch

`vc-hgcmd` will ask for branch name to merge.

#### Default pull arguments

You can customize default `hg pull` command arguments.
By default it's `--update`. You can change it for particular pull by invoking `vc-pull` with prefix argument.

#### Branches and tags as revision completion table

Instead of list of all revisions of file `vc-hgcmd` provides list of named branches and tags.
It's very useful on `vc-retrieve-tag`.
You can specify `-C` to run `hg update` with `-C` flag and discard all uncommitted changes.

#### Filenames in `vc-annotate` buffer is omitted

They are mostly useless in annotate buffer.
To find out right filename to annotate `vc-hgcmd` uses `status --rev <rev> -C file`.

#### `previous-revision` and `next-revision` respect files

Keys `p` and `n` in annotation buffer works correctly.

#### Create tag

`vc-hgcmd` creates tag on `vc-create-tag`.
If `vc-create-tag` is invoked with prefix argument then named branch will be created.

#### Predefined commit message

While committing merge changes commit message will be set to `merged <branch>` if
different branch was merged or to `merged <node>`.

Additionally predefined commit message passed to custom function `vc-hgcmd-log-edit-message-function` so one can change it. For example, to include current task in commit message:

```elisp
(defun my/hg-commit-message (original-message)
  (if org-clock-current-task
      (concat org-clock-current-task " " original-message)
    original-message))

(custom-set-variables
 '(vc-hgcmd-log-edit-message-function 'my/hg-commit-message))
```

#### Run custom hg commands

Interactive function `vc-hgcmd-runcommand` allow execute custom command

#### Interactive

It is possible to answer to hg questions, e.g. pick action during merge

#### View changes made by revision; diff to parents

Additional bindings in `log-view-mode`:
- <kbd>c c</kbd> view change made by revision at point (`-c` option to `hg diff` command)
- <kbd>c 1</kbd> view diff between revision at point and its first parent
- <kbd>c 2</kbd> view diff between revision at point and its second parent

<kbd>C c</kbd>, <kbd>C 1</kbd> and <kbd>C 2</kbd> show corresponding diffs for whole changeset

#### View log for revset

Command `vc-hgcmd-print-log-revset` allows to print log for revset, e.g. `branch(branch1) or branch(branch2)`

## Installation

### 1. Install package

`vc-hgcmd` available on [MELPA](http://melpa.org):

<kbd>M-x</kbd> `package-install` <kbd>RET</kbd> `vc-hgcmd` <kbd>RET</kbd>.

Alternatively, download `vc-hgcmd.el` and run:

<kbd>M-x</kbd> `package-install-file` <kbd>RET</kbd> `<path-to-vc-hgcmd-el>` <kbd>RET</kbd>

### 2. Enable `Hgcmd` backend

Add `Hgcmd` to `vc-handled-backends` before or instead `Hg`.
