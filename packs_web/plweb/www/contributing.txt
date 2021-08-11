# How do I contribute?

## Several ways to contribute to SWI-Prolog

Here are some ideas from the easiest one:

    - [Report a bug](<bug.html>)
    - Build the [community](<community.txt>), help answer questions in the forum
    - Write or improve documentation
    - [Submit an add-on](</howto/Pack.html>) (a SWI-Prolog "pack")
    - [Submit a patch](<howto/SubmitPatch.html>)

## How do I improve the website?

You can edit pages like this by making pull requests on these GitHub repositories:

    - The source code for these wiki pages (static content) is in
      [plweb-www](https://github.com/SWI-Prolog/plweb-www)
    - The source code for this web server is in
      [plweb](https://github.com/SWI-Prolog/plweb)

You can preview your edits in your local machine. Follow the
instructions in the [plweb](https://github.com/SWI-Prolog/plweb)
repository.

## How do I write or improve documentation?

The documentation is part of the source code and thus the general
guidelines of [Submit a patch](<howto/SubmitPatch.html>) apply.
The documentation itself appears in three formats:

    - LaTeX, mostly in the ``man`` subdirectory. The files have
    extension ``.doc``, but are in fact LaTeX files that are
    preprocessed to create automatic links to predicates and functions.
    - Markdown files, mostly ``.md``, but ``.txt`` is also used.
    - Documentation in the source code
      using [PlDoc](</pldoc/package/pldoc>)

To preview your new documentation, build the system and use help/1:

    ?- help(mypredicate).

See CMAKE.md in the source directory for building the PDF documentation.
