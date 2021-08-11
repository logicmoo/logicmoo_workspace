# Getting Started

_Getting Started_ with Prolog depends on your background and goal. Here
are some options.

  - __I don't know anything about Prolog and want to learn the language__.  Prolog is
    quite different than any other programming language you may know.   A background
    in math and functional programming helps to some extend.  Start with a good
    introduction text.  Examples are

    - [Simply Logical](http://book.simply-logical.space/)
    - [Learn Prolog Now](http://lpn.swi-prolog.org/)

    Both are online text books that embed [SWISH](https://swish.swi-prolog.org).
    SWISH is an online version of SWI-Prolog.  SWISH differs from a local SWI-Prolog
    installation:

    - As it runs on a shared server it enforces a __sandbox__ that doesn't allow
      running any dangerous code and rejects code it cannot _prove_ to be safe.
    - No state is maintained between queries.
    - Input/output, concurrency and many other SWI-Prolog features are not or
      only in a limited way supported.
    - It does provide a nice web based interface that allows for rich output
      based on HTML or high level vizualization libraries such as
      [C3.js](https://c3js.org/)

  - __I've made my first steps at Prolog and want to start a real project.__ Start
    with a local installation for your platform.  Familiarise yourself with the
    SWI-Prolog [toplevel](https://www.swi-prolog.org/pldoc/man?section=quickstart)
    and the IDE tools such as the [GUI debugger](https://blog.inductorsoftware.com/blog/SWIPrologGraphicalDebugger).  Establish your debug/edit/reload cycle based on edit/1 and
    make/0.  The edit/1 primitive finds predicates, files, modules, etc. and hands
    their position to the built-in editor PceEmacs or the editor of your choice.

    SWI-Prolog comes with a lot of [features](https://www.swi-prolog.org/features.html)
    that may be useful useful to your project.  Also consider the
    [add-ons](https://www.swi-prolog.org/pack/list).

  - __I know some Prolog and I want to use SWI-Prolog for data analysis__.  Here
    SWISH may come really handy, but you do not want the limitations of the shared
    SWISH server.  You could consider a local installation of SWISH and R
    for SWI-Prolog, either from source or (starting) from the
    [Docker images](https://www.swi-prolog.org/Docker.html)

  - __I know some Prolog and I want to use SWI-Prolog as a (web) service__.  Start
    with a local installation for your platform and the
    [Web application tutorial](https://www.github.com/Anniepoo/swiplwebtut/blob/master/web.adoc)



