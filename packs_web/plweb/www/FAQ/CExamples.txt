---+ Where do I find examples mixing C and Prolog?

The best source of examples are the sources of the _packages_.  They vary from very simple to highly complicated.  You can examine the packages through the
[[GIT web interface][</git>]].  Good examples packages are:

  $ nlp :
  Rather basic usage of the foreign interface.

  $ clib :
  Both simple and more complicated code to access OS primitives.

  $ zlib :
  Extensive example of using the stream interface to define IO filters.

You may also want to look at the Prolog source itself.  Many of the
foreign predicates use the public interface.  Some use more low-level
primitives and the code uses a different mechanism to register foreign
predicates.

@see Please also check out the [[C++ interface][</pldoc/package/pl2cpp.html>]]