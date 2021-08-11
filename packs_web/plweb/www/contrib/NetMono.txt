---+ SWI-Prolog 2-Way interface to .NET/Mono

---++ Introduction*

This is an overview of an interface which allows SWI-Prolog programs to
dynamically create and manipulate .NET objects.

Here are some significant features of the interface and its implementation:

  * API is similar to that of XPCE: the four main interface calls are
  cli_new, cli_call, cli_set and cli_get (there is a single cli_free,
  though .NET's garbage collection is extended transparently into
  Prolog)

  * Uses @/1 to construct representations of certain .NET values; if @/1
  is defined as a prefix operator (as used by XPCE), then you can write
  @false, @true, @null, @void etc. in your source code; otherwise (and
  for portability) you'll have to write e.g. '@'(true) etc.

  * cli_call/4 (modeled from JPL's jpl_call/4) resolves overloaded
  methods automatically and dynamically, inferring the types of the
  call's actual parameters, and identifying the most specific of the
  applicable method implementations (similarly, cli_new resolves
  overloaded constructors)

  * Completely dynamic: no precompilation is required to manipulate any
  .NET classes which can be found at run time, and any objects which can
  be instantiated from them

  * Interoperable with Uwe Lesta's SwiPlCS's .NET API.

  * Autoloads (defines) new prolog preds marked with [PrologVisible] in
  your DLLs - supporting you coding nondeterministic and deterministic
  versions.

  * Exploits the Invocation API of the .NET P/Invoke Interface: this is
  a mandatory feature of any compliant .NET

  * Implemented with a fair amount of C# code and Prolog code in one
  module (swicli.pl) (which I believe to be ISO Standard Prolog
  compliant and portable) and a SWI-Prolog-specific foreign library
  (swicli[32].dll for Windows and swicli[32].so *nix), implemented in
  ANSI C but making a lot of use of the SWI-Prolog Foreign Language
  Interface Then uses Swicli.Library.dll (Managed binary) that runs on
  both Mono and .NET runtimes.

  * the Prolog-calls-CLI (mine) and CLI-calls-Prolog (Ewe's) parts of
  SWICLI are largely independent; mine concentrates on representing all
  .NET data values and objects within Prolog, and supporting
  manipulation of objects; Ewe's concentrates on representing any Prolog
  term within .NET, and supporting the calling of goals within Prolog
  and the retrieving of results back into .NET

  * @(terms) are canonical (two references are ==/2 equal if-and-only-if
  they refer to the same object within the .NET)

  * are represented as structures containing a distinctive atom so as to
  exploit SWI-Prolog's atom garbage collection: when an object reference
  is garbage-collected in Prolog, the .NET garbage collector is
  informed, so there is sound and complete overall garbage collection of
  .NET objects within the combined Prolog+.NET system

  * .NET class methods can be called by name: SWICLI invisibly fetches
  (and caches) essential details of method invocation, exploiting .NET
  Reflection facilities

  * Reason about the types of .NET data values, object references,
  fields and methods: SWICLI supports a canonical representation of all
  .NET types as structured terms (e.g. array(array(byte))) and also as
  atomic .NET signatures

  * when called from Prolog, void methods return a @(void) value (which
  is distinct from all other SWICLI values and references)

  * Tested on Windows XP, Windows7 and Fedora Linux, but is believed to
  be readily portable to SWI-Prolog on other platforms as far as is
  feasible, .NET data values and object references are represented
  within Prolog canonically and without loss of information (minor
  exceptions: .NET float and double values are both converted to Prolog
  float values; .NET byte, char, short, int and long values are all
  converted to Prolog integer values; the type distinctions which are
  lost are normally of no significance)

  * Requires .NET 2.0 and class libraries (although it doesn't depend on
  any .NET 2-specific facilities, and originally was developed for use
  with both 1.0 thru 4.0 .NETs, I haven't tested it with 1.0 recently,
  and don't support this)

---++ Example

  ==
  ?- use_module(library(swicli)).

  ?- cliCall('System.Threading.ThreadPool','GetAvailableThreads'(X,Y),_).

  X=499, Y=1000
  ==

Doc root will be findable from
http://code.google.com/p/opensim4opencog/wiki/SwiCLI

@see    CSharp.txt
@author    Douglas Miles
