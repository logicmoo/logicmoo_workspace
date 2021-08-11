---+ Serving plain files from the SWI-Prolog HTTPD

Although the SWI-Prolog web-server is intended   to serve documents that
need to be computed  dynamically,  serving   plain  files  is  sometimes
necessary. There are several options to serve files.


---++ Locating physical files

Before we can serve files, we first need to  be able to find them on the
file-system.  This is a general topic, but we discuss it here because it
is especially relevant to serving files.  There are three options:

  1. Specify absolute file names (i.e., files that begin with / (or
  <drive>:/ in windows).  This is typically a bad idea as it makes
  moving your server to another environment complicated.

  2. Specify relative file names, where the server's working directory
  is the basis.  This is much better than using absolute path, but still
  makes moving things around complicated.

  3. Use file-path mechanism implemented by absolute_file_name/3
  (search over a path) and user:file_search_path/2 (define paths). This
  is a bit similar to the PATH variable used to find executables in
  operating systems.  It allows for multiple paths and the specification
  may be non-deterministic. This non-determinism can be used to find
  files in multiple physical locations using the same specification.
  A good example of this can be found in serve_files_in_directory/2.

---++ Code snippets

All  code  snippets   below   assume   a    running   server   and   the
library(http/http_dispatch)  loaded.  This  means   that  running  these
examples require a file with this  content   to  be  loaded (8080 is the
port; change as you please).

  ==
  :- use_module(library(http/thread_httpd)).
  :- use_module(library(http/http_dispatch)).

  :- initialization
        http_server(http_dispatch, [port(8080)]).
  ==

All snippets are supposed to be loaded from a source-file, as opposed to
be typed in the console.


---+++ Serve a few files

Sometimes you have a fully  dynamic  server   that  needs  to serve, for
example, a few images or CSS files.   You  can solve this directly using
http_reply_file/3. The example below serves   =|/favicon.ico|=  from the
local file =|favicon.ico|=.

  ==
  :- http_handler('/favicon.ico', http_reply_file('favicon.ico', []), []).
  ==

The  predicate  http_reply_file/3  (by  default)    deals  with  caching
(=|If-modified-since|=)   and   deals   with    different   content-type
(mime-type) using the predicate file_mime_type/2.


---+++ Serving many `server support' files

Bigger servers often have lots of static files that contain images, CSS,
JavaScript,  etc.  A   good   way   to    facilitate   this   is   using
serve_files_in_directory/2  from  library(http/http_server_files).  This
predicate assumes aliases for  searching  files   (see  above)  and  for
specifying HTTP locations as defined by library(http/http_path).

In  the  next  example,  we  create  a   serve  that  serves  images  on
=|/images/...|= from a directory   =|/srv/htdocs/icons/...|=.  Note that
this server has no means for browsing by   the user. It answers the file
or 404, which is typically precisely what   we  want for server resource
files.

First, we specify a path _alias_ for the location handled by the server.

  ==
  :- use_module(library(http/http_path)).

  http:location(images,	root(images), []).
  ==

Next, we specify a file _alias_ for  the location on the file-system. We
do this in two steps: (1) we define an alias for our entire document-set
and (2) then we define  the  target   =icons=  alias  relative  to this
central alias. This two-step approach allows  for updating the server to
a modified environment easily. Note that   the =document_root= alias can
be in a different Prolog file (e.g., a global configuration file).

  ==
  :- multifile user:file_search_path/2.

  user:file_search_path(document_root,	'/srv/htdocs').
  user:file_search_path(icons,		document_root(icons)).
  ==

Finally, serve_files_in_directory combines the two aliases. It is common
to use the _same_ alias name for both   the  HTTP path and file. We used
two different ones in this example to clarify their roles.

  ==
  :- http_handler(images(.), serve_files_in_directory(icons), [prefix]).
  ==

---+++ Serving a user-browsable directory hierarchy

If you want to give the user access  to   a  set  of files, the above is
unsuitable because it does not   support  directory browsing. SWI-Prolog
5.11.29  and  5.10.6  provide  library(http/http_files)  to  solve  this
problem.     The     predicate      http_reply_from_files/3     combines
http_reply_file/3 with http_reply_dirindex/3. Here is  the complete code
to serve files  to  a  server  running   at  port  8080  from  the local
directory. Note that the  server  disallows   access  to  ../,  etc., to
protect sensitive files on the system.

  ==
  :- use_module(library(http/thread_httpd)).
  :- use_module(library(http/http_dispatch)).
  :- use_module(library(http/http_files)).

  :- http_handler(root(.), http_reply_from_files('.', []), [prefix]).

  :- initialization
	  http_server(http_dispatch, [port(8080)]).
  ==

@see	Older versions may download http_files.pl from
	http://www.swi-prolog.org/git/packages/http.git/blob/HEAD:/http_files.pl

@see	PWP.txt describes _|Prolog Well-formed Pages|_, a form of dynamic
	HTML service.
