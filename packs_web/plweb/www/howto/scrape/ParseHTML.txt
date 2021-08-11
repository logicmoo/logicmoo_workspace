---+ How to parse HTML pages?

This is a bit more complicated than what you may expect.  First of all,
you do not want to process HTML pages as text.  Parsing HTML text without
prior translation of the HTML into a _|data structure|_ is bound to be sensitive
to pages created in different ways.  But, HTML found in the wild is often
invalid.  Worse, it is sometimes in fact XHTML, while documents that claim
to be XHTML often contain HTML features, such as missing close-tags, use
of uppercase, etc.

SWI-Prolog's [[SGML/SML parser][</pldoc/package/sgml.html>]] can produce results that are often good enough for scraping.   The best option set depends  a little on the nature of the document.  Notable different _encoding_ or sites using strict and proper XML may ask for different options.   Below is some code to get started.  It silently ignores all errors.  This is often what you want
for real scraping, but viewing the messages may be helpful if you get weird
output.

==
http_load_html(URL, DOM) :-
        setup_call_cleanup(http_open(URL, In,
                           [ timeout(60)
                           ]),
                           (   dtd(html, DTD),
                               load_structure(stream(In),
                                              DOM,
                                              [ dtd(DTD),
                                                dialect(sgml),
                                                shorttag(false),
                                                max_errors(-1),
                                                syntax_errors(quiet)
                                              ])
                           ),
                           close(In)).
==

Note that the use of setup_call_cleanup/3 guarantees that the opened
connection will eventually be closed.  However, it does *not* stop exceptions and errors are a normal part of processing web-documents.  Often, the scraper
will use a control structure as below to process a URL and just ignore it if
it is not possible to retrieve data from the URL.  Note that we give this as
example code instead of libraries because the demands for an actual scraper
can vary a lot.  E.g., it might be unacceptable to give up and if there is
an error you may want to wait and retry, or your decision may depend on the
error returned, etc.

==
scrape_no_error(URL) :-
	catch(http_load_html(URL, DOM), Error,
	      (	  print_message(warning, Error),
		  fail
	      )), !,
	process_dom(DOM).
scrape_no_error(_).
==

@see http_open/3 for opening web-pages.  If you need to handle cookies,
     SWI-Prolog 5.10.3/5.11.16 provide library(http/http_cookie).
@see setup_call_cleanup/3 is your friend to avoid loosing file handles
     in case of failure or errors.
@see load_structure/3 processes many more options.
