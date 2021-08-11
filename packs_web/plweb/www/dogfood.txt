# Eat Your Own Dog Food

This page presents a brief history of hosting and developing
http://www.swi-prolog.org. The website runs SWI-Prolog and is basically
`[PlDoc](</pldoc/package/pldoc.html>), the SWI-Prolog source
documentation system on steroids'. This site replaced a classic
[Apache](http://httpd.apache.org/) server built from static
documentation pages, a [Twiki](http://twiki.org/)-based wiki and
[Bugzilla](http://www.bugzilla.org/). The site was re-implemented as a
SWI-Prolog-based HTTP server that extends PlDoc for the following
reasons:

  1. PlDoc unites the core [reference
  manual](</pldoc/doc_for?object=manual>) with the [package
  documentation](</pldoc/doc_for?object=packages>) and documentation
  extracted from all loaded sources in a coherent interface. In
  addition, it provides search, dynamically generated hyperlinks
  between the various pieces of documentation, and display of the
  source code.  This is far better than the fragmented and
  not properly interlinked static documentation of the old site.

  2. SWI-Prolog comes with extensive libraries for [providing web
  services](</howto/http/>).  Running the website on the system is
  just the ideal opportunity to test these services and prove to the
  Prolog community that they are a vital piece of the infrastructure
  and that we trust them.  The server

    - welcomes about 4,000 visitors each day, viewing about
      30,000 pages (120,000 _hits_) (2013)
    - serves about 350 Gb of data per month
    - serves downloads from static files
    - serves manual pages from on-the-fly post-processed HTML
        documents
    - serves dynamically generated HTML pages from documented
        source files, coloured source files, etc.
    - analyses, indexes and presents [packs](</pack/list>).  It
        also presents the meta-data for pack_install/1
    - runs a REST API to handle user annotations.

## Lessons learned

Eating our own dog food first of all faced us with stability and
scalability issues. The most pressing stability issues encountered were
memory and handle leaks on misbehaving clients and broken service
handlers. Scalability issues have resulted in removing the maximum
number of threads limit and adding _thread pools_
(thread_pool_create/3).

Various new facilities were added and existing facilities were enhanced
to deal with the site's requirements. Examples are the CGI facilities
and HTTP authorization, including [OpenID](http://openid.net/) support.
This website was a strong motivation to add [quasi
quotations](</pldoc/man?section=quasiquotations>) for embedding long
HTML and JavaScript fragments.  The news and annotation facilities are
the first components that use the new [dict](</pldoc/man?section=dicts>)
datatype.

## Contributors

The website was originally developed by Jan Wielemaker. In 2012, Anne
Ogborn took the initiative to redesign the site. She analysed the
structure and log data to redesign the contents of the home page, the
menu, navigation aids, _did you know_, and download page. [Jessica
Chan](http://jessicachanstudios.com/) did the visual design of the
current website. She did not yet approve all details, so blame Jan
Wielemaker for everything that looks out-of-place, ugly or both. [Wouter
Beek](http://wouterbeek.com/) implemented the news and new annotation
facility.

@see [Can I replace a LAMP stack with SWI-Prolog?](</FAQ/PrologLAMP.txt>)
