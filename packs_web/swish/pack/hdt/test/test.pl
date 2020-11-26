:- use_module(library(semweb/rdf11)).
:- use_module(library(hdt)).

:- rdf_register_prefix(mdb, 'http://purl.org/collections/nl/dss/mdb/').

:- dynamic dss_hdt/1.

dss(HDT) :-
	dss_hdt(HDT0), !,
	HDT = HDT0.
dss(HDT) :-
	hdt_open(HDT0, 'dss.hdt'),
	asserta(dss_hdt(HDT0)),
	HDT = HDT0.

:- rdf_meta
	t(r,r,o),
	h(r,r,o),
	subject(r),
	predicate(r),
	shared(r),
	object(o),
	tid(r,r,o).

t(S,P,O) :-
	dss(HDT),
	hdt_search(HDT,S,P,O).

h(S,P,O) :-
	dss(HDT),
	hdt_header(HDT,S,P,O).

tl(S,P,O) :-
	setup_call_cleanup(
	    hdt_open(HDT, 'test_literals.hdt'),
	    hdt_search(HDT,S,P,O),
	    hdt_close(HDT)).

s(From, Role, MaxCount, Suggestions) :-
	dss(HDT),
	hdt_suggestions(HDT, From, Role, MaxCount, Suggestions).

subject(P) :-
	dss(HDT),
	hdt_subject(HDT,P).

predicate(P) :-
	dss(HDT),
	hdt_predicate(HDT,P).

object(P) :-
	dss(HDT),
	hdt_object(HDT,P).

shared(P) :-
	dss(HDT),
	hdt_shared(HDT,P).

property(P) :-
	dss(HDT),
	hdt_property(HDT,P).

tid(S,P,O) :-
	dss(HDT),
	Triple   = t(S,P,O),
	TripleID = t(SID,PID,OID),
	hdt_pre_triple(HDT, Triple, TripleID),
	hdt_search_id(HDT,SID,PID,OID),
	hdt_post_triple(HDT, Triple, TripleID).

:- if(current_predicate(hdt_insert/4)).

c(File) :-
	catch(delete_file(File), _, true),
	hdt_create(HDT),
	hdt_insert(HDT,
		   'http://example.org/s',
		   'http://example.org/p',
		   'http://example.org/o'),
	hdt_save(HDT, File),
	hdt_close(HDT).

:- endif.

