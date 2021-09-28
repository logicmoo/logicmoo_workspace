macro(engl) :-
	l(eng),
	ro("l_out.rtf"),
	sm(rtf),
	ldb("./gospels_db/sinott_eng.pl"),
	ldb("./gospels_db/l_templ_eng.pl"),
	set_id_format(2),
	pf("l.txt").

macro(gr) :-
	l(eng),
	l2(eng),
	sm(rtf),
	smf(1),
	ro("ciao.rtf"),
	pf("fruffi.txt").

macro(tpig) :-
	l(tpi),
	l2(eng),
	swtt(0),
	sm(rtf),
	so("tpi_nsmg.rtf"),
	pg.

macro(go) :-	gdoc:print_transcr_tables(eng:e,eng:e,rtf,2,_).
