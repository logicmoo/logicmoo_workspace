
:-use_module(library(jpl)).

:-dynamic(m_prologAlien/1).
prologAlien(PL):-m_prologAlien(PL),!.
prologAlien(PL):-
      jpl_get_static('com.cyc.tool.subl.jrtl.nativeCode.subLisp.PrologAlien','me',PL),!,
      assert(m_prologAlien(PL)).
