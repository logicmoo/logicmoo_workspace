// if s = 12 then s := ew.
// while total > seuil do if taxe < rev then while ind = sas do ds := sas.
compile :-
  readSentence(t_codeSource),
  compileInstr(t_codeSource, ["."], t_codeCible),
  afficheCodeCible(t_codeCible).

compileInstr(["if"|l], l_rest, [c_cond, jzero(e), c_instrThen, etiq(e)]) :-
  compileCond(l, ["then"|l1], c_cond),
  compileInstr(l1, l_rest, c_instrThen), !.

compileInstr(["while"|l], l_rest, [etiq(e1), c_cond, jzero(e2), c_instr, jump(e1), etiq(e2)]) :-
  compileCond(l, ["do"|l1], c_cond),
  compileInstr(l1, l_rest, c_instr), !.

compileInstr([v, ":", "=", v1|l], l, store(v1, v)).

compileCond([a1, v_oper, a2|l], l, t_oper(a1, a2)) :-
  membre([v_oper, t_oper], [["=", eql], [">", sup], ["<", inf]]).

afficheCodeCible([]).
afficheCodeCible([x|y]) :-
  v is "aminePlatform.util.Term":isTerm(x),
  v = true,
  writeln(x),
  afficheCodeCible(y), !.
afficheCodeCible([x|y]) :-
  afficheCodeCible(x),
  afficheCodeCible(y), !.