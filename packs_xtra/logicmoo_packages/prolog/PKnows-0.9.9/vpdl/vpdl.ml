(*
  Theorem Prover for PDL-plus-variable-assignments (VPDL) based on the
  standard "pdlMark" prover from TWB.

  We have added a new program type [ ! vassign ] which assigns values
  to variables in the enclosed program, and a collection of new tableaux
  rule to push vassigns inside other modalities.  Finally, when the vassign
  reaches a raw formula term, it is applied as a simple string substitution.

  The result is an incredibly fragile syntax but it works for our purposes.

  We've also added simple term-based equality handling using a special rule.

*)

CONNECTIVES
[ "~";"&";"v";"->";"<->";"<";">";"[";"]";"U";"*";";";"?";"!";"<=";"=="]

GRAMMAR

vassign :=
      ATOM <= ATOM
    | vassign ! vassign
;;

program := 
      * program
    | ? formula
    | program U program
    | program ; program
    | ! vassign
    | ATOM
;;

formula :=
     ATOM | Verum | Falsum
    | formula == formula
    | formula & formula
    | formula v formula
    | formula -> formula
    | formula <-> formula
    | < program > formula 
    | [ program ] formula 
    | ~ formula
;;

expr := formula;;
END


open VpdlRewrite
open VpdlFunctions


HISTORIES
  HCore  : ListFormulaSet.olist := new ListFormulaSet.olist
END

VARIABLES
  uev : FormulaIntSet.set := new FormulaIntSet.set;
  mrk : bool := false
END

(*  Functions for use on lists-of-formulae  *)

let nnf = List.map nnf_term

let vsubs pattn = 
  match pattn with 
  | (vpattn,fpattn) -> List.map vsubs1 ( List.combine vpattn fpattn )
  ;;

let vmerge pattn = 
  match pattn with
  | (vpattn1,vpattn2,fpattn) -> List.map vmerge1 ( List.combine ( List.combine vpattn1 vpattn2 ) fpattn)
  ;;

let eq_terms pattn = 
  match pattn with
  | (tpattn1,tpattn2) -> List.map eq_terms1 ( List.combine tpattn1 tpattn2 )
  ;;

let neq_terms pattn = 
  match pattn with
  | (tpattn1,tpattn2) -> List.map neq_terms1 ( List.combine tpattn1 tpattn2 )
  ;;

let is_raw_fml f =
  is_raw_fml1 (List.hd f)

(*  Actual Tableaux Rules  *)

TABLEAU

  (*  Termination Rules  *)

  RULE Id { P } ; { ~ P } == Stop
  BACKTRACK [
      uev := uevundef ();
      mrk := true
  ]
  END

  RULE False { Falsum } == Stop
  BACKTRACK [
      uev := uevundef ();
      mrk := true
  ]
  END

  (*  Equality-handling rules  *)

  RULE Eq { A == B } ; Z === eq_terms ( A , B ) ; Z
  BACKTRACK [ uev := set_uev_All(uev@1, Z) ]
  END

  RULE NEq { ~ ( A == B ) } ; Z ===  neq_terms ( A , B ) ; Z
  BACKTRACK [ uev := set_uev_All(uev@1, Z) ]
  END

  (*  Standard Alpha Rules  *)

  RULE And { A & B } ; Z === A ; B ; Z 
  BACKTRACK [ uev := set_uev_All(uev@1, Z) ]
  END

  RULE UnionBox { [ A U B ] P } ; Z === [ A ] P ;  [ B ] P ; Z
  BACKTRACK [ uev := set_uev_All(uev@1, Z) ]
  END

  RULE SeqBox { [ A ; B ] P } ; Z === [ A ] [ B ] P ; Z
  BACKTRACK [ uev := set_uev_All(uev@1, Z) ]
  END

  RULE StarBox { [ * A ] P } ; Z === P ; [ A ] [ * A ] P ; Z 
  BACKTRACK [ uev := set_uev_All(uev@1, Z) ]
  END

  RULE TestDia { < ? F > P } ; Z === F ; P ; Z
  BACKTRACK [ uev := set_uev_Inh(uev@1, < ? F > P, P, Z) ]
  END

  RULE SeqDia { < A ; B > P } ; Z === < A > < B > P ; Z
  BACKTRACK [ uev := set_uev_Inh(uev@1, < A ; B > P, < A > < B > P, Z) ]
  END

  (*  Standard Beta Rules  *)

  RULE Or
        { P v Q } ; Z 
  ====================
      P ; Z ||| Q ; Z

  BRANCH [ [ doNextChild_disj(mrk@1, uev@1) ] ]
  BACKTRACK [
      uev := uev_disj_all(mrk@all, uev@all, Z);
      mrk := mrk_disj(mrk@all)
  ]
  END

  RULE TestBox
  { [ ? F ] P } ; Z === nnf ( ~ F ) ; Z ||| P ; Z

  BRANCH [ [ doNextChild_disj(mrk@1, uev@1) ] ]
  BACKTRACK [
      uev := uev_disj_all(mrk@all, uev@all, Z);
      mrk := mrk_disj(mrk@all)
  ]
  END

  RULE UnionDia
  { < A U B > P } ; Z === < A > P ; Z ||| < B > P ; Z

  BRANCH [ [ doNextChild_disj(mrk@1, uev@1) ] ]
  BACKTRACK [
      uev := uev_disj_union(mrk@all, uev@all, < A U B > P, < A > P, < B > P, Z);
      mrk := mrk_disj(mrk@all)
  ]
  END

  RULE StarDia
  { < * A > P } ; Z === P ; Z ||| < A > < * A > P ; Z 

  BRANCH [ [ doNextChild_disj(mrk@1, uev@1) ] ]
  BACKTRACK [
      uev := uev_disj_star(mrk@all, uev@all, < * A > P , P, < A > < * A > P, Z);
      mrk := mrk_disj(mrk@all)
  ]
  END

  (*  assignment-handling alpha rules  *)

  RULE VBox { [ ! V ] P } ; Z === < ! V > P ; Z
  BACKTRACK [ uev := set_uev_All(uev@1, Z) ]
  END

  RULE VRawDia { < ! V > < A > P } ; Z === < A > < ! V > P ; Z
  BACKTRACK [ uev := set_uev_Inh(uev@1, < ! V > < A > P, < A > < ! V > P, Z) ]
  END

  RULE VRawBox { < ! V > [ A ] P } ; Z === [ A ] < ! V > P ; Z
  BACKTRACK [ uev := set_uev_All(uev@1, Z) ]
  END

  RULE VRawFml { < ! V > P } ; Z === vsubs ( V , P ) ; Z
  COND   [ is_raw_fml(P) ]
  BACKTRACK [ uev := set_uev_Inh(uev@1, < ! V > P, P, Z) ]
  END

  RULE VMerge { < ! V > < ! X > P } ; Z === vmerge (V, X, P) ; Z
  BACKTRACK [ uev := set_uev_Inh(uev@1, < ! V > < ! X > P, vmerge (V, X, P), Z) ]
  END

  RULE VAnd { < ! V > ( A & B ) } ; Z === < ! V > A ; < ! V > B ; Z 
  BACKTRACK [ uev := set_uev_All(uev@1, Z) ]
  END

  RULE VUnionBox { < ! V > [ A U B ] P } ; Z === < ! V > [ A ] P ;  < ! V > [ B ] P ; Z
  BACKTRACK [ uev := set_uev_All(uev@1, Z) ]
  END

  RULE VSeqBox { < ! V > [ A ; B ] P } ; Z === < ! V > [ A ] [ B ] P ; Z
  BACKTRACK [ uev := set_uev_All(uev@1, Z) ]
  END

  RULE VStarBox { < ! V > [ * A ] P } ; Z === < ! V > P ; < ! V > [ A ] [ * A ] P ; Z 
  BACKTRACK [ uev := set_uev_All(uev@1, Z) ]
  END

  RULE VTestDia { < ! V > < ? F > P } ; Z === vsubs ( V , F ) ; < ! V > P ; Z
  BACKTRACK [ uev := set_uev_Inh(uev@1, < ! V > < ? F > P, < ! V > P, Z) ]
  END

  RULE VSeqDia { < ! V > < A ; B > P } ; Z === < ! V > < A > < B > P ; Z
  BACKTRACK [ uev := set_uev_Inh(uev@1, < ! V > < A ; B > P, < ! V > < A > < B > P, Z) ]
  END

  (*  assignment-handling beta rules  *)

  RULE VOr
        { < ! V > ( P v Q ) } ; Z 
  ======================================
      < ! V > P ; Z ||| < ! V > Q ; Z

  BRANCH [ [ doNextChild_disj(mrk@1, uev@1) ] ]
  BACKTRACK [
      uev := uev_disj_all(mrk@all, uev@all, Z);
      mrk := mrk_disj(mrk@all)
  ]
  END

  RULE VTestBox
  { < ! V > [ ? F ] P } ; Z === nnf ( vsubs ( V , ~ F ) ) ; Z ||| < ! V > P ; Z

  BRANCH [ [ doNextChild_disj(mrk@1, uev@1) ] ]
  BACKTRACK [
      uev := uev_disj_all(mrk@all, uev@all, Z);
      mrk := mrk_disj(mrk@all)
  ]
  END

  RULE VUnionDia
  { < ! V > < A U B > P } ; Z === < ! V > < A > P ; Z ||| < ! V > < B > P ; Z

  BRANCH [ [ doNextChild_disj(mrk@1, uev@1) ] ]
  BACKTRACK [
      uev := uev_disj_union(mrk@all, uev@all, < ! V > < A U B > P, < ! V > < A > P, < ! V > < B > P, Z);
      mrk := mrk_disj(mrk@all)
  ]
  END

  RULE VStarDia
  { < ! V > < * A > P } ; Z === < ! V > P ; Z ||| < ! V > < A > < * A > P ; Z 

  BRANCH [ [ doNextChild_disj(mrk@1, uev@1) ] ]
  BACKTRACK [
      uev := uev_disj_star(mrk@all, uev@all, < ! V > < * A > P , < ! V > P, < ! V > < A > < * A > P, Z);
      mrk := mrk_disj(mrk@all)
  ]
  END

  (*  looping and modality-removing rules  *)

  RULE K 
  { < A > P } ; [ A ] Y ; < B > E ; [ C ] F ; Z
  ==============================================
       P ; Y ||| [ A ] Y ; < B > E ; [ C ] F

  COND   [ loop_check(P, Y, HCore) ]
  ACTION [ [ HCore := push(P, Y, HCore) ] ; [] ]
  BRANCH [ [ test_ext(mrk@1, uev@1, P, HCore) ] ]
  BACKTRACK [
      uev := uev_ext(mrk@all, uev@all, < A > P, P);
      mrk := mrk_ext(mrk@all)
  ]
  CACHE := true
  END

  RULE Loop
       < A > X ; [ B ] Y
       ==================
             Stop

  BACKTRACK [
      uev := uev_loop(< A > X, [ B ] Y, HCore);
      mrk := false
  ]
  CACHE := true
  END

END

STRATEGY := 
  let stop = tactic (False ! Id) in 
  let eq = tactic ( Eq ! NEq ) in
  let vnorm = tactic (VBox ! VRawDia ! VRawBox ! VMerge ! VRawFml) in
  let valpha = tactic (VAnd ! VUnionBox ! VSeqDia ! VSeqBox ! VTestDia ! VStarBox) in
  let vbeta = tactic (VOr ! VUnionDia ! VStarDia ! VTestBox ) in
  let alpha = tactic (And ! UnionBox ! SeqDia ! SeqBox ! TestDia ! StarBox) in
  let beta = tactic (Or ! TestBox ! UnionDia ! StarDia) in
  let sat = tactic (stop ! eq ! vnorm ! valpha ! alpha ! vbeta ! beta)
  in tactic ((sat ! K ! Loop)*)

let exit = function
  | true -> "Closed"
  | false -> "Open"

PP := List.map nnf_snf_term
NEG := List.map neg_term
EXIT := exit (mrk@1)

MAIN
