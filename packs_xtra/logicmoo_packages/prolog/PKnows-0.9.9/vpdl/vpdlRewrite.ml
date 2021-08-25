source Vpdl

let neg_term = function formula ( a ) -> formula ( ~ a )

let rec nnf_term = function
  | formula ( a & b ) -> formula ( {nnf_term a} & {nnf_term b} )
  | formula ( ~ ( a & b ) ) ->
      formula ( {nnf_term formula ( ~ a )} v {nnf_term formula ( ~ b )} )
  | formula ( a v b ) -> formula ({nnf_term a} v {nnf_term b})
  | formula ( ~ ( a v b ) ) ->
      formula ( {nnf_term formula ( ~ a )} & {nnf_term formula ( ~ b )} )
  | formula ( a <-> b ) ->
      formula ( {nnf_term formula ( a -> b )} & {nnf_term formula ( b -> a )} )
  | formula ( ~ ( a <-> b ) ) ->
      formula ( {nnf_term formula ( ~ (a -> b) )} v {nnf_term formula ( ~ (b -> a) )} )
  | formula ( a -> b ) -> nnf_term formula ( (~ a) v b )
  | formula ( ~ (a -> b) ) -> nnf_term formula ( a & (~ b) )
  | formula ( ~ ~ a ) -> nnf_term a
  | formula ( < p > a ) -> formula ( < {nnf_prog p} > {nnf_term a} )
  | formula ( ~ ( < p > a ) ) -> formula ( [ {nnf_prog p} ] {nnf_term ( formula ( ~ a ) )} )
  | formula ( [ p ] a ) -> formula ( [ {nnf_prog p} ] {nnf_term a} )
  | formula ( ~ ( [ p ] a ) ) -> formula ( < {nnf_prog p} > {nnf_term ( formula ( ~ a ) )} )
  | formula ( ~ ( a == b ) ) as f -> f
  | formula ( a == b ) as f -> f
  | formula (Verum) as f -> f
  | formula (Falsum) as f -> f
  | formula (~ Verum) -> formula (Falsum)
  | formula (~ Falsum) -> formula (Verum)
  | formula ( ~ A ) as f -> f
  | formula ( A ) as f -> f
and nnf_prog = function
  | program ( * a ) -> program ( * {nnf_prog a} )
  | program ( ? f ) -> program ( ? {nnf_term f} )
  | program ( a U b ) -> program ( {nnf_prog a} U {nnf_prog b} )
  | program ( a ; b ) -> program ( {nnf_prog a} ; {nnf_prog b} )
  | _ as p -> p

let rec eps_free = function
  | program ( * a ) -> false
  | program ( ? f ) -> false
  | program ( ! _ ) -> false
  | program ( a U b ) -> (eps_free a) && (eps_free b)
  | program ( a ; b ) -> (eps_free a) || (eps_free b)
  | _ -> true

let rec snf_term = function
  | formula ( a & b ) -> formula ( {snf_term a} & {snf_term b} )
  | formula ( a v b ) -> formula ({snf_term a} v {snf_term b})
  | formula ( < p > a ) -> formula ( < {snf_prog p} > {snf_term a} )
  | formula ( [ p ] a ) -> formula ( [ {snf_prog p} ] {snf_term a} )
  | formula ( a ) as f -> f
and snf_prog = function
  | program ( * a ) ->
      begin 
        match ef_prog a with
        | None -> program ( ? Verum )
        | Some a1 -> program ( * a1 )
      end
  | program ( ? f ) -> program ( ? {snf_term f} )
  | program ( a U b ) -> program ( {snf_prog a} U {snf_prog b} )
  | program ( a ; b ) -> program ( {snf_prog a} ; {snf_prog b} )
  | _ as p -> p
and ef_prog = function
  | program ( * a ) ->
      begin
        match ef_prog a with
        | None -> None
        | Some a1 -> Some (program ( a1 ; ( * a1 ) ))
      end
  | program ( ? f ) -> None
  | program ( ! _ ) -> None
  | program ( a U b ) ->
      begin
        match (ef_prog a, ef_prog b) with
        | (None, ob1) -> ob1
        | (oa1, None) -> oa1
        | (Some a1, Some b1) -> Some (program ( a1 U b1 ))
      end
  | program ( a ; b ) ->
      let sa = snf_prog a in
      let sb = snf_prog b in
      if eps_free a || eps_free b then Some (program ( sa ; sb ))
      else
        begin
          match (ef_prog a, ef_prog b) with
          | (None, None) -> None
          | (None, Some b1) -> Some (program ( sa ; b1 ))
          | (Some a1, None) -> Some (program ( a1 ; sb ))
          | (Some a1, Some b1) -> Some (program ( ( a1 ; sb ) U ( sa ; b1 ) ))
        end
  | _ as p -> Some p

let nnf_snf_term f =
  let nnf = nnf_term f in
  let snf = snf_term nnf in
  snf


(*  The actual subtitution mechanism - it's just string replacement  *)
let vsubs_str oldval newval s  =
    Str.global_replace (Str.regexp_string oldval) newval s

(*  vsubs wrapper operating on fml atoms  *)
let vsubs_atom (`Atom text,`Atom name,`Atom value) =
    `Atom (vsubs_str name value text)


(*  push variable substitutions down to atomic formulae

    we assume that we only subs into raw formulae, not formulae with
    modalities - the other cases are just here to keep the typechecker happy,
    and they leave the formula unmodified 
*)
let rec vsubs_do = function
  | ( formula ( a & b ) , o,n ) -> formula ( { vsubs_do (a,o,n) } & { vsubs_do (b,o,n) } )
  | ( formula ( a v b ) , o,n ) -> formula ( { vsubs_do (a,o,n) } v { vsubs_do (b,o,n) } )
  | ( formula ( ~ a ) , o,n ) -> formula ( ~ { vsubs_do (a,o,n) } )
  | ( formula ( a -> b ) , o,n ) ->  formula ( { vsubs_do (a,o,n) } -> { vsubs_do (b,o,n) } )
  | ( formula ( a <-> b ) , o,n ) ->  formula ( { vsubs_do (a,o,n) } <-> { vsubs_do (b,o,n) } )
  | ( formula ( < p > a ) as f , o,n ) -> f
  | ( formula ( [ p ] a ) as f , o,n ) -> f
  | ( formula ( a == b ) , o,n ) -> formula ( { vsubs_do(a,o,n) } == { vsubs_do(b,o,n) } )
  | ( formula (Verum) as f, o,n ) -> f
  | ( formula (Falsum) as f, o,n ) -> f
  | ( formula (A) as f , o,n ) -> formula ( { vsubs_atom(f,o,n) } )


(*  recursively handling multiple var bindings in a single modality  *)
let rec vsubs1 = function
  | ( vassign ( A <= B ) , f ) -> formula ( { vsubs_do(f,a,b) } )
  | ( vassign ( a ! b ) , f ) -> formula ( { vsubs1( b , vsubs1(a,f) ) } )

(*  check whether a vassign already contains a given var name  *)
let rec vcontains = function
  | ( vassign ( A <= B ) , vv ) -> if vv=a then true else false
  | ( vassign ( a ! b ), vv ) -> if vcontains(a,vv) then true
                                 else vcontains(b,vv)

(*  merge a base vassign into the given vassign  *)
let rec vmerge_do = function
  | ( vassign ( A <= B ) as v1 , v2 ) -> 
                                   let merged = vassign ( v2 ! v1)
                                   in if vcontains(v2,a) then v2 else merged
  | ( vassign (a ! b ) , v2 ) -> vmerge_do(b,vmerge_do(a,v2))

(*  merge two vassign modalities, applying the result to given fml  *)
let rec vmerge1 = function
  | ( ( v1, v2 ), f) -> let merged = vassign ( { vmerge_do(v1,v2) } )
                        in formula ( < ! merged > f )

(*  equality testing of terms  *)
let eq_terms1 = function
  | ( t1 , t2 ) -> if t1 = t2 then formula ( Verum ) else formula ( Falsum )
let neq_terms1 = function
  | ( t1 , t2 ) -> if t1 = t2 then formula ( Falsum ) else formula ( Verum )

(*  check for application to raw formulae  *)
let is_raw_fml1 = function
  | formula ( [ _ ] _ ) -> false
  | formula ( < _ > _ ) -> false
  | _ -> true

