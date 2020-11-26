source Vpdl

module FormulaSet = TwbSet.Make(
  struct
    type t = formula
    let to_string = formula_printer
    let copy s = s
  end
 )

module FormulaIntSet = TwbSet.Make(
  struct
    type t = ( formula * formula * int )
    let to_string (f1,f2,i) =
      Printf.sprintf "(%i,%s,%s)" i (formula_printer f1) (formula_printer f2)
    let copy s = s
  end
 )

module ListFormulaSet = TwbList.Make(
  struct
    type t = ( formula * FormulaSet.set )
    let to_string (f,s) = Printf.sprintf "(%s,%s)" (formula_printer f) s#to_string
    let copy (f,s) = (f, s#copy)
  end
 )


let uevundef () = new FormulaIntSet.set


let uevfilter f uev =
  let lst = List.fold_left f [] uev#elements in
  new FormulaIntSet.set#addlist lst

let set_uev_All(uev, z) =
  let filter res t =
    match t with
    | (x1, x2, i) when List.mem x1 z -> t::res
    | _ -> res
  in
  uevfilter filter uev

let set_uev_Inh (uev, f, df, z) =
  let f = List.hd f in
  let df = List.hd df in
  let filter res (x1, x2, i) =
    let res = if x1 = df then (f, x2, i)::res else res in
    if List.mem x1 z then (x1, x2, i)::res else res
  in
  uevfilter filter uev

let set_uev_Star (uev, f, df, z) =
  let f = List.hd f in
  let df = List.hd df in
  let filter res (x1, x2, i) =
    let res = if x1 = df && not(x2 = f) then (f, x2, i)::res else res in
    if List.mem x1 z then (x1, x2, i)::res else res
  in
  uevfilter filter uev


let doNextChild_disj (mrk, uev) = mrk || (not uev#is_empty)

let takemin uev1 uev2 =
  let l2 = uev2#elements in
  let filter res (x1, x2, i) =
    try
      let (_, _, j) = List.find (fun (y1, y2, _) -> y1 = x1 && y2 = x2) l2 in
      (x1, x2, min i j)::res
    with Not_found -> res
  in
  uevfilter filter uev1

let uev_disj_all (mrks, uevs, z) =
  match (mrks, uevs) with
  | ([mrk1], [uev1]) -> uevundef()
  | ([mrk1; mrk2], _) when mrk1 && mrk2 -> uevundef ()
  | ([mrk1; mrk2], [uev1; _]) when (not mrk1) && mrk2 -> set_uev_All(uev1, z)
  | ([mrk1; mrk2], [_; uev2]) when mrk1 && (not mrk2) -> set_uev_All(uev2, z)
  | (_, [uev1; uev2]) -> takemin (set_uev_All(uev1, z)) (set_uev_All(uev2, z))
  | _ -> failwith "uev_disj_or"

let uev_disj_union (mrks, uevs, f, df1, df2, z) =
  match (mrks, uevs) with
  | ([mrk1], [uev1]) -> uevundef()
  | ([mrk1; mrk2], _) when mrk1 && mrk2 -> uevundef ()
  | ([mrk1; mrk2], [uev1; _]) when (not mrk1) && mrk2 -> set_uev_Inh(uev1, f, df1, z)
  | ([mrk1; mrk2], [_; uev2]) when mrk1 && (not mrk2) -> set_uev_Inh(uev2, f, df2, z)
  | (_, [uev1; uev2]) -> takemin (set_uev_Inh(uev1, f, df1, z)) (set_uev_Inh(uev2, f, df2, z))
  | _ -> failwith "uev_disj_union"

let uev_disj_star (mrks, uevs, f, df1, df2, z) =
  match (mrks, uevs) with
  | ([mrk1], [uev1]) -> uevundef()
  | ([mrk1; mrk2], _) when mrk1 && mrk2 -> uevundef ()
  | ([mrk1; mrk2], [uev1; _]) when (not mrk1) && mrk2 -> set_uev_Star(uev1, f, df1, z)
  | ([mrk1; mrk2], [_; uev2]) when mrk1 && (not mrk2) -> set_uev_Inh(uev2, f, df2, z)
  | (_, [uev1; uev2]) -> takemin (set_uev_Star(uev1, f, df1, z)) (set_uev_Inh(uev2, f, df2, z))
  | _ -> failwith "uev_disj_star"

let mrk_disj mrks =
  if List.length mrks = 1 then false
  else List.nth mrks 1


let loop_check (diax, box, hcore) =
  let dia = List.hd diax in
  let nodeset = (new FormulaSet.set)#addlist (dia::box) in
  not(List.exists (fun (f, s) -> f = dia && nodeset#is_equal s) hcore#elements)

let push (diax, box, hcore) = 
  let nodeset = (new FormulaSet.set)#addlist (diax@box) 
  in hcore#add (List.hd diax, nodeset)

let test_ext (mrk, uev, diax, hcore) =
  if mrk then false
  else
    let dia = List.hd diax in
    let len = hcore#length in
    let chkloop (x1, x2, i) = (x1 = dia) && (i >= len) in
    not (List.exists chkloop uev#elements)

let uev_ext (mrks, uevs, fulldiax, diax) =
  if List.length mrks = 1 then uevundef()
  else
    let mrk2 = List.nth mrks 1 in
    if mrk2 then uevundef ()
    else
      let fulldia = List.hd fulldiax in
      let dia = List.hd diax in
      let uev1 = (List.hd uevs) in
      let uev2 = (List.nth uevs 1) in
      let filter res t =
        match t with
        | (x1, x2, i) when x1 = dia -> (fulldia, x2, i)::res
        | _ -> res
      in
      let uev = uevfilter filter uev1 in
      uev#addlist uev2#elements
        
let mrk_ext mrks = 
  if List.length mrks = 1 then true
  else List.nth mrks 1


let uev_loop (diax, box, hcore) = 
  if diax = [] then new FormulaIntSet.set
  else
    let hcore = hcore#elements in
    let filterbox a res = function
      | formula ( [ x ] p ) when x = a -> p::res
      | _ -> res
    in
    let rec index p nodeset hcore n = 
      match hcore with
      | [] -> failwith "index"
      | (f, s)::tl -> 
          if f = p && nodeset#is_equal s then n
          else index p nodeset tl (n+1)
    in
    let getindex = function
      | formula ( < a >  p ) as f ->
          let boxa = List.fold_left (filterbox a) [] box in
          let nodeset = (new FormulaSet.set)#addlist (p::boxa) in
          let i = index p nodeset hcore 0 in
          (f, i)
      | _ ->  failwith "uev_loop2"
    in
    let rec getEvent f i = function
      | formula ( < * a > p ) as df -> (f, df, i)::(getEvent f i p)
      | formula ( < a > p ) -> getEvent f i p
      | _ -> []
    in
    let dialist = List.map getindex diax in
    let lst = List.fold_left (fun res (f, i) -> (getEvent f i f) @ res) [] dialist in
    new FormulaIntSet.set#addlist lst

let filterbox (a, s) =
  let a = List.hd a in
  let fltrbx res = function
    | formula ( [ x ] p ) when x = a -> p::res
    | _ -> res
  in
  List.fold_left fltrbx [] s
