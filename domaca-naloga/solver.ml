type available = { loc : int * int; possible : int list } 

(* TODO: tip stanja ustrezno popravite, saj boste med reševanjem zaradi učinkovitosti
   želeli imeti še kakšno dodatno informacijo *)

type state = { problem : Model.problem; current_grid : int option Model.grid;  mutable moznosti : available list}


let print_state (state : state) : unit =
  Model.print_grid
    (function None -> "?" | Some digit -> string_of_int digit)
    state.current_grid

type response = Solved of Model.solution | Unsolved of state | Fail of state


(* ------------pomožne funkcije za iskanje možnsti v celici----------------------------------------------- *)
let rec remove elt list = match list with
  |x::xs -> if x = elt then remove elt xs else x :: remove elt xs
  |[] -> []

let vse_moznosti = [1;2;3;4;5;6;7;8;9]

let int_option_list_to_int_list (list : int option list) = 
  let rec aux acc lst = match lst with 
    |[] -> acc 
    |x::xs -> 
      match x with
        |Some x -> aux (x :: acc) xs 
        |None -> aux acc xs
  in
  aux [] list

(* l2 brez l1 *)
let razlika_seznamov l1 l2 = 
  let rec aux razlika list1 list2 = match list1 with
    |[] -> razlika
    |x::xs -> aux (remove x razlika) xs list2
  in
  aux l2 l1 l2

let presek_seznamov l1 l2 =
  razlika_seznamov (razlika_seznamov l1 l2) l2 

(* nam vrne seznam števk ki v tistem stolpcu/vrstici/boxu še niso porabljene *)
let neporabljene (arr : int option Array.t) (moznosti : int list) =
  let porabljene = int_option_list_to_int_list (Array.to_list arr) in
  razlika_seznamov porabljene moznosti

(* pomozne funkcije za racunanje stevilke boxa *)
let row_level row_ind = match row_ind with
  |0|1|2 -> 0
  |3|4|5 -> 3
  |6|7|8 -> 6
  |_ -> failwith "That row does not exist"
let col_level col_ind = match col_ind with
  |0|1|2 -> 0
  |3|4|5 -> 1
  |6|7|8 -> 2
  |_ -> failwith "That column does not exist"
let box_number (r, c) =
  (row_level r) + (col_level c)

let mozne_stevke_v_celici ((r, c) as loc) grid =
  match grid.(r).(c) with 
  |Some x -> []
  |None -> 
    let neporabljene_v_vrstici = neporabljene (Model.get_row grid r) vse_moznosti in 
    let neporabljene_v_stoplcu = neporabljene (Model.get_column grid c) vse_moznosti in 
    let neporabljene_v_boxu = neporabljene (Model.get_box grid (box_number loc)) vse_moznosti in
    match (neporabljene_v_vrstici, neporabljene_v_stoplcu, neporabljene_v_boxu) with
      |([], _, _) | (_, [], _) | (_, _, []) -> []
      |([x], _, _) | (_, [x], _) | (_, _, [x]) ->  [x]
      |_ ->
      presek_seznamov neporabljene_v_vrstici (presek_seznamov neporabljene_v_stoplcu neporabljene_v_boxu)

(* funkcije za initialize_state *)

let uredi_po_dolzini (list : available list) =
  let razdalja x y = List.length x.possible - List.length y.possible in 
  List.sort razdalja list

let zapisi_seznam_moznosti (grid : int option Model.grid) = 
  let rec aux (i, j) grid acc =
    match grid.(i).(j) with 
    |Some x ->
      (
      if (i, j) = (8, 8) then acc
      else
          if j < 8 then aux (i, (j + 1)) grid acc else 
          if i < 8 then aux ((i + 1), 0) grid acc else acc
      )
    |None -> 
      (
      let moznosti = mozne_stevke_v_celici (i, j) grid in
      let available = {loc = (i, j) ; possible = moznosti} in
      (
        if (i, j) = (8, 8) then (available :: acc)
        else
            if j < 8 then aux (i, (j + 1)) grid (available :: acc) else 
            if i < 8 then aux ((i + 1), 0) grid (available :: acc) else acc
        )
      )
  in 
  uredi_po_dolzini (aux (0, 0) grid [])

let validate_state (state : state) : response =
  let unsolved =
    Array.exists (Array.exists Option.is_none) state.current_grid
  in
  if unsolved then Unsolved state
  else
    (* Option.get ne bo sprožil izjeme, ker so vse vrednosti v mreži oblike Some x *)
    let solution = Model.map_grid Option.get state.current_grid in
    if Model.is_valid_solution state.problem solution then Solved solution
    else Fail state


(* funkciji ki dolocita vrednost celice *)

let prepisi_na_novo_mesto f grid =
  let grid' = Model.copy_grid grid in
  let () = f grid' in
  grid'
let doloci_stevko stevka (i, j) (grid : int option Model.grid) =
  (grid.(i).(j) <- (Some stevka))

let z_doloceno_stevko stevka (i, j) (grid : int option Model.grid) = 
  prepisi_na_novo_mesto (doloci_stevko stevka (i, j)) grid

(* ------------------------------------------------------------------------------------ *)

let dopolni_trivialne_resitve state =
  let rec aux grid moznosti = 
    match moznosti with 
    |[] -> grid
    |x::xs ->
      match x.possible with 
      |[y] -> aux (z_doloceno_stevko y (x.loc) grid) xs
      |_ -> aux grid xs
    in 
  let new_grid = aux state.current_grid state.moznosti in
    {
    problem = state.problem;
    current_grid = new_grid;
    moznosti = zapisi_seznam_moznosti new_grid
    }

let pocisti state = 
  let rec aux original =
    match original.moznosti with 
    |[] -> original
    |x::xs -> 
      match x.possible with 
      |[a] -> aux (dopolni_trivialne_resitve original)
      |_ -> original
    in 
    aux state

let initialize_state (problem : Model.problem) : state =
  pocisti {current_grid = Model.copy_grid problem.initial_grid; problem = problem; moznosti = zapisi_seznam_moznosti (problem.initial_grid)}

(* v trenutnem stanju poišče hipotezo, glede katere
   se je treba odločiti. Če ta obstaja, stanje razveji na dve stanji:
   v prvem predpostavi, da hipoteza velja, v drugem pa ravno obratno.
   Če bo vaš algoritem najprej poizkusil prvo možnost, vam morda pri drugi
   za začetek ni treba zapravljati preveč časa, saj ne bo nujno prišla v poštev. *)

let branch_state (state : state) : (state * state) option =
  match state.moznosti with 
  |[] -> None
  |x::xs -> 
   match x.possible with 
   |[] -> None
   |y::ys ->
    let new_grid = (z_doloceno_stevko y x.loc state.current_grid) in 
    let first = {
      problem = state.problem;
      current_grid = new_grid;
      moznosti = (zapisi_seznam_moznosti (new_grid))
     }
    in 
    let second = {
      problem = state.problem;
      current_grid = state.current_grid;
      moznosti = {loc = x.loc; possible = ys} :: xs
    }
  in 
  Some  (pocisti first, pocisti second)


(* pogledamo, če trenutno stanje vodi do rešitve *)
let rec solve_state (state : state) =
  (* uveljavimo trenutne omejitve in pogledamo, kam smo prišli *)
  match validate_state state with
  | Solved solution ->
      (* če smo našli rešitev, končamo *)
      Some solution
  | Fail fail ->
      (* prav tako končamo, če smo odkrili, da rešitev ni *)
      None
  | Unsolved state ->
      (* če še nismo končali, raziščemo stanje, v katerem smo končali *)
      explore_state state

and explore_state (state : state) =
  (* pri raziskovanju najprej pogledamo, ali lahko trenutno stanje razvejimo *)
  match branch_state state with
  | None ->
      (* če stanja ne moremo razvejiti, ga ne moremo raziskati *)
      None
  | Some (st1, st2) -> (
      (* če stanje lahko razvejimo na dve možnosti, poizkusimo prvo *)
      match solve_state st1 with
      | Some solution ->
          (* če prva možnost vodi do rešitve, do nje vodi tudi prvotno stanje *)
          Some solution
      | None ->
          (* če prva možnost ne vodi do rešitve, raziščemo še drugo možnost *)
          solve_state st2 )

let solve_problem (problem : Model.problem) =
  problem |> initialize_state |> solve_state