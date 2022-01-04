type available = { loc : int * int; possible : int list } 

(* TODO: tip stanja ustrezno popravite, saj boste med reševanjem zaradi učinkovitosti
   želeli imeti še kakšno dodatno informacijo *)
type state = { problem : Model.problem; current_grid : int option Model.grid }


let print_state (state : state) : unit =
  Model.print_grid
    (function None -> "?" | Some digit -> string_of_int digit)
    state.current_grid

type response = Solved of Model.solution | Unsolved of state | Fail of state

let initialize_state (problem : Model.problem) : state =
  { current_grid = Model.copy_grid problem.initial_grid; problem }

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

(* ------------pomožne funkcije----------------------------------------------------------------------------------------------------------------------- *)
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


(* nam vrne seznam števk ki v tistme stolpcu/vrstici/boxu še niso porabljene *)
let neporabljene (arr : int option Array.t) (moznosti : int list) =
  let porabljene = int_option_list_to_int_list (Array.to_list arr) in
  razlika_seznamov porabljene moznosti

(* pomozne funkcije za racunanje stevilke box *)
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
  let neporabljene_v_vrstici = neporabljene (Model.get_row grid r) vse_moznosti in 
  let neporabljene_v_stoplcu = neporabljene (Model.get_column grid c) vse_moznosti in 
  let neporabljene_v_boxu = neporabljene (Model.get_box grid (box_number loc)) vse_moznosti in
  match (neporabljene_v_vrstici, neporabljene_v_stoplcu, neporabljene_v_boxu) with
    |([], _, _) | (_, [], _) | (_, _, []) -> []
    |([x], _, _) | (_, [x], _) | (_, _, [x]) ->  [x]
    |_ -> presek_seznamov neporabljene_v_vrstici (presek_seznamov neporabljene_v_stoplcu neporabljene_v_boxu)


(* funkcije za dopolnjevanje celic ki imajo samo eno moznost *)
let dopolni_trivialno_celico (celica : int option) moznosti = 
  match celica with 
    |Some x -> Some x 
    |None -> 
        match moznosti with 
          |[x] -> Some x
          |_ -> None

let rec int_list_to_int_option_list = function 
  |[] -> [] 
  |x::xs -> (Some x) :: int_list_to_int_option_list xs

let dopolni_trivialne_resitve grid = 
  Array.init 9 
  (fun r -> 
    Array.init 9 
    (fun c -> 
      let celica = grid.(r).(c) in
      let moznosti = mozne_stevke_v_celici (r, c) grid in
      dopolni_trivialno_celico celica moznosti
      )
    )

(* pomozne funkcije za razvejanje *)

(* popravi funkcijo s tem da uporabis type available *)
let najdi_celico_z_dvema_moznostma grid =
  let rec aux (i : int) (j : int ) = 
    let moznosti = mozne_stevke_v_celici (i, j) grid in
    let st = List.length moznosti in
    match st with 
      |2 -> ((i, j), moznosti)
      |_ -> if j < 7 then aux i (j + 1)  else aux (i + 1) 0
  in
  aux 0 0

(* vzame lokacijo celice kateri zelimo zamenjati stevko na
 prvem ali drugem mestu seznama moznosti (odlocamo se med dvema, saj zelimo razvejati stanje).
  vrne unit!! torej samo zamenja in nic ne vrne *)
let doloci_stevko (r, c) mesto moznosti (grid : int option Model.grid) = match moznosti with 
    |x::y::[] -> 
     ( match mesto with 
        |1 -> (grid.(r).(c) <- Some x )
        |2 -> (grid.(r).(c) <- Some y )
        |_ -> failwith "V tej celici imam le dve možnosti, zato izberi 1 ali 2.")
    |_ -> failwith "Izbirati znam le med dvema možnostma"

let prepisi_na_novo_mesto f grid =
  let grid' = Model.copy_grid grid in
  let () = f grid' in
  grid'
  
(* zamenja stevko in vrne mrezo z novim podatkom in s tem ne spremeni stare *)
let z_doloceno_stevko (r, c) (mesto : int) (moznosti : int list) grid = 
  prepisi_na_novo_mesto (doloci_stevko (r, c) mesto moznosti) grid 

(* ---------------------------------------------------------------------------------------------------------------------------------------------------- *)

let branch_state (state : state) : (state * state) option =
  (* TODO: Pripravite funkcijo, ki v trenutnem stanju poišče hipotezo, glede katere
     se je treba odločiti. Če ta obstaja, stanje razveji na dve stanji:
     v prvem predpostavi, da hipoteza velja, v drugem pa ravno obratno.
     Če bo vaš algoritem najprej poizkusil prvo možnost, vam morda pri drugi
     za začetek ni treba zapravljati preveč časa, saj ne bo nujno prišla v poštev. *)
  let grid = state.current_grid in
  let ((i, j) , moznosti) = najdi_celico_z_dvema_moznostma grid in 
  let prva_moznost = z_doloceno_stevko (i, j) 1 moznosti grid in
  let prvo_stanje = {problem = state.problem; current_grid =  prva_moznost} in
  (* popravi!!! upostevaj predlog iz prejsnjega komentarja - z if stavkom in validate state? *)
  let druga_moznost = z_doloceno_stevko (i, j) 2 moznosti grid in
  let drugo_stanje = {problem = state.problem; current_grid =  druga_moznost} in
  Some (prvo_stanje, drugo_stanje)


(* pogledamo, če trenutno stanje vodi do rešitve *)
let rec solve_state (state : state) =
  (* uveljavimo trenutne omejitve in pogledamo, kam smo prišli *)
  (* TODO: na tej točki je stanje smiselno počistiti in zožiti možne rešitve *)
  let new_grid = dopolni_trivialne_resitve state.current_grid in
  let new_state = {problem = state.problem; current_grid = new_grid} in 
  match validate_state new_state with
  | Solved solution ->
      (* če smo našli rešitev, končamo *)
      Some solution
  | Fail fail ->
      (* prav tako končamo, če smo odkrili, da rešitev ni *)
      None
  | Unsolved state' ->
      (* če še nismo končali, raziščemo stanje, v katerem smo končali *)
      explore_state state'

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
