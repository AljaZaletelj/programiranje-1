(* Pomožni tip, ki predstavlja mrežo .... *)

type 'a grid = 'a Array.t Array.t

(* Funkcije za prikaz mreže.
  Te definiramo najprej, da si lahko z njimi pomagamo pri iskanju napak. *)

(* Razbije seznam [lst] v seznam seznamov dolžine [size] *)
let chunkify size lst =
  let rec aux chunk chunks n lst =
    match (n, lst) with
    | _, [] when chunk = [] -> List.rev chunks
    | _, [] -> List.rev (List.rev chunk :: chunks)
    | 0, _ :: _ -> aux [] (List.rev chunk :: chunks) size lst
    | _, x :: xs -> aux (x :: chunk) chunks (n - 1) xs
  in
  aux [] [] size lst

let string_of_list string_of_element sep lst =
  lst |> List.map string_of_element |> String.concat sep

let string_of_nested_list string_of_element inner_sep outer_sep =
  string_of_list (string_of_list string_of_element inner_sep) outer_sep

let string_of_row string_of_cell row =
  let string_of_cells =
    row |> Array.to_list |> chunkify 3
    |> string_of_nested_list string_of_cell "" "│"
  in
  "┃" ^ string_of_cells ^ "┃\n"

let print_grid string_of_cell grid =
  let ln = "───" in
  let big = "━━━" in
  let divider = "┠" ^ ln ^ "┼" ^ ln ^ "┼" ^ ln ^ "┨\n" in
  let row_blocks =
    grid |> Array.to_list |> chunkify 3
    |> string_of_nested_list (string_of_row string_of_cell) "" divider
  in
  Printf.printf "┏%s┯%s┯%s┓\n" big big big;
  Printf.printf "%s" row_blocks;
  Printf.printf "┗%s┷%s┷%s┛\n" big big big

(* Funkcije za dostopanje do elementov mreže *)

let get_row (grid : 'a grid) (row_ind : int) = grid.(row_ind)

let rows grid = List.init 9 (get_row grid)

let get_column (grid : 'a grid) (col_ind : int) =
  Array.init 9 (fun row_ind -> grid.(row_ind).(col_ind))

let columns grid = List.init 9 (get_column grid)

let get_box (grid : 'a grid) (box_ind : int) =
  let (first_row_ind, first_col_ind ) =  match box_ind with
    |0 -> (0,0)
    |1 -> (0,3)
    |2 -> (0,6)
    |3 -> (3,0)
    |4 -> (3,3)
    |5 -> (3,6)
    |6 -> (6,0)
    |7 -> (6,3)
    |8 -> (6,6)
    |_ -> failwith "Kvadratek s to številko ne obstaja, saj je to sudoku s 9 * 9 mrežo!"
  in
  let first = Array.init 3 (fun ind -> grid.(first_row_ind).(first_col_ind + ind))
  in
  let second = Array.init 3 (fun ind -> grid.(first_row_ind + 1).(first_col_ind + ind))
  in 
  let third = Array.init 3 (fun ind -> grid.(first_row_ind + 2).(first_col_ind + ind))
  in 
  third |> Array.append second |> Array.append first

let boxes grid = List.init 9 (get_box grid)

(* Funkcije za ustvarjanje novih mrež *)

let map_grid (f : 'a -> 'b) (grid : 'a grid) : 'b grid =
  Array.init 9 (fun ind -> (Array.map f (get_row grid ind)))

let copy_grid (grid : 'a grid) : 'a grid = map_grid (fun x -> x) grid

let foldi_grid (f : int -> int -> 'a -> 'acc -> 'acc) (grid : 'a grid)
    (acc : 'acc) : 'acc =
  let acc, _ =
    Array.fold_left
      (fun (acc, row_ind) row ->
        let acc, _ =
          Array.fold_left
            (fun (acc, col_ind) cell ->
              (f row_ind col_ind cell acc, col_ind + 1))
            (acc, 0) row
        in
        (acc, row_ind + 1))
      (acc, 0) grid
  in
  acc

let row_of_string cell_of_char str =
  List.init (String.length str) (String.get str) |> List.filter_map cell_of_char

let grid_of_string cell_of_char str =
  let grid =
    str |> String.split_on_char '\n'
    |> List.map (row_of_string cell_of_char)
    |> List.filter (function [] -> false | _ -> true)
    |> List.map Array.of_list |> Array.of_list
  in
  if Array.length grid <> 9 then failwith "Nepravilno število vrstic";
  if Array.exists (fun x -> x <> 9) (Array.map Array.length grid) then
    failwith "Nepravilno število stolpcev";
  grid

(* Model za vhodne probleme *)

type problem = { initial_grid : int option grid }

let int_option_to_string = function
  |Some digit -> string_of_int digit
  |None -> " "

let print_problem problem : unit = 
    print_grid int_option_to_string problem.initial_grid

let problem_of_string str =
  let cell_of_char = function
    | ' ' -> Some None
    | c when '1' <= c && c <= '9' -> Some (Some (Char.code c - Char.code '0'))
    | _ -> None
  in
  { initial_grid = grid_of_string cell_of_char str }

(* Model za izhodne rešitve *)

type solution = int grid

let print_solution solution = 
  print_grid string_of_int solution

let int_option_to_int = function
  |Some digit -> digit
  |None -> 0

(* pomozne funkcije za is_valid_solution, !popravi vse funkcije za option int *)

let preveri_1_do_9 (seznam : int list) = match seznam with 
  |[] -> false
  |x::xs -> (List.sort compare seznam) = [1;2;3;4;5;6;7;8;9]

let grid_to_list_list (grid : int grid) =  
  let big_list = Array.to_list grid in 
  List.map (Array.to_list) big_list

let rec preveri_sezname seznam_seznamov =
  List.for_all (preveri_1_do_9) seznam_seznamov

let preveri_istolezne_komponente (mat1 : int array array) (mat2 : int array array) = 
  let rec aux (i, j) m1 m2 =
    let prva = m1.(i).(j) in 
    match prva with 
      |0 -> if j < 8 then aux (i, (j + 1)) m1 m2  else 
              if i < 8 then aux ((i + 1), 0) m1 m2 else true  
      |_ -> 
        let enakost = (m1.(i).(j) = m2.(i).(j)) in 
        match enakost with 
          |false -> false 
          |true -> 
            if j < 8 then aux (i, (j + 1)) m1 m2  else 
              if i < 8 then aux ((i + 1), 0) m1 m2 else true  
  in 
  aux (0, 0) mat1 mat2


(* mora preveriti da ima vsaka vrstica/stolp/box 1-9 in da ima podane stevke na istem mestu kot original *)
let is_valid_solution problem (solution : int array array) =
  let original = (problem.initial_grid |> map_grid int_option_to_int) in
  match preveri_istolezne_komponente original solution  with 
    |false -> false 
    |true -> 
      let vrstice = rows solution in 
        match preveri_sezname (List.map Array.to_list vrstice) with 
          |false -> false 
          |true -> 
            let stolpci = columns solution in 
              match preveri_sezname (List.map Array.to_list stolpci) with 
                |false ->false
                |true -> 
                  let skatle = boxes solution in         
                    match preveri_sezname (List.map Array.to_list skatle) with 
                      |false -> false 
                      |true -> true



(* -------------------------------------------------------------- *)
(* ----SOLVER--------------------------------------------------- *)
type available = { loc : int * int; possible : int list } 

(* TODO: tip stanja ustrezno popravite, saj boste med reševanjem zaradi učinkovitosti
   želeli imeti še kakšno dodatno informacijo *)

type state = { problem : problem; current_grid : int option grid;  mutable moznosti : available list}


let print_state (state : state) : unit =
  print_grid
    (function None -> "?" | Some digit -> string_of_int digit)
    state.current_grid

type response = Solved of solution | Unsolved of state | Fail of state


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

(* nam vrne seznam števk ki v tistme stolpcu/vrstici/boxu še niso porabljene *)
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
    let neporabljene_v_vrstici = neporabljene (get_row grid r) vse_moznosti in 
    let neporabljene_v_stoplcu = neporabljene (get_column grid c) vse_moznosti in 
    let neporabljene_v_boxu = neporabljene (get_box grid (box_number loc)) vse_moznosti in
    match (neporabljene_v_vrstici, neporabljene_v_stoplcu, neporabljene_v_boxu) with
      |([], _, _) | (_, [], _) | (_, _, []) -> []
      |([x], _, _) | (_, [x], _) | (_, _, [x]) ->  [x]
      |_ ->
      presek_seznamov neporabljene_v_vrstici (presek_seznamov neporabljene_v_stoplcu neporabljene_v_boxu)

(* funkcije za initialize_state *)


let zapisi_seznam_moznosti (grid : int option grid) = 
  let rec aux (i, j) grid acc =
    let moznosti = mozne_stevke_v_celici (i, j) grid in
    let availabe = {loc = (i, j) ; possible = moznosti} in 
    if j < 8 then aux (i, (j + 1)) grid (availabe :: acc)  else 
      if i < 8 then aux ((i + 1), 0) grid (availabe :: acc) else acc 
    in 
    aux (0, 0) grid []


let initialize_state (problem : problem) : state =
  { current_grid = copy_grid problem.initial_grid; problem; moznosti = zapisi_seznam_moznosti (problem.initial_grid)}

let validate_state (state : state) : response =
  let unsolved =
    Array.exists (Array.exists Option.is_none) state.current_grid
  in
  if unsolved then Unsolved state
  else
    (* Option.get ne bo sprožil izjeme, ker so vse vrednosti v mreži oblike Some x *)
    let solution = map_grid Option.get state.current_grid in
    if is_valid_solution state.problem solution then Solved solution
    else Fail state



(* funkciji ki dolocita vrednost celice *)

let prepisi_na_novo_mesto f grid =
  let grid' = copy_grid grid in
  let () = f grid' in
  grid'
let doloci_stevko stevka (i, j) (grid : int option grid) =
  (grid.(i).(j) <- (Some stevka))

let z_doloceno_stevko stevka (i, j) (grid : int option grid) = 
  prepisi_na_novo_mesto (doloci_stevko stevka (i, j)) grid

(* ------------------------------------------------------------------------------------ *)


(* TODO: Pripravite funkcijo, ki v trenutnem stanju poišče hipotezo, glede katere
   se je treba odločiti. Če ta obstaja, stanje razveji na dve stanji:
   v prvem predpostavi, da hipoteza velja, v drugem pa ravno obratno.
   Če bo vaš algoritem najprej poizkusil prvo možnost, vam morda pri drugi
   za začetek ni treba zapravljati preveč časa, saj ne bo nujno prišla v poštev. *)
let branch_state (state : state) : (state * state) option =
   match state.moznosti with 
   |[] -> failwith "padem ker je match state.moznosti prazen pri branch state"
   |x::xs -> 
    match x.possible with 
    |[] -> None 
    |y::ys -> 
      let new_grid = z_doloceno_stevko y x.loc state.current_grid in 
      Some (
      {problem = state.problem;
      current_grid = new_grid;
      moznosti = xs},
      {problem = state.problem;
      current_grid = (z_doloceno_stevko y x.loc state.current_grid);
      moznosti = {loc = x.loc; possible = ys} :: xs}
      )

let dopolni_trivialne_resitve state =
  let rec aux grid moznosti = 
    match moznosti with 
    |[] -> grid
    |x::xs ->
      match x.possible with 
      |[y] -> aux (z_doloceno_stevko y (x.loc) grid) xs
      |_ -> aux grid xs
    in 
  aux state.current_grid state.moznosti

(* pogledamo, če trenutno stanje vodi do rešitve *)
let rec solve_state (state : state) =
  (* uveljavimo trenutne omejitve in pogledamo, kam smo prišli *)
  (* TODO: na tej točki je stanje smiselno počistiti in zožiti možne rešitve *)
  let new_grid = dopolni_trivialne_resitve state in 
  let new_state = {
    problem = state.problem;
    current_grid = new_grid;
    moznosti = zapisi_seznam_moznosti new_grid
  }
 in
  match validate_state new_state with
  | Solved solution ->
      (* če smo našli rešitev, končamo *)
      Some solution
  | Fail fail ->
      (* prav tako končamo, če smo odkrili, da rešitev ni *)
      None
  | Unsolved new_state ->
      (* če še nismo končali, raziščemo stanje, v katerem smo končali *)
      explore_state new_state

and explore_state (state : state) =
  (* pri raziskovanju najprej pogledamo, ali lahko trenutno stanje razvejimo *)
  match branch_state state with
  | None ->
      (* če stanja ne moremo razvejiti, ga ne moremo raziskati *)
      (* None *) failwith "padem pri explore state ker je branch state poslal none"
  | Some (st1, st2) -> (
      (* če stanje lahko razvejimo na dve možnosti, poizkusimo prvo *)
      match solve_state st1 with
      | Some solution ->
          (* če prva možnost vodi do rešitve, do nje vodi tudi prvotno stanje *)
          Some solution
      | None ->
          (* če prva možnost ne vodi do rešitve, raziščemo še drugo možnost *)
          solve_state st2 )

let solve_problem (problem : problem) =
  problem |> initialize_state |> solve_state






(* primeri za pomoč *)
  let primer_neresen = [|
    [|None; None; None; Some 6; None; Some 2; None; None; None|];
    [|Some 4; None; None; None; Some 5; None; None; None; Some 1|];
    [|None; Some 8; Some 5; None; Some 1; None; Some 6; Some 1; None|];
    [|None; Some 3; Some 8; Some 2; None; Some 6; Some 7; Some 1; None|];
    [|None; None; None; None; None; None; None; None; None|];
    [|None; Some 1; Some 9; Some 4; None; Some 7; Some 3; Some 5; None|];
    [|None; Some 2; Some 6; None; Some 4; Some 5; Some 3; None; None|];
    [|Some 9; None; None; None; Some 2; None; None; None; Some 7|];
    [|None; None; None; Some 8; None; Some 9; None; None; None|]
    |]
  let primer_problem = {initial_grid = primer_neresen}
    (*
    ┏━━━┯━━━┯━━━┓
    ┃   │6 2│   ┃
    ┃4  │ 5 │  1┃
    ┃ 85│ 1 │62 ┃
    ┠───┼───┼───┨
    ┃ 38│2 6│71 ┃
    ┃   │   │   ┃
    ┃ 19│4 7│35 ┃
    ┠───┼───┼───┨
    ┃ 26│ 4 │53 ┃
    ┃9  │ 2 │  7┃
    ┃   │8 9│   ┃
    ┗━━━┷━━━┷━━━┛ *)

let primer_1_neresen = [|
[|Some 4; Some 8; Some 3; Some 9; Some 2; Some 1; Some 6; Some 5;  Some 7|];
[|Some 9; Some 6; Some 7; Some 3; None; Some 5; Some 8; Some 2; Some 1|];
[|Some 2; Some 5; Some 1; Some 8; Some 7; Some 6; Some 4; Some 9; Some 3|];
[|Some 5; Some 4; Some 8; Some 1; Some 3; Some 2; Some 9; Some 7;Some 6|];
[|Some 7; Some 2; Some 9; None; Some 6; Some 4; None; Some 3; Some 8|];
[|Some 1; Some 3; Some 6; Some 7; Some 9; Some 8; None; Some 4; Some 5|];
[|Some 3; Some 7; Some 2; Some 6; Some 8; Some 9; Some 5; Some 1;Some 4|];
[|Some 8; Some 1; Some 4; Some 2; Some 5; Some 3; Some 7; Some 6;Some 9|];
[|Some 6; Some 9; Some 5; Some 4; Some 1; Some 7; Some 3; Some 8;Some 2|]|]

let primer_1_neresen_int = 
[|
  [| 4; 8; 3; 9; 2; 1; 6; 5; 7|];
  [| 9; 6; 7; 3; 0; 5; 8; 2; 1|];
  [| 2; 5; 1; 8; 7; 6; 4; 9; 3|];
  [| 5; 4; 8; 1; 3; 2; 9; 7; 6|];
  [| 7; 2; 9; 0; 6; 4; 0; 3; 8|];
  [| 1; 3; 6; 7; 9; 8; 0; 4; 5|];
  [| 3; 7; 2; 6; 8; 9; 5; 1; 4|];
  [| 8; 1; 4; 2; 5; 3; 7; 6; 9|];
  [| 6; 9; 5; 4; 1; 7; 3; 8; 2|]
|]


 let primer_1_problem ={initial_grid = primer_1_neresen}

let primer_1_state = {problem = primer_1_problem; current_grid = primer_1_neresen; moznosti = zapisi_seznam_moznosti primer_1_neresen}


let primer_enostaven_solution_int = 
  [|
    [|4; 8; 3; 9; 2; 1; 6; 5; 7|];
    [|9; 6; 7; 3; 4; 5; 8; 2; 1|];
    [|2; 5; 1; 8; 7; 6; 4; 9; 3|];
    [|5; 4; 8; 1; 3; 2; 9; 7; 6|];
    [|7; 2; 9; 5; 6; 4; 1; 3; 8|];
    [|1; 3; 6; 7; 9; 8; 2; 4; 5|];
    [|3; 7; 2; 6; 8; 9; 5; 1; 4|];
    [|8; 1; 4; 2; 5; 3; 7; 6; 9|];
    [|6; 9; 5; 4; 1; 7; 3; 8; 2|]
    |]


(* primer 2 *)

let primer_2_string = 
"
┏━━━┯━━━┯━━━┓
┃2  │ 8 │3  ┃
┃ 6 │ 7 │ 84┃
┃ 3 │5  │2 9┃
┠───┼───┼───┨
┃   │1 5│4 8┃
┃   │   │   ┃
┃4 2│7 6│   ┃
┠───┼───┼───┨
┃3 1│  7│ 4 ┃
┃72 │ 4 │ 6 ┃
┃  4│ 1 │  3┃
┗━━━┷━━━┷━━━┛"

let primer_2_neresen = [|
[|Some 2 ; None ; None ; None ; Some 8 ; None ; Some 3 ; None ;  None |];
[|None ; Some 6 ; None ; None ; Some 7; None ; None ; Some 8 ; Some 4 |];
[|None ; Some 3 ; None ; Some 5 ; None ; None ; Some 2 ; None ; Some 9 |];
[|None ; None ; None ; Some 1 ; None ; Some 5 ; Some 4 ; None ;Some 8 |];
[|None ; None ; None ; None; None ; None ; None; None ; None |];
[|Some 4 ; None ; Some 2 ; Some 7 ; None ; Some 6 ; None; None ; None |];
[|Some 3 ; None ; Some 1 ; None ; None ; Some 7 ; None ; Some 4 ;None |];
[|Some 7 ; Some 2 ; None ; None ; Some 4 ; None ; None ; Some 6 ;None |];
[|None ; None ; Some 4 ; None ; Some 1 ; None ; None ; None ;Some 3 |]|]
let primer_2_problem = {initial_grid = primer_2_neresen}

let primer_2_state = {
  problem = primer_2_problem;
  current_grid = primer_2_neresen;
  moznosti = zapisi_seznam_moznosti primer_2_neresen
}
let primer_2_trivialen = 
[|
[|Some 2; None; None; None; Some 8; None; Some 3; None; None|];
[|None; Some 6; None; None; Some 7; None; None; Some 8; Some 4|];
[|None; Some 3; None; Some 5; Some 6; None; Some 2; None; Some 9|];
[|None; None; None; Some 1; None; Some 5; Some 4; None; Some 8|];
[|None; None; None; None; None; None; None; None; None|];
[|Some 4; None; Some 2; Some 7; None; Some 6; None; None; None|];
[|Some 3; None; Some 1; None; None; Some 7; None; Some 4; None|];
[|Some 7; Some 2; None; None; Some 4; None; None; Some 6; None|];
[|None; None; Some 4; None; Some 1; None; None; None; Some 3|]|] 