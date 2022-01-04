(* Pomožni tip, ki predstavlja mrežo *)

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


(* ?? *)
let is_valid_solution problem solution = 
  (problem.initial_grid |> map_grid int_option_to_int) = solution


(* neumnosti----------------------------------------------------------------------------------------------------- *)

(* primeri za pomoč *)
let f n = string_of_int n

let primer_int = [|
  [|1;2;3;4;5;6;7;8;9|];
  [|2;2;2;2;2;2;2;2;2|];
  [|3;3;3;3;3;3;3;3;3|];
  [|4;4;4;4;4;4;4;4;4|];
  [|5;5;5;5;5;5;5;5;5|];
  [|6;6;6;6;6;6;6;6;6|];
  [|7;7;7;7;7;7;7;7;7|];
  [|8;8;8;8;8;8;8;8;8|];
  [|9;9;9;9;9;9;9;9;9|]
  |] 

  (* 
  print_grid f primer vrne
┏━━━┯━━━┯━━━┓
┃123│456│789┃
┃222│222│222┃
┃333│333│333┃
┠───┼───┼───┨
┃444│444│444┃
┃555│555│555┃
┃666│666│666┃
┠───┼───┼───┨
┃777│777│777┃
┃888│888│888┃
┃999│999│999┃
┗━━━┷━━━┷━━━┛
 *)

  let primer_option = [|
    [|Some 1; Some 2; Some 3; Some 4; Some 5; Some 6; Some 7; Some 8; Some 9|];
    [|Some 2; Some 2; Some 2; Some 2; Some 2; Some 2; Some 2; Some 2; Some 2|];
    [|Some 3; Some 3; Some 3; Some 3; Some 3; Some 3; Some 3; Some 3; Some 3|];
    [|Some 4; Some 4; Some 4; Some 4; Some 4; Some 4; Some 4; Some 4; Some 4|];
    [|Some 5; Some 5; Some 5; Some 5; Some 5; Some 5; Some 5; Some 5; Some 5|];
    [|None; Some 6; Some 6; Some 6; Some 6; Some 6; Some 6; Some 6; Some 6|];
    [|Some 7; Some 7; Some 7; Some 7; Some 7; Some 7; Some 7; Some 7; Some 7|];
    [|Some 8; Some 8; Some 8; Some 8; Some 8; Some 8; Some 8; Some 8; Some 8|];
    [|Some 9; Some 9; Some 9; Some 9; Some 9; Some 9; Some 9; Some 9; Some 9|]
    |]

  let primer_none = [|
    [|None; None; None; None; None; None; None; None; None|];
    [|None; None; None; None; None; None; None; None; None|];
    [|None; None; None; None; None; None; None; None; None|];
    [|None; None; None; None; None; None; None; None; None|];
    [|None; None; None; None; None; None; None; None; None|];
    [|None; None; None; None; None; None; None; None; None|];
    [|None; None; None; None; None; None; None; None; None|];
    [|None; None; None; None; None; None; None; None; None|];
    [|None; None; None; None; None; None; None; None; None|]
    |]

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



 let primer_string = "
┏━━━┯━━━┯━━━┓
┃483│921│657┃
┃967│3 5│821┃
┃251│876│493┃
┠───┼───┼───┨
┃548│132│976┃
┃729│ 64│ 38┃
┃136│798│ 45┃
┠───┼───┼───┨
┃372│689│514┃
┃814│253│769┃
┃695│417│382┃
┗━━━┷━━━┷━━━┛"

let primer_enostaven_neresen = [|
[|Some 4; Some 8; Some 3; Some 9; Some 2; Some 1; Some 6; Some 5;  Some 7|];
[|Some 9; Some 6; Some 7; Some 3; None; Some 5; Some 8; Some 2; Some 1|];
[|Some 2; Some 5; Some 1; Some 8; Some 7; Some 6; Some 4; Some 9; Some 3|];
[|Some 5; Some 4; Some 8; Some 1; Some 3; Some 2; Some 9; Some 7;Some 6|];
[|Some 7; Some 2; Some 9; None; Some 6; Some 4; None; Some 3; Some 8|];
[|Some 1; Some 3; Some 6; Some 7; Some 9; Some 8; None; Some 4; Some 5|];
[|Some 3; Some 7; Some 2; Some 6; Some 8; Some 9; Some 5; Some 1;Some 4|];
[|Some 8; Some 1; Some 4; Some 2; Some 5; Some 3; Some 7; Some 6;Some 9|];
[|Some 6; Some 9; Some 5; Some 4; Some 1; Some 7; Some 3; Some 8;Some 2|]|]

let primer_enostaven_problem ={initial_grid = primer_enostaven_neresen}













(* --------------------------------------------------- *)
(* ----SOLVER--------------------------------------------------- *)
type available = { loc : int * int; possible : int list } 

(* TODO: tip stanja ustrezno popravite, saj boste med reševanjem zaradi učinkovitosti
   želeli imeti še kakšno dodatno informacijo *)
type state = { problem : problem; current_grid : int option grid }

(* primer za pomoc *) 

let primer_state = {problem = primer_problem; current_grid = primer_neresen}
let primer_enostaven_state = {problem = primer_enostaven_problem; current_grid = primer_enostaven_neresen}

let print_state (state : state) : unit =
  print_grid
    (function None -> "?" | Some digit -> string_of_int digit)
    state.current_grid

type response = Solved of solution | Unsolved of state | Fail of state

let initialize_state (problem : problem) : state =
  { current_grid = copy_grid problem.initial_grid; problem }

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
  let neporabljene_v_vrstici = neporabljene (get_row grid r) vse_moznosti in 
  let neporabljene_v_stoplcu = neporabljene (get_column grid c) vse_moznosti in 
  let neporabljene_v_boxu = neporabljene (get_box grid (box_number loc)) vse_moznosti in
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

(* vzame lokacijo celice kateri zelimo zamenjati stekvo na
 prvem ali drugem mestu seznama moznosti (odlocamo se med dvema, saj zelimo razvejati stanje).
  vrne unit!! torej samo zamenja in nic ne vrne *)
let doloci_stevko (r, c) mesto moznosti (grid : int option grid) = match moznosti with 
    |x::y::[] -> 
     ( match mesto with 
        |1 -> (grid.(r).(c) <- Some x )
        |2 -> (grid.(r).(c) <- Some y )
        |_ -> failwith "V tej celici imam le dve možnosti, zato izberi 1 ali 2.")
    |_ -> failwith "Izbirati znam le med dvema možnostma"

let prepisi_na_novo_mesto f grid =
  let grid' = copy_grid grid in
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

let solve_problem (problem : problem) =
  problem |> initialize_state |> solve_state
