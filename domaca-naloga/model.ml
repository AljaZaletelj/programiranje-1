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

(* pomozne funkcije za is_valid_solution*)
let preveri_1_do_9 (seznam : int list) = match seznam with 
  |[] -> false
  |x::xs -> (List.sort compare seznam) = [1;2;3;4;5;6;7;8;9]

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

(*preveriti da ima vsaka vrstica/stolp/box vse števke 1-9 in da so vse zapolnjene celice originala enake tistim v rešitvi *)
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