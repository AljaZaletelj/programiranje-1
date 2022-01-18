(* a *)
(*----------------------------------------------------------------------------*]
  Napišite predikat `je_urejena : int * int * int -> bool`, ki pove, ali je 
  podana trojica celih števil urejena strogo naraščajoče.
[*----------------------------------------------------------------------------*)

let je_urejena (x, y, z) =
  if x < y then y < z else false

(* b *)
(*----------------------------------------------------------------------------*]
  Napišite funkcijo `poskusi_deljenje : float option -> float option -> float option`, 
  ki sprejme morebitni deljenec in morebitni delitelj ter vrne rezultat deljenja, 
  če se to da, ali pa `None`, če ne (kadar kakšnega argumenta ni ali pa bi prišlo 
  do deljenja z nič). 
  
    # poskusi_deljenje (Some 1.0) (Some 2.0);;
    - : float option = Some 0.5
    # poskusi_deljenje (Some 1.0) (Some 0.0);;
    - : float option = None
    # poskusi_deljenje None (Some 2.0);;
    - : float option = None
[*----------------------------------------------------------------------------*)

let poskusi_deljenje ((x : float option) , (y : float option)) = 
  match x, y with 
  |None, _ | _, None -> None 
  |Some x, Some y -> 
    match y with 
    |0. -> None
    |_ -> Some (x /. y)

(* c *)
(*----------------------------------------------------------------------------*]
  Definirajte funkcijo `zavrti : 'a list -> int -> 'a list`, ki seznam zavrti 
  za dano število mest v levo (v vsaki rotaciji se prvi element prestavi na 
  konec seznama).
  
    # zavrti [1; 2; 3; 4; 5] 2;;
    - : int list = [3; 4; 5; 1; 2]
[*----------------------------------------------------------------------------*)

let rec zavrti (list : 'a list) k = 
  if k <= 0 then list else 
    match list with 
      |[] -> []
      |x::xs -> (zavrti xs (k - 1) ) @ [x]

(* d *)
(*----------------------------------------------------------------------------*]
  Napišite funkcijo `razdeli : ('a -> int) -> 'a list -> ('a list *  'a list * 'a list)|`, 
  ki sprejme cenilno funkcijo in seznam elementov. Vrne naj trojico, kjer so na prvem 
  mestu vsi elementi za katere je cenilna funkcija negativna, na drugem vsi, kjer 
  je enaka 0, na tretjem pa vsi preostali elementi.
  Elementi naj v seznamih nastopajo v enakem vrstnem redu kot v prvotnem seznamu. 
  Za vse točke naj bo funkcija repno rekurzivna.
  
    # razdeli ((-) 3) [1; 2; 3; 4; 5; 6];;
    - : int list * int list * int list = ([4; 5; 6], [3], [1; 2])
[*----------------------------------------------------------------------------*)


let razdeli (f : ('a -> int)) (list : 'a list) = 
  let rec aux f list neg nic poz = 
    match list with 
    |[] -> (List.rev neg), (List.rev nic), (List.rev poz)
    |x::xs -> 
      if (f x) < 0 then aux f xs (x :: neg) nic poz 
      else 
        if (f x) = 0 then aux f xs neg (x:: nic) poz 
        else aux f xs neg nic (x :: poz)
  in 
  aux f list [] [] []