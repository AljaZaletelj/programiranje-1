(*============================================================================*]
  Pri tej nalogi bomo za slovar uporabili kar enostavno implementacijo z 
  asociativnim seznamom, ki smo jo spoznali na predavanjih.
  S spodaj definiranimi funkcijami si lahko pomagate pri vseh podnalogah.
[*============================================================================*)

type ('a, 'b) slovar = ('a * 'b) list

let prazen_slovar : ('a, 'b) slovar = []

let velikost (m : ('a, 'b) slovar) = List.length m

let vsebuje (x : 'a) (m : ('a, 'b) slovar) = List.mem_assoc x m

(* Vrne vrednost, ki pripada ključu ali None *)
let najdi x (m : ('a, 'b) slovar) = List.assoc_opt x m

(* Doda vrednost v slovar in povozi prejšnjo, če obstaja *)
let dodaj (k, v) (m : ('a, 'b) slovar) = (k, v) :: List.remove_assoc k m

(*============================================================================*]
  Matematične izraze predstavimo z dvojiškimi drevesi, v katerih vozlišča predstavljajo 
  aritmetične operacije, listi pa števila ali spremenljivke, predstavljene z nizi.
  Izraz v drevo pretvorimo tako, da pri operaciji levi podizraz vzamemo za levo 
  poddrevo, desni podizraz za desno, v vozlišče pa zapišemo operator.
[*============================================================================*)

type operator = Plus | Minus | Krat | Deljeno

type 'a izraz =
  | Spremenljivka of string
  | Konstanta of 'a
  | Operacija of ('a izraz * operator * 'a izraz)

(* (x - 3)- (y * (z / x))  *)
let primer =
  Operacija
    ( Operacija (Spremenljivka "x", Minus, Konstanta 3),
      Minus,
      Operacija
        ( Spremenljivka "y",
          Krat,
          Operacija (Spremenljivka "z", Deljeno, Spremenljivka "x") ) )

(* a *)
(*----------------------------------------------------------------------------*]
  Napišite funkcijo `prestej : izraz -> int`, ki vrne število vseh "različnih" 
  spremenljivk v izrazu.
[*----------------------------------------------------------------------------*)

let vse_spremenljivke izraz =
  let rec aux acc izraz = match izraz with 
    |Konstanta _ -> acc 
    |Operacija (i1 , _ , i2) -> (aux acc i1) @ (aux acc i2)
    |Spremenljivka a -> (a :: acc)
  in 
  aux [] izraz

let razlicne_spremenljivke vse =
  let rec aux acc vse = match vse with 
    |[] -> acc 
    |x::xs -> if (List.mem x acc) then aux acc xs else aux (x :: acc) xs
  in 
  aux [] vse

let prestej izraz = List.length (razlicne_spremenljivke (vse_spremenljivke izraz))


(* b *)
(*----------------------------------------------------------------------------*]
Napišite funkcijo `izlusci : 'a izraz -> (string * int) slovar`, ki sprejme izraz 
in vrne slovar, ki pove, kolikokrat se posamezna spremenljivka pojavi v izrazu. 
Vrstni red v slovarju ni pomemben.
[*----------------------------------------------------------------------------*)

let zapisi_slovar_spremenljivk list = 
    let rec aux list slovar = match list with 
    |[] -> slovar
    |x :: xs -> aux xs (dodaj (x, 0) slovar)
  in 
  aux list prazen_slovar


let izlusci (izraz : 'a izraz)  = 
  let vse_spremenljivke = vse_spremenljivke izraz in 
  let slovar = zapisi_slovar_spremenljivk (razlicne_spremenljivke vse_spremenljivke) in 
  let rec aux list slovar = match list with 
    |[] -> slovar 
    |x :: xs -> 
      let stara_vrednost = najdi x slovar in
      match stara_vrednost with 
        |None -> aux xs (dodaj (x, 1) slovar)
        |Some v -> aux xs (dodaj (x, v + 1) slovar)
  in aux vse_spremenljivke slovar



(* c *)
(*----------------------------------------------------------------------------*]
  Napišite funkcijo `izracunaj : (string * int) slovar -> int izraz -> option int`, 
  ki sprejme izraz in slovar vrednosti spremenljivk ter poskuša izračunati vrednost 
  izraza. Če to ni mogoče (deljenje z 0 ali manjkajoča definicija spremenljivke), 
  naj bo rezultat `None`. 
    # izracunaj [("x",3); ("y", 4); ("z",5)] primer;;
    - : int option = Some (-4)
[*----------------------------------------------------------------------------*)

let izracunaj slovar (izraz : int izraz) = false

(* c *)
(*----------------------------------------------------------------------------*]
  Ocenite časovno zahtevnost funkcije `izracunaj` v odvisnosti od velikosti 
  izraza `n` (torej števila vseh vozlišč in listov v drevesu) ter števila različnih 
  spremenljivk `m`.
  Kako se časovna zahtevnost spremeni, če bi za slovar uporabili uravnoteženo iskalno drevo?
[*----------------------------------------------------------------------------*)
