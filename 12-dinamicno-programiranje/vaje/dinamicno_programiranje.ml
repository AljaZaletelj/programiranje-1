(* ========== Vaje 6: Dinamično programiranje  ========== *)


(*----------------------------------------------------------------------------*]
 Požrešna miška se nahaja v zgornjem levem kotu šahovnice. Premikati se sme
 samo za eno polje navzdol ali za eno polje na desno in na koncu mora prispeti
 v desni spodnji kot. Na vsakem polju šahovnice je en sirček. Ti sirčki imajo
 različne (ne-negativne) mase. Miška bi se rada kar se da nažrla, zato jo
 zanima, katero pot naj ubere.

 Funkcija [max_cheese cheese_matrix], ki dobi matriko [cheese_matrix] z masami
 sirčkov in vrne največjo skupno maso, ki jo bo miška požrla, če gre po
 optimalni poti.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # max_cheese test_matrix;;
 - : int = 13
[*----------------------------------------------------------------------------*)

let test_matrix = 
  [| [| 1 ; 2 ; 0 |];
     [| 2 ; 4 ; 5 |];
     [| 7 ; 0 ; 1 |] |]

(* let rec max_cheese polje = 
   let n = Array.length polje in 
   let aux (i, j) acc = match (i, j) with
      |(n , _) | (_, n) -> acc 
      |_ -> max_cheese () *)


(*----------------------------------------------------------------------------*]
 Poleg količine sira, ki jo miška lahko poje, jo zanima tudi točna pot, ki naj
 jo ubere, da bo prišla do ustrezne pojedine.

 Funkcija [optimal_path] naj vrne optimalno pot, ki jo mora miška ubrati, da se
 čim bolj nažre. Ker je takih poti lahko več, lahko funkcija vrne poljubno.
 Pripravite tudi funkcijo [convert_path], ki pot pretvori v seznam tež sirčkov
 na poti.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # optimal_path_bottom test_matrix;;
 - : mouse_direction list = [Right; Down; Down; Right; Down]
 # optimal_path test_matrix |> convert_path test_matrix;;
 - : int list = [1; 2; 4; 5; 1]
[*----------------------------------------------------------------------------*)

type mouse_direction = Down | Right

let rec optimal_path_bottom matrix = 
   let rec aux vrs stol = 
      let curr =  matrix.(vrs).(stol) in 
      (* desno *)
      let max_desno, pot_desno = 
         if stol < Array.length matrix.(0) then 
            let best, pot = aux vrs (stol + 1) in 
            best, pot 
         else 
            (0, [])
      in
      (* desno *)
      let max_dol, pot_dol = 
         if vrs + 1 < Array.length matrix.(0) then 
               let best, pot = aux (vrs + 1) stol in 
               best, pot 
         else 
               (0, [])
      in
      let max_pot = if max_dol > max_desno then Down :: pot_dol else Right :: pot_desno in
      (curr + (max max_desno max_dol), max_pot)
   in
   let (_, pot) = aux 0, 0 in 
   pot


(* d(x,y) = max d(x+1, y) d(x, y+1) + neveljavne stvari *)


(*----------------------------------------------------------------------------*]
 Rešujemo problem sestavljanja alternirajoče obarvanih stolpov. Imamo štiri
 različne tipe gradnikov, dva modra in dva rdeča. Modri gradniki so višin 2 in
 3, rdeči pa višin 1 in 2.

 Funkcija [alternating_towers] za podano višino vrne število različnih stolpov
 dane višine, ki jih lahko zgradimo z našimi gradniki, kjer se barva gradnikov
 v stolpu izmenjuje (rdeč na modrem, moder na rdečem itd.). Začnemo z gradnikom
 poljubne barve.

 Namig: Uporabi medsebojno rekurzivni pomožni funkciji z ukazom [and].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # alternating_towers 10;;
 - : int = 35
[*----------------------------------------------------------------------------*)

let rec neki x = 
   neki2 (x - 3)
   and neki2 x = 
      neki (x - 2)


(* 
koliko moznosti da se konca z rdecimi   modrimi 
                           0     0        0
                           1     1        0
                           2     1        1  
                           3     1        2
                           4

*)


let alternating_towers n = 
   let rec konec_rdeca h = 
      if h = 0 then 0 else if h <= 2 then 1 else 
         (konec_modra h - 1) + (konec_modra h - 2)
   and konec_modra h = 
      if h <= 1 then 0 else if h <= 3 then h-1 else
         (konec_rdeca h - 2) + (konec_rdeca h - 3)
   in 
   (konec_modra n) + (konec_rdeca n)


(*----------------------------------------------------------------------------*]
 Izračunali smo število stolpov, a naše vrle gradbince sedaj zanima točna
 konfiguracija. Da ne pride do napak pri sestavljanju, bomo stolpe predstavili
 kar kot vsotne tipe. 

 Stolp posamezne barve so temelji (Bottom), ali pa kot glava bloka pripadajoče
 barve in preostanek, ki je stolp nasprotne barve.

 Definirajte funkcijo [enumerate_towers], ki vrne seznam vseh stolpov podane
 dolžine. Stolpe lahko vrne v poljubnem vrstnem redu. Funkcija naj hitro (in
 brez) prekoračitve sklada deluje vsaj do višine 20.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # enumerate_towers 4;;
 - : tower list = 
    [Red (TopRed (Red2, TopBlue (Blue2, RedBottom)));
     Red (TopRed (Red1, TopBlue (Blue3, RedBottom)));
     Red (TopRed (Red1, TopBlue (Blue2, TopRed (Red1, BlueBottom))));
     Blue (TopBlue (Blue3, TopRed (Red1, BlueBottom)));
     Blue (TopBlue (Blue2, TopRed (Red2, BlueBottom)))]
[*----------------------------------------------------------------------------*)


type blue_block = Blue3 | Blue2
type red_block = Red2 | Red1

type red_tower = TopRed of red_block * blue_tower | RedBottom
and blue_tower = TopBlue of blue_block * red_tower | BlueBottom

type tower = Red of red_tower | Blue of blue_tower

(*----------------------------------------------------------------------------*]
 Vdrli ste v tovarno čokolade in sedaj stojite pred stalažo kjer so ena ob
 drugi naložene najboljše slaščice. Želite si pojesti čim več sladkorja, a
 hkrati poskrbeti, da vas ob pregledu tovarne ne odkrijejo. Da vas pri rednem
 pregledu ne odkrijejo, mora biti razdalija med dvema zaporednima slaščicama,
 ki ju pojeste vsaj `k`.

 Napišite funkcijo [ham_ham], ki sprejme seznam naravnih števil dolžine `n`, ki
 predstavljajo količino sladkorja v slaščicah v stalaži in parameter `k`,
 najmanjšo razdalijo med dvema slaščicama, ki ju še lahko varno pojeste.
 Funkcija naj vrne seznam zastavic `bool`, kjer je `i`-ti prižgan natanko tedaj
 ko v optimalni požrtiji pojemo `i`-to slaščico.

 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # ham_ham test_shelf 1;;
 - : bool list = [false; true; false; true; false; true; false; true; false]
 # ham_ham test_shelf 2;;
 - : bool list = [false; true; false; false; false; true; false; false; false]
[*----------------------------------------------------------------------------*)

let test_shelf = [1;2;-5;3;7;19;-30;1;0]

let rec ham_ham list k = 
   let rec skip k sweets skips = 
      match k, sweets with 
         |0, _ -> sweets, skips 
         |k, [] -> [], skips 
         |k, s::sws -> (skip (k - 1) sws (false :: skips))
   in 
   let rec nom_nom = function 
      |[] -> 0, []
      |s :: sws -> 
         let nom = 
            let sws_after_skip, skips = skip k sws [] in 
            let v_nom, instr_nom = nom_nom sws_after_skip in 
            v_nom + s, true :: skips @ instr_nom
         in 
         let no_nom = 
            let v_no_nom, inst_no_nom = nom_nom sws in 
            v_no_nom, false :: inst_no_nom 
         in 
         max nom no_nom
   in
   let v_opt, instr_opt = nom_nom list in 
instr_opt
   




