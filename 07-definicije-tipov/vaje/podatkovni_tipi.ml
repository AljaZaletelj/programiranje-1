(* ========== Vaja 3: Definicije Tipov  ========== *)

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 Pri modeliranju denarja ponavadi uporabljamo racionalna števila. Problemi se
 pojavijo, ko uvedemo različne valute.
 Oglejmo si dva pristopa k izboljšavi varnosti pri uporabi valut.
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

(*----------------------------------------------------------------------------*]
 Definirajte tipa [euro] in [dollar], kjer ima vsak od tipov zgolj en
 konstruktor, ki sprejme racionalno število.
 Nato napišite funkciji [euro_to_dollar] in [dollar_to_euro], ki primerno
 pretvarjata valuti (točne vrednosti pridobite na internetu ali pa si jih
 izmislite).

 Namig: Občudujte informativnost tipov funkcij.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # dollar_to_euro;;
 - : dollar -> euro = <fun>
 # dollar_to_euro (Dollar 0.5);;
 - : euro = Euro 0.4305
[*----------------------------------------------------------------------------*)
type euro = Euro of float
type dollar = Dollar of float

let dollar_to_euro dollar = match dollar with
       | Dollar d -> Euro (0.88 *. d)

let euro_to_dollar (Euro d) = Dollar (d *. 1.1)

(*----------------------------------------------------------------------------*]
 Definirajte tip [currency] kot en vsotni tip z konstruktorji za jen, funt
 in švedsko krono. Nato napišite funkcijo [to_pound], ki primerno pretvori
 valuto tipa [currency] v funte.

 Namig: V tip dodajte še švicarske franke in se navdušite nad dejstvom, da vas
        Ocaml sam opozori, da je potrebno popraviti funkcijo [to_pound].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # to_pound (Yen 100.);;
 - : currency = Pound 0.007
[*----------------------------------------------------------------------------*)

type currency = Gbp of float | Jen of float | Krona of float

let to_pound c = match c with
 | Gbp _ -> c
 | Jen y -> Gbp ( y /. 100.0 )
 | Krona k -> Gbp ( k /. 6.34)

let to_pound = function
  | Gbp x -> Gbp x
  | Jen x -> Gbp 0.007
  | Krona x -> Gbp 0.085

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 Želimo uporabljati sezname, ki hranijo tako cela števila kot tudi logične
 vrednosti. To bi lahko rešili tako da uvedemo nov tip, ki predstavlja celo
 število ali logično vrednost, v nadaljevanju pa bomo raje konstruirali nov tip
 seznamov.

 Spomnimo se, da lahko tip [list] predstavimo s konstruktorjem za prazen seznam
 [Nil] (oz. [] v Ocamlu) in pa konstruktorjem za člen [Cons(x, xs)] (oz.
 x :: xs v Ocamlu).
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

type my_list = Prazen | Neprazen of (int * my_list)

(*----------------------------------------------------------------------------*]
 Definirajte tip [intbool_list] z konstruktorji za:
  1.) prazen seznam,
  2.) člen z celoštevilsko vrednostjo,
  3.) člen z logično vrednostjo.

 Nato napišite testni primer, ki bi predstavljal "[5; true; false; 7]".
[*----------------------------------------------------------------------------*)

type intbool_list = 
       | Nil 
       | KonInt of (int * intbool_list) 
       | KonBool of (bool * intbool_list)

let testni_primer = KonInt(5 , KonBool( true, KonBool(false, (KonInt ( 7 , Nil)))))

(*----------------------------------------------------------------------------*]
 Funkcija [intbool_map f_int f_bool ib_list] preslika vrednosti [ib_list] v nov
 [intbool_list] seznam, kjer na elementih uporabi primerno od funkcij [f_int]
 oz. [f_bool].
[*----------------------------------------------------------------------------*)

let rec intbool_map f_i f_b lst = match lst with 
       |Nil -> Nil
       |KonInt (int , rep) -> KonInt(f_i int, intbool_map f_i f_b rep)
       |KonBool (bool, rep) -> KonBool(f_b bool, intbool_map f_i f_b rep)

(*----------------------------------------------------------------------------*]
 Funkcija [intbool_reverse] obrne vrstni red elementov [intbool_list] seznama.
 Funkcija je repno rekurzivna.
[*----------------------------------------------------------------------------*)
let intbool_reverse lst =
    let rec aux acc = function
       |Nil -> acc
       |KonInt(x, xs) -> aux (KonInt(x, acc)) xs
       |KonBool(x, xs) -> aux (KonBool(x, acc)) xs
    in
    aux Nil lst

(*----------------------------------------------------------------------------*]
 Funkcija [intbool_separate ib_list] loči vrednosti [ib_list] v par [list]
 seznamov, kjer prvi vsebuje vse celoštevilske vrednosti, drugi pa vse logične
 vrednosti. Funkcija je repno rekurzivna in ohranja vrstni red elementov.
[*----------------------------------------------------------------------------*)

let rec intbool_separate lst = 
       let rec aux l ints bools = 
              match l with
              | Nil -> (List.rev ints, List.rev bools)
              | KonInt (i, rep) -> aux rep (i::ints) bools
              | KonBool (b, rep) -> aux rep ints (b::bools)
       in 
       aux lst [] []

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 Določeni ste bili za vzdrževalca baze podatkov za svetovno priznano čarodejsko
 akademijo "Effemef". Vaša naloga je konstruirati sistem, ki bo omogočil
 pregledno hranjenje podatkov.
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)


(*----------------------------------------------------------------------------*]
 Čarodeje razvrščamo glede na vrsto magije, ki se ji posvečajo. Definirajte tip
 [magic], ki loči med magijo ognja, magijo ledu in magijo arkane oz. fire,
 frost in arcane.

 Ko se čarodej zaposli na akademiji, se usmeri v zgodovino, poučevanje ali
 raziskovanje oz. historian, teacher in researcher. Definirajte tip
 [specialisation], ki loči med temi zaposlitvami.
[*----------------------------------------------------------------------------*)

type magic = Fire | Frost | Arcane
type specialisation = Historian | Teacher | Researcher

(*----------------------------------------------------------------------------*]
 Vsak od čarodejev začne kot začetnik, nato na neki točki postane študent,
 na koncu pa SE lahko tudi zaposli.
 Definirajte tip [status], ki določa ali je čarodej:
  a.) začetnik [Newbie],
  b.) študent [Student] (in kateri vrsti magije pripada in koliko časa študira),
  c.) zaposlen [Employed] (in vrsto magije in specializacijo).

 Nato definirajte zapisni tip [wizard] z poljem za ime in poljem za trenuten
 status.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # professor;;
 - : wizard = {name = "Matija"; status = Employed (Fire, Teacher)}
[*----------------------------------------------------------------------------*)

type status = Newbie | Student of (magic * int) | Employed of (magic * specialisation)

type wizard = {
       name: string;
       status: status;
}

let professor = {name = "Matija"; status = Employed (Fire, Teacher)}

(*----------------------------------------------------------------------------*]
 Želimo prešteti koliko uporabnikov posamezne od vrst magije imamo na akademiji.
 Definirajte zapisni tip [magic_counter], ki v posameznem polju hrani število
 uporabnikov magije.
 Nato definirajte funkcijo [update counter magic], ki vrne nov števec s
 posodobljenim poljem glede na vrednost [magic].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # update {fire = 1; frost = 1; arcane = 1} Arcane;;
 - : magic_counter = {fire = 1; frost = 1; arcane = 2}
[*----------------------------------------------------------------------------*)

type magic_counter = {
       fire: int;
       frost: int;
       arcane: int;
}

let update (({fire; frost=zmrznjeni; arcane}) as counter) magic_type = match magic_type with
       | Arcane -> {arcane = counter.arcane + 1; frost = counter.frost; fire = counter.fire}
       | Fire -> {counter with fire = counter.fire + 1}
       | Frost -> {counter with frost =  zmrznjeni + 1}

(* let update counter = function
       | Fire -> {counter with fire = counter.fire + 1}
       | Frost -> {counter with frost = counter.frost + 1}
       | Arcane -> {counter with arcane = counter.arcane + 1} *)

let zero_magic = {
       fire = 0; frost = 0; arcane = 0
}

(*----------------------------------------------------------------------------*]
 Funkcija [count_magic] sprejme seznam čarodejev in vrne števec uporabnikov
 različnih vrst magij.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # count_magic [professor; professor; professor];;
 - : magic_counter = {fire = 3; frost = 0; arcane = 0}
[*----------------------------------------------------------------------------*)

let count_magic lst =
       let rec aux acc l = match l with
              | [] -> acc
              | x::xs -> (
                match x.status with
                 |Student (m, _) |Employed (m, _) -> aux (update acc m) xs
                 |_ -> aux acc xs
              )
in
aux zero_magic lst


(*----------------------------------------------------------------------------*]
 Želimo poiskati primernega kandidata za delovni razpis. Študent lahko postane
 zgodovinar po vsaj treh letih študija, raziskovalec po vsaj štirih letih
 študija in učitelj po vsaj petih letih študija.
 Funkcija [find_candidate magic specialisation wizard_list] poišče prvega
 primernega kandidata na seznamu čarodejev in vrne njegovo ime, čim ustreza
 zahtevam za [specialisation] in študira vrsto [magic]. V primeru, da ni
 primernega kandidata, funkcija vrne [None].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # let jaina = {name = "Jaina"; status = Student (Frost, 4)};;
 # find_candidate Frost Researcher [professor; jaina];;
 - : string option = Some "Jaina"
[*----------------------------------------------------------------------------*)

let rec find_candidate magic specialisation wizard_list =
       let year =
         match specialisation with
         |Historian -> 3
         |Researcher -> 4
         |Teacher -> 5
       in 
       let rec search wizard_list = match wizard_list with
         |[] -> None
         |{name; status} :: wizards ->
              match status with
                |Student(m, y) when m = magic && y >= year -> Some name
                |_ -> search wizards
       in
search wizard_list
