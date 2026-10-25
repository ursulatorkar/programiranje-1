type kartezicno = { re : float; im : float };;
type polarno = { radij : float; kot : float };;
type datum = { dan : int; mesec : int; leto : int };;

let abs { re; im } = sqrt (re ** 2. +. im ** 2.);;
abs { re = 3.; im = 4. };;    (* 5. *)

let abs' { radij; _ } = radij;;
abs' { radij = 2.; kot = 1. };;   (* 2. *)

let je_prestopno leto =
  (leto mod 4 = 0 && leto mod 100 <> 0) || leto mod 400 = 0;;

let dolzina_meseca leto = function
  | 4 | 6 | 9 | 11 -> 30
  | 2 -> if je_prestopno leto then 29 else 28
  | _ -> 31;;

let je_veljaven { dan; mesec; leto } =
  let veljaven_dan = 1 <= dan && dan <= dolzina_meseca leto mesec
  and veljaven_mesec = 1 <= mesec && mesec <= 12
  in veljaven_dan && veljaven_mesec;;

je_veljaven { dan = 29; mesec = 2; leto = 2024 };;   (* true *)
je_veljaven { dan = 31; mesec = 4; leto = 2024 };;   (* false *)

let konjugiraj z = { z with im = -. z.im }
let pred_sto_leti d = { d with leto = d.leto - 100 }

let naredi_datum dan mesec leto =
  let datum = { dan; mesec; leto } in
  if je_veljaven datum then Some datum else None
(* int -> int -> int -> datum option *)

type geometrijski_objekt =
  | Tocka
  | Krog of float                  (* polmer *)
  | Pravokotnik of float * float   (* višina, širina *)

type geometrijski_objekt =
  | Tocka
  | Krog of float
  | Pravokotnik of float * float;;

let ploscina = function
  | Tocka -> 0.
  | Krog r -> 3.14 *. r ** 2.
  | Pravokotnik (v, s) -> v *. s;;

ploscina (Krog 1.);;              (* 3.14 *)
ploscina (Pravokotnik (2., 3.));; (* 6. *)

[Tocka; Pravokotnik (1., 2.); Tocka; Krog 3.]
|> List.map ploscina
|> List.fold_left (+.) 0.;;       (* 30.26 *)

type naslov = Naslov of string;;
type telefon = Telefon of string;;

type dostava =
  | OsebniPrevzem
  | PoPosti of naslov
  | HitraDostava of naslov * telefon;;

HitraDostava (Naslov "Jadranska 21", Telefon "01 4766 600");;   (* deluje *)
HitraDostava (Telefon "01 4766 600", Naslov "Jadranska 21");;   (* napaka *)

let je_v_ljubljani (Telefon stevilka) =
  String.sub stevilka 0 2 = "01";;

je_v_ljubljani (Telefon "01 4766 600");;   (* true *)
je_v_ljubljani (Telefon "02 123 456");;    (* false *)

type naravno =
  | Nic
  | Naslednik of naravno

let rec vsota m n =
  match m with
  | Nic -> n
  | Naslednik m' -> Naslednik (vsota m' n)

type 'a seznam =
  | Prazen
  | Sestavljen of 'a * 'a seznam

type 'a drevo = Prazno | Sestavljeno of 'a drevo * 'a * 'a drevo;;

let list x = Sestavljeno (Prazno, x, Prazno);;
let d = Sestavljeno (Sestavljeno (list 1, 3, Prazno), 5, list 8);;

let rec velikost = function
  | Prazno -> 0
  | Sestavljeno (l, _, d) -> velikost l + 1 + velikost d;;

type 'a drevo = Prazno | Sestavljeno of 'a drevo * 'a * 'a drevo;;

let list x = Sestavljeno (Prazno, x, Prazno);;
let d = Sestavljeno (Sestavljeno (list 1, 3, Prazno), 5, list 8);;

let rec velikost = function
  | Prazno -> 0
  | Sestavljeno (l, _, d) -> velikost l + 1 + velikost d;;

type 'a drevo = Prazno | Sestavljeno of 'a drevo * 'a * 'a drevo;;

let list x = Sestavljeno (Prazno, x, Prazno);;
let d = Sestavljeno (Sestavljeno (list 1, 3, Prazno), 5, list 8);;

let rec velikost = function
  | Prazno -> 0
  | Sestavljeno (l, _, d) -> velikost l + 1 + velikost d;;