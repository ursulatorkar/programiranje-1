module Datum = struct
  type t = { dan : int; mesec : int; leto : int }

  let je_prestopno leto =
    (leto mod 4 = 0 && leto mod 100 <> 0) || leto mod 400 = 0

  let dolzina_meseca leto = function
    | 4 | 6 | 9 | 11 -> 30
    | 2 -> if je_prestopno leto then 29 else 28
    | _ -> 31

  let je_veljaven datum =
    let veljaven_dan = 1 <= datum.dan && datum.dan <= dolzina_meseca datum.leto datum.mesec
    and veljaven_mesec = 1 <= datum.mesec && datum.mesec <= 12
    in veljaven_dan && veljaven_mesec

  let naredi dan mesec leto =
    let datum = { dan; mesec; leto } in
    if je_veljaven datum then Some datum else None

  let to_string { dan; mesec; leto } =
    Format.sprintf "%04d-%02d-%02d" leto mesec dan
end;;

Datum.dolzina_meseca 1991 6;;                      (* 30 *)
Datum.je_prestopno 2024;;                          (* true *)

{ Datum.dan = 25; mesec = 6; leto = 1991 } |> Datum.to_string;;   (* "1991-06-25" *)
Datum.{ dan = 1; mesec = 1; leto = 2000 } |> Datum.to_string;;   (* "2000-01-01" *)

Datum.naredi 29 2 2024;;     (* Some { dan = 29; mesec = 2; leto = 2024 } *)
Datum.naredi 31 4 2024;;     (* None *)



module type DATUM = sig
  type t = { dan : int; mesec : int; leto : int }
  val je_prestopno : int -> bool
  val dolzina_meseca : int -> int -> int
  val je_veljaven : t -> bool
  val naredi : int -> int -> int -> t option
  val to_string : t -> string
end


module type DATUM = sig
  type t = { dan : int; mesec : int; leto : int }
  val naredi : int -> int -> int -> t option
  val to_string : t -> string
end;;

(* manjka to_string, zato pride do napake *)
(*module Slab : DATUM = struct
  type t = { dan : int; mesec : int; leto : int }
  let naredi dan mesec leto = Some { dan; mesec; leto }
end;;*)

(* napačen tip naredi, zato pride do napake *)
(*module Slab2 : DATUM = struct
  type t = { dan : int; mesec : int; leto : int }
  let naredi (dan, mesec, leto) = Some { dan; mesec; leto }
  let to_string _ = ""
end;;*)

module type DATUM = sig
  type t
  val naredi : int -> int -> int -> t option
  val to_string : t -> string
end;;

module Datum : DATUM = struct
  type t = { dan : int; mesec : int; leto : int }

  let je_prestopno leto =
    (leto mod 4 = 0 && leto mod 100 <> 0) || leto mod 400 = 0

  let dolzina_meseca leto = function
    | 4 | 6 | 9 | 11 -> 30
    | 2 -> if je_prestopno leto then 29 else 28
    | _ -> 31

  let je_veljaven d =
    1 <= d.mesec && d.mesec <= 12
    && 1 <= d.dan && d.dan <= dolzina_meseca d.leto d.mesec

  let naredi dan mesec leto =
    let d = { dan; mesec; leto } in
    if je_veljaven d then Some d else None

  let to_string { dan; mesec; leto } =
    Format.sprintf "%04d-%02d-%02d" leto mesec dan
end;;

Datum.naredi 25 6 1991;;          (* Some <abstr> *)
Datum.naredi 32 13 2024;;         (* None *)

match Datum.naredi 25 6 1991 with
| Some d -> Datum.to_string d
| None -> "neveljaven";;          (* "1991-06-25" *)

let stevilo_razlicnih xs =
  let rec aux ze_videni = function
    | [] -> List.length ze_videni
    | x :: xs ->
        if List.mem x ze_videni
        then aux ze_videni xs
        else aux (x :: ze_videni) xs
  in
  aux [] xs;;

stevilo_razlicnih [1; 2; 1; 2; 1; 2; 3];;   (* 3 *)
stevilo_razlicnih ["a"; "b"; "a"];;         (* 2 *)
stevilo_razlicnih [];;                      (* 0 *)
List.mem 3 [1; 2; 3];;                      (* true *)

let stopaj f x =
  let zacetek = Sys.time () in
  let y = f x in
  let konec = Sys.time () in
  Printf.printf "Porabljen čas: %f ms\n" (1000. *. (konec -. zacetek));
  y

let nakljucni_seznam m n = List.init n (fun _ -> Random.int m);;
let seznam_zaporednih n = List.init n (fun i -> i);;

let stopaj f x =
  let zacetek = Sys.time () in
  let y = f x in
  let konec = Sys.time () in
  let izpis =
    Printf.sprintf "Porabljen čas: %f ms\n" (1000. *. (konec -. zacetek))
  in
  print_endline izpis;
  y;;


module type MNOZICA = sig
  type 'a t
  val prazna : 'a t
  val dodaj : 'a -> 'a t -> 'a t
  val velikost : 'a t -> int
  val vsebuje : 'a -> 'a t -> bool
end;;

module Mnozica : MNOZICA = struct
  type 'a t = 'a list
  let prazna = []
  let velikost m = List.length m
  let vsebuje x m = List.mem x m
  let dodaj x m = if vsebuje x m then m else x :: m
end;;

let m = Mnozica.prazna |> Mnozica.dodaj 1 |> Mnozica.dodaj 2 |> Mnozica.dodaj 1;;
Mnozica.velikost m;;        (* 2 *)
Mnozica.vsebuje 2 m;;       (* true *)
Mnozica.vsebuje 5 m;;       (* false *)
m;;                         (* <abstr>, ker je notranjost skrita *)

let stevilo_razlicnih xs =
  let rec aux ze_videni = function
    | [] -> Mnozica.velikost ze_videni
    | x :: xs -> aux (Mnozica.dodaj x ze_videni) xs
  in
  aux Mnozica.prazna xs;;

stevilo_razlicnih [1; 2; 1; 2; 1; 2; 3];;              (* 3 *)
stopaj stevilo_razlicnih (seznam_zaporednih 2000);;    (* enako počasno kot prej *)