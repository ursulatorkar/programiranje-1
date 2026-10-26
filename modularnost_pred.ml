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

dolzina_meseca 2024 2;;      (* napaka, ker je funkcija znotraj modula *)