let rec fakulteta =
  function
  | 0 -> 1
  | n -> n * fakulteta (n - 1);;

let rec vsota_prvih n =
  if n = 0 then 0
  else vsota_prvih (n-1) + n;;

let rec fakulteta n =
  if n = 0 then 1 else n * fakulteta (n - 1)



let rec fib n =
  if n = 0 then 0
  else if n = 1 then 1
  else fib (n - 1) + fib (n - 2)

let hitri_fib n =
  let rec aux n a b =
    if n = 0 then a else aux (n - 1) b (a + b)
  in
  aux n 0 1

let rec vsota_prvih n =
  match n with
  | 0 -> 0
  | _ -> vsota_prvih (n - 1) + n

let rec fakulteta n =
  match n with
  | 0 -> 1
  | _ -> n * fakulteta (n - 1)

let rec fib n =
  match n with
  | 0 -> 0
  | 1 -> 1
  | _ -> fib (n - 1) + fib (n - 2)

let rec fib = function
  | 0 -> 0
  | 1 -> 1
  | n -> fib (n - 1) + fib (n - 2);;

fib 10;;   (* 55 *)

let je_prestopno leto =
  (leto mod 4 = 0 && leto mod 100 <> 0) || leto mod 400 = 0;;

let dolzina_meseca leto = function
  | 4 | 6 | 9 | 11 -> 30
  | 2 -> if je_prestopno leto then 29 else 28
  | _ -> 31;;

let polozaj_tocke = function
  | (0, 0) -> "izhodišče"
  | (_, 0) -> "abscisa"
  | (0, _) -> "ordinata"
  | (_, _) -> "nekje drugje";;

let veljaven_trikotnik a b c =
  a + b > c && a + c > b && b + c > a

let oblika_trikotnika = function
  | (0, _, _) | (_, 0, _) | (_, _, 0) -> "izrojen"
  | (a, b, c) when not (veljaven_trikotnik a b c) -> "neveljaven"
  | (a, b, c) when a = b && b = c -> "enakostraničen"
  | (a, b, c) when a = b || b = c || a = c -> "enakokrak"
  | (_, _, _) -> "poljuben"

match int_of_string_opt niz with
  | Some 0 -> "nič"
  | Some _ -> "neko število"
  | None   -> "ni število"

let rec vsota = function [] -> 0 | g :: r -> g + vsota r
let rec dolzina = function [] -> 0 | _ :: r -> 1 + dolzina r
let rec map f = function [] -> [] | g :: r -> f g :: map f r  

let vsota xs   = List.fold_right ( + ) xs 0
let zmnozek xs = List.fold_right ( * ) xs 1
let dolzina xs = List.fold_right (fun _ acc -> 1 + acc) xs 0
let map f xs   = List.fold_right (fun x acc -> f x :: acc) xs []

let f' x =
  let rec aux acc x =
    if x = 0 then acc else aux (acc + 1) (x - 1)
  in
  aux 0 x

(* ne-repna *)
let rec f x = if x = 0 then 0 else 1 + f (x - 1)

(* repna *)
let f' x =
  let rec aux acc x =
    if x = 0 then acc else aux (acc + 1) (x - 1)
  in aux 0 x

let vsota' sez =
  let rec aux acc = function
    | [] -> acc
    | g :: r -> aux (acc + g) r
  in aux 0 sez

let map' f sez =
  let rec aux acc = function
    | [] -> List.rev acc
    | g :: r -> aux (f g :: acc) r
  in aux [] sez