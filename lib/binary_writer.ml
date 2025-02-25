type t =
  | Single of (unit -> string)
  | Pair of t * t
  | Multi of t list
  | Empty

type 'a writer = 'a -> t

let empty = Empty
let append a b = Pair (a, b)
let concat l = Multi l

let run write bind return =
  let ( >>= ) = bind in
  let rec inner = function
    | Single f -> write (f ())
    | Pair (t1, t2) -> inner t1 >>= fun () -> inner t2
    | Multi l ->
      let rec sequence = function
        | [] -> return ()
        | x :: xs -> inner x >>= fun () -> sequence xs
      in
        sequence l
    | Empty -> return ()
  in
    inner
;;

let char c = Single (fun () -> String.make 1 c)
let bytes s = Single (fun () -> s)

module LE = EndianString.LittleEndian_unsafe
module BE = EndianString.BigEndian_unsafe

let wrap cnt f =
  fun v ->
  Single
    (fun () ->
      let s = Bytes.make cnt '\000' in
      let () = f s 0 v in
        Bytes.unsafe_to_string s)
;;

let int8 = wrap 1 EndianBytes.LittleEndian.set_int8
let int16le = wrap 2 EndianBytes.LittleEndian.set_int16
let int16be = wrap 2 EndianBytes.BigEndian.set_int16
let int32le = wrap 4 EndianBytes.LittleEndian.set_int32
let int32be = wrap 4 EndianBytes.BigEndian.set_int32
let int64le = wrap 8 EndianBytes.LittleEndian.set_int64
let int64be = wrap 8 EndianBytes.BigEndian.set_int64
let pair t1 t2 = fun (v1, v2) -> append (t1 v1) (t2 v2)
let triple t1 t2 t3 = fun (v1, v2, v3) -> concat [ t1 v1; t2 v2; t3 v3 ]

let list t =
  let rec loop = function
    | [] -> Empty
    | x :: xs -> append (t x) (loop xs)
  in
    loop
;;
