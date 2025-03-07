let main () =
  let write () =
    let open Binary_writer in
    let t = concat [ bytes "Hello"; char ' '; bytes "world!" ] in
    let s = Binary_string.Writer.run t in
    let b = Buffer.create 16 in
    let () = Binary_buffer.Writer.run b t in
      assert (Buffer.contents b = s);
      Printf.printf "%s\n" s
  in
    write ();
    let str () =
      let module M = Binary_monad.Utils (Binary_string.Reader) in
      let open M.Infix in
      let module MR = Binary_reader.Monadic (Binary_string.Reader) in
      let open MR in
      let s = Printf.sprintf "%cabcdef" (Char.chr 4) in
      let a = Array.make 2 (Char.chr 0) in
      let reader = pair (uint8 >>= list char) (array a char) in
      let (r, _), _ = Binary_string.Reader.run reader s in
        List.iter (Printf.printf "%c\n") r;
        Array.iter (Printf.printf "%c\n") a
    in
      str ();
      flush stdout;
      let act () =
        let open Lwt in
        let module AR = Binary_reader.Applicative (Binary_lwt.Reader) in
        let open AR in
        let reader = AR.pair (bytes 2) (bytes 2) in
          Lwt_io.with_file
            Lwt_io.Input
            "/etc/hostname"
            (Binary_lwt.Reader.run reader)
          >>= fun (a, b) -> Lwt_io.printf "%s %s\n" a b
      in
        Lwt_main.run (act ())
;;

main ()
