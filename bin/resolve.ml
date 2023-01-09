let _usage () =
  Printf.fprintf stderr "usage: %s hostname\n%!" Sys.argv.(0);
  Unix._exit 1

let asyncP hostname =
  Getaddrinfo.Async.P.getaddrinfo ~post_fork:(fun () -> ()) hostname "" []
  |> Getaddrinfo.Async.P.wait

let asyncT hostname =
  Getaddrinfo.Async.T.getaddrinfo hostname "" []
  |> Getaddrinfo.Async.T.wait

let sync hostname =
  Getaddrinfo.getaddrinfo hostname "" []

let int_of_fd fd = (Obj.magic fd :> int)

let check_hostname hostname =
  (* The marker dance is trying to find a fd leak *)
  let fd_marker1 = Unix.dup Unix.stdout in
  let s  = sync hostname
  and a  = asyncP hostname
  and at = asyncT hostname
  in
  let fd_marker2 = Unix.dup Unix.stdout in
  let i1 = int_of_fd fd_marker1 in
  let i2 = int_of_fd fd_marker2 in
  assert ((succ i1) = i2);
  Unix.close fd_marker1;
  Unix.close fd_marker2;
  assert (s = a);
  assert (a = at);
  match s with
  | Error e -> Printf.eprintf "%s\n%!" (Getaddrinfo.error_to_string e)
  | Ok ail -> List.iter (fun ai -> Format.printf "%a\n%!" Getaddrinfo.pp ai) ail

let () =
  (* if Array.length Sys.argv <> 2 then *)
  (*   usage (); *)
  (* let hostname = Sys.argv.(1) in *)
  List.iter check_hostname [
    "www.google.com"; "oidjasiodjasi.doisaj"; "haesbaert.org"; "goatse.cx"; "disney.com";
  ]
