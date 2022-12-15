let _ai_to_string (ai:Unix.addr_info) =
  let family = match ai.ai_family with
    | PF_UNIX  -> "AF_UNIX"
    | PF_INET  -> "AF_INET"
    | PF_INET6 -> "AF_INET6"
  in
  let socktype = match ai.ai_socktype with
    | SOCK_STREAM -> "SOCK_STREAM"
    | SOCK_DGRAM -> "SOCK_DGRAM"
    | SOCK_RAW -> "SOCK_RAW"
    | SOCK_SEQPACKET -> "SOCK_SEQPACKET"
  in
  let protocol = string_of_int ai.ai_protocol in
  let addr = match ai.ai_addr with
    | ADDR_UNIX s -> s
    | ADDR_INET (a, p) -> Unix.string_of_inet_addr a ^ ":" ^ (string_of_int p)
  in
  let canonname = "\"\"" in
  Printf.sprintf "%s, %s, %s, %s, %s" family socktype protocol addr canonname

let dump  = function
  | Error e ->
    Printf.fprintf stderr "%s\n%!" (Getaddrinfo.error_to_string e)
  | Ok ail ->
    List.iter (fun c -> Printf.printf "%s\n%!" (Getaddrinfo.to_hum c)) ail

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

let check_hostname vflag hostname =
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
  if vflag then
    dump s

let () =
  let vflag = ref false in
  let speclist = [("-v", Arg.Set vflag, "verbose")] in
  Arg.parse speclist (fun _ -> invalid_arg "lala") "noclue";
  List.iter (check_hostname !vflag) [
    "www.google.com"; "oidjasiodjasi.doisaj"; "haesbaert.org"; "goatse.cx"; "disney.com";
  ]
