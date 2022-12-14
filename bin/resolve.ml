
let ai_to_string (ai:Unix.addr_info) =
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

let usage () =
  Printf.fprintf stderr "usage: %s hostname\n%!" Sys.argv.(0);
  Unix._exit 1

let () =
  if Array.length Sys.argv <> 2 then
    usage ();
  let hostname = Sys.argv.(1) in
  match Getaddrinfo.getaddrinfo hostname "" [] with
  | Error e ->
    Printf.fprintf stderr "%s\n%!" (Getaddrinfo.error_to_string e)
  | Ok candidates ->
    List.iter (fun c -> Printf.printf "%s\n%!" (ai_to_string c)) candidates
  
  
