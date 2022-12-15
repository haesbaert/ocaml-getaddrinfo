(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*   Copyright 2022 Christiano Haesbaert                                  *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)
open Sexplib.Std

module Unix = struct
  include Unix

  type socket_domain = Unix.socket_domain =
    | PF_UNIX                     (** Unix domain *)
    | PF_INET                     (** Internet domain (IPv4) *)
    | PF_INET6                    (** Internet domain (IPv6) *)
  [@@deriving sexp]

  type socket_type = Unix.socket_type =
    | SOCK_STREAM                (** Stream socket *)
    | SOCK_DGRAM                  (** Datagram socket *)
    | SOCK_RAW                    (** Raw socket *)
    | SOCK_SEQPACKET              (** Sequenced packets socket *)
  [@@deriving sexp]

  type inet_addr = Unix.inet_addr

  let inet_addr_of_sexp sexp = string_of_sexp sexp |> Unix.inet_addr_of_string
  let sexp_of_inet_addr ia = Unix.string_of_inet_addr ia |> sexp_of_string

  type sockaddr = Unix.sockaddr =
    | ADDR_UNIX of string
    | ADDR_INET of inet_addr * int
  [@@deriving sexp]

  type addr_info = Unix.addr_info =
    { ai_family : socket_domain;          (** Socket domain *)
      ai_socktype : socket_type;          (** Socket type *)
      ai_protocol : int;                  (** Socket protocol number *)
      ai_addr : sockaddr;                 (** Address *)
      ai_canonname : string               (** Canonical host name  *)
    }
  [@@deriving sexp]

end

(* keep in sync with C stubs *)
type error =
  | EAI_ADDRFAMILY
  | EAI_AGAIN
  | EAI_BADFLAGS
  | EAI_BADHINTS
  | EAI_FAIL
  | EAI_FAMILY
  | EAI_MEMORY
  | EAI_NODATA
  | EAI_NONAME
  | EAI_OVERFLOW
  | EAI_PROTOCOL
  | EAI_SERVICE
  | EAI_SOCKTYPE
  | EAI_SYSTEM
[@@deriving sexp]

let error_to_string = function
  | EAI_ADDRFAMILY -> "address family for name not supported"
  | EAI_AGAIN      -> "temporary failure in name resolution"
  | EAI_BADFLAGS   -> "invalid value for ai_flags"
  | EAI_BADHINTS   -> "invalid value for hints"
  | EAI_FAIL       -> "non-recoverable failure in name resolution"
  | EAI_FAMILY     -> "ai_family not supported"
  | EAI_MEMORY     -> "memory allocation failure"
  | EAI_NODATA     -> "no address associated with name"
  | EAI_NONAME     -> "name or service is not known"
  | EAI_OVERFLOW   -> "argument buffer overflow"
  | EAI_PROTOCOL   -> "resolved protocol is unknown"
  | EAI_SERVICE    -> "service not supported for ai_socktype"
  | EAI_SOCKTYPE   -> "ai_socktype not supported"
  | EAI_SYSTEM     -> "system error"

let to_hum ai =
  Unix.sexp_of_addr_info ai |> Sexplib.Sexp.to_string_hum

let to_hums ais =
  sexp_of_list Unix.sexp_of_addr_info ais |> Sexplib.Sexp.to_string_hum

external getaddrinfo : string -> string -> Unix.getaddrinfo_option list ->
  (Unix.addr_info list, error) result
  = "caml_local_getaddrinfo"

module Async = struct

  (* XXX There must be a better way to do this ? *)
  type res =
    | Yay of Unix.addr_info list
    | Nay of error
  [@@deriving sexp]

  let getaddrinfo ~(post_fork : (unit -> unit)) host service ops =
    let r, w = Unix.pipe () in
    match Unix.fork () with
    | 0 ->     (* child *)
      Unix.close r;
      post_fork ();
      let oc = Unix.out_channel_of_descr w in
      let v = match getaddrinfo host service ops with
        | Ok x -> Yay x
        | Error x -> Nay x
      in
      let () =
        try Sexplib.Sexp.output_mach oc (sexp_of_res v) with _ -> ()
      in
      Out_channel.close_noerr oc;
      Unix._exit 0
    | pid ->   (* parent *)
      Unix.close w;
      (pid, r)

  let fetch r =
    let ic = Unix.in_channel_of_descr r in
    try
      let x =
        Fun.protect ~finally:(fun () -> In_channel.close_noerr ic)
          (fun () -> Sexplib.Sexp.input_sexp ic)
        |> res_of_sexp
      in
      match x with Yay x -> Ok x | Nay x -> Error x
    with
      End_of_file | Failure _ -> Error EAI_SYSTEM (* best bet *)

end
