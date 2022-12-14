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

type socket_domain = Unix.socket_domain =
  | PF_UNIX                     (** Unix domain *)
  | PF_INET                     (** Internet domain (IPv4) *)
  | PF_INET6                    (** Internet domain (IPv6) *)
[@@deriving sexp]
(* type socket_domain = [%import: Unix.socket_domain] [@@deriving sexp] *)

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

type addr_info = Unix.addr_info

external getaddrinfo : string -> string -> Unix.getaddrinfo_option list ->
  (addr_info list, error) result
  = "caml_local_getaddrinfo"
