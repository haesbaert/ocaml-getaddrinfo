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

external getaddrinfo : string -> string -> Unix.getaddrinfo_option list ->
  (Unix.addr_info list, error) result
  = "caml_local_getaddrinfo"
