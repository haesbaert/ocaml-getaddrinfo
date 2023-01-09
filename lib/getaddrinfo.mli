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

(** like the ones in getaddrinfo(3). *)
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

val error_to_string : error -> string
(** [error_to_string e] is like gai_strerror(3). *)

val pp : Format.formatter -> Unix.addr_info -> unit
(** is a pretty printer for {!Unix.addr_info}. *)

val to_sexp : Unix.addr_info -> Sexplib0.Sexp.t
(** is the sexp of {!Unix.addr_info}. *)

val of_sexp : Sexplib0.Sexp.t -> Unix.addr_info
(** is the [Unix.addr_info] of a sexp. *)

val to_hum : Unix.addr_info -> string
(** is the human readable sexp of {!Unix.addr_info} as a string. *)

val getaddrinfo :  string -> string -> Unix.getaddrinfo_option list ->
  (Unix.addr_info list, error) result
(** is like {!Unix.getaddrinfo} but returns a result instead. The
    proper error from getaddrinfo(3) is returned in case of an error,
    unlike the stdlib version where you only get an empty list. *)

module Async : sig

  module P : sig

    type t

    val pid_of_t : t -> int

    val fd_of_t : t -> Unix.file_descr

    val getaddrinfo :
      post_fork:(unit -> unit) ->
      string -> string -> Unix.getaddrinfo_option list -> t

    val wait : t -> (Unix.addr_info list, error) result

  end

  module T : sig

    type t

    val tid_of_t : t -> Thread.t

    val fd_of_t : t -> Unix.file_descr

    val getaddrinfo : string -> string -> Unix.getaddrinfo_option list -> t

    val wait : t -> (Unix.addr_info list, error) result

  end
end
