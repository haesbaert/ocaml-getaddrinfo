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

val to_hum : Unix.addr_info -> string

val to_hums : Unix.addr_info list -> string

external getaddrinfo : string -> string -> Unix.getaddrinfo_option list -> (Unix.addr_info list, error) result
  = "caml_local_getaddrinfo"

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
