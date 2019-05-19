type t = {header: Cstruct.t; payload: Cstruct.t}

val of_string : string -> t

val get_dst : t -> Macaddr.t

val get_src : t -> Macaddr.t

val get_ethertype : t -> ([> `ARP | `IPV4], string) result

val get_payload : t -> bytes
