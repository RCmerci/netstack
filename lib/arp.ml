open Core

(* https://en.wikipedia.org/wiki/Address_Resolution_Protocol *)
[%%cstruct
type arp_header =
  {htype: uint16_t; ptype: uint16_t; hlen: uint8_t; plen: uint8_t; op: uint16_t}
[@@big_endian]]

type t = {header: Cstruct.t; data: Cstruct.t}

let of_ethernet s : t =
  let open Cstruct in
  let header, data = split (of_string s) sizeof_arp_header in
  {header; data}

let get_htype t =
  match get_arp_header_htype t.header with
  | 1 ->
      Ok `ETHERNET
  | v ->
      Error (Printf.sprintf "Unknown arp htype: %d" v)

type ptype = IPV4 [@@deriving sexp, compare]

let get_ptype t =
  match get_arp_header_ptype t.header with
  | 0x0800 ->
      Ok IPV4
  | v ->
      Error (Printf.sprintf "Unknown arp ptype: %d" v)

let get_hlen t = get_arp_header_hlen t.header

let get_plen t = get_arp_header_plen t.header

let get_op t =
  match get_arp_header_op t.header with
  | 1 ->
      Ok `REQUEST
  | 2 ->
      Ok `REPLY
  | v ->
      Error (Printf.sprintf "Illgeal arp op: %d" v)

let get_sha t =
  let hlen = get_hlen t in
  Cstruct.sub t.data 0 hlen

let get_spa t =
  let hlen = get_hlen t in
  let plen = get_plen t in
  Cstruct.sub t.data hlen plen

module Ptype_SenderProtoAddr = Map.Make (struct
  type t = ptype * string [@@deriving sexp, compare]
end)

type translate_Table = string Ptype_SenderProtoAddr.t

(* https://tools.ietf.org/html/rfc826 *)
let arp_algo t (trans_table : translate_Table) =
  let open Result.Monad_infix in
  get_htype t
  >>= fun _ ->
  get_ptype t
  >>= fun ptype ->
  let key = (ptype, Cstruct.to_string (get_spa t)) in
  let merge = Ptype_SenderProtoAddr.find trans_table key in
  let trans_table' =
    match merge with
    | Some _ ->
        Ptype_SenderProtoAddr.update trans_table key ~f:(fun _ ->
            Cstruct.to_string (get_sha t) )
    | None ->
        trans_table
  in
