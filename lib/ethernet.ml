open Core

[%%cstruct
type ethernet_header =
  {dst: uint8_t [@len 6]; src: uint8_t [@len 6]; ethertype: uint16_t}
[@@big_endian]]

type t = {header: Cstruct.t; payload: Cstruct.t}

let of_string s : t =
  let open Cstruct in
  let header, payload = split (of_string s) sizeof_ethernet_header in
  {header; payload}

let get_dst t : Macaddr.t =
  Cstruct.to_string (get_ethernet_header_dst t.header) |> Macaddr.of_bytes_exn

let get_src t : Macaddr.t =
  Cstruct.to_string (get_ethernet_header_src t.header) |> Macaddr.of_bytes_exn

let get_ethertype t =
  match get_ethernet_header_ethertype t.header with
  | 0x0806 ->
      Ok `ARP
  | 0x0800 ->
      Ok `IPV4
  | v ->
      Error (Printf.sprintf "Unknown ethertype: %d" v)

let get_payload t = Cstruct.to_bytes t.payload
