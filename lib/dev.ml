open Core

type t = {ip: Ipaddr.V4.t; mac: Macaddr.t; mtu: int; tap: Tap.t}

let create ip mac mtu tap =
  {ip= Ipaddr.V4.of_string_exn ip; mac= Macaddr.of_string_exn mac; mtu; tap}

let get_ip t = t.ip

let get_mac t = t.mac

let get_mtu t = t.mtu

let send t = Tap.send t.tap

let recv_loop t f =
  let rec aux buf =
    let r, len = Tap.recv t.tap 2048 in
    if len > 0 then aux (r :: buf) else String.concat @@ List.rev buf
  in
  let rec loop () =
    f @@ aux [] ;
    loop ()
  in
  loop ()
