type t = {ip: Ipaddr.V4.t; mac: Macaddr.t; mtu: int}

let create ip mac mtu =
  {ip= Ipaddr.V4.of_string_exn ip; mac= Macaddr.of_string_exn mac; mtu}

let get_ip t = t.ip

let get_mac t = t.mac

let get_mtu t = t.mtu

let send_tap _buf = failwith "not impl yet"

let dev = create "10.0.0.4" "00:0c:29:6d:50:25" 1500

let loop = create "127.0.0.1" "00:00:00:00:00:00" 1500
