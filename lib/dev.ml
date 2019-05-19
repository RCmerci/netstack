open Core

type t = {ip: Ipaddr.V4.t; mac: Macaddr.t; mtu: int}

let create ip mac mtu = {ip; mac; mtu}
