open Core

type t = {fd: Unix.File_descr.t; name: string}

let create cidr addr =
  let fd, name = Tuntap.opentap ~devname:"tap1" () in
  let open Shexp_process in
  eval (run "ip" ["link"; "set"; "dev"; name; "up"]) ;
  eval (run "ip" ["route"; "add"; "dev"; name; cidr]) ;
  eval (run "ip" ["address"; "add"; "dev"; name; "local"; addr]) ;
  {fd; name}

let send t buf = Unix.write t.fd ~buf

let recv t len =
  let buf = Bytes.create len in
  let len' = Unix.read t.fd ~buf in
  (Bytes.to_string buf, len')
