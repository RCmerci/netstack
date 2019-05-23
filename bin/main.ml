open Netstack

let tap = Tap.create "10.0.0.0/24" "10.0.0.5"

let dev = Dev.create "10.0.0.4" "00:0c:29:6d:50:25" 1500

let loop = Dev.create "127.0.0.1" "00:00:00:00:00:00" 1500

let e = Ethernet.
