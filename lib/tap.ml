type t = {fd: Unix.file_descr; name: string}

let create_tap () =
  let fd, name = Tuntap.opentap ~devname:"tap1" () in
  {fd; name}
