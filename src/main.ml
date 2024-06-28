open Devkit

let token = "aba831d7-66ed-41d3-b757-fc719f5ad979"

let comm data =
  match Web.http_request
    ~ua:"dysfunctional piglets"
    ~timeout:5
    ~verbose:true
    ~headers:["Authorization: Bearer " ^ token]
    ~body:(`Raw ("application/icfp",data))
    `POST
    "https://boundvariable.space/communicate"
  with
  | `Ok s -> s
  | `Error s -> Exn.fail "comm %S failed: %s" data s

let () =
  print_endline @@ comm "ICFP"
