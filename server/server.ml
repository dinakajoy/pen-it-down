open Lwt.Syntax

module Store = Irmin_unix.Git.FS.KV (Irmin.Contents.String)

module Config = struct
  type info = Store.info
  let remote = None
  let info = Irmin_unix.info
end

module Server = Irmin_server.Make (Store)

let store_location = "./store"

let config = Irmin_git.config ~bare:true store_location

let main =
  let config = Irmin_mem.config () in
  let uri = Uri.of_string "tcp://localhost:9090" in
  let* server = Server.v ~uri config in
  let () = Format.printf "Listening on %a@." Uri.pp uri in
  Server.serve server

let () = Lwt_main.run main
