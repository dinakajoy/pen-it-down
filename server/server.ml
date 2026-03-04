open Lwt.Syntax
module Store = Irmin_git_unix.FS.KV (Irmin.Contents.String)
module Server = Irmin_server_unix.Make (Store)

let info () = Store.Info.v ~author:"" ~message:"Saving data..." (Unix.gettimeofday () |> Int64.of_float)

let main =
  let uri = Uri.of_string "ws://localhost:9090/ws" in
  let config = Irmin_git.config "penit" in
  let* store = Store.Repo.v config in
  let* main = Store.main store in
  (* Check if store is empty and have a default note *)
  let* root_contents = Store.list main [] in
  let* () =
    if root_contents = [] then
      Store.set_exn
        ~info
        main
        [ "Init" ]
        "# A markdown note!!"
    else
      Lwt.return_unit
  in
  let* server = Server.v ~uri config in
  let () = Format.printf "Listening on %a@." Uri.pp uri in
  Server.serve server

let () = Lwt_main.run main
