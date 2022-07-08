open Lwt.Syntax

module KV = 
  Irmin_git.Generic_KV
    (Irmin_indexeddb.Content_store)
    (Irmin_indexeddb.Branch_store)

module Str = KV.Make (Irmin.Contents.String)
module Client = Irmin_client_jsoo.Make (Str)

module Sync = Irmin.Sync.Make (Client)
(* module Sync = Irmin.Sync.Make (Str) *)

let info message () =
  Str.Info.v
    (Unix.gettimeofday () |> Int64.of_float)
    ~author:"penit-client" ~message

let server_store () = 
  let uri = Uri.of_string "ws://localhost:9090/ws" in
  let config = Irmin_client_jsoo.config uri in
  let* repo = Client.Repo.v config in
  let* store = Client.main repo in
  Lwt.return store

let get_store () =
  let config = Irmin_indexeddb.config "penit" in
  let* store_repo = Str.Repo.v config in 
  let* store = Str.main store_repo in 
  Lwt.return store

let pull_store () =
  let* r_store = server_store () in
  let _ = Sync.pull_exn r_store (Client.E ()) `Set in 
  Lwt.return "pull"

let push_store () =
  let* r_store = server_store () in
  let* _ = Sync.push_exn r_store (Client.E ()) in 
  Lwt.return "push"

let get_path key =
  Str.Schema.Path.v key

let temporary_save t k v =
  let+ response = Str.set ~info:(info "Saving note") t k v in
  match response with
  | Ok () -> "Saved..."
  | Error _ -> "Error saving..."

let temporary_delete t k =
  let+ response = Str.remove ~info:(info "Deleting note") t k in
  match response with
  | Ok () -> "Deleted..."
  | Error _ -> "Error deleting..."

let get_content t k = Str.get t k

let list t =
  let* store_list = Str.list t [] in 
  Lwt.return @@ List.map fst store_list