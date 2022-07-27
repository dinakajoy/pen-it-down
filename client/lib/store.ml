open Lwt.Syntax

module KV =
  Irmin_git.Generic_KV
    (Irmin_indexeddb.Content_store)
    (Irmin_indexeddb.Branch_store)

module Str = KV.Make (Irmin.Contents.String)
module Client = Irmin_client_jsoo.Make (Str)
module Sync = Irmin.Sync.Make (Str)

let get_path key = Str.Schema.Path.v key

let get_value t k = Str.get t k

let list t =
  let* store_list = Str.list t [] in
  Lwt.return @@ List.map fst store_list

let info message () =
  Str.Info.v
    (Unix.gettimeofday () |> Int64.of_float)
    ~author:"penit-client" ~message

let temporary_save t k v =
  let+ response = Str.set ~info:(info "Saving note") t k v in
  match response with Ok () -> "Saved..." | Error _ -> "Error saving..."

let temporary_delete t k =
  let+ response = Str.remove ~info:(info "Deleting note") t k in
  match response with Ok () -> "Deleted..." | Error _ -> "Error deleting..."

let cached_store = ref None

let get_store () =
  match !cached_store with
  | Some s -> Lwt.return s
  | None ->
    let config = Irmin_indexeddb.config "penit" in
    let* store_repo = Str.Repo.v config in
    let* store = Str.main store_repo in
    cached_store := Some store;
    Lwt.return store

let push_store () =
  let uri = Uri.of_string "ws://localhost:9090/ws" in
  let* client = Client.connect uri in
  let* main = Client.main client in
  let* s = get_store () in
  let* res = Client.ping client in
  match res with
  | Ok () -> (
    let* status =
    Sync.push_exn s (Irmin.Sync.remote_store (module Client) main)
      in
      match status with
      | `Empty -> Lwt.return "Push returned empty"
      | `Head _ -> Lwt.return "Pushed")
  | Error _ -> Lwt.return "PUSH ERROR"

let pull_store () =
  let uri = Uri.of_string "ws://localhost:9090/ws" in
  let* client = Client.connect uri in
  let* main = Client.main client in
  let* s = get_store () in
  let* res = Client.ping client in
  match res with
  | Ok () -> (
    let* status =
    Sync.pull_exn s (Irmin.Sync.remote_store (module Client) main) `Set
      in
      match status with
      | `Empty -> Lwt.return "Pull returned empty"
      | `Head _ -> Lwt.return "Pulled")
  | Error _ -> Lwt.return "PULL ERROR"
