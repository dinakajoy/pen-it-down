open Lwt.Infix
(* open Lwt.Syntax *)
module S = Irmin_mem.KV.Make (Irmin.Contents.String)
module Client = Irmin_client_jsoo.Make (S)
module Str = 
  Irmin_git.Generic_KV
    (Irmin_indexeddb.Content_store)
    (Irmin_indexeddb.Branch_store)

let repo = "/tmp/penit"

(* let save_note note_name note_content =
  let uri = Uri.of_string "ws://localhost:9090/ws" in
  let config = Irmin_client_jsoo.config uri in
  let* repo = Client.Repo.v config in
  let* t = Client.main repo in
  let+ () = Client.set_exn t ~info:Client.Info.none [ note_name ] note_content in
  () *)

let info message () =
  Irmin.Info.v
    ~date:(Unix.gettimeofday () |> Int64.of_float)
    ~author:"penit-client" message

let local_commit t k v =
  Str.set ~info:(info "some message goes here") t.staging k v >|= function
  | Ok () -> Brr.Console.log [ Jstr.v "Successful commit" ]
  | Error _ -> Brr.Console.warn [ Jstr.v "Set error" ]

let local_get t k = Str.get t.staging k

let list t =
  Str.list t.staging [] >>= fun lst -> Lwt.return @@ List.map fst lst

let init uri =
  let config =
    Irmin_git.config ~bare:true
      ~config:(Irmin_indexeddb.config "client-db")
      repo
  in
  Str.Repo.v config >>= fun repo ->
  Str.master repo >>= fun main ->
  (* Abusing the API here a little for this one off case... *)
  sync ~merge:false { main; staging = main; uri } >>= fun _ ->
  Str.Branch.find repo "staging" >>= fun commit ->
  match commit with
  | None ->
      Str.clone ~src:main ~dst:"staging" >>= fun staging ->
      Lwt.return { main; staging; uri }
  | Some c ->
      Str.of_branch repo "staging" >>= fun staging ->
      Str.Head.get main >>= fun head ->
      if compare_commit head c < 0 then Lwt.return { main; staging; uri }
      else Lwt.return { main; staging; uri }