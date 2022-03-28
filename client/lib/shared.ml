open Lwt.Syntax

module Store = Irmin_mem.KV.Make (Irmin.Contents.String)
module Client = Irmin_client_unix.Make (Store)

let display_text result = 
  let open Brr in
  let result_element =  (Document.find_el_by_id G.document) (Jstr.v "result") in
  match result_element with
  | Some v -> El.set_prop (El.Prop.jstr (Jstr.v "innerHTML")) (Jstr.v result) v
  | None -> () 

let main =
  let uri = Uri.of_string "tcp://localhost:9090" in
  let* client = Client.connect ~uri () in
  let+ result = Client.ping client in
  match result with
  | Ok _ -> display_text "OK"
  | Error _ -> display_text "Error"

(* let n = Lwt_main.run main *)
