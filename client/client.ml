(* open Lwt.Syntax *)
(* Open Omd *)
module S = Irmin_mem.KV.Make (Irmin.Contents.String)
module Client = Irmin_client_jsoo.Make (S)
module Store = Irmin_client_jsoo.Store.Make (S)

let display_text result = 
  let open Brr in
  let result_element =  (Document.find_el_by_id G.document) (Jstr.v "result") in
  match result_element with
  | Some v -> El.set_prop (El.Prop.jstr (Jstr.v "innerHTML")) (Jstr.v result) v
  | None -> () 

let preview_markdown elem = 
  let open Brr in
  let data = Jv.to_string elem in
  let data = [El.txt (Jstr.v data)] in
  let markdown_preview_btn =  (Document.find_el_by_id G.document) (Jstr.v "markdown_preview") in
  match markdown_preview_btn with
  | Some elem -> El.set_children elem data
  | None -> () 

let open_modal () = 
  let open Brr in
  let modal = (Document.find_el_by_id G.document) (Jstr.v "note_modal") in 
  Option.iter
    (fun elem ->
      El.(set_inline_style Style.display (Jstr.v "block") elem))
    modal

let close_modal () = 
  let open Brr in
  let modal = (Document.find_el_by_id G.document) (Jstr.v "note_modal") in 
  Option.iter
    (fun elem ->
      El.(set_inline_style Style.display (Jstr.v "none") elem))
    modal

let () =
  let open Brr in
  let modal_btn = (Document.find_el_by_id G.document) (Jstr.v "new_note") in
  Option.iter
    (fun elem ->
      Ev.listen Ev.click (fun _ -> open_modal ()) (El.as_target elem))
      modal_btn;
  let close_btn = (Document.find_el_by_id G.document) (Jstr.v "close_modal") in
  Option.iter
    (fun elem ->
      Ev.listen Ev.click (fun _ -> close_modal ()) (El.as_target elem))
      close_btn;
  let markdown_content = (Document.find_el_by_id G.document) (Jstr.v "markdown_content") in
  Option.iter
    (fun elem ->
      Ev.listen Ev.keyup
        (fun e -> preview_markdown (Jv.get (Jv.get (Ev.to_jv e) "target") "value" ))
        (El.as_target elem))
    markdown_content
