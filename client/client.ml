open Brr

(* let display_text result = 
  let result_element =  (Document.find_el_by_id G.document) (Jstr.v "result") in
  match result_element with
  | Some v -> El.set_prop (El.Prop.jstr (Jstr.v "innerHTML")) (Jstr.v result) v
  | None -> () *)

let set_date () = 
  let date_span =  (Document.find_el_by_id G.document) (Jstr.v "date") in
  match date_span with
  | Some v ->  El.set_prop (El.Prop.jstr (Jstr.v "innerHTML")) (Jstr.v "2022") v;
  | None -> ()

let () =
  set_date ();
  (* Lwt_main.run Shared.main *)
