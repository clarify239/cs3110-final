(**file to be deleted later, testing to see if user input 
typed in terminal shows up on webpage*)

open Lwt.Syntax

let clients : Dream.websocket list ref = ref []

let remove_client ws =
  clients := List.filter (fun c -> c != ws) !clients

let broadcast msg =
  Lwt_list.iter_p
    (fun ws ->
      Lwt.catch
        (fun () -> Dream.send ws msg)
        (fun _ -> Lwt.return_unit))
    !clients

let rec stdin_loop () =
  let* line_opt = Lwt_io.read_line_opt Lwt_io.stdin in
  match line_opt with
  | None -> Lwt.return_unit
  | Some line ->
      let* () = broadcast line in
      stdin_loop ()

let ws_handler _req =
  Dream.websocket (fun ws ->
    clients := ws :: !clients;
    Lwt.finalize
      (fun () ->
        let rec keep_open () =
          let* msg = Dream.receive ws in
          match msg with
          | None -> Lwt.return_unit
          | Some _ -> keep_open ()
        in
        keep_open ())
      (fun () ->
        remove_client ws;
        Lwt.return_unit))

let () =
  Lwt.async stdin_loop;

  Dream.run
  @@ Dream.logger
  @@ Dream.router [
       Dream.get "/" (Dream.from_filesystem "public" "printOcaml.html");
       Dream.get "/ws" ws_handler;
     ]