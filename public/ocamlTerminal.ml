(**file to be deleted later, testing to see if user input typed in terminal
   shows up on webpage*)
open Lwt.Syntax

let clients : Dream.websocket list ref = ref []
let bracket = ref "our group name"
let answer = ref "group 67"
let remove_client ws = clients := List.filter (fun c -> c != ws) !clients

let broadcast msg =
  Lwt_list.iter_p
    (fun ws ->
      Lwt.catch (fun () -> Dream.send ws msg) (fun _ -> Lwt.return_unit))
    (*pushes messages to page*) !clients

let broadcast_bracket () = broadcast ("BRACKET|" ^ !bracket)

let handle_guess guess =
  if String.trim guess = !answer then (
    print_endline "correct";
    bracket := guess;
    broadcast_bracket ())
  else (
    print_endline "Incorrect";
    Lwt.return_unit)

let rec stdin_loop () =
  let* line_opt = Lwt_io.read_line_opt Lwt_io.stdin in
  match line_opt with
  | None -> Lwt.return_unit
  | Some line ->
      let* () = broadcast line in
      stdin_loop ()

let ws_handler _req =
  Dream.websocket (fun ws ->
      (*opens websocket*)
      clients := ws :: !clients;
      Lwt.finalize
        (fun () ->
          let* () = Dream.send ws ("BRACKET|" ^ !bracket) in
          let rec keep_open () =
            let* msg = Dream.receive ws in
            (*recieves browser messages*)
            match msg with
            | None -> Lwt.return_unit
            | Some guess ->
                let* () = handle_guess guess in
                keep_open ()
          in
          keep_open ())
        (fun () ->
          remove_client ws;
          Lwt.return_unit))

let () =
  (* Lwt.async stdin_loop; *)
  Dream.run @@ Dream.logger
  @@ Dream.router
       [
         Dream.get "/" (Dream.from_filesystem "public" "game.html");
         Dream.get "/ws" ws_handler;
         Dream.get "/**" (Dream.static "public");
       ]
