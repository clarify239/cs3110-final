(** Bracket City entry-point that wires the Dream HTTP/WebSocket server to the static frontend assets and pins down the broadcast architecture (one shared puzzle, multi-client fan-out via a [clients] list) that [main_logic.ml] later builds the full game loop on top of. *)
open Lwt.Syntax

(** All currently connected browser sessions; every push goes through this list so a single guess updates every player at once. *)
let clients : Dream.websocket list ref = ref []

(** The puzzle string the frontend currently displays — mutated when a correct guess lands so the next [BRACKET|] broadcast reflects the new state. *)
let bracket = ref "our group name"

(** The answer the server checks each incoming guess against; kept as a [ref] so a future loader can swap puzzles without restarting the server. *)
let answer = ref "group 67"

(** [remove_client ws] drops [ws] from [clients] when its session ends so closed sockets are not retained or written to. *)
let remove_client ws = clients := List.filter (fun c -> c != ws) !clients

(** [broadcast msg] sends [msg] to every connected client in parallel, swallowing per-socket send failures so one dead client cannot block the rest. *)
let broadcast msg =
  Lwt_list.iter_p
    (fun ws ->
      Lwt.catch (fun () -> Dream.send ws msg) (fun _ -> Lwt.return_unit))
    !clients

(** [broadcast_bracket ()] pushes the current [!bracket] to every client tagged with the [BRACKET|] protocol prefix the frontend listens for. *)
let broadcast_bracket () = broadcast ("BRACKET|" ^ !bracket)

(** [handle_guess guess] checks [guess] against [!answer]; on a match it updates [!bracket] and broadcasts the new state, otherwise it logs and returns without touching shared state. *)
let handle_guess guess =
  if String.trim guess = !answer then (
    print_endline "correct";
    bracket := guess;
    broadcast_bracket ())
  else (
    print_endline "Incorrect";
    Lwt.return_unit)

(** [stdin_loop ()] is a developer-only Lwt loop that forwards every line typed into the server's terminal to all clients, useful for poking the broadcast path without a real guess. *)
let rec stdin_loop () =
  let* line_opt = Lwt_io.read_line_opt Lwt_io.stdin in
  match line_opt with
  | None -> Lwt.return_unit
  | Some line ->
      let* () = broadcast line in
      stdin_loop ()

(** [ws_handler req] handles one [/ws] connection: registers the socket in [clients], pushes the initial bracket, then loops on incoming guesses until the client disconnects, with [Lwt.finalize] guaranteeing cleanup. *)
let ws_handler _req =
  Dream.websocket (fun ws ->
      clients := ws :: !clients;
      Lwt.finalize
        (fun () ->
          let* () = Dream.send ws ("BRACKET|" ^ !bracket) in
          let rec keep_open () =
            let* msg = Dream.receive ws in
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
         Dream.get "/" (Dream.from_filesystem "public" "index.html");
         Dream.get "/game" (Dream.from_filesystem "public" "game.html");
         Dream.get "/ws" ws_handler;
         Dream.get "/**" (Dream.static "public");
       ]
