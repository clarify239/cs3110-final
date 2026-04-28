(* =============================================================================
   main_logic.ml — Dynamic game loop for Bracket City
   =============================================================================
   This file is the replacement for main.ml once the game is fully wired up.
   It connects the OCaml game engine (lib/game.ml, lib/parser.ml) to the Dream
   WebSocket server so that the browser frontend gets real puzzle data instead
   of the hardcoded strings in main.ml.

   TO RUN THIS SERVER (instead of main.ml):
     dune exec bin/main_logic.exe
   from the project root directory.

   ARCHITECTURE OVERVIEW:
   - All puzzles are loaded from JSON once at startup into [all_puzzles].
   - Each WebSocket connection (one per browser tab) gets its own puzzle [ref],
     so players are independent. This is "per-session" mode.
   - The game state IS the puzzle tree — Game.submit mutates [node.solved] flags
     in place, so [!state] always reflects current progress without any extra
     bookkeeping.
   - The frontend (game-scripts.js) already handles "BRACKET|..." messages.
     New message types (INCORRECT, WIN, EXPOSED) are stubbed out below and
     just need matching JS handlers to be activated.
   ============================================================================= *)

open Lwt.Syntax
open Cs3110_final

(* -----------------------------------------------------------------------------
   STARTUP: Load all puzzles from the JSON data file.
   -----------------------------------------------------------------------------
   This runs exactly once when the module is first loaded (i.e. at server
   startup), not on every request. All WebSocket sessions share this list and
   call choose_puzzle to get their own independent copy.

   PATH NOTE: "data/ver2_NESTED_puzzles.json" is relative to the directory
   where you run `dune exec`, which is the project root. This differs from the
   test suite, which uses "../data/..." because tests run from
   _build/default/test/.
   ----------------------------------------------------------------------------- *)
let all_puzzles : Types.puzzle list =
  Parser.load_puzzles "data/ver2_NESTED_puzzles.json"

(* -----------------------------------------------------------------------------
   DEFAULT DIFFICULTY
   -----------------------------------------------------------------------------
   STUB: This is hardcoded to "hard" for now. Once a lobby or difficulty-select
   page exists, difficulty should be passed as a query parameter on the WebSocket
   URL (e.g. ws://localhost:8080/ws?difficulty=easy) and extracted from the
   Dream.request inside ws_handler.

   To change difficulty right now, edit this string.
   Valid values mirror what is in the JSON: "easy", "medium", "hard".
   ----------------------------------------------------------------------------- *)
let default_difficulty : string = "hard"

(* -----------------------------------------------------------------------------
   CONNECTED CLIENTS LIST
   -----------------------------------------------------------------------------
   [clients] tracks every open WebSocket. It is only used by [_broadcast_all],
   which is stubbed out for a future collaborative mode. In per-session mode
   (the current design) we never need to contact all clients — each handler
   only talks to its own [ws].

   If you remove collaborative mode entirely, you can delete this ref and
   [_broadcast_all] without touching anything else.
   ----------------------------------------------------------------------------- *)
let clients : Dream.websocket list ref = ref []

(* Remove a single WebSocket from the global client list on disconnect. *)
let remove_client (ws : Dream.websocket) : unit =
  clients := List.filter (fun c -> c != ws) !clients

(* -----------------------------------------------------------------------------
   send_to: Send one message to one WebSocket, silently ignoring errors.
   -----------------------------------------------------------------------------
   Dream.send raises an exception if the socket is already closed (e.g. the
   browser tab was closed mid-game). We catch that here so the server does not
   crash on a disconnected client.
   ----------------------------------------------------------------------------- *)
let send_to (ws : Dream.websocket) (msg : string) : unit Lwt.t =
  Lwt.catch
    (fun () -> Dream.send ws msg)
    (fun _exn -> Lwt.return_unit)

(* -----------------------------------------------------------------------------
   STUB: _broadcast_all — Send the same message to every connected client.
   -----------------------------------------------------------------------------
   Not used in per-session mode. Activate this for a collaborative mode where
   all players share one puzzle: replace [send_to ws] calls with
   [_broadcast_all] wherever you want all clients to see an update.
   ----------------------------------------------------------------------------- *)
let _broadcast_all (msg : string) : unit Lwt.t =
  Lwt_list.iter_p (fun ws -> send_to ws msg) !clients

(* -----------------------------------------------------------------------------
   send_bracket: Render current puzzle state and push it to one client.
   -----------------------------------------------------------------------------
   "BRACKET|" is the message prefix that game-scripts.js already handles:

     ws.onmessage = (event) => {
       if (msg.startsWith("BRACKET|")) {
         bracket1.textContent = "[" + msg.slice("BRACKET|".length) + "]";
       }
     };

   Game.render returns the puzzle string with unsolved nodes shown as [clue]
   and solved nodes shown as their bare answer. The frontend wraps the whole
   thing in [ ] itself, so we do NOT add outer brackets here.
   ----------------------------------------------------------------------------- *)
let send_bracket (ws : Dream.websocket) (state : Types.puzzle ref) : unit Lwt.t
    =
  send_to ws ("BRACKET|" ^ Game.render !state)

(* -----------------------------------------------------------------------------
   STUB: _send_exposed — Tell the client which answers are currently guessable.
   -----------------------------------------------------------------------------
   Game.exposed returns the "frontier" nodes: unsolved nodes whose children are
   all solved. At game start these are the leaves. As the player guesses
   correctly, their parents become the new frontier.

   Sending this list lets the frontend display a hint panel like:
     "You can guess: Chap, name, Michael, should, head, forward"
   without revealing the full answer tree structure.

   TO ACTIVATE:
   1. Uncomment the [send_to] call in this function body.
   2. Add a handler in game-scripts.js:
        if (msg.startsWith("EXPOSED|")) {
          const answers = msg.slice("EXPOSED|".length).split(",");
          // render answers somewhere in the UI as hints
        }
   3. Uncomment the [_send_exposed] calls inside [handle_guess] and [ws_handler].
   ----------------------------------------------------------------------------- *)
let _send_exposed (ws : Dream.websocket) (state : Types.puzzle ref) :
    unit Lwt.t =
  let exposed_nodes = Game.exposed !state in
  let answer_list = List.map (fun (n : Types.node) -> n.answer) exposed_nodes in
  let csv = String.concat "," answer_list in
  (* STUB: uncomment the line below once the frontend handles "EXPOSED|" *)
  (* send_to ws ("EXPOSED|" ^ csv) *)
  ignore csv;
  ignore ws;
  Lwt.return_unit

(* -----------------------------------------------------------------------------
   STUB: _send_incorrect — Tell the client their guess was wrong.
   -----------------------------------------------------------------------------
   Currently a wrong guess causes nothing visible to happen — the input clears
   and the bracket stays the same, which is confusing. This stub sends the
   rejected guess back so the frontend can flash an error or shake the input.

   TO ACTIVATE:
   1. Uncomment the [send_to] call in this function body.
   2. Add a handler in game-scripts.js:
        if (msg.startsWith("INCORRECT|")) {
          const badGuess = msg.slice("INCORRECT|".length);
          // flash error, shake input box, increment wrong-guess counter, etc.
        }
   3. Uncomment the [_send_incorrect] call inside [handle_guess].
   ----------------------------------------------------------------------------- *)
let _send_incorrect (ws : Dream.websocket) (guess : string) : unit Lwt.t =
  (* STUB: uncomment the line below once the frontend handles "INCORRECT|" *)
  (* send_to ws ("INCORRECT|" ^ guess) *)
  ignore guess;
  ignore ws;
  Lwt.return_unit

(* -----------------------------------------------------------------------------
   STUB: _send_win — Tell the client they have solved the whole puzzle.
   -----------------------------------------------------------------------------
   Game.is_won checks whether the root node is solved. The root can only be
   solved after every node in the tree has been guessed correctly, so this fires
   exactly once per game.

   TO ACTIVATE:
   1. Uncomment the [send_to] call in this function body.
   2. Add a handler in game-scripts.js:
        if (msg.startsWith("WIN|")) {
          const finalAnswer = msg.slice("WIN|".length);
          // show congratulations screen, confetti, play sound, etc.
        }
   3. Uncomment the [_send_win] call inside [handle_guess].
   ----------------------------------------------------------------------------- *)
let _send_win (ws : Dream.websocket) (state : Types.puzzle ref) : unit Lwt.t =
  (* STUB: uncomment the line below once the frontend handles "WIN|" *)
  (* send_to ws ("WIN|" ^ !state.root.answer) *)
  ignore state;
  ignore ws;
  Lwt.return_unit

(* -----------------------------------------------------------------------------
   handle_guess: Process one guess from the player.
   -----------------------------------------------------------------------------
   This is the core of the game loop for a single turn.

   HOW Game.submit WORKS (so you understand what happens here):
   - Game.submit normalizes the guess (trim + uppercase) before comparing, so
     "chap", " Chap ", and "CHAP" all correctly match the answer "Chap".
   - It scans the exposed frontier for a node whose normalized answer matches.
   - If found: flips that node's [solved] flag to true IN PLACE inside the
     puzzle tree, and returns true.
   - If not found: returns false and leaves the tree unchanged.

   CORRECT GUESS FLOW:
   1. Re-render the bracket and push it to the client. Because [solved] flags
      were already mutated by Game.submit, Game.render now shows the newly
      solved node as its bare answer (no brackets) and everything else unchanged.
   2. Check Game.is_won — if the root is now solved, the whole puzzle is done.
      Send the WIN stub and stop. Otherwise send the EXPOSED stub so the client
      knows what to guess next.

   INCORRECT GUESS FLOW:
   Send the INCORRECT stub. The bracket display does not change.
   ----------------------------------------------------------------------------- *)
let handle_guess (ws : Dream.websocket) (state : Types.puzzle ref)
    (guess : string) : unit Lwt.t =
  let correct = Game.submit guess !state in
  if correct then begin
    (* Tree was mutated in place; re-rendering now shows the updated state. *)
    let* () = send_bracket ws state in
    if Game.is_won !state then
      (* Puzzle complete — send WIN then let keep_open continue to idle.
         STUB: replace with [_send_win ws state] once frontend handles "WIN|" *)
      _send_win ws state
    else
      (* More nodes remain — send the new exposed frontier.
         STUB: replace with [_send_exposed ws state] once frontend handles
         "EXPOSED|" *)
      _send_exposed ws state
  end
  else
    (* Wrong guess — bracket unchanged, notify the client.
       STUB: replace with [_send_incorrect ws guess] once frontend handles
       "INCORRECT|" *)
    _send_incorrect ws guess

(* -----------------------------------------------------------------------------
   ws_handler: Handle one WebSocket connection (one browser session).
   -----------------------------------------------------------------------------
   Dream calls this function whenever a browser opens ws://localhost:8080/ws.

   STEP-BY-STEP:
   1. Register the socket in [clients] for potential future broadcast_all use.
   2. Pick a fresh puzzle via Parser.choose_puzzle using [default_difficulty].
      Each call to choose_puzzle uses Random.self_init so sessions get different
      puzzles (or the same one by chance if the pool is small).
   3. If no puzzle matches the difficulty (shouldn't happen with valid data), log
      the error, send an ERROR| message, and close the socket cleanly.
   4. Wrap the puzzle in a [ref] so [handle_guess] can read its [solved] flags
      across the lifetime of the keep_open loop.
   5. Send the initial BRACKET| so the player sees the puzzle immediately on
      connect, before typing anything.
   6. Enter [keep_open]: a tail-recursive Lwt loop that blocks on Dream.receive,
      processes each incoming guess via [handle_guess], and exits when the client
      disconnects (Dream.receive returns None).
   7. [Lwt.finalize] guarantees [remove_client] runs whether the loop exits
      cleanly or an exception is raised.

   STUB NOTE — difficulty from the browser:
   To let the player choose difficulty on a lobby page, change game-scripts.js
   to open the WebSocket with a query param:
     const ws = new WebSocket(`ws://${location.host}/ws?difficulty=easy`);
   Then replace [ignore req] and [default_difficulty] here with:
     let diff = Dream.query req "difficulty"
                |> Option.value ~default:"hard"
     in
   ----------------------------------------------------------------------------- *)
let ws_handler (req : Dream.request) : Dream.response Lwt.t =
  (* STUB: [req] is ignored until difficulty comes from the browser.
     Replace [ignore req] with query-param extraction when ready. *)
  ignore req;
  Dream.websocket (fun ws ->
      clients := ws :: !clients;

      (* Parser.choose_puzzle returns an option; we handle None explicitly
         rather than calling Option.get so the server never crashes on bad data. *)
      let state_opt = Parser.choose_puzzle default_difficulty all_puzzles in

      (* Lwt.finalize ensures cleanup runs even if an exception bubbles up. *)
      Lwt.finalize
        (fun () ->
          match state_opt with
          | None ->
              Dream.log "No puzzles found for difficulty: %s" default_difficulty;
              send_to ws
                ("ERROR|No puzzles available for difficulty: "
               ^ default_difficulty)
          | Some puzzle ->
              let state = ref puzzle in

              (* Push the initial render so the player sees the puzzle on load. *)
              let* () = send_bracket ws state in

              (* STUB: also push the initial exposed set once frontend handles it.
                 Uncomment the line below when _send_exposed is activated. *)
              (* let* () = _send_exposed ws state in *)

              (* Process guesses one at a time until the client disconnects. *)
              let rec keep_open () =
                let* msg = Dream.receive ws in
                match msg with
                | None ->
                    (* None = client closed the tab or lost connection. *)
                    Lwt.return_unit
                | Some guess ->
                    let* () = handle_guess ws state guess in
                    keep_open ()
              in
              keep_open ())
        (fun () ->
          remove_client ws;
          Lwt.return_unit))

(* -----------------------------------------------------------------------------
   ENTRY POINT: Start the Dream HTTP + WebSocket server.
   -----------------------------------------------------------------------------
   Routes are identical to main.ml so the same frontend files are served.
   The only change is /ws now calls our dynamic [ws_handler] instead of the
   hardcoded one in main.ml.

   HOW TO SWITCH THE PROJECT TO THIS FILE:
   1. Rename bin/main.ml    → bin/main_stub.ml   (keep it for reference)
   2. Rename bin/main_logic.ml → bin/main.ml
   3. Revert bin/dune back to:
        (executable (name main) (libraries dream yojson cs3110_final))
   ----------------------------------------------------------------------------- *)
let () =
  Dream.run @@ Dream.logger
  @@ Dream.router
       [
         Dream.get "/" (Dream.from_filesystem "public" "index.html");
         Dream.get "/game" (Dream.from_filesystem "public" "game.html");
         Dream.get "/ws" ws_handler;
         Dream.get "/**" (Dream.static "public");
       ]
