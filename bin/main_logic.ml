(** main_logic.ml — Dynamic game loop for Bracket City

    This file connects the OCaml game engine (lib/game.ml, lib/parser.ml) to the
    Dream WebSocket server so that the browser frontend gets real puzzle data
    instead of the hardcoded strings in main.ml. *)

open Lwt.Syntax
open Cs3110_final

(** Load all puzzles from the JSON data file.

    This runs exactly once when the module is first loaded (i.e. at server
    startup). All WebSocket sessions share this list and call [choose_puzzle] to
    get their own copy. *)
let all_puzzles : Types.puzzle list =
  Parser.load_puzzles "data/ver2_NESTED_puzzles.json"

(** [default_difficulty] is the difficulty level applied to all puzzle
    selections. Difficulty is passed as a query parameter and extracted from the
    [Dream.request] inside [ws_handler]. *)
let default_difficulty : string = "hard"

(** [clients] tracks every open WebSocket. It is only used by [_broadcast_all],
    which is stubbed out for a future collaborative mode. In per-session mode,
    each handler only talks to its own [ws]. *)
let clients : Dream.websocket list ref = ref []

(** [remove_client ws] removes [ws] from the global client list on disconnect.
*)
let remove_client (ws : Dream.websocket) : unit =
  clients := List.filter (fun c -> c != ws) !clients

(** [send_to ws msg] sends one message to one WebSocket, silently ignoring
    errors.

    [Dream.send] raises an exception if the socket is already closed (e.g. the
    browser tab was closed mid-game). We catch that here so the server does not
    crash on a disconnected client. *)
let send_to (ws : Dream.websocket) (msg : string) : unit Lwt.t =
  Lwt.catch (fun () -> Dream.send ws msg) (fun _exn -> Lwt.return_unit)

(** [_broadcast_all msg] sends the same message to every connected client.

    Not used in per-session mode. This is for a collaborative mode where all
    players share one puzzle. We replace [send_to ws] calls with
    [_broadcast_all] to send an update to all clients. *)
let _broadcast_all (msg : string) : unit Lwt.t =
  Lwt_list.iter_p (fun ws -> send_to ws msg) !clients

(** [send_bracket ws state] renders current puzzle state and push it to one
    client.

    [Game.render] returns the puzzle string with unsolved nodes shown as [clue]
    and solved nodes shown as their bare answer. The frontend wraps the whole
    thing in [ ] itself, so we don't add outer brackets here. *)
let send_bracket (ws : Dream.websocket) (state : Types.puzzle ref) : unit Lwt.t
    =
  send_to ws ("BRACKET|" ^ Game.render !state)

(** [send_progress ws state] sends the current solved/total node counts to the
    client.

    The message format is: [PROGRESS|solved|total]. *)

let send_progress (ws : Dream.websocket) (state : Types.puzzle ref) : unit Lwt.t
    =
  let solved, total = Game.progress !state in
  send_to ws (Printf.sprintf "PROGRESS|%d|%d" solved total)

(** [elapsed_seconds start_time] returns the number of whole seconds elapsed
    since [start_time] (a Unix timestamp from [Unix.gettimeofday ()]). *)

let elapsed_seconds (start_time : float) : int =
  int_of_float (Unix.gettimeofday () -. start_time)

(** [send_timer ws start_time] sends the elapsed time since [start_time] to the
    client.

    The message format is: [TIMER|seconds]. *)

let send_timer (ws : Dream.websocket) (start_time : float) : unit Lwt.t =
  send_to ws (Printf.sprintf "TIMER|%d" (elapsed_seconds start_time))

(** [_send_exposed ws state] tells the client which answers are currently
    guessable.

    [Game.exposed] returns the unsolved nodes whose children are all solved. As
    the player guesses correctly, their parents become the new leaves. Sending
    this list lets the frontend display a hint panel *)
let _send_exposed (ws : Dream.websocket) (state : Types.puzzle ref) : unit Lwt.t
    =
  let exposed_nodes = Game.exposed !state in
  let answer_list = List.map (fun (n : Types.node) -> n.answer) exposed_nodes in
  let csv = String.concat "," answer_list in
  ignore csv;
  ignore ws;
  Lwt.return_unit

(** [_send_incorrect ws guess] tells the client their guess was wrong.

    This sends the rejected [guess] back so the frontend can alert an error The
    message format is: [INCORRECT|guess]. *)
let _send_incorrect (ws : Dream.websocket) (guess : string) : unit Lwt.t =
  send_to ws ("INCORRECT|" ^ guess)

(** [send_stats ws session] sends the player's victory statistics to the
    frontend. Fields computed from [session]: [total_guesses]: counts every
    submitted answer attempt [wrong_guesses]: counts guesses that did not match
    any exposed node [hints_used]: counts hint usage correct guesses :
    [total_guesses - wrong_guesses] accuracy: integer percentage,
    [correct_guesses * 100 / total_guesses] The message format sent to the
    frontend is: [STATS|total|wrong|hints|accuracy|score|grade|streak] *)

let send_stats (ws : Dream.websocket) (session : Score.session ref) : unit Lwt.t
    =
  let s = !session in
  let total = Score.total_attempts s in
  let accuracy_pct = if total = 0 then 100 else s.correct_count * 100 / total in
  let sum = Score.make_summary s in
  send_to ws
    (Printf.sprintf "STATS|%d|%d|%d|%d|%d|%s|%d" total s.wrong_count
       s.hint_count accuracy_pct sum.final_score sum.grade s.max_streak)

(** [_send_win ws state] sells the client they have solved the whole puzzle.

    [Game.is_won] checks whether the root node is solved. The root can only be
    solved after every node in the tree has been guessed correctly, so this
    fires exactly once per game.*)
let _send_win (ws : Dream.websocket) (state : Types.puzzle ref) : unit Lwt.t =
  send_to ws ("WIN|" ^ !state.root.answer)

(** [handle_guess ws state session start_time guess] processes one guess from
    the player.

    Steps: 1. Normalise the guess and check it against the currently exposed
    nodes via [Game.submit]. 2. If correct: update [session] with score,
    re-render the bracket, update progress, and check [Game.is_won]. If the
    puzzle is now complete, send [TIMER], [STATS], and [WIN] messages. 3. If
    incorrect: increment the wrong-guess counter in [session] and send
    [INCORRECT]. *)
let handle_guess (ws : Dream.websocket) (state : Types.puzzle ref)
    (session : Score.session ref) (start_time : float) (guess : string) :
    unit Lwt.t =
  let exposed_before = Game.exposed !state in
  let normed = Game.normalize guess in
  let maybe_node =
    List.find_opt
      (fun (n : Types.node) -> Game.normalize n.answer = normed)
      exposed_before
  in
  let correct = Game.submit guess !state in
  if correct then begin
    (session :=
       match maybe_node with
       | Some n -> Score.apply_correct_combo_from_puzzle !session !state n
       | None -> Score.apply_correct !session !state.difficulty 0);
    let* () = send_bracket ws state in
    let* () = send_progress ws state in
    let exposed = Game.exposed !state in
    match exposed with
    | [ n ] when n == !state.root ->
        n.solved <- true;
        let* () = send_bracket ws state in
        let* () = send_progress ws state in
        let* () = send_timer ws start_time in
        let* () = send_stats ws session in
        _send_win ws state
    | _ ->
        if Game.is_won !state then begin
          let* () = send_timer ws start_time in
          let* () = send_stats ws session in
          _send_win ws state
        end
        else _send_exposed ws state
  end
  else begin
    session := Score.apply_wrong !session;
    _send_incorrect ws guess
  end

(** ws_handler: Handle one WebSocket connection (one browser session).

    Dream calls this function whenever a browser opens ws://localhost:8080/ws.

    1. Register the socket 2. Pick a fresh puzzle via Parser.choose_puzzle 3. If
    no puzzle matches the difficulty, log the error, send an ERROR| message, and
    close the socket. 4. Wrap the puzzle in a [ref] so [handle_guess] can read
    its [solved] flags 5. Send the initial BRACKET| so the player sees the
    puzzle immediately 6. Enter [keep_open] a tail-recursive Lwt loop that
    blocks on Dream.receive, processes each incoming guess via [handle_guess],
    and exits when the client disconnects (Dream.receive returns None). 7.
    [Lwt.finalize] guarantees [remove_client] runs *)

(** [handle_hint ws state session chip_body] handles a first-letter hint
    request.

    Looks up the exposed node whose body matches [chip_body] via
    [Game.hint_first_letter]. If found, charges the hint to [session] and sends
    [HINT|letter] to the client. If no matching node is found, returns without
    sending anything. *)
let handle_hint (ws : Dream.websocket) (state : Types.puzzle ref)
    (session : Score.session ref) (chip_body : string) : unit Lwt.t =
  match Game.hint_first_letter chip_body !state with
  | None -> Lwt.return_unit
  | Some letter ->
      session := Score.apply_hint_of_kind !session Score.First_letter;
      send_to ws ("HINT|" ^ letter)

(** [handle_hint_length ws state session chip_body] handles an answer-length
    hint request.

    Looks up the exposed node whose body matches [chip_body] via
    [Game.hint_answer_length]. If found, charges the hint to [session] and sends
    [HINT_LEN|n] to the client. If no matching node is found, returns without
    sending anything. *)
let handle_hint_length (ws : Dream.websocket) (state : Types.puzzle ref)
    (session : Score.session ref) (chip_body : string) : unit Lwt.t =
  match Game.hint_answer_length chip_body !state with
  | None -> Lwt.return_unit
  | Some n ->
      session := Score.apply_hint_of_kind !session Score.Length;
      send_to ws (Printf.sprintf "HINT_LEN|%d" n)

(** [handle_hint_words ws state session chip_body] handles a word-count hint
    request.

    Looks up the exposed node whose body matches [chip_body] via
    [Game.hint_word_count]. If found, charges the hint to [session] and sends
    [HINT_WORDS|n] to the client. If no matching node is found, returns without
    sending anything. *)
let handle_hint_words (ws : Dream.websocket) (state : Types.puzzle ref)
    (session : Score.session ref) (chip_body : string) : unit Lwt.t =
  match Game.hint_word_count chip_body !state with
  | None -> Lwt.return_unit
  | Some n ->
      session := Score.apply_hint_of_kind !session Score.Word_count;
      send_to ws (Printf.sprintf "HINT_WORDS|%d" n)

(** [handle_reveal ws state session start_time chip_body] reveals the answer for
    one node.

    Calls [Game.reveal_by_body] to mark the node as solved without a guess. If
    successful, charges a [Reveal] hint to [session], re-renders the bracket,
    and checks for win conditions identically to [handle_guess]. If [chip_body]
    does not match any exposed node, returns without sending anything. *)
let handle_reveal (ws : Dream.websocket) (state : Types.puzzle ref)
    (session : Score.session ref) (start_time : float) (chip_body : string) :
    unit Lwt.t =
  if Game.reveal_by_body chip_body !state then begin
    session := Score.apply_hint_of_kind !session Score.Reveal;
    let* () = send_bracket ws state in
    let* () = send_progress ws state in

    let exposed = Game.exposed !state in
    match exposed with
    | [ n ] when n == !state.root ->
        n.solved <- true;
        let* () = send_bracket ws state in
        let* () = send_progress ws state in
        let* () = send_timer ws start_time in
        let* () = send_stats ws session in
        _send_win ws state
    | _ ->
        if Game.is_won !state then begin
          let* () = send_timer ws start_time in
          let* () = send_stats ws session in
          _send_win ws state
        end
        else Lwt.return_unit
  end
  else Lwt.return_unit

(** [load_next_puzzle ws picker state start_time] pulls the next puzzle from the
    per-session [picker] and replace the current [state], resets [start_time].
    If the picker is empty, sends DONE| so the frontend can show an
    end-of-session screen. *)
let load_next_puzzle ws picker state start_time difficulty =
  match Parser.choose_puzzle picker difficulty with
  | None -> send_to ws "DONE|No more puzzles available"
  | Some new_puzzle ->
      state := new_puzzle;
      start_time := Unix.gettimeofday ();
      let* () = send_bracket ws state in
      let* () = send_progress ws state in
      send_timer ws !start_time

(** [handle_skip ws picker state session start_time] records a skip in [session]
    and immediately loads the next puzzle via [load_next_puzzle]. *)
let handle_skip (ws : Dream.websocket) (picker : Parser.picker)
    (state : Types.puzzle ref) (session : Score.session ref)
    (start_time : float ref) difficulty : unit Lwt.t =
  session := Score.apply_skip !session;
load_next_puzzle ws picker state start_time difficulty
(** [handle_next ws picker state start_time] advances to the next puzzle without
    penalising the session (used after a win).

    Delegates directly to [load_next_puzzle]. *)
let handle_next (ws : Dream.websocket) (picker : Parser.picker)
    (state : Types.puzzle ref) (start_time : float ref) difficulty: unit Lwt.t =
load_next_puzzle ws picker state start_time difficulty

(** [starts_with prefix s] returns [true] if [s] begins with [prefix]. *)
let starts_with (prefix : string) (s : string) : bool =
  let n = String.length prefix in
  String.length s >= n && String.sub s 0 n = prefix

(** [strip_prefix prefix s] returns [s] with the leading [prefix] removed.

    Behaviour is undefined if [s] does not start with [prefix]; always guard
    with [starts_with] first. *)
let strip_prefix (prefix : string) (s : string) : string =
  let n = String.length prefix in
  String.sub s n (String.length s - n)

(** [ws_handler req] handles one WebSocket connection (one browser session).

    Dream calls this function whenever a browser opens [ws://localhost:8080/ws].

    Steps: 1. Register the socket in [clients]. 2. Pick a fresh puzzle via
    [Parser.choose_puzzle] using a per-session [picker] so each browser session
    gets its own no-repeat queue. 3. If no puzzle matches [default_difficulty],
    log the error, send an [ERROR|] message, and close the socket. 4. Wrap the
    puzzle in a [ref] so [handle_guess] and friends can mutate [solved] flags.
    5. Send the initial [BRACKET], [PROGRESS], and [TIMER] messages so the
    player sees the puzzle immediately on load. 6. Enter [keep_open], a
    tail-recursive Lwt loop that blocks on [Dream.receive], dispatches each
    incoming message to the appropriate handler, and exits when the client
    disconnects ([Dream.receive] returns [None]). 7. [Lwt.finalize] guarantees
    [remove_client] runs on disconnect or error. *)
let ws_handler (req : Dream.request) : Dream.response Lwt.t =
  let difficulty =
    Dream.query req "difficulty" |> Option.value ~default:default_difficulty
  in
  Dream.log "Chosen difficulty: %s" difficulty;
  Dream.websocket (fun ws ->
      clients := ws :: !clients;

      (* One picker per WebSocket connection so each browser session gets its
         own no-repeat queue independent of other connected clients. *)
      let picker = Parser.make_picker all_puzzles in
      let state_opt = Parser.choose_puzzle picker difficulty in
      (* Lwt.finalize ensures cleanup runs *)
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
              let session = ref (Score.make_session ()) in
              (* start_time is a ref so SKIP/NEXT can reset it per puzzle. *)
              let start_time = ref (Unix.gettimeofday ()) in
              (* Push the initial render so the player sees the puzzle on
                 load. *)
              let* () = send_bracket ws state in
              let* () = send_progress ws state in
              let* () = send_timer ws !start_time in

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
                | Some raw ->
                    let* () =
                      if starts_with "HINT_LEN|" raw then
                        handle_hint_length ws state session
                          (strip_prefix "HINT_LEN|" raw)
                      else if starts_with "HINT_WORDS|" raw then
                        handle_hint_words ws state session
                          (strip_prefix "HINT_WORDS|" raw)
                      else if starts_with "HINT|" raw then
                        handle_hint ws state session (strip_prefix "HINT|" raw)
                      else if starts_with "REVEAL|" raw then
                        handle_reveal ws state session !start_time
                          (strip_prefix "REVEAL|" raw)
                      else if starts_with "SKIP|" raw then
                        handle_skip ws picker state session start_time difficulty
                      else if starts_with "NEXT|" raw then
                        handle_next ws picker state start_time difficulty
                      else handle_guess ws state session !start_time raw
                    in
                    keep_open ()
              in
              keep_open ())
        (fun () ->
          remove_client ws;
          Lwt.return_unit))

(** Start the Dream HTTP + WebSocket server.

    Routes are identical to [main.ml] so the same frontend files are served:
    - [GET /] → [public/index.html]
    - [GET /game] → [public/game.html]
    - [GET /ws] → [ws_handler]
    - [GET /**] → static files under [public/] *)
let () =
  Dream.run @@ Dream.logger
  @@ Dream.router
       [
         Dream.get "/" (Dream.from_filesystem "public" "index.html");
         Dream.get "/game" (Dream.from_filesystem "public" "game.html");
         Dream.get "/ws" ws_handler;
         Dream.get "/**" (Dream.static "public");
       ]
