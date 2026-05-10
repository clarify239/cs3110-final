(**Parses puzzle JSON data into puzzle type*)
val parse_puzzle : Yojson.Basic.t -> Types.puzzle

(**Loads a JSON file of puzzles and creates a list of processed puzzles*)
val load_puzzles : string -> Types.puzzle list

(** A puzzle picker that owns its own per-difficulty no-repeat queues. Build
    one per session so two sessions do not share state. *)
type picker

(**[make_picker puzzles] creates a fresh picker over [puzzles].*)
val make_picker : Types.puzzle list -> picker

(** [choose_puzzle picker difficulty] returns the next unsolved puzzle of the
    given difficulty from [picker], cycling without repeats until the queue
    empties (then reshuffling). Returns [None] if no unsolved puzzle of that
    difficulty exists. *)
val choose_puzzle : picker -> string -> Types.puzzle option
