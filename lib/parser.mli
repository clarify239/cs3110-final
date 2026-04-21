(**Parses puzzle JSON data into puzzle type*)
val parse_puzzle : Yojson.Basic.t -> Types.puzzle

(**Loads a JSON file of puzzles and creates a list of processed puzzles*)
val load_puzzles : string -> Types.puzzle list
