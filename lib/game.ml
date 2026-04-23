open Types

(* Types.node already carries a solved flag, so the parsed puzzle IS the game
   state — just alias the type for clarity in function signatures. *)
type state = puzzle

(* PURPOSE: canonicalize a string for answer comparison so "July", " July ", and
   "july" all match the same node. STEPS: 1. Trim leading/trailing whitespace.
   2. Uppercase every ASCII letter. NOTE: internal whitespace is preserved —
   answers like "Apollo 11" need the space between tokens. *)
let normalize (s : string) : string =
  let upper = String.uppercase_ascii s in
  String.trim upper

(* PURPOSE: split a label on {N} placeholders into alternating literal text and
   slot markers. The ONLY place that understands {N} syntax — the parser and
   Types treat labels as opaque strings. STEPS: 1. Scan the input for the
   pattern "{<digits>}". 2. Emit Text s for each stretch of literal text
   between/around placeholders. 3. Emit Slot i for each {i} encountered.
   EXAMPLE: "like most {0}lin and {1} movies" -> [ Text "like most "; Slot 0;
   Text "lin and "; Slot 1; Text " movies" ] NOTE: this does NOT recurse into
   children. Nesting is handled by render_node, which calls itself on
   children.(i) when it sees Slot i. A '{' not followed by digits+'}' is kept as
   literal text. Adjacent slots (e.g. "{0}{1}") produce NO empty Text between
   them. *)
let tokenize_label (label : string) : label_part list =
  let n = String.length label in
  (* scan_digits i: walk forward over ASCII digits starting at i. Returns
     (index_after_digits, digit_string). Empty string means no digits. *)
  let rec scan_digits i =
    if i < n && label.[i] >= '0' && label.[i] <= '9' then
      let e, s = scan_digits (i + 1) in
      (e, String.make 1 label.[i] ^ s)
    else (i, "")
  in
  (* Only emit Text when the buffer is non-empty — this is what keeps adjacent
     slots from producing a spurious Text "" between them. *)
  let flush buf acc = if buf = "" then acc else Text buf :: acc in
  let rec aux i buf acc =
    if i >= n then List.rev (flush buf acc)
    else if label.[i] = '{' then
      let j, digits = scan_digits (i + 1) in
      if digits <> "" && j < n && label.[j] = '}' then
        (* Valid {N}: flush text buffer, emit Slot, skip past '}'. *)
        aux (j + 1) "" (Slot (int_of_string digits) :: flush buf acc)
      else
        (* Stray '{' — treat as literal text and keep scanning. *)
        aux (i + 1) (buf ^ "{") acc
    else aux (i + 1) (buf ^ String.make 1 label.[i]) acc
  in
  aux 0 "" []

(* PURPOSE: render one node to its display string in the current state. Called
   recursively — a node's slots are filled by rendering its children. STEPS: 1.
   If n.solved -> return n.answer verbatim, WITHOUT brackets. 2. Else tokenize
   n.label and walk the token list, building a body: - Text s -> append s - Slot
   i -> append (render_node (List.nth n.children i)) 3. Wrap the body as "[" ^
   body ^ "]" and return. *)
let rec render_node (n : node) : string =
  if n.solved then n.answer
  else
    let part_to_string = function
      | Text s -> s
      | Slot i -> render_node (List.nth n.children i)
    in
    let body =
      tokenize_label n.label |> List.map part_to_string |> String.concat ""
    in
    "[" ^ body ^ "]"

(* PURPOSE: produce the full puzzle display string shown to the player. STEPS:
   1. Call render_node on state.root. 2. Strip the outermost "[" and "]" — the
   root isn't inside anything, so the final string (e.g. "GM makes its 100
   millionth car") should not be bracketed. *)
let render (s : state) : string =
  if s.root.solved then s.root.answer
  else
    let full = render_node s.root in
    String.sub full 1 (String.length full - 2)

(* PURPOSE: collect every node the player can currently attempt — unsolved nodes
   whose children are ALL solved. At start these are the leaves. STEPS: 1. DFS
   from state.root. 2. At each unsolved node n: - if every child of n is solved
   -> include n (stop recursing here; the subtree below is fully solved). - else
   recurse into n's unsolved children to find deeper exposed nodes. 3. Return
   the collected list. *)
let exposed (_s : state) : node list = failwith "TODO"

(* PURPOSE: accept a user answer; if it matches an exposed node's answer, return
   a modified state (switch the mutable flag solved) with that node flipped to
   solved. ASSUMPTION: puzzle answers are unique across the whole tree, so at
   most one node can match a given input. STEPS: 1. Let input' = normalize
   input. 2. Find the unique node n where: - n is exposed (unsolved, all
   children solved), AND - normalize n.answer = input'. 3. If found -> return
   true. If not found -> return false. *)
let submit (user_input : string) (game_s : state) : bool =
  let can_be_solved : node list = exposed game_s in

  let len = List.length can_be_solved in

  let corrected_input = normalize user_input in

  let rec find_match index input =
    if index < len then
      if normalize (List.nth can_be_solved index).answer = corrected_input then (
        (List.nth can_be_solved index).solved <- true;
        true)
      else find_match (index + 1) input
    else false
  in
  find_match 0 corrected_input

(* checks if the player has won the game by checking if the root node is
   solved*)
let is_won (_s : state) : bool = if _s.root.solved = true then true else false
