(* Abstraction function: A node {label; answer; children; solved} represents a
   single bracket in a puzzle. [label] is the clue shown to the user with {0},
   {1}, ... as placeholders for child answers. [answer] is the correct string
   the user must guess. [children] are the sub-brackets whose answers fill the
   placeholders in [label]. [solved] is true if the user has correctly guessed
   this bracket's answer, otherwise false.

   Representation Invariant: The number of distinct slot indices in [label] must
   equal List.length children. [answer] must be a non-empty string. Slot indices
   in [label] must be 0-indexed and be continuous (i.e. if there are n children,
   slots must be {0}..{n-1}). *)
type node = {
  label : string;
  answer : string;
  children : node list;
  mutable solved : bool;
}

(* Abstraction function: A puzzle {id; difficulty; theme; title; root}
   represents a full puzzle. [id] is a unique identifier. [difficulty] is one of
   "easy", "medium", or "hard". [theme] is a short description of the puzzle's
   topic. [title] is the display name. [root] is the top-level bracket node from
   which all sub-brackets descend.

   Representation Invariant: [id] must be a positive integer. [difficulty] must
   be one of "easy", "medium", or "hard". [title] and [theme] must be non-empty
   strings. *)
type puzzle = {
  id : int;
  difficulty : string;
  theme : string;
  title : string;
  root : node;
}

(* Abstraction Function: A label_part is a token from a parsed label string.
   [Text s] represents a string [s] to display to the user. [Slot i] represents
   a placeholder at index [i] whose display value is filled in by the answer of
   the i-th child node.

   Representation Invariant: All [Slot i] indices must be non-negative and
   correspond to valid child indices of the parent node. *)
type label_part =
  | Text of string
  | Slot of int
