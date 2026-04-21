(*node representing a prompt, answer and subprompts*)
type node = {
  label : string;
  answer : string;
  children : node list;
  solved : bool;
}

(* a full puzzle loaded from JSON *)
type puzzle = {
  id : int;
  difficulty : string;
  theme : string;
  title : string;
  root : node;
}

type label_part =
  | Text of string
  | Slot of int
