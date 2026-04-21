open Types
open Yojson.Basic.Util

(*parses bracket information in puzzle into node*)
let rec parse_node json =
  {
    label = json |> member "label" |> to_string;
    answer = json |> member "answer" |> to_string;
    children = json |> member "children" |> to_list |> List.map parse_node;
  }

let parse_puzzle json =
  {
    id = json |> member "id" |> to_int;
    difficulty = json |> member "difficulty" |> to_string;
    theme = json |> member "theme" |> to_string;
    title = json |> member "title" |> to_string;
    root = json |> member "root" |> parse_node;
  }

let load_puzzles filepath =
  Yojson.Basic.from_file filepath |> to_list |> List.map parse_puzzle
