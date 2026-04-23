open OUnit2
open Cs3110_final.Types
open Cs3110_final.Parser
open Cs3110_final.Game

(** list of puzzles from the json file*)
let puzzles = load_puzzles "../data/ver2_NESTED_puzzles.json"

(** the first puzzle in the list of loaded puzzles*)
let puzzle = choose_puzzle "hard" puzzles |> Option.get

(* Pretty-print a label_part list so assert_equal failures are readable. *)
let string_of_part = function
  | Text s -> Printf.sprintf "Text %S" s
  | Slot i -> Printf.sprintf "Slot %d" i

let string_of_parts parts =
  "[" ^ String.concat "; " (List.map string_of_part parts) ^ "]"

let tests =
  "test suite"
  >::: [
         ( "parse_puzzle parses puzzle id correctly" >:: fun _ ->
           assert_equal 1 puzzle.id );
         ( "parse_puzzle parses title correctly" >:: fun _ ->
           assert_equal "GM's 100 Millionth Car" puzzle.title );
         ( "parse_puzzle parses difficulty correctly" >:: fun _ ->
           assert_equal "hard" puzzle.difficulty );
         ( "parse_puzzle and parse_node parses the root node and its label \
            correctly"
         >:: fun _ ->
           assert_equal "{0}{1} makes its 100 millionth car" puzzle.root.label
         );
         ( "parse_puzzle and parse_node parses the answer to prompts correctly"
         >:: fun _ ->
           assert_equal "GM makes its 100 millionth car" puzzle.root.answer );
         ( "parse_node is able to parse all of the child nodes (brackets)"
         >:: fun _ ->
           let children = puzzle.root.children in
           assert_equal 2 (List.length children);
           assert_equal "G" (List.hd children).answer;
           assert_equal "silent" (List.hd (List.hd children).children).answer );
         ( "tokenize_label handles text-slot-text-slot-text (spec example)"
         >:: fun _ ->
           assert_equal
             [
               Text "like most ";
               Slot 0;
               Text "lin and ";
               Slot 1;
               Text " movies";
             ]
             (tokenize_label "like most {0}lin and {1} movies")
             ~printer:string_of_parts );
         ( "tokenize_label emits no empty Text between adjacent slots"
         >:: fun _ ->
           assert_equal
             [ Slot 0; Slot 1; Text " makes its 100 millionth car" ]
             (tokenize_label "{0}{1} makes its 100 millionth car")
             ~printer:string_of_parts );
         ( "tokenize_label returns a single Text when no placeholders exist"
         >:: fun _ ->
           assert_equal
             [ Text "stick for dry lips" ]
             (tokenize_label "stick for dry lips")
             ~printer:string_of_parts );
         ( "tokenize_label handles a slot at the very start (no leading Text)"
         >:: fun _ ->
           assert_equal [ Slot 0; Text "lin" ] (tokenize_label "{0}lin")
             ~printer:string_of_parts );
         ( "tokenize_label parses multi-digit slot indices" >:: fun _ ->
           assert_equal
             [ Slot 10; Text "x"; Slot 2 ]
             (tokenize_label "{10}x{2}")
             ~printer:string_of_parts );
         ( "tokenize_label treats a stray '{' with no closing '}' as literal \
            text"
         >:: fun _ ->
           assert_equal
             [ Text "cost is {5 dollars" ]
             (tokenize_label "cost is {5 dollars")
             ~printer:string_of_parts );
         ( "choose_puzzle returns a random puzzle with the right difficulty \
            and None if there are no puzzles"
         >:: fun _ ->
           let difficult_puzzle = choose_puzzle "hard" puzzles in

           match difficult_puzzle with
           | None -> assert_failure "Expected Some but got None"
           | Some p ->
               assert_equal "hard" p.difficulty;

               let easy_puzzle = Option.get (choose_puzzle "easy" puzzles) in
               assert_equal "computer science" easy_puzzle.theme;
               let no_puzzle = choose_puzzle "medium" puzzles in
               assert_equal None no_puzzle );
         ( "testing normalize function" >:: fun _ ->
           let word = "    good   " in
           assert_equal "GOOD" (normalize word) );
         ( "render strips the outermost brackets from render_node s.root when \
            the root is unsolved"
         >:: fun _ ->
           let full = render_node puzzle.root in
           let body = String.sub full 1 (String.length full - 2) in
           assert_equal body (render puzzle) ~printer:(fun s -> s) );
         ( "render returns the root's answer verbatim when the root is solved"
         >:: fun _ ->
           let p = List.hd (load_puzzles "../data/ver2_NESTED_puzzles.json") in
           p.root.solved <- true;
           assert_equal "GM makes its 100 millionth car" (render p) );
       ]

let _ = run_test_tt_main tests
