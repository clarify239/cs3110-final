open OUnit2
open Cs3110_final.Types
open Cs3110_final.Parser

let puzzles = load_puzzles "../data/ver2_NESTED_puzzles.json"
let puzzle = List.hd puzzles

let tests =
  "test suite"
  >::: [
         ( "parse_puzzle parses puzzle id correctly" >:: fun _ ->
           assert_equal 1 puzzle.id );
         ( "parse_puzzle parses title correctly" >:: fun _ ->
           assert_equal "Moon Landing" puzzle.title );
         ( "parse_puzzle parses difficulty correctly" >:: fun _ ->
           assert_equal "easy" puzzle.difficulty );
         ( "parse_puzzle and parse_node parses the root node and its label \
            correctly"
         >:: fun _ ->
           assert_equal
             "NASA's mission that landed on the moon on July 20, 1969"
             puzzle.root.label );
         ( "parse_puzzle and parse_node parses the answer to prompts correctly"
         >:: fun _ -> assert_equal "Apollo 11" puzzle.root.answer );
         ( "parse_node is able to parse all of the child nodes (brackets)"
         >:: fun _ ->
           let children = puzzle.root.children in
           assert_equal 3 (List.length children);
           assert_equal "July" (List.hd children).answer;
           assert_equal "eleventh" (List.hd (List.hd children).children).answer;
           assert_equal [] (List.nth children 2).children );
       ]

let _ = run_test_tt_main tests
