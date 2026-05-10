open OUnit2
open Cs3110_final.Types
open Cs3110_final.Parser
open Cs3110_final.Game

(** list of puzzles from the json file*)
let puzzles = load_puzzles "../data/ver2_NESTED_puzzles.json"

(** the first puzzle in the loaded puzzles*)
let puzzle = List.hd puzzles

(** a random hard puzzle in the list of loaded puzzles*)
let puzzle_hard =
  choose_puzzle (make_picker puzzles) "hard" |> Option.get

(** Pretty-print a label_part list so assert_equal failures are readable. *)
let string_of_part = function
  | Text s -> Printf.sprintf "Text %S" s
  | Slot i -> Printf.sprintf "Slot %d" i

(** A dummy child node for testing purposes. Represents a simple bracket node in
    a puzzle. *)
let dummy_node = { label = ""; answer = ""; children = []; solved = false }

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
           let picker = make_picker puzzles in
           let difficult_puzzle = choose_puzzle picker "hard" in

           match difficult_puzzle with
           | None -> assert_failure "Expected Some but got None"
           | Some p ->
               assert_equal "hard" p.difficulty;

               let easy_puzzle = Option.get (choose_puzzle picker "easy") in
               assert_equal "easy" easy_puzzle.difficulty;
               let no_puzzle = choose_puzzle picker "legendary" in
               assert_equal None no_puzzle );
         ( "testing normalize function" >:: fun _ ->
           let word = "    good   " in
           assert_equal "GOOD" (normalize word) );
         ( "normalize preserves internal whitespace" >:: fun _ ->
           assert_equal "APOLLO 11" (normalize "Apollo 11") );
         ( "normalize returns empty string unchanged" >:: fun _ ->
           assert_equal "" (normalize "") );
         ( "normalize preserves numbers and non-alpha characters" >:: fun _ ->
           assert_equal "ABC123" (normalize "abc123") );
         ( "normalize is idempotent on already-uppercase input" >:: fun _ ->
           assert_equal "HELLO" (normalize "HELLO") );
         ( "render returns render_node s.root when the root is unsolved"
         >:: fun _ ->
           assert_equal (render_node puzzle_hard.root) (render puzzle_hard)
             ~printer:(fun s -> s) );
         ( "render returns the root's answer verbatim when the root is solved"
         >:: fun _ ->
           let p = List.hd (load_puzzles "../data/ver2_NESTED_puzzles.json") in
           p.root.solved <- true;
           assert_equal "GM makes its 100 millionth car" (render p) );
         ( "render_node returns bare answer for a solved node (no brackets)"
         >:: fun _ ->
           let n =
             { label = "some clue"; answer = "London"; children = []; solved = true }
           in
           assert_equal "London" (render_node n) );
         ( "render_node wraps an unsolved leaf in brackets" >:: fun _ ->
           let n =
             {
               label = "some clue";
               answer = "London";
               children = [];
               solved = false;
             }
           in
           assert_equal "[some clue]" (render_node n) );
         ( "render_node returns body without outer brackets when a child is \
            unsolved"
         >:: fun _ ->
           let child =
             {
               label = "child clue";
               answer = "Paris";
               children = [];
               solved = false;
             }
           in
           let parent =
             {
               label = "city: {0}";
               answer = "city: Paris";
               children = [ child ];
               solved = false;
             }
           in
           (* child is unsolved so parent is not yet answerable — no outer
              brackets. child itself is a leaf so it gets inner brackets. *)
           assert_equal "city: [child clue]" (render_node parent) );
         ( "render_node wraps parent in brackets once all children are solved"
         >:: fun _ ->
           let child =
             {
               label = "child clue";
               answer = "Paris";
               children = [];
               solved = true;
             }
           in
           let parent =
             {
               label = "city: {0}";
               answer = "city: Paris";
               children = [ child ];
               solved = false;
             }
           in
           assert_equal "[city: Paris]" (render_node parent) );
         ( "exposed returns every leaf of a fresh puzzle (nothing solved yet)"
         >:: fun _ ->
           let p = List.hd (load_puzzles "../data/ver2_NESTED_puzzles.json") in
           let answers = List.map (fun n -> n.answer) (exposed p) in
           assert_equal 6 (List.length answers);
           assert_bool "Chap should be exposed" (List.mem "Chap" answers);
           assert_bool "forward should be exposed" (List.mem "forward" answers)
         );
         ( "exposed drops a solved leaf but keeps its siblings" >:: fun _ ->
           let p = List.hd (load_puzzles "../data/ver2_NESTED_puzzles.json") in
           let _ = submit "Chap" p in
           let answers = List.map (fun n -> n.answer) (exposed p) in
           assert_bool "Chap should no longer be exposed"
             (not (List.mem "Chap" answers));
           assert_bool "name should still be exposed" (List.mem "name" answers)
         );
         (* submit direct tests *)
         ( "submit returns true on a correct answer" >:: fun _ ->
           let leaf =
             { label = "clue"; answer = "Paris"; children = []; solved = false }
           in
           let p =
             {
               id = 0;
               difficulty = "easy";
               theme = "";
               title = "";
               solved_puzzle = false;
               root = leaf;
             }
           in
           assert_equal true (submit "Paris" p) );
         ( "submit returns false on a wrong answer" >:: fun _ ->
           let leaf =
             { label = "clue"; answer = "Paris"; children = []; solved = false }
           in
           let p =
             {
               id = 0;
               difficulty = "easy";
               theme = "";
               title = "";
               solved_puzzle = false;
               root = leaf;
             }
           in
           assert_equal false (submit "London" p) );
         ( "submit is case-insensitive" >:: fun _ ->
           let leaf =
             { label = "clue"; answer = "Paris"; children = []; solved = false }
           in
           let p =
             {
               id = 0;
               difficulty = "easy";
               theme = "";
               title = "";
               solved_puzzle = false;
               root = leaf;
             }
           in
           assert_equal true (submit "paris" p) );
         ( "submit trims leading and trailing whitespace" >:: fun _ ->
           let leaf =
             { label = "clue"; answer = "Paris"; children = []; solved = false }
           in
           let p =
             {
               id = 0;
               difficulty = "easy";
               theme = "";
               title = "";
               solved_puzzle = false;
               root = leaf;
             }
           in
           assert_equal true (submit "  Paris  " p) );
         ( "submit returns false when the node is already solved" >:: fun _ ->
           let leaf =
             { label = "clue"; answer = "Paris"; children = []; solved = false }
           in
           let p =
             {
               id = 0;
               difficulty = "easy";
               theme = "";
               title = "";
               solved_puzzle = false;
               root = leaf;
             }
           in
           ignore (submit "Paris" p);
           (* node is now solved and no longer exposed *)
           assert_equal false (submit "Paris" p) );
         ( "exposed is empty after all nodes are solved" >:: fun _ ->
           let p = List.hd (load_puzzles "../data/ver2_NESTED_puzzles.json") in
           let rec solve_all () =
             if not (is_won p) then begin
               List.iter (fun n -> ignore (submit n.answer p)) (exposed p);
               solve_all ()
             end
           in
           solve_all ();
           assert_equal [] (exposed p) );
         ( "exposed drops a solved leaf but keeps its siblings \
            (data-independent)"
         >:: fun _ ->
           let p = List.hd (load_puzzles "../data/ver2_NESTED_puzzles.json") in
           let before = exposed p in
           let target = List.hd before in
           let _ = submit target.answer p in
           let after = exposed p in
           assert_bool "solved node should not be in exposed list"
             (not (List.exists (fun n -> n.answer = target.answer) after));
           assert_bool "at least one sibling should remain exposed"
             (List.length after > 0) );
         ( "choose_puzzle only returns unsolved puzzles" >:: fun _ ->
           let p1 =
             {
               id = 1;
               difficulty = "easy";
               theme = "";
               title = "";
               solved_puzzle = true;
               root = dummy_node;
             }
           in
           let p2 =
             {
               id = 2;
               difficulty = "easy";
               theme = "";
               title = "";
               solved_puzzle = false;
               root = dummy_node;
             }
           in

           let puzzles = [ p1; p2 ] in
           assert_equal (Some p2) (choose_puzzle (make_picker puzzles) "easy");
           let puzzles =
             [
               {
                 id = 1;
                 difficulty = "easy";
                 theme = "";
                 title = "";
                 solved_puzzle = true;
                 root = dummy_node;
               };
               {
                 id = 2;
                 difficulty = "easy";
                 theme = "";
                 title = "";
                 solved_puzzle = false;
                 root = dummy_node;
               };
               {
                 id = 3;
                 difficulty = "easy";
                 theme = "";
                 title = "";
                 solved_puzzle = false;
                 root = dummy_node;
               };
             ]
           in
           let puzzle = choose_puzzle (make_picker puzzles) "easy" in
           let is_correct =
             match puzzle with
             | Some p -> p.id = 2 || p.id = 3
             | None -> false
           in
           assert_equal true is_correct );
         ( " choose_puzzle only returns unsolved puzzles of the right difficulty"
         >:: fun _ ->
           let p1 =
             {
               id = 1;
               difficulty = "easy";
               theme = "";
               title = "";
               solved_puzzle = false;
               root = dummy_node;
             }
           in
           let p2 =
             {
               id = 2;
               difficulty = "medium";
               theme = "";
               title = "";
               solved_puzzle = false;
               root = dummy_node;
             }
           in
           let p3 =
             {
               id = 3;
               difficulty = "hard";
               theme = "";
               title = "";
               solved_puzzle = false;
               root = dummy_node;
             }
           in

           let puzzles = [ p1; p2; p3 ] in
           assert_equal (Some p1) (choose_puzzle (make_picker puzzles) "easy")
         );
         ( "choose_puzzle returns None on an empty puzzle list" >:: fun _ ->
           assert_equal None (choose_puzzle (make_picker []) "easy") );
         (* is_won tests *)
         ( "is_won returns false on a fresh puzzle" >:: fun _ ->
           let leaf =
             { label = "clue"; answer = "Paris"; children = []; solved = false }
           in
           let p =
             {
               id = 0;
               difficulty = "easy";
               theme = "";
               title = "";
               solved_puzzle = false;
               root = leaf;
             }
           in
           assert_equal false (is_won p) );
         ( "is_won returns true after the root is solved" >:: fun _ ->
           let leaf =
             { label = "clue"; answer = "Paris"; children = []; solved = false }
           in
           let p =
             {
               id = 0;
               difficulty = "easy";
               theme = "";
               title = "";
               solved_puzzle = false;
               root = leaf;
             }
           in
           ignore (submit "Paris" p);
           assert_equal true (is_won p) );
         ( "is_won sets solved_puzzle flag on the puzzle record" >:: fun _ ->
           let leaf =
             { label = "clue"; answer = "Paris"; children = []; solved = false }
           in
           let p =
             {
               id = 0;
               difficulty = "easy";
               theme = "";
               title = "";
               solved_puzzle = false;
               root = leaf;
             }
           in
           ignore (submit "Paris" p);
           ignore (is_won p);
           assert_equal true p.solved_puzzle );
         (* count_nodes tests *)
         ( "count_nodes returns 1 for a single leaf node" >:: fun _ ->
           assert_equal 1 (count_nodes dummy_node) );
         ( "count_nodes returns 3 for a node with two leaf children" >:: fun _ ->
           let c1 = { label = ""; answer = "A"; children = []; solved = false } in
           let c2 = { label = ""; answer = "B"; children = []; solved = false } in
           let parent =
             {
               label = "{0} {1}";
               answer = "A B";
               children = [ c1; c2 ];
               solved = false;
             }
           in
           assert_equal 3 (count_nodes parent) );
         ( "count_nodes returns 4 for a three-level deep tree" >:: fun _ ->
           let gc1 =
             { label = ""; answer = "A"; children = []; solved = false }
           in
           let gc2 =
             { label = ""; answer = "B"; children = []; solved = false }
           in
           let child =
             {
               label = "{0} {1}";
               answer = "A B";
               children = [ gc1; gc2 ];
               solved = false;
             }
           in
           let root =
             {
               label = "{0}";
               answer = "A B";
               children = [ child ];
               solved = false;
             }
           in
           assert_equal 4 (count_nodes root) );
         ( "count_nodes on the first loaded puzzle exceeds its leaf count"
         >:: fun _ ->
           let p = List.hd (load_puzzles "../data/ver2_NESTED_puzzles.json") in
           assert_bool "total nodes should be more than the 6 exposed leaves"
             (count_nodes p.root > 6) );
         (* count_solved tests *)
         ( "count_solved returns 0 on a fresh puzzle" >:: fun _ ->
           let p = List.hd (load_puzzles "../data/ver2_NESTED_puzzles.json") in
           assert_equal 0 (count_solved p.root) );
         ( "count_solved increments by 1 after solving one leaf" >:: fun _ ->
           let p = List.hd (load_puzzles "../data/ver2_NESTED_puzzles.json") in
           let _ = submit "Chap" p in
           assert_equal 1 (count_solved p.root) );
         ( "count_solved increments by 1 after solving any leaf (data-independent)"
         >:: fun _ ->
           let p = List.hd (load_puzzles "../data/ver2_NESTED_puzzles.json") in
           let leaf = List.hd (exposed p) in
           let _ = submit leaf.answer p in
           assert_equal 1 (count_solved p.root) );
         ( "count_solved counts every solved node including parent nodes"
         >:: fun _ ->
           let child =
             { label = ""; answer = "X"; children = []; solved = true }
           in
           let parent =
             {
               label = "{0}";
               answer = "X parent";
               children = [ child ];
               solved = true;
             }
           in
           assert_equal 2 (count_solved parent) );
         (* progress tests *)
         ( "progress returns (0, total) on a fresh puzzle" >:: fun _ ->
           let p = List.hd (load_puzzles "../data/ver2_NESTED_puzzles.json") in
           let solved, total = progress p in
           assert_equal 0 solved;
           assert_bool "total node count should be positive" (total > 0) );
         ( "progress solved count increments after a correct submission"
         >:: fun _ ->
           let p = List.hd (load_puzzles "../data/ver2_NESTED_puzzles.json") in
           let _ = submit "Chap" p in
           let solved, _ = progress p in
           assert_equal 1 solved );
         ( "progress solved count increments after a correct submission \
            (data-independent)"
         >:: fun _ ->
           let p = List.hd (load_puzzles "../data/ver2_NESTED_puzzles.json") in
           let leaf = List.hd (exposed p) in
           let _ = submit leaf.answer p in
           let solved, _ = progress p in
           assert_equal 1 solved );
         ( "progress returns (total, total) after winning the puzzle" >:: fun _ ->
           let p = List.hd (load_puzzles "../data/ver2_NESTED_puzzles.json") in
           let rec solve_all () =
             if not (is_won p) then begin
               List.iter (fun n -> ignore (submit n.answer p)) (exposed p);
               solve_all ()
             end
           in
           solve_all ();
           let solved, total = progress p in
           assert_equal total solved );
         (* hint_first_letter tests *)
         ( "hint_first_letter returns Some with correct first letter for a leaf \
            chip"
         >:: fun _ ->
           let leaf =
             {
               label = "a clue";
               answer = "Apple";
               children = [];
               solved = false;
             }
           in
           let p =
             {
               id = 99;
               difficulty = "easy";
               theme = "";
               title = "";
               solved_puzzle = false;
               root =
                 {
                   label = "{0}";
                   answer = "Apple";
                   children = [ leaf ];
                   solved = false;
                 };
             }
           in
           assert_equal (Some "A") (hint_first_letter "a clue" p) );
         ( "hint_first_letter returns None for a body that matches no exposed \
            node"
         >:: fun _ ->
           let p = List.hd (load_puzzles "../data/ver2_NESTED_puzzles.json") in
           assert_equal None (hint_first_letter "this matches nothing" p) );
         ( "hint_first_letter returns None for a solved node (no longer exposed)"
         >:: fun _ ->
           let leaf =
             {
               label = "a clue";
               answer = "Apple";
               children = [];
               solved = true;
             }
           in
           let p =
             {
               id = 99;
               difficulty = "easy";
               theme = "";
               title = "";
               solved_puzzle = false;
               root =
                 {
                   label = "{0}";
                   answer = "Apple";
                   children = [ leaf ];
                   solved = false;
                 };
             }
           in
           (* leaf is already solved so it is not exposed *)
           assert_equal None (hint_first_letter "a clue" p) );
         ( "hint_first_letter works on a real loaded puzzle's exposed node"
         >:: fun _ ->
           let p = List.hd (load_puzzles "../data/ver2_NESTED_puzzles.json") in
           let first_exposed = List.hd (exposed p) in
           let rendered = render_node first_exposed in
           let body =
             String.sub rendered 1 (String.length rendered - 2)
           in
           let expected = Some (String.make 1 first_exposed.answer.[0]) in
           assert_equal expected (hint_first_letter body p) );
         ( "hint_first_letter returns the parent's first letter once its \
            children are solved"
         >:: fun _ ->
           let leaf1 =
             { label = "clue1"; answer = "Alpha"; children = []; solved = false }
           in
           let leaf2 =
             { label = "clue2"; answer = "Beta"; children = []; solved = false }
           in
           let parent =
             {
               label = "{0} and {1}";
               answer = "Gamma";
               children = [ leaf1; leaf2 ];
               solved = false;
             }
           in
           let p =
             {
               id = 99;
               difficulty = "easy";
               theme = "";
               title = "";
               solved_puzzle = false;
               root = parent;
             }
           in
           leaf1.solved <- true;
           leaf2.solved <- true;
           (* parent is now exposed; its body = "Alpha and Beta" *)
           assert_equal (Some "G") (hint_first_letter "Alpha and Beta" p) );
       ]

let _ = run_test_tt_main tests
