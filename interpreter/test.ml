open OUnit2
open Ast
open Eval

(* TODO: AST comparator & printer *)
let make_parse_test
    (name : string)
    (program : string)
    (expected_ast : program) : test =
  name >:: (fun _ ->
      assert_equal expected_ast (parse program))

let parser_tests = [
  make_parse_test "Empty prog" "" (Prog ([],[]));
  make_parse_test "No func" "MMRLMM" 
    (Prog ([], [Move; Move; Right; Left; Move; Move]))
]

let make_stream_test
    (name : string)
    (program : string)
    (n_moves : int)
    (expected_moves : command list) : test =
  name >:: (fun _ ->
      assert_equal expected_moves 
        (program |> parse |> init_stream |> take n_moves))

let eval_tests = [
  make_stream_test "Empty stream" "" 0 []; 
  (* hd, tl, take. *)
]

(* TODO: Add Bisect. *)

let suite =
  "test suite for Interpreter"  >::: List.flatten [
    parser_tests;
    eval_tests;
  ]

let _ = run_test_tt_main suite