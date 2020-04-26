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
    (Prog ([], [Move; Move; Right; Left; Move; Move]));
  make_parse_test "Func application" "f=MRM;[f]" 
    (Prog ([Def ("f", [Move; Right; Move])], [App ("f")]));
  make_parse_test "Infinite stream" "f=MRM[f];[f]" 
    (Prog ([Def ("f", [Move; Right; Move; App ("f")])], [App ("f")]));
  make_parse_test "Multiple funcs" "g=MR;f=M[g];[f]"
    (Prog ([Def ("g", [Move; Right]); Def ("f", [Move; App ("g")])], 
           [App ("f")]))
]

let make_hd_test
    (name : string)
    (program : string)
    (expected_head : prim_move) : test =
  name >:: (fun _ ->
      assert_equal expected_head (program |> parse |> init_stream |> hd))

let make_stream_test
    (name : string)
    (program : string)
    (n_moves : int)
    (expected_moves : prim_move list) : test =
  name >:: (fun _ ->
      assert_equal expected_moves 
        (program |> parse |> init_stream |> take n_moves))

let make_has_def_test
    (name : string)
    (program : string)
    (expected_ast : program) : test =
  name >:: (fun _ ->
      assert_equal expected_ast (program |> parse |> has_def))

(** TODO: Test exception raising in hd, tl, and has_def. 
    Test mutual recursion.*)
let eval_tests = [
  make_stream_test "Empty stream" "" 0 []; 
  make_hd_test "No func hd" "MMRML" M;
  make_stream_test "No func stream" "MMRML" 3 [M; M; R];
  make_stream_test "Taking more than stream length" "MMRML" 10 [M; M; R; M; L];
  make_stream_test "Func application stream" "f=MRM;[f]" 3 [M; R; M];
  make_stream_test "Infinite stream" "f=MRM[f];[f]" 7 [M; R; M; M; R; M; M];
  make_has_def_test "No func has_def" "MMRML" (parse "MMRML");
  make_has_def_test "Func application has_def" "f=MRM;[f]" (parse "f=MRM;[f]");
  make_has_def_test "Infinite stream has_def" "f=MRM[f];[f]" 
    (parse "f=MRM[f];[f]");
]

(* TODO: Add Bisect. *)

let suite =
  "test suite for Interpreter"  >::: List.flatten [
    parser_tests;
    eval_tests;
  ]

let _ = run_test_tt_main suite