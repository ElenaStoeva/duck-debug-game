open OUnit2
open Interpreter.Ast
open Interpreter.Eval
open Interpreter.Check
open Model.Grid
open Model.State

(* TODO: AST comparator & printer *)
let make_parse_test
    (name : string)
    (program : string)
    (expected_ast : program) : test =
  name >:: (fun _ ->
      assert_equal expected_ast (parse program))

let parser_tests = [
  make_parse_test "Empty prog" "" (Prog ([],[]));
  make_parse_test "Empty chars" "   " (Prog ([],[]));
  make_parse_test "No func" "MMRLMM" 
    (Prog ([], [Move; Move; Right; Left; Move; Move]));
  make_parse_test "No func with color" "M12L3M" 
    (Prog ([], [Move; Color(1); Color(2); Left; Color(3); Move]));
  make_parse_test "Func application" "f=MRM;[f]" 
    (Prog ([Def ("f", [], [Move; Right; Move])], [FunApp ("f", [])]));
  make_parse_test "Infinite stream" "f=MRM[f];[f]" 
    (Prog (
        [Def ("f", [], [Move; Right; Move; FunApp ("f", [])])], [FunApp ("f", [])])
    );
  make_parse_test "Multiple funcs" "g=MR;f=M[g];[f]"
    (Prog ([Def ("g", [], [Move; Right]); Def ("f", [], [Move; FunApp ("g", [])])], 
           [FunApp ("f", [])]))
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

let make_check_ast_test
    (name : string)
    (program : string)
    (expected_ast : program) : test =
  name >:: (fun _ ->
      assert_equal expected_ast (program |> parse |> check_ast))

(** TODO: Test exception raising in hd, tl, and check_ast. 
    Test mutual recursion.*)
let eval_tests = [
  make_stream_test "Empty stream" "" 0 []; 
  make_hd_test "No func hd" "MMRML" M;
  make_stream_test "No func stream" "MMRML" 3 [M; M; R];
  make_stream_test "Taking more than stream length" "MMRML" 10 [M; M; R; M; L];
  make_stream_test "Func application stream" "f=MRM;[f]" 3 [M; R; M];
  make_stream_test "Infinite stream" "f=MR1M[f];[f]" 12 
    [M; R; C(1); M; M; R; C(1); M; M; R; C(1); M];
  make_stream_test "Function with arg stream" "f -x=xR[f -xM];[f -M]" 9 
    [M; R; M; M; R; M; M; M ;R];
  make_stream_test "Function with args stream" 
    "f -x -y = xyR[f -xM -y];[f -M -L]" 13 
    [M; L; R; M; M; L; R; M; M; M; L; R; M];
  make_stream_test "Function ordered args stream" 
    "f -x -y = xyR[f -xM -y];[f -MRL -LR]" 27
    [M; R; L; L; R; R; M; R; L; M; L; R; R; M; R; L; M; M; L; R; R; M; R; 
     L; M; M; M];
  make_stream_test "Empty fun" "f -x=MRx[f -xM];[f -]" 9 
    [M; R; M; R; M; M; R; M; M]; 
  make_check_ast_test "No func check_ast" "MMRML" (parse "MMRML");
  make_check_ast_test "Func application check_ast" "f=MRM;[f]" 
    (parse "f=MRM;[f]");
  make_check_ast_test "Multiple func app check" "f=MRM;g=M;M[g]R[f]" 
    (parse "f=MRM;g=M;M[g]R[f]");
  make_check_ast_test "Infinite stream check_ast" "f=MRM[f];[f]" 
    (parse "f=MRM[f];[f]");
  make_check_ast_test "Color check" "f=1M2RM3[f];123[f]" 
    (parse "f=1M2RM3[f];123[f]");
]

let grid = "../../../../resources/json_files/example_with_walls.json" |> Yojson.Basic.from_file |> from_json

let state1 = init_state grid

let state2 = move state1

let state3 = turn Right state2 

let state4 = color Blue state3

let state_tests = [
  "Check initial agent position" >:: 
  (fun _ ->  assert_equal (1,1) (get_agent_x grid,get_agent_y grid));
  "Check agent position after moving" >:: 
  (fun _ ->  assert_equal (1,2,N) (get_agent state2));
  "Check agent orientation after rotating" >:: 
  (fun _ ->  assert_equal (1,2,E) (get_agent state3));
  "Check color of current square after coloring" >:: 
  (fun _ ->  assert_equal Blue (get_current_color state4));
  "Check moving to a wall" >:: 
  (fun _ ->  OUnit2.assert_raises Invalid_move (fun () -> move state4));
  "Check moving off the grid" >:: 
  (fun _ ->  OUnit2.assert_raises Invalid_move (fun () -> turn Left state4 |> move |> move));
]

let suite =
  "test suite for Interpreter"  >::: List.flatten [
    parser_tests;
    eval_tests;
    state_tests;
  ]

let _ = run_test_tt_main suite