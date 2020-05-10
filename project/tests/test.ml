(** Test plan for the system:

    We tested the Interpreter, the Grid module, and the State module by OUnit
     tests. We manually tested the game interfaces CLI and the GUI, and their 
     backend support module Controller.

    The OUnit tests were developed by both Black box testing and Glass box 
    testing. Accordingly, each member performed Glass box testing on the 
    modules that they implemented and Black box testing on the rest of the 
    modules. We also used Bisect, trying to maximize the coverage for each 
    of the modules.

    For the Black box testing, we believe that our tests cover not only the 
    typical cases but also most of the possible boundary cases. Also, Bisect
     helped us improve the Glass box testing a lot. Thus, we believe that our
    testing approach demonstrates the correctness of our system. 
    
    We tested the remaining system manually as the controller and the 
    applications worked as a unit and testing their isolated parts independently
    was not better than manually testing the application. *)

open OUnit2
open Interpreter.Ast
open Interpreter.Eval
open Interpreter.Check
open Model.Grid
open Model.State

let make_parse_test
    (name : string)
    (program : string)
    (expected_ast : program) : test =
  name >:: (fun _ ->
      assert_equal expected_ast (parse program))

(** Test suite or Interpreter *)
let parser_tests = [
  make_parse_test "Empty prog" "" (Prog ([],[]));
  make_parse_test "Empty chars" "   " (Prog ([],[]));
  make_parse_test "Move and Turn only" "MMRLMM" 
    (Prog ([], [Move; Move; Right; Left; Move; Move]));
  make_parse_test "Colors only" "12321" 
    (Prog ([], [Color(1); Color(2); Color(3); Color(2);Color(1);]));  
  make_parse_test "Mixed" "M12L3M" 
    (Prog ([], [Move; Color(1); Color(2); Left; Color(3); Move]));
  make_parse_test "Empty Func application" "f=;[f]" 
    (Prog ([Def ("f", [], [])], [FunApp ("f", [])]));
  make_parse_test "Func application no color" "f=MRM;[f]" 
    (Prog ([Def ("f", [], [Move; Right; Move])], [FunApp ("f", [])]));
  make_parse_test "Func application with color" "f=M1M;[f]" 
    (Prog ([Def ("f", [], [Move; Color(1); Move])], [FunApp ("f", [])]));
  make_parse_test "Two funcs" "g=MR;f=M[g];[f]"
    (Prog ([Def ("g", [], [Move; Right]); 
            Def ("f", [], [Move; FunApp ("g", [])])], 
           [FunApp ("f", [])]));
  make_parse_test "Three funcs" "g=MR;f=M[g];p=R[f];[p]"
    (Prog ([Def ("g", [], [Move; Right]); 
            Def ("f", [], [Move; FunApp ("g", []);]);
            Def ("p", [], [Right; FunApp ("f", []);])], 
           [FunApp ("p", [])]));
  make_parse_test "One infinite stream" "f=MRM[f];[f]" 
    (Prog (
        [Def ("f", [], [Move; Right; Move; 
                        FunApp ("f", [])])], [FunApp ("f", [])])
    );
  make_parse_test "Multiple funcs with an infinite stream" "g=MR[g];f=M[g];[f]" 
    (Prog ([Def ("g", [], [Move; Right; FunApp ("g", [])]);
            Def ("f", [], [Move; FunApp ("g", [])])], 
           [FunApp ("f", [])]));      
  make_parse_test "Two infinite streams" "g=MR[g];f=M[g][f];[f]" 
    (Prog ([Def ("g", [], [Move; Right; FunApp ("g", [])]);
            Def ("f", [], [Move; FunApp ("g", []); FunApp ("f", [])])], 
           [FunApp ("f", [])]));
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
  make_check_ast_test "Two func app check" "f=MRM;g=M;M[g]R[f]" 
    (parse "f=MRM;g=M;M[g]R[f]");
  make_check_ast_test "Three func app check" "f=MRM;g=M;p=L;M[g]L[p]R[f]" 
    (parse "f=MRM;g=M;p=L;M[g]L[p]R[f]");
  make_check_ast_test "Infinite stream check_ast" "f=MRM[f];[f]" 
    (parse "f=MRM[f];[f]");
  make_check_ast_test "Two infinite streams" "f=MRM[f];g=M[f][g];[g]" 
    (parse "f=MRM[f];g=M[f][g];[g]");
  make_check_ast_test "Func in an infinite stream" "g=MR;f=MRM[g][f];[f]" 
    (parse "g=MR;f=MRM[g][f];[f]");
  make_check_ast_test "Color check" "f=1M2RM3[f];123[f]" 
    (parse "f=1M2RM3[f];123[f]");
  "Check undefined function" >:: 
  (fun _ ->  
     OUnit2.assert_raises 
       Undefined_function (fun () -> "f=MRM;[g]"|> parse |> check_ast););
  "Check undefined color" >:: 
  (fun _ ->  
     OUnit2.assert_raises 
       Undefined_color (fun () -> "f=M5M;[f]"|> parse |> check_ast););
]

(** A grid with obstacles*)
let grid1 = "../../../../resources/json_files/example_with_walls.json" 
            |> Yojson.Basic.from_file 
            |> from_json

(** A grid without obstacles*)
let grid2 = "../../../../resources/json_files/example.json" 
            |> Yojson.Basic.from_file 
            |> from_json

(** Position (1,1,N) *)
let state1 = init_state grid1

(** Test suite for State *)
let state_tests = [
  "Check initial agent position1" >:: 
  (fun _ ->  assert_equal (1,1) (get_agent_x grid1,get_agent_y grid1));
  "Check initial agent position2" >:: 
  (fun _ ->  assert_equal (1,1,W) (grid2 |> init_state |> get_agent));
  "Check moving to north" >:: 
  (fun _ ->  assert_equal (1,2,N) ( state1 |> move |> get_agent));
  "Check moving to south" >:: 
  (fun _ ->  assert_equal (1,1,S) 
      (state1 |> move |> turn Right |> turn Right  
       |> move |> get_agent));
  "Check moving to west" >:: 
  (fun _ ->  assert_equal (1,1,W) 
      (state1 |> turn Right  |> move |> turn Left |>
       turn Left |> move |> get_agent));
  "Check turning right" >:: 
  (fun _ ->  assert_equal (1,2,E) (state1|> move |> turn Right  |> get_agent));
  "Check turning left" >:: 
  (fun _ ->  assert_equal (1,2,W) (state1 |> move  |> turn Left |> get_agent));
  "Check color of current square after coloring" >:: 
  (fun _ ->  assert_equal Blue (state1 |> color Blue |> get_current_color));
  "Check moving to an obstacle" >:: 
  (fun _ ->  OUnit2.assert_raises (Wall_exception) 
      (fun () ->state1 |> move |> 
                turn Right 
                |> color Blue  
                |> move ));
  "Check moving off the grid1 from west" >:: 
  (fun _ ->  OUnit2.assert_raises Invalid_move 
      (fun () -> state1 |> turn Left |> move));
  "Check moving off the grid1 from north" >:: 
  (fun _ ->  OUnit2.assert_raises Invalid_move 
      (fun () -> state1 |> move |> move |> move));   
  "Check moving off the grid1 from east" >:: 
  (fun _ ->  OUnit2.assert_raises Invalid_move 
      (fun () -> state1 |> turn Left |> move |> move |> move));   
  "Check moving off the grid1 from south" >:: 
  (fun _ ->  OUnit2.assert_raises Invalid_move 
      (fun () -> state1 |> turn Left |> turn Left |> move));                                               
  "Check turning left from west" >:: 
  (fun _ ->  assert_equal (1,1,S) 
      ( state1 |> turn Left |> turn Left  |> get_agent));
  "Check turning right from west" >:: 
  (fun _ ->  assert_equal (1,1,N) 
      ( state1 |> turn Left |> turn Right |> get_agent));
  "Check turning right from south" >:: 
  (fun _ ->  assert_equal (1,1,W) 
      ( state1 |> turn Left |> turn Left |> turn Right |> get_agent));  
  "Check turning left from south" >:: 
  (fun _ ->  assert_equal (1,1,E) 
      (state1 |> turn Left|> turn Left |> turn Left |> get_agent));                                                           
  "Check no winning" >:: (fun _ -> assert (not (check_win state1 grid1)));
  "Check winning" >:: 
  (fun _ -> assert 
    (let state = state1 |> color Green |> move |> color Green 
                 |> move |> color Red in check_win state grid1));
  "Check steps left" >::
  (fun _ ->  assert_equal 49 
      ( move state1 |> get_steps));
  "Check initial score" >::
  (fun _ ->  assert_equal 130 
      ( grid1 |> get_score));      
  "Check instructions" >::
  (fun _ ->  assert_equal 
      "This is just a 3x3 grid with randomly colored squares."
      ( grid1 |> get_instructions));     

]

(** Full test suite for the system*)
let suite =
  "test suite for Interpreter"  >::: List.flatten [
    parser_tests;
    eval_tests;
    state_tests;
  ]

let _ = run_test_tt_main suite