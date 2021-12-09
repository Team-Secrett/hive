:- use_module('./environment', [
  parse_action/2,
  step/1,
  get_pieces/1
]).

% Test mosquito
test_mosquito() :-
  parse_action("wq1", A),
  step(A),
  parse_action("bq1 NW wq1", A2),
  step(A2),
  parse_action("ws1 NW bq1", A3),
  step(A3),
  parse_action("wm1 S wq1", A4),
  step(A4),
  parse_action("ba1 SW wq1", A5),
  step(A5),
  parse_action("wm1 NE wq1", A6),
  step(A6),
  get_pieces(Pieces),
  write(Pieces),
  write('\n').

test() :-
  write("################## TEST 1: ########################\n"),
  test_mosquito(),
  write("################## PASSED: ########################\n").
