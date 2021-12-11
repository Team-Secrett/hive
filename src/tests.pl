:- use_module('./environment', [
  parse_action/2,
  step/2,
  get_pieces/1,
  winner/1
]).
:- use_module('./moves/utils').
:- use_module('./moves/index').
:- use_module('./lib/string_methods').
:- use_module('./agent').

do_simulation([], Turn).
do_simulation([Current | ActionStrs], Turn) :-
  parse_action(Current, Action),
  step(Action, Turn),
  get_pieces(P),
  NextTurn is Turn + 1,
  do_simulation(ActionStrs, NextTurn).

% Test mosquito
test_mosquito() :-
  do_simulation([
    "wq1",
    "bq1 NW wq1",
    "ws1 NW bq1",
    "wm1 S wq1",
    "ba1 SW wq1",
    "wm1 NE wq1"
  ], 1).

% If a Piece is over another, this another piece can't move
test_beetle_over_piece() :-
  do_simulation([
    "ws1",
    "bb1 NE ws1",
    "wa1 SE ws1",
    "bb1 O wa1",
    "wa1 NE ws1"
  ], 1).

test_break_hive() :-
  do_simulation([
    "wq1",
    "bq1 NW wq1",
    "wa1 NW bq1",
    "bq1 N wq1"
  ], 1).

test_black_wins() :-
  add_piece(piece(q, w, 1, 0, 0, 0)),

  add_piece(piece(a, b, 1, 0, -1, 0)),
  add_piece(piece(a, w, 1, 1, -1, 0)),
  add_piece(piece(a, b, 1, 1, 0, 0)),
  add_piece(piece(a, w, 1, 0, 1, 0)),
  add_piece(piece(a, b, 1, -1, 1, 0)),
  add_piece(piece(a, w, 1, -1, 0, 0)),
  winner(W),
  write_lines(["Winner:", W]),
  W = 2.

test_white_wins() :-
  add_piece(piece(q, b, 1, 0, 0, 0)),

  add_piece(piece(a, w, 1, 0, -1, 0)),
  add_piece(piece(a, b, 1, 1, -1, 0)),
  add_piece(piece(a, w, 1, 1, 0, 0)),
  add_piece(piece(a, b, 1, 0, 1, 0)),
  add_piece(piece(a, w, 1, -1, 1, 0)),
  add_piece(piece(a, b, 1, -1, 0, 0)),
  winner(W),
  write_lines(["Winner:", W]),
  W = 1.

test_tie() :-
  add_piece(piece(q, b, 1, 0, 0, 0)),

  % Surround black queen
  add_piece(piece(a, w, 1, 0, -1, 0)),
  add_piece(piece(a, b, 1, 1, -1, 0)),
  add_piece(piece(a, w, 1, 1, 0, 0)),
  add_piece(piece(a, b, 1, 0, 1, 0)),
  add_piece(piece(a, w, 1, -1, 1, 0)),
  add_piece(piece(q, w, 1, -1, 0, 0)), % white queen

  % Surround white queen
  add_piece(piece(a, b, 1, -1, -1, 0)),
  add_piece(piece(a, b, 1, -2, 0, 0)),
  add_piece(piece(a, b, 1, -2, 1, 0)),

  winner(W),
  write_lines(["Winner:", W]),
  W = 0.

test_game() :-
  do_simulation([
    "wa1",
    "bq1 NW wa1"
    % "wa1 S wq1"
  ], 1).

test_spider_moves() :-
  do_simulation([
    "wq1",
    "bs1 S wq1",
    "wa1 N wq1"
  ], 1).

test() :-
  test_spider_moves(),
  get_pieces(P),
  write('\n').
