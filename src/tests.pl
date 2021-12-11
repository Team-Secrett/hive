:- use_module('./environment').
:- use_module('./moves/utils').
:- use_module('./moves/index').
:- use_module('./lib/string_methods').
:- use_module('./agent').

do_simulation([], _).
do_simulation([Current | ActionStrs], Turn) :-
  parse_action(Current, Action),
  step(Action, Turn),
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

test_undo() :-
  choose_action(w, Action, 1),
  write_line(Action),
  step(Action, 1),
  choose_action(b, Action2, 2),
  write_line(Action2),
  step(Action2, 2),
  choose_action(w, Action3, 3),
  write_line(Action3),
  step(Action3, 3),
  choose_action(b, Action4, 4),
  write_line(Action4),
  step(Action4, 4),
  choose_action(w, Action5, 5),
  write_line(Action5),
  step(Action5, 5),
  choose_action(b, Action6, 6),
  write_line(Action6),
  step(Action6, 6),
  choose_action(w, Action7, 7),
  write_line(Action7),
  step(Action7, 7),
  choose_action(b, Action8, 8),
  write_line(Action8),
  step(Action8, 8),
  undo_action(Action8, 8),
  undo_action(Action7, 7),
  undo_action(Action6, 6),
  undo_action(Action5, 5),
  undo_action(Action4, 4),
  undo_action(Action3, 3),
  undo_action(Action2, 2),
  undo_action(Action, 1). 

test() :-
  test_undo(),
  get_pieces(P),
  write(P),
  write('\n').
