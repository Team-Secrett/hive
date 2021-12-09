:- use_module('./environment', [
  parse_action/2,
  step/1,
  get_pieces/1,
  winner/1
]).
:- use_module('./moves/utils').
:- use_module('./lib/string_methods').

do_simulation([]).
do_simulation([Current | ActionStrs]) :-
  parse_action(Current, Action),
  step(Action),
  get_pieces(P),
  do_simulation(ActionStrs).

% Test mosquito
test_mosquito() :-
  do_simulation([
    "wq1",
    "bq1 NW wq1",
    "ws1 NW bq1",
    "wm1 S wq1",
    "ba1 SW wq1",
    "wm1 NE wq1"
  ]),
  get_pieces(Pieces),
  write(Pieces),
  write('\n').

% If a Piece is over another, this another piece can't move
test_beetle_over_piece() :-
  do_simulation([
    "ws1",
    "bb1 NE ws1",
    "wa1 SE ws1",
    "bb1 O wa1",
    "wa1 NE ws1"
  ]).

test_break_hive() :-
  do_simulation([
    "wq1",
    "bq1 NW wq1",
    "wa1 NW bq1",
    "bq1 N wq1"
  ]).

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

test() :-
  (test_beetle_over_piece() ; true),
  get_pieces(Pieces),
  write(Pieces),
  write('\n').
