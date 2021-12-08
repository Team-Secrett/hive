% :- module(utils, [
%   step/2,
%   get_player_turn/2,
%   cell_neighbours/4
% ]).
:- dynamic piece/6.

% Get the 6 neighbours of a cell
cell_neighbours(R, D, NR, ND) :-
  UP is R - 1,
  DOWN is R + 1,
  LEFT is D - 1,
  RIGHT is D + 1,
  NR = [UP, UP, R, DOWN, DOWN, R],
  ND = [D, RIGHT, RIGHT, D, LEFT, LEFT].

% Check if 2 cells are adjacent
is_adjacent(R1, D1, R2, D2) :-
  R1 = R2, D1 is D2 - 1;
  R1 = R2, D1 is D2 + 1;
  R1 is R2 - 1, D1 = D2;
  R1 is R2 - 1, D1 is D2 + 1;
  R1 is R2 + 1, D1 is D2 - 1;
  R1 is R2 + 1, D1 = D2.


% Piece methods

% Add a piece to local DB
add_piece(Piece) :- assert(Piece).

% Remove a piece from local DB
remove_piece(Piece) :- retract(Piece).

% Get DB pieces
get_pieces(Pieces) :-
  findall(
    piece(Class, Color, Id, R, D, Stacked),
    piece(Class, Color, Id, R, D, Stacked),
    Pieces
  ).

% Get the piece neighbours of a position
piece_neighbours(R, D, Neighbours) :-
  findall(
    piece(Class, Color, Id, NR, ND, Stacked),
    (
      piece(Class, Color, Id, NR, ND, Stacked),
      is_adjacent(NR, ND, R, D)
    ),
    Neighbours
  ).

position_filled(position(R, D, S)) :- piece(_, _, _, R, D, S).

% Move a piece to a new position or creat it if does not exist
move_piece(piece(Class, Color, Id, R, D, S), position(NR, ND, NS)) :-
  (piece(Class, Color, Id, R, D, S)
    -> remove_piece(piece(Class, Color, Id, R, D, S)) ; true
  ),
  add_piece(piece(Class, Color, Id, NR, ND, NS)).

move_queen(piece(Class, Color, Id, R, D, S), position(NR, ND, NS)) :-
  Class = 'q',
  (
    is_adjacent(R, D, NR, ND);
    (R = -1, D = -1, S = -1)
  ),
  \+ position_filled(position(NR, ND, NS)),
  move_piece(piece(Class, Color, Id, R, D, S), position(NR, ND, NS)).

move_ant(piece(Class, Color, Id, R, D, S), position(NR, ND, NS)) :-
  Class = 'a',
  (
    is_adjacent(R, D, NR, ND);
    (R = -1, D = -1, S = -1)
  ),
  move_piece(piece(Class, Color, Id, R, D, S), position(NR, ND, NS)).

get_side_position(position(R, D, S), Side, position(NR, ND, NS)) :-
  Side = "/*",
  NR is R - 1,
  ND = D,
  NS = S.
get_side_position(position(R, D, S), Side, position(NR, ND, NS)) :-
  Side = "|*",
  NR = R,
  ND is D - 1,
  NS = S.
get_side_position(position(R, D, S), Side, position(NR, ND, NS)) :-
  Side = "\\*",
  NR is R + 1,
  ND is D - 1,
  NS = S.
get_side_position(position(R, D, S), Side, position(NR, ND, NS)) :-
  Side = "*/",
  NR is R + 1,
  ND = D,
  NS = S.
get_side_position(position(R, D, S), Side, position(NR, ND, NS)) :-
  Side = "*|",
  NR = R,
  ND is D + 1,
  NS = S.
get_side_position(position(R, D, S), Side, position(NR, ND, NS)) :-
  Side = "*\\",
  NR is R - 1,
  ND is D + 1,
  NS = S.
get_side_position(position(R, D, S), Side, position(NR, ND, NS)) :-
  Side = "=*",
  NR = R,
  ND = D,
  NS is S + 1.

% Get the player turn (1 or 2) based on current Turn number
get_player_turn(Turn, Ans) :- Ans is 2 - mod(Turn, 2).

get_piece_from_str(Str, Piece) :-
  string_chars(Str, [Color, Class, Id]),
  atom_number(Id, IId),
  (
    (piece(Class, Color, IId, R, D, S), Piece = piece(Class, Color, IId, R, D, S));
    (\+ piece(Class, Color, IId, R, D, S), Piece = piece(Class, Color, IId, -1, -1, -1))
  ).

parse_action(ActionStr, Action) :-
  string_length(ActionStr, Length),
  Length = 3,
  string_chars(ActionStr, [Color, Class, Id]),
  atom_number(Id, IId),
  Piece = piece(Class, Color, IId, 0, 0, 0),

  Action = action(Piece, Piece, "=*").
parse_action(ActionStr, Action) :-
  string_length(ActionStr, Length),
  Length = 8,
  sub_string(ActionStr, 0, 3, _, Str1),
  sub_string(ActionStr, _, 3, 0, Str2),
  sub_string(ActionStr, 3, 2, _, Side),

  get_piece_from_str(Str1, Piece1),
  get_piece_from_str(Str2, Piece2),

  Action = action(Piece1, Piece2, Side).

step(
  action(
    piece(Class1, Color1, Id1, R1, D1, S1),
    piece(Class2, Color2, Id2, R2, D2, S2),
    Side
  )
) :-
  Class1 = Class2, Color1 = Color2, Id1 = Id2,
  add_piece(piece(Class1, Color1, Id1, 0, 0, 0)), !.
step(
  action(
    piece(Class1, Color1, Id1, R1, D1, S1),
    piece(Class2, Color2, Id2, R2, D2, S2),
    Side
  )
) :-
  get_side_position(position(R2, D2, S2), Side, position(NR, ND, NS)),
  (R1 = -1, D1 = -1, S1 = -1 ->
    (
      move_piece(piece(Class1, Color1, Id1, R2, D2, S2), position(NR, ND, NS))
    );
    (
      move_queen(piece(Class1, Color1, Id1, R1, D1, S1), position(NR, ND, NS));
      move_ant(piece(Class1, Color1, Id1, R1, D1, S1), position(NR, ND, NS))
    )
  ).

% Test methods
test() :-
  parse_action("wq1", Action),
  step(Action),
  parse_action("bq1/*wq1", Action2),
  step(Action2),
  parse_action("wa1*\\bq1", Action3),
  step(Action3),
  parse_action("wq1\\*bq1", Action4),
  step(Action4),
  get_pieces(Pieces),
  write(Pieces).
