:- module(utils, [
  step/1,
  get_player_turn/2,
  cell_neighbours/4,
  parse_action/2,
  get_pieces/1
]).
:- dynamic piece/6.

% Get the 6 neighbours of a cell
cell_neighbours(Q, R, NQ, NR) :-
  UP is R - 1,
  DOWN is R + 1,
  LEFT is Q - 1,
  RIGHT is Q + 1,
  NQ = [Q, RIGHT, RIGHT, Q, LEFT, LEFT],
  NR = [UP, UP, R, DOWN, DOWN, R].

% Check if 2 cells are adjacent
is_adjacent(Q1, R1, Q2, R2) :-
  (Q1 is Q2, R1 is R2 - 1);
  (Q1 is Q2, R1 is R2 + 1);
  (Q1 is Q2 - 1, R1 is R2);
  (Q1 is Q2 - 1, R1 is R2 + 1);
  (Q1 is Q2 + 1, R1 is R2 - 1);
  (Q1 is Q2 + 1, R1 is R2).


% Piece methods

% Add a piece to local DB
add_piece(Piece) :- assert(Piece).

% Remove a piece from local DB
remove_piece(Piece) :- retract(Piece).

% Get DB pieces
get_pieces(Pieces) :-
  findall(
    piece(Class, Color, Id, Q, R, Stacked),
    piece(Class, Color, Id, Q, R, Stacked),
    Pieces
  ).

% Get the piece neighbours of a position
piece_neighbours(Q, R, Neighbours) :-
  findall(
    piece(Class, Color, Id, NQ, NQ, Stacked),
    (
      piece(Class, Color, Id, NQ, NQ, Stacked),
      is_adjacent(NQ, NQ, Q, R)
    ),
    Neighbours
  ).

position_filled(Q, R) :- piece(_, _, _, Q, R, _).
position_filled(position(Q, R, S)) :- piece(_, _, _, Q, R, S).

deltas(Q1, R1, Q2, R2, DQ, DR) :-
  DQ is Q2 - Q1,
  DR is R2 - R1.

grasshopper_can_move(Start, Start, _).
grasshopper_can_move(position(Q1, R1, _), position(Q2, R2, _), position(DQ, DR, _)) :-
  position_filled(Q1, R1),
  NQ is Q1 + DQ,
  NR is R1 + DR,
  grasshopper_can_move(position(NQ, NR, _), position(Q2, R2, _), position(DQ, DR, _)).

% Move a piece to a new position or creat it if does not exist
move_piece(piece(Class, Color, Id, Q, R, S), position(NQ, NR, NS)) :-
  (piece(Class, Color, Id, Q, R, S)
    -> remove_piece(piece(Class, Color, Id, Q, R, S)) ; true
  ),
  add_piece(piece(Class, Color, Id, NQ, NR, NS)).

move_queen(piece(Class, Color, Id, Q, R, S), position(NQ, NR, NS)) :-
  Class = 'q',
  write("Moving queen"),
  (
    is_adjacent(Q, R, NQ, NR);
    (Q = -1, R = -1, S = -1)
  ),
  \+ position_filled(position(NQ, NR, NS)),
  move_piece(piece(Class, Color, Id, Q, R, S), position(NQ, NR, NS)).

move_ant(piece(Class, Color, Id, Q, R, S), position(NQ, NR, NS)) :-
  Class = 'a',
  write("Moving ant"),
  \+ position_filled(position(NQ, NR, NS)),
  move_piece(piece(Class, Color, Id, Q, R, S), position(NQ, NR, NS)).

move_beetle(piece(Class, Color, ID, Q, R, S), position(NQ, NR, NS)) :-
  Class = 'b',
  write("Moving beetle"),
  (
    is_adjacent(Q, R, NQ, NR);
    (Q = -1, R = -1, S = -1)
  ),
  move_piece(piece(Class, Color, Id, Q, R, S), position(NQ, NR, NS)).

move_grasshopper(piece(Class, Color, ID, Q, R, S), position(NQ, NR, NS)) :-
  Class = 'g',
  write("Moving grasshopper"),
  \+ position_filled(position(NQ, NR, NS)),
  is_adjacent(AdjQ, AdjR, Q, R),
  deltas(Q, R, AdjQ, AdjR, DQ, DR),
  grasshopper_can_move(position(Q, R, S), position(NQ, NR, NS), position(DQ, DR, 0)).


get_side_position(position(Q, R, S), Side, position(NQ, NR, NS)) :-
  Side = "N",
  NQ = Q,
  NR is R - 1,
  NS = S.
get_side_position(position(Q, R, S), Side, position(NQ, NR, NS)) :-
  Side = "NW",
  NQ is Q - 1,
  NR = R,
  NS = S.
get_side_position(position(Q, R, S), Side, position(NQ, NR, NS)) :-
  Side = "SW",
  NQ is Q - 1,
  NR is R + 1,
  NS = S.
get_side_position(position(Q, R, S), Side, position(NQ, NR, NS)) :-
  Side = "NE",
  NQ is Q + 1,
  NR is R - 1,
  NS = S.
get_side_position(position(Q, R, S), Side, position(NQ, NR, NS)) :-
  Side = "SE",
  NQ is Q + 1,
  NR = R,
  NS = S.
get_side_position(position(Q, R, S), Side, position(NQ, NR, NS)) :-
  Side = "S",
  NQ = Q,
  NR is R + 1,
  NS = S.
get_side_position(position(Q, R, S), Side, position(NQ, NR, NS)) :-
  Side = "O",
  NQ = Q,
  NR = R,
  NS is S + 1.

% Get the player turn (1 or 2) based on current Turn number
get_player_turn(Turn, Ans) :- Ans is 2 - mod(Turn, 2).

get_piece_from_str(Str, Piece) :-
  string_chars(Str, [Color, Class, Id]),
  atom_number(Id, IId),
  (
    (piece(Class, Color, IId, Q, R, S), Piece = piece(Class, Color, IId, Q, R, S));
    (\+ piece(Class, Color, IId, Q, R, S), Piece = piece(Class, Color, IId, -1, -1, -1))
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
  Length > 3,
  split_string(ActionStr, " ", "", [Str1, Side, Str2]),
  get_piece_from_str(Str1, Piece1),
  get_piece_from_str(Str2, Piece2),
  Action = action(Piece1, Piece2, Side).

step(
  action(
    piece(Class1, Color1, Id1, Q1, R1, S1),
    piece(Class2, Color2, Id2, Q2, R2, S2),
    Side
  )
) :-
  Class1 = Class2, Color1 = Color2, Id1 = Id2,
  add_piece(piece(Class1, Color1, Id1, 0, 0, 0)), !.
step(
  action(
    piece(Class1, Color1, Id1, Q1, R1, S1),
    piece(Class2, Color2, Id2, Q2, R2, S2),
    Side
  )
) :-
  get_side_position(position(Q2, R2, S2), Side, position(NQ, NR, NS)),
  (Q1 = -1, R1 = -1, S1 = -1 ->
    (
      move_piece(piece(Class1, Color1, Id1, Q1, R1, S1), position(NQ, NR, NS))
    );
    (
      move_queen(piece(Class1, Color1, Id1, Q1, R1, S1), position(NQ, NR, NS));
      move_ant(piece(Class1, Color1, Id1, Q1, R1, S1), position(NQ, NR, NS));
      move_beetle(piece(Class1, Color1, Id1, Q1, R1, S1), position(NQ, NR, NS));
      move_grasshopper(piece(Class1, Color1, Id1, Q1, R1, S1), position(NQ, NR, NS));
      (
        write("Invalid move.\n"),
        false
      )
    )
  ).

% Test methods
test() :-
  parse_action("wq1", Action),
  step(Action),
  parse_action("bq1 NW wq1", Action2),
  step(Action2),
  parse_action("wg1 NW bq1", Action3),
  step(Action3),
  parse_action("wg1 SE wq1", Action4),
  step(Action4),
  % parse_action("wa1 NE bq1", Action3),
  % step(Action3),
  % parse_action("wq1 S bq1", Action4),
  % step(Action4),
  % parse_action("bb1 S wq1", Action5),
  % step(Action5),
  % parse_action("bb1 O wq1", Action6),
  % step(Action6),
  % parse_action("bb1 S wq1", Action7),
  % step(Action7),
  get_pieces(Pieces),
  write(Pieces).
