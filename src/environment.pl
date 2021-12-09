:- module(utils, [
  step/1,
  get_player_turn/2,
  cell_neighbours/4,
  parse_action/2,
  get_pieces/1
]).
:- use_module('./lib/string_methods', [
  write_lines/1
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
    piece(Class, Color, Id, NQ, NR, Stacked),
    (
      piece(Class, Color, Id, NQ, NR, Stacked),
      is_adjacent(NQ, NR, Q, R)
    ),
    Neighbours
  ).

position_filled(Q, R) :- piece(_, _, _, Q, R, _).
position_filled(position(Q, R, S)) :- piece(_, _, _, Q, R, S).

% Has a neighbour in hive
is_touching_hive(Q, R) :-
  piece_neighbours(Q, R, Neighbours),
  length(Neighbours, L),
  L > 0.

% find the pieces in connected component starting at a Piece
connected_component([], Marked, Component) :- append([], Marked, Component).
connected_component([piece(Class, Color, Id, Q, R, S) | Stack], Marked, Component) :-
  piece_neighbours(Q, R, Neighbours),
  member(Next, Neighbours),
  \+ member(Next, Marked),
  append(Stack, [Next], NStack),
  append(Marked, [Next], NMarked),
  connected_component([piece(Class, Color, Id, Q, R, S) | NStack], NMarked, Component), !.
connected_component([Current | Stack], Marked, Component) :-
  connected_component(Stack, Marked, Component).

% Check if a piece is an articulation point (breaks the hive if removed)
articulation_point(piece(_, _, _, _, _, S)) :- S > 0.
articulation_point(piece(Class, Color, Id, Q, R, S)) :-
  get_pieces(Pieces),
  length(Pieces, LPieces),
  Remaining is LPieces - 1,
  piece_neighbours(Q, R, [Start|_]),
  remove_piece(piece(Class, Color, Id, Q, R, S)),
  connected_component([Start], [], Component),
  add_piece(piece(Class, Color, Id, Q, R, S)),
  length(Component, LComponent),
  Remaining \= LComponent.

deltas(Q1, R1, Q2, R2, DQ, DR) :-
  DQ is Q2 - Q1,
  DR is R2 - R1.

grasshopper_can_move(Start, Start, _).
grasshopper_can_move(position(Q1, R1, _), position(Q2, R2, _), position(DQ, DR, _)) :-
  position_filled(Q1, R1),
  NQ is Q1 + DQ,
  NR is R1 + DR,
  grasshopper_can_move(position(NQ, NR, _), position(Q2, R2, _), position(DQ, DR, _)).

grasshopper_can_move(piece(Class, Color, Id, Q, R, S), position(NQ, NR, NS)) :-
  \+ position_filled(position(NQ, NR, NS)),
  is_adjacent(AdjQ, AdjR, Q, R),
  deltas(Q, R, AdjQ, AdjR, DQ, DR),
  grasshopper_can_move(position(Q, R, S), position(NQ, NR, NS), position(DQ, DR, 0)).

spider_can_move(piece(Class, Color, Id, Q, R, S), position(NQ, NR, NS)) :-
  remove_piece(piece(Class, Color, Id, Q, R, S)),
  (
    \+ position_filled(position(NQ, NR, NS)),
    is_touching_hive(NQ, NR),
    is_adjacent(AdjQ, AdjR, Q, R),
    \+ position_filled(position(AdjQ, AdjR, 0)),
    is_touching_hive(AdjQ, AdjR),
    is_adjacent(AdjQ2, AdjR2, AdjQ, AdjR),
    \+ position_filled(position(AdjQ2, AdjR2, 0)),
    is_touching_hive(AdjQ2, AdjR2),
    \+ (AdjQ2 = Q, AdjR2 = R),
    \+ (NQ = AdjQ, NR = AdjR),
    is_adjacent(NQ, NR, AdjQ2, AdjR2)
  ) ->
    add_piece(piece(Class, Color, Id, Q, R, S));
    (add_piece(piece(Class, Color, Id, Q, R, S)), false).

ladybug_can_move(piece(Class, Color, Id, Q, R, S), position(NQ, NR, NS)) :-
  remove_piece(piece(Class, Color, Id, Q, R, S)),
  (
    \+ position_filled(position(NQ, NR, NS)),
    is_adjacent(AdjQ, AdjR, Q, R),
    position_filled(position(AdjQ, AdjR, 0)),
    is_adjacent(AdjQ2, AdjR2, AdjQ, AdjR),
    position_filled(position(AdjQ2, AdjR2, 0)),
    is_adjacent(NQ, NR, AdjQ2, AdjR2)
  ) ->
    add_piece(piece(Class, Color, Id, Q, R, S));
    (add_piece(piece(Class, Color, Id, Q, R, S)), false).

queen_can_move(piece(Class, Color, Id, Q, R, S), position(NQ, NR, NS)) :-
  \+ position_filled(position(NQ, NR, NS)),
  is_touching_hive(NQ, NR),
  is_adjacent(Q, R, NQ, NR).

ant_can_move(piece(Class, Color, Id, Q, R, S), position(NQ, NR, NS)) :-
  \+ position_filled(position(NQ, NR, NS)),
  is_touching_hive(NQ, NR).

beetle_can_move(piece(Class, Color, Id, Q, R, S), position(NQ, NR, NS)) :-
  \+ position_filled(position(NQ, NR, NS)),
  is_touching_hive(NQ, NR),
  is_adjacent(Q, R, NQ, NR).

mosquito_can_move(piece(Class, Color, Id, Q, R, S), position(NQ, NR, NS), C) :-
  (C = 'q', queen_can_move(piece(Class, Color, Id, Q, R, S), position(NQ, NR, NS)));
  (C = 'a', ant_can_move(piece(Class, Color, Id, Q, R, S), position(NQ, NR, NS)));
  (C = 'l', ladybug_can_move(piece(Class, Color, Id, Q, R, S), position(NQ, NR, NS)));
  (C = 's', spider_can_move(piece(Class, Color, Id, Q, R, S), position(NQ, NR, NS)));
  (C = 'g', grasshopper_can_move(piece(Class, Color, Id, Q, R, S), position(NQ, NR, NS)));
  (C = 'b', beetle_can_move(piece(Class, Color, Id, Q, R, S), position(NQ, NR, NS))).

mosquito_can_move(piece(Class, Color, Id, Q, R, S), position(NQ, NR, NS)) :-
  piece_neighbours(Q, R, Neighbours),
  member(piece(NeighbourClass, _, _, _, _, _), Neighbours),
  mosquito_can_move(piece(Class, Color, Id, Q, R, S), position(NQ, NR, NS), NeighbourClass).

% Move a piece to a new position or creat it if does not exist
move_piece(piece(Class, Color, Id, Q, R, S), position(NQ, NR, NS)) :-
  (piece(Class, Color, Id, Q, R, S)
    -> remove_piece(piece(Class, Color, Id, Q, R, S)) ; true
  ),
  add_piece(piece(Class, Color, Id, NQ, NR, NS)).

move_queen(piece(Class, Color, Id, Q, R, S), position(NQ, NR, NS)) :-
  Class = 'q',
  queen_can_move(piece(Class, Color, Id, Q, R, S), position(NQ, NR, NS)),
  move_piece(piece(Class, Color, Id, Q, R, S), position(NQ, NR, NS)).

move_ant(piece(Class, Color, Id, Q, R, S), position(NQ, NR, NS)) :-
  Class = 'a',
  ant_can_move(piece(Class, Color, Id, Q, R, S), position(NQ, NR, NS)),
  move_piece(piece(Class, Color, Id, Q, R, S), position(NQ, NR, NS)).

move_beetle(piece(Class, Color, Id, Q, R, S), position(NQ, NR, NS)) :-
  Class = 'b',
  beetle_can_move(piece(Class, Color, Id, Q, R, S), position(NQ, NR, NS)),
  move_piece(piece(Class, Color, Id, Q, R, S), position(NQ, NR, NS)).

move_grasshopper(piece(Class, Color, Id, Q, R, S), position(NQ, NR, NS)) :-
  Class = 'g',
  grasshopper_can_move(piece(Class, Color, Id, Q, R, S), position(NQ, NR, NS)),
  move_piece(piece(Class, Color, Id, Q, R, S), position(NQ, NR, NS)).

move_spider(piece(Class, Color, Id, Q, R, S), position(NQ, NR, NS)) :-
  Class = 's',
  spider_can_move(piece(Class, Color, Id, Q, R, S), position(NQ, NR, NS)),
  move_piece(piece(Class, Color, Id, Q, R, S), position(NQ, NR, NS)).

move_ladybug(piece(Class, Color, Id, Q, R, S), position(NQ, NR, NS)) :-
  Class = 'l',
  ladybug_can_move(piece(Class, Color, Id, Q, R, S), position(NQ, NR, NS)),
  move_piece(piece(Class, Color, Id, Q, R, S), position(NQ, NR, NS)).

move_mosquito(piece(Class, Color, Id, Q, R, S), position(NQ, NR, NS)) :-
  Class = 'm',
  mosquito_can_move(piece(Class, Color, Id, Q, R, S), position(NQ, NR, NS)),
  move_piece(piece(Class, Color, Id, Q, R, S), position(NQ, NR, NS)).


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
      position_filled(NQ, NR)
      -> (write("The position is already occuppied.\n"), false)
      ; move_piece(piece(Class1, Color1, Id1, Q1, R1, S1), position(NQ, NR, NS))
    );
    (
      articulation_point(piece(Class1, Color1, Id1, Q1, R1, S1))
      -> (write("This move disconnect the hive.\n"), false)
      ; (
        move_queen(piece(Class1, Color1, Id1, Q1, R1, S1), position(NQ, NR, NS));
        move_ant(piece(Class1, Color1, Id1, Q1, R1, S1), position(NQ, NR, NS));
        move_beetle(piece(Class1, Color1, Id1, Q1, R1, S1), position(NQ, NR, NS));
        move_grasshopper(piece(Class1, Color1, Id1, Q1, R1, S1), position(NQ, NR, NS));
        move_spider(piece(Class1, Color1, Id1, Q1, R1, S1), position(NQ, NR, NS));
        move_ladybug(piece(Class1, Color1, Id1, Q1, R1, S1), position(NQ, NR, NS));
        move_mosquito(piece(Class1, Color1, Id1, Q1, R1, S1), position(NQ, NR, NS));
        (
          write("Invalid move.\n"),
          false
        )
      )
    )
  ).

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
  write(Pieces).

% Test grasshopper
test_grasshopper() :-
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
  step(A6).
