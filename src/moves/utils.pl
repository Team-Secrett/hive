:- module(move_utils, [
  is_adjacent/4,
  add_piece/1,
  remove_piece/1,
  get_pieces/1,
  cell_neighbours/4,
  piece_neighbours/3,
  position_filled/1,
  position_filled/2,
  is_touching_hive/2,
  connected_component/3,
  articulation_point/1,
  deltas/6,
  get_side_position/3,
  has_piece_over/1,
  is_surrounded/2
]).
:- dynamic piece/6.

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

% Get the 6 neighbours of a cell
cell_neighbours(Q, R, NQ, NR) :-
  UP is R - 1,
  DOWN is R + 1,
  LEFT is Q - 1,
  RIGHT is Q + 1,
  NQ = [Q, RIGHT, RIGHT, Q, LEFT, LEFT],
  NR = [UP, UP, R, DOWN, DOWN, R].

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

% check if position is occupied by some other piece
position_filled(Q, R) :- piece(_, _, _, Q, R, _).
position_filled(position(Q, R, S)) :- piece(_, _, _, Q, R, S).

has_piece_over(position(Q, R, S)) :-
  get_pieces(Pieces),
  NS is S + 1,
  Piece = piece(Class, Color, Id, Q, R, NS),
  member(Piece, Pieces).

is_surrounded(Q, R) :-
  piece_neighbours(Q, R, Neighbours),
  length(Neighbours, Length),
  Length = 6.

% check if has a neighbour in hive
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
