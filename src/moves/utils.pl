:- module(move_utils, [
  is_adjacent/4,
  add_piece/1,
  remove_piece/1,
  get_pieces/1,
  get_pieces/2,
  cell_neighbours/4,
  piece_neighbours/3,
  position_filled/1,
  position_filled/2,
  is_touching_hive/2,
  is_touching_hive/4,
  connected_component/3,
  articulation_point/1,
  deltas/6,
  get_side_position/3,
  has_piece_over/1,
  is_surrounded/2,
  has_white_neighbour/1,
  has_black_neighbour/1,
  valid_piece/1,
  positions/2,
  all_pieces/1,
  all_pieces/2,
  has_neighbour_with_color/2,
  pieces_in_bag/3
]).
:- use_module('./src/lib/string_methods').
:- dynamic piece/6.
:- dynamic positions/3.
:- dynamic action/3.

% Check if 2 cells are adjacent
is_adjacent(Q1, R1, Q2, R2) :-
  (Q1 = Q2, R1 is R2 - 1);
  (Q1 = Q2, R1 is R2 + 1);
  (Q1 is Q2 - 1, R1 = R2);
  (Q1 is Q2 - 1, R1 is R2 + 1);
  (Q1 is Q2 + 1, R1 is R2 - 1);
  (Q1 is Q2 + 1, R1 = R2).

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

% Get DB pieces by a color
get_pieces(PieceColor, Pieces) :-
  findall(
    piece(Class, Color, Id, Q, R, Stacked),
    (
      piece(Class, Color, Id, Q, R, Stacked),
      Color = PieceColor
    ),
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

% check if Piece is a valid piece of the game
valid_piece(piece(Class, Color, Id, Q, N, S)) :-
  (Class = q, Id = 1);
  (Class = a, Id >= 1, Id =< 3);
  (Class = g, Id >= 1, Id =< 3);
  (Class = b, Id >= 1, Id =< 2);
  (Class = s, Id >= 1, Id =< 2);
  (Class = m, Id = 1);
  (Class = l, Id = 1);
  (Class = p, Id = 1).

% get all pieces on the game by color
all_pieces(Color, Pieces) :-
  Pieces = [
    piece(q, Color, 1, -1, -1, -1),
    piece(a, Color, 1, -1, -1, -1),
    piece(a, Color, 1, -1, -1, -1),
    piece(a, Color, 1, -1, -1, -1),
    piece(g, Color, 1, -1, -1, -1),
    piece(g, Color, 1, -1, -1, -1),
    piece(g, Color, 1, -1, -1, -1),
    piece(b, Color, 1, -1, -1, -1),
    piece(b, Color, 1, -1, -1, -1),
    piece(s, Color, 1, -1, -1, -1),
    piece(s, Color, 1, -1, -1, -1),
    piece(m, Color, 1, -1, -1, -1),
    piece(l, Color, 1, -1, -1, -1),
    piece(p, Color, 1, -1, -1, -1)
  ].

% get all pieces on the game
all_pieces(Pieces) :-
  all_pieces(w, WPieces),
  all_pieces(b, BPieces),
  append(WPieces, BPieces, Pieces).

% get all pieces that are not in board
pieces_in_bag(Color, Pieces, Length) :-
  all_pieces(Color, AllPieces),
  get_pieces(Color, BoardPieces),
  findall(
    piece(Class, Color, Id, Q, R, S),
    (
      member(piece(Class, Color, Id, Q, R, S), AllPieces),
      \+ piece(Class, Color, Id, _, _, _)
    ),
    Pieces
  ),
  length(Pieces, Length).

% get all pieces that are not in board
pieces_in_bag(Pieces, Length) :-
  pieces_in_bag(w, WPieces, _),
  pieces_in_bag(b, BPieces, _),
  append(WPieces, BPieces, Pieces),
  length(Pieces, Length).

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

is_touching_hive(Q, R) :-
  piece_neighbours(Q, R, Neighbours),
  length(Neighbours, L),
  L > 0.

% check if (Q1, R1) has a neighbour in hive disctint from (Q2, R2)
is_touching_hive(Q1, R1, Q2, R2) :-
  piece_neighbours(Q1, R1, Neighbours),
  member(piece(_, _, _, Q, R, _), Neighbours),
  \+ (Q = Q2, R = R2).

% position has white neighbour
has_white_neighbour(position(Q, R, S)) :-
  piece_neighbours(Q, R, Neighbours),
  member(piece(_, Color, _, _, _, _), Neighbours),
  Color = w.

has_black_neighbour(position(Q, R, S)) :-
  piece_neighbours(Q, R, Neighbours),
  member(piece(_, Color, _, _, _, _), Neighbours),
  Color = b.

has_neighbour_with_color(Color, Pos) :-
  (Color = w, has_white_neighbour(Pos));
  (Color = b, has_black_neighbour(Pos)).

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

% get side positions of a piece
side_positions(piece(Class, Color, Id, Q, R, S), SidePositions) :-
  findall(Pos, get_side_position(position(Q, R, S), _, Pos), SidePositions).

% get game positions (filled & frontiers)
positions([], PositionsSoFar, Positions) :- PositionsSoFar = Positions.
positions([CurrentPiece | Pieces], PositionsSoFar, Positions) :-
  side_positions(CurrentPiece, SidePositions),
  append(PositionsSoFar, SidePositions, NPositionsSoFar),
  sort(NPositionsSoFar, SortedNPositionsSoFar), % remove duplicates
  positions(Pieces, SortedNPositionsSoFar, Positions).

% get game positions (no params)
positions(Positions, Len) :-
  get_pieces(Pieces),
  positions(Pieces, [], Positions),
  length(Positions, Len).
