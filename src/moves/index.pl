:- module(moves, [
  move_piece/2,
  move_grasshopper/2,
  move_queen/2,
  move_ant/2,
  move_beetle/2,
  move_spider/2,
  move_ladybug/2,
  move_mosquito/2,
  ant_can_move/2,
  piece_moves/2,
  add_moves/2,
  add_moves/1
]).
:- use_module('./src/lib/string_methods').
:- use_module('./src/moves/utils', [piece/6, position/3]).
:- use_module('./src/moves/utils').

% Move a piece to a new position or creat it if does not exist
move_piece(piece(Class, Color, Id, Q, R, S), position(NQ, NR, NS)) :-
(piece(Class, Color, Id, Q, R, S)
  -> remove_piece(piece(Class, Color, Id, Q, R, S)) ; true
),
add_piece(piece(Class, Color, Id, NQ, NR, NS)).

grasshopper_can_move(Start, Start, _).
grasshopper_can_move(position(Q1, R1, _), position(Q2, R2, _), position(DQ, DR, _)) :-
  position_filled(Q1, R1),
  NQ is Q1 + DQ,
  NR is R1 + DR,
  grasshopper_can_move(position(NQ, NR, _), position(Q2, R2, _), position(DQ, DR, _)).

grasshopper_can_move(piece(Class, Color, Id, Q, R, S), position(NQ, NR, NS)) :-
  NS = 0,
  \+ has_piece_over(position(Q, R, S)),
  \+ articulation_point(piece(Class, Color, Id, Q, R, S)),
  \+ is_adjacent(NQ, NR, Q, R),
  \+ position_filled(position(NQ, NR, NS)),
  is_adjacent(AdjQ, AdjR, Q, R),
  deltas(Q, R, AdjQ, AdjR, DQ, DR),
  grasshopper_can_move(position(Q, R, S), position(NQ, NR, NS), position(DQ, DR, 0)).

spider_can_move(piece(Class, Color, Id, Q, R, S), position(NQ, NR, NS)) :-
  NS = 0,
  \+ has_piece_over(position(Q, R, S)),
  \+ articulation_point(piece(Class, Color, Id, Q, R, S)),
  \+ position_filled(position(NQ, NR, NS)),
  is_touching_hive(NQ, NR, Q, R),
  is_adjacent(AdjQ, AdjR, Q, R),
  \+ position_filled(position(AdjQ, AdjR, 0)),
  is_touching_hive(AdjQ, AdjR, Q, R),
  is_adjacent(AdjQ2, AdjR2, AdjQ, AdjR),
  \+ position_filled(position(AdjQ2, AdjR2, 0)),
  is_touching_hive(AdjQ2, AdjR2, Q, R),
  \+ (AdjQ2 = Q, AdjR2 = R),
  \+ (NQ = AdjQ, NR = AdjR),
  is_adjacent(NQ, NR, AdjQ2, AdjR2).

ladybug_can_move(piece(Class, Color, Id, Q, R, S), position(NQ, NR, NS)) :-
  NS = 0,
  \+ has_piece_over(position(Q, R, S)),
  \+ articulation_point(piece(Class, Color, Id, Q, R, S)),
  \+ position_filled(position(NQ, NR, NS)),
  is_adjacent(AdjQ, AdjR, Q, R),
  position_filled(position(AdjQ, AdjR, 0)),
  is_adjacent(AdjQ2, AdjR2, AdjQ, AdjR),
  position_filled(position(AdjQ2, AdjR2, 0)),
  is_adjacent(NQ, NR, AdjQ2, AdjR2),
  \+ (AdjQ2 = Q, AdjR2 = R).

queen_can_move(piece(Class, Color, Id, Q, R, S), position(NQ, NR, NS)) :-
  NS = 0,
  \+ has_piece_over(position(Q, R, S)),
  \+ articulation_point(piece(Class, Color, Id, Q, R, S)),
  \+ position_filled(position(NQ, NR, NS)),
  is_touching_hive(NQ, NR, Q, R),
  is_adjacent(Q, R, NQ, NR).

ant_can_move(piece(Class, Color, Id, Q, R, S), position(NQ, NR, NS)) :-
  NS = 0,
  \+ has_piece_over(position(Q, R, S)),
  \+ articulation_point(piece(Class, Color, Id, Q, R, S)),
  \+ position_filled(position(NQ, NR, NS)),
  is_touching_hive(NQ, NR, Q, R).

beetle_can_move(piece(Class, Color, Id, Q, R, S), position(NQ, NR, NS)) :-
  \+ has_piece_over(position(Q, R, S)),
  \+ articulation_point(piece(Class, Color, Id, Q, R, S)),
  \+ position_filled(position(NQ, NR, NS)),
  is_touching_hive(NQ, NR, Q, R),
  is_adjacent(Q, R, NQ, NR).

mosquito_can_move(piece(Class, Color, Id, Q, R, S), position(NQ, NR, NS), C) :-
  (C = q, queen_can_move(piece(Class, Color, Id, Q, R, S), position(NQ, NR, NS)));
  (C = a, ant_can_move(piece(Class, Color, Id, Q, R, S), position(NQ, NR, NS)));
  (C = l, ladybug_can_move(piece(Class, Color, Id, Q, R, S), position(NQ, NR, NS)));
  (C = s, spider_can_move(piece(Class, Color, Id, Q, R, S), position(NQ, NR, NS)));
  (C = g, grasshopper_can_move(piece(Class, Color, Id, Q, R, S), position(NQ, NR, NS)));
  (C = b, beetle_can_move(piece(Class, Color, Id, Q, R, S), position(NQ, NR, NS))).

mosquito_can_move(piece(Class, Color, Id, Q, R, S), position(NQ, NR, NS)) :-
  piece_neighbours(Q, R, Neighbours),
  member(piece(NeighbourClass, _, _, _, _, _), Neighbours),
  mosquito_can_move(piece(Class, Color, Id, Q, R, S), position(NQ, NR, NS), NeighbourClass).

piece_can_move(piece(Class, Color, Id, Q, R, S), Position) :-
  (Class = q, queen_can_move(piece(Class, Color, Id, Q, R, S), Position));
  (Class = a, ant_can_move(piece(Class, Color, Id, Q, R, S), Position));
  (Class = l, ladybug_can_move(piece(Class, Color, Id, Q, R, S), Position));
  (Class = s, spider_can_move(piece(Class, Color, Id, Q, R, S), Position));
  (Class = g, grasshopper_can_move(piece(Class, Color, Id, Q, R, S), Position));
  (Class = b, beetle_can_move(piece(Class, Color, Id, Q, R, S), Position));
  (Class = m, mosquito_can_move(piece(Class, Color, Id, Q, R, S), Position)).

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

% list piece moves
piece_moves(Piece, Moves) :-
  positions(Positions, _),
  findall(
    Position,
    (
      member(Position, Positions),
      piece_can_move(Piece, Position)
    ),
    Ans
  ),
  sort(Ans, Moves).

oposite_color(Color, Oposite) :- Color = w, Oposite = b.
oposite_color(Color, Oposite) :- Color = b, Oposite = w.

% list add moves
add_moves(Color, Moves) :-
  positions(Positions, _),
  get_pieces(BoardPieces),
  length(BoardPieces, BoardLen),
  oposite_color(Color, Oposite),
  findall(
    position(Q, R, S),
    (
      member(position(Q, R, S), Positions),
      S = 0,
      \+ position_filled(position(Q, R, S)),
      (
        % only check neighbours when there is more than 1 piece in board
        (BoardLen > 1 -> \+ has_neighbour_with_color(Oposite, position(Q, R, S)) ; true)
      )
    ),
    Ans
  ),
  sort(Ans, Sorted),
  length(Sorted, Length),
  (
    (Length = 0 -> Moves = [position(0, 0, 0)]) ; Moves = Sorted
  ).

add_moves(Moves) :-
  add_moves(w, WMoves),
  add_moves(b, BMoves),
  append(WMoves, BMoves, Moves).
