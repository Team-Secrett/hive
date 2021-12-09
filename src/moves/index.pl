:- module(moves, [
  move_piece/2,
  move_grasshopper/2,
  move_queen/2,
  move_ant/2,
  move_beetle/2,
  move_spider/2,
  move_ladybug/2,
  move_mosquito/2
]).
:- use_module([utils], [piece/6]).
:- use_module([utils]).

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
