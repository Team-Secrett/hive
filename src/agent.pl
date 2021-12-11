:- module(agent, [choose_action/3]).
:- use_module('./lib/string_methods').
:- use_module('./moves/index').
:- use_module('./moves/utils').
:- use_module('./environment').

% Move queen if is not been moved already
choose_action(Color, Action, Turn) :-
  (Turn = 7 ; Turn = 8),
  pieces_in_bag(Color, Pieces, _),
  member(piece(q, Color, Id, -1, -1, -1), Pieces),
  add_moves(Color, Moves),
  random_member(position(PQ, PR, PS), Moves),
  piece_neighbours(PQ, PR, Neighbours),
  member(piece(NClass, NColor, NId, NQ, NR, NS), Neighbours),
  \+ (NClass = q, NColor = Color, NId = Id),
  get_side_position(position(NQ, NR, NS), Side, position(PQ, PR, PS)),
  Action = action(piece(q, Color, Id, -1, -1, -1), piece(NClass, NColor, NId, NQ, NR, NS), Side), !.

choose_action(Color, Action, Turn) :-
  move_actions(Color, MoveActions),
  add_actions(Color, AddActions),
  append(MoveActions, AddActions, Actions),
  random_member(Action, Actions), !.
