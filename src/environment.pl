:- module(utils, [
  step/2,
  get_player_turn/2
]).

% Get the player turn (1 or 2) based on current Turn number
get_player_turn(Turn, Ans) :- Ans is 2 - mod(Turn, 2).

% Make a move based on MoveStr code & updates env
step(MoveStr, Color) :-
  sub_string(MoveStr, 0, 1, _, Color).

