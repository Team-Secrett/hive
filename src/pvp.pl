:- module(pvp, [ pvp/0 ]).

:- use_module('./environment', [
  step/2,
  get_player_turn/2
]).


turn_info(Turn) :-
  get_player_turn(Turn, PlayerTurn),
  write_list(['Turn: ', Turn, '\n']),
  write_list(['Player ', PlayerTurn, '\n']).

pvp_loop(Turn) :-
  turn_info(Turn),
  write_line('Type your move'),
  read(MoveStr),
  write_list(['> You typed: ', MoveStr, '\n']),
  step(MoveStr),

  NextTurn is Turn + 1,
  pvp_loop(NextTurn).

pvp() :- pvp_loop(1).

