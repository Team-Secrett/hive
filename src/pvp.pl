:- module(pvp, [ pvp/0 ]).

:- use_module('./lib/string_methods', [
  write_list/1,
  write_line/1,
  write_lines/1
]).
:- use_module('./environment', [
  parse_action/2,
  step/1,
  get_player_turn/2,
  get_pieces/1
]).


turn_info(Turn) :-
  get_player_turn(Turn, PlayerTurn),
  write_list(['Turn: ', Turn, '\n']),
  write_list(['Player ', PlayerTurn, '\n']),
  get_pieces(Pieces),
  write_line(Pieces).

clear_screen() :- write('\33\[2J').

pvp_loop(Turn) :-
  clear_screen(),
  turn_info(Turn),
  write_line('Type your move'),
  read_line_to_string(user_input, MoveStr),
  write_list(['> You typed: ', MoveStr, '\n']),
  parse_action(MoveStr, Action),
  write_list(["Action: ", Action, '\n']),
  (step(Action) ; true),
  NextTurn is Turn + 1,
  pvp_loop(NextTurn).

pvp() :- pvp_loop(1).
