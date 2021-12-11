:- module(pvp, [ pvp/0 ]).

:- use_module('./lib/string_methods', [
  write_list/1,
  write_line/1,
  write_lines/1
]).
:- use_module('./environment').
:- use_module('./ui').

pvp_loop(_) :-
  winner(W),
  W = 1,
  write_line("Player 1 wins"), !.
pvp_loop(_) :-
  winner(W),
  W = 2,
  write_line("Player 2 wins"), !.
pvp_loop(_) :-
  winner(W),
  W = 0,
  write_line("Is a tie"), !.
pvp_loop(Turn) :-
  turn_info(Turn),
  write_line('Type your move'),
  read_line_to_string(user_input, MoveStr),
  clear_screen(),
  write_list(['> You typed: ', MoveStr, '\n']),
  parse_action(MoveStr, Action),
  step(Action, Turn)
    -> (NextTurn is Turn + 1, pvp_loop(NextTurn))
    ; pvp_loop(Turn).

pvp() :- pvp_loop(1).
