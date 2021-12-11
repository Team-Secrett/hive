:- module(ui, [
  welcome_message/0,
  turn_info/1,
  clear_screen/0
]).
:- use_module('./environment').
:- use_module('./moves/index').
:- use_module('./moves/utils').
:- use_module('./lib/string_methods.pl').


% Print welcome message
welcome_message() :-
  write_list(['Welcome to Hive Game', '\nUser']).

turn_info(Turn) :-
  get_player_turn(Turn, PlayerTurn),
  write_list(['Turn: ', Turn, '\n']),
  write_list(['Player ', PlayerTurn, '\n']),
  get_pieces(Pieces),
  write_line(Pieces).

clear_screen() :- write('\33\[2J').
