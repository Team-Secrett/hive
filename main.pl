:- use_module('./src/lib/string_methods', [
  write_list/1,
  write_line/1,
  write_lines/1
]).
:- use_module('./src/pvp', [pvp]).
:- use_module('./src/pve', [pve]).

get_player_turn(Turn, Ans) :- Ans is 2 - mod(Turn, 2).

handle_option(Option) :-
  Option =:= "1",
  write_line('Option 1'),
  pve().
handle_option(Option) :-
  Option =:= "2",
  write_line('Option 2'),
  pvp().

main() :-
  write_lines([
    'Welcome to Hive Game!',
    'Please select an option:',
    '1: Play against computer',
    '2: Play against player'
  ]),
  write('> '),
  read_line_to_string(user_input, Option),
  handle_option(Option).
