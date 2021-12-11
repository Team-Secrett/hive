:- module(pve, [pve/0]).

:- use_module('./lib/string_methods', [
  write_list/1,
  write_line/1,
  write_lines/1
]).
:- use_module('./environment', [
  parse_action/2,
  step/2,
  get_player_turn/2,
  get_pieces/1,
  winner/1
]).
:- use_module('./agent').
:- use_module('./ui').

turn_color(Turn, Color) :-
  get_player_turn(Turn, PTurn),
  (
    (PTurn = 1, Color = w);
    (PTurn = 2, Color = b)
  ).

handle_player_turn(PlayerColor, Turn) :-
  turn_info(Turn),
  write_line('Type your move'),
  read_line_to_string(user_input, MoveStr),
  clear_screen(),
  write_list(['> You typed: ', MoveStr, '\n']),
  parse_action(MoveStr, Action),
  step(Action, Turn)
    -> (NextTurn is Turn + 1, pve_loop(PlayerColor, NextTurn))
    ; pve_loop(PlayerColor, Turn).

handle_agent_turn(PlayerColor, Turn) :-
  write_line("Choosing action"),
  turn_color(Turn, Color),
  write_lines([Turn, Color]),
  choose_action(Color, Action, Turn),
  write_lines(["Agent choose", Action]),
  (step(Action, Turn)
    -> (NextTurn is Turn + 1, pve_loop(PlayerColor, NextTurn))
    ; (pve_loop(PlayerColor, NextTurn), write("FAILED"))
  ).
pve_loop(PlayerColor, Turn) :-
  winner(W),
  W = 1,
  write_line("Player 1 wins"), !.
pve_loop(PlayerColor, Turn) :-
  winner(W),
  W = 2,
  write_line("Player 2 wins"), !.
pve_loop(PlayerColor, Turn) :-
  winner(W),
  W = 0,
  write_line("Is a tie"), !.
pve_loop(PlayerColor, Turn) :-
  get_player_turn(Turn, PTurn),
  PTurn = 1,
  (PlayerColor = "w" ->
    handle_player_turn(PlayerColor, Turn);
    handle_agent_turn(PlayerColor, Turn)).
pve_loop(PlayerColor, Turn) :-
  get_player_turn(Turn, PTurn),
  PTurn = 2,
  (PlayerColor = "b" ->
    handle_player_turn(PlayerColor, Turn);
    handle_agent_turn(PlayerColor, Turn)).


pve() :-
  write_line("Which color do you want? (b/w)"),
  read_line_to_string(user_input, PlayerColor),
  clear_screen(),
  (PlayerColor \= "b", PlayerColor \= "w" -> pve() ; true),
  pve_loop(PlayerColor, 1).
