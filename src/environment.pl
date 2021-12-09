:- module(environment, [
  step/1,
  parse_action/2,
  get_player_turn/2
]).
:- use_module('./lib/string_methods').
:- use_module('./moves/index').
:- use_module('./moves/utils', [piece/6]).
:- use_module('./moves/utils').

% Get the player turn (1 or 2) based on current Turn number
get_player_turn(Turn, Ans) :- Ans is 2 - mod(Turn, 2).

get_piece_from_str(Str, Piece) :-
  string_chars(Str, [Color, Class, Id]),
  atom_number(Id, IId),
  (
    (piece(Class, Color, IId, Q, R, S), Piece = piece(Class, Color, IId, Q, R, S));
    (\+ piece(Class, Color, IId, Q, R, S), Piece = piece(Class, Color, IId, -1, -1, -1))
  ).

parse_action(ActionStr, Action) :-
  string_length(ActionStr, Length),
  Length = 3,
  string_chars(ActionStr, [Color, Class, Id]),
  atom_number(Id, IId),
  Piece = piece(Class, Color, IId, 0, 0, 0),
  Action = action(Piece, Piece, "=*").
parse_action(ActionStr, Action) :-
  string_length(ActionStr, Length),
  Length > 3,
  split_string(ActionStr, " ", "", [Str1, Side, Str2]),
  get_piece_from_str(Str1, Piece1),
  get_piece_from_str(Str2, Piece2),
  Action = action(Piece1, Piece2, Side).

step(
  action(
    piece(Class1, Color1, Id1, Q1, R1, S1),
    piece(Class2, Color2, Id2, Q2, R2, S2),
    Side
  )
) :-
  Class1 = Class2, Color1 = Color2, Id1 = Id2,
  add_piece(piece(Class1, Color1, Id1, 0, 0, 0)), !.
step(
  action(
    piece(Class1, Color1, Id1, Q1, R1, S1),
    piece(Class2, Color2, Id2, Q2, R2, S2),
    Side
  )
) :-
  get_side_position(position(Q2, R2, S2), Side, position(NQ, NR, NS)),
  (Q1 = -1, R1 = -1, S1 = -1 ->
    (
      position_filled(NQ, NR)
      -> (write("The position is already occuppied.\n"), false)
      ; move_piece(piece(Class1, Color1, Id1, Q1, R1, S1), position(NQ, NR, NS))
    );
    (
      articulation_point(piece(Class1, Color1, Id1, Q1, R1, S1))
      -> (write("This move disconnect the hive.\n"), false)
      ; (
        move_queen(piece(Class1, Color1, Id1, Q1, R1, S1), position(NQ, NR, NS));
        move_ant(piece(Class1, Color1, Id1, Q1, R1, S1), position(NQ, NR, NS));
        move_beetle(piece(Class1, Color1, Id1, Q1, R1, S1), position(NQ, NR, NS));
        move_grasshopper(piece(Class1, Color1, Id1, Q1, R1, S1), position(NQ, NR, NS));
        move_spider(piece(Class1, Color1, Id1, Q1, R1, S1), position(NQ, NR, NS));
        move_ladybug(piece(Class1, Color1, Id1, Q1, R1, S1), position(NQ, NR, NS));
        move_mosquito(piece(Class1, Color1, Id1, Q1, R1, S1), position(NQ, NR, NS));
        (
          write("Invalid move.\n"),
          false
        )
      )
    )
  ).
