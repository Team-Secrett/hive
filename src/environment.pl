:- module(environment, [
  step/2,
  undo_action/2,
  parse_action/2,
  get_player_turn/2,
  winner/1,
  move_actions/1,
  move_actions/2,
  add_actions/2,
  get_piece_from_str/2
]).
:- use_module('./lib/string_methods').
:- use_module('./moves/index').
:- use_module('./moves/utils', [piece/6, action/3]).
:- use_module('./moves/utils').

% Get the player turn (1 or 2) based on current Turn number
get_player_turn(Turn, Ans) :- Ans is 2 - mod(Turn, 2).

get_piece_from_str(Str, Piece) :-
  string_chars(Str, [Color, Class, Id]),
  atom_number(Id, IId),
  (
    (piece(Class, Color, IId, Q, R, S), Piece = piece(Class, Color, IId, Q, R, S));
    (\+ piece(Class, Color, IId, _, _, _), Piece = piece(Class, Color, IId, -1, -1, -1))
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

can_move(piece(Class, Color, Id, Q, R, S)) :-
  \+ (
    articulation_point(piece(Class, Color, Id, Q, R, S)),
    write("This move disconnect the hive.\n")
  ),
  \+ (
    has_piece_over(position(Q, R, S)),
    write("Cannot move, has a piece over.\n")
  ).

can_add(piece(Class, Color, Id, Q, R, S), position(NQ, NR, NS), Turn) :-
  get_player_turn(Turn, PlayerTurn),
  \+ (
    \+ valid_piece(piece(Class, Color, Id, Q, R, S)),
    write("Invalid piece.\n")
  ),
  \+ (
    PlayerTurn = 1, Color = b,
    write("You should move a white piece.\n")
  ),
  \+ (
    PlayerTurn = 2, Color = w,
    write("You should move a black piece.\n")
  ),
  \+ (
    position_filled(NQ, NR),
    write("The position is already occuppied.\n")
  ),
  \+ (
    (Turn > 2, Color = b, has_white_neighbour(position(NQ, NR, NS))),
    write("New position touch white piece.\n")
  ),
  \+ (
    (Turn > 2, Color = w, has_black_neighbour(position(NQ, NR, NS))),
    write("New position touch black piece.\n")
  ).

positions_to_actions(_, [], ActionsSoFar, Actions) :- Actions = ActionsSoFar, !.
positions_to_actions(piece(Class, Color, Id, Q, R, S), [position(PQ, PR, _) | _], _, Actions) :-
  piece_neighbours(PQ, PR, Neighbours),
  length(Neighbours, Len),
  Len = 0,
  Actions = [action(
    piece(Class, Color, Id, Q, R, S),
    piece(Class, Color, Id, Q, R, S),
    _
  )], !.
positions_to_actions(piece(Class, Color, Id, Q, R, S), [position(PQ, PR, PS) | Positions], ActionsSoFar, Actions) :-
  is_adjacent(Q, R, PQ, PR),
  piece_down(position(PQ, PR, PS), PieceDown),
  append(
    ActionsSoFar,
    [action(
      piece(Class, Color, Id, Q, R, S),
      PieceDown,
      "O"
    )],
    NActions
  ),
  positions_to_actions(piece(Class, Color, Id, Q, R, S), Positions, NActions, Actions), !.
positions_to_actions(piece(Class, Color, Id, Q, R, S), [position(PQ, PR, PS) | Positions], ActionsSoFar, Actions) :-
  piece_neighbours(PQ, PR, Neighbours),
  member(piece(NClass, NColor, NId, NQ, NR, NS), Neighbours),
  \+ (NClass = Class, NColor = Color, NId = Id),
  get_side_position(position(NQ, NR, NS), Side, position(PQ, PR, PS)),
  append(
    ActionsSoFar,
    [action(
      piece(Class, Color, Id, Q, R, S),
      piece(NClass, NColor, NId, NQ, NR, NS),
      Side
    )],
    NActions
  ),
  positions_to_actions(piece(Class, Color, Id, Q, R, S), Positions, NActions, Actions).

build_move_actions([], ActionsSoFar, Actions) :-
  Actions = ActionsSoFar, !.
build_move_actions([CurrentPiece | Pieces], ActionsSoFar, Actions) :-
  piece_moves(CurrentPiece, Moves),
  positions_to_actions(CurrentPiece, Moves, [], CurrentActions),
  append(ActionsSoFar, CurrentActions, NActions),
  build_move_actions(Pieces, NActions, Actions).

% Get available move actions by a color
move_actions(Color, Actions) :-
  get_pieces(Color, Pieces),
  build_move_actions(Pieces, [], Actions).

% Get available move actions
move_actions(Actions) :-
  move_actions(w, WhiteActions),
  move_actions(b, BlackActions),
  append(WhiteActions, BlackActions, Actions).

build_add_actions([], ActionsSoFar, Actions) :-
  Actions = ActionsSoFar, !.
build_add_actions([piece(Class, Color, Id, Q, R, S) | Pieces], ActionsSoFar, Actions) :-
  add_moves(Color, Moves),
  positions_to_actions(piece(Class, Color, Id, Q, R, S), Moves, [], CurrentActions),
  append(ActionsSoFar, CurrentActions, NActions),
  build_add_actions(Pieces, NActions, Actions).

% Get available add actions
add_actions(Color, Actions) :-
  pieces_in_bag(Color, Pieces, _),
  build_add_actions(Pieces, [], Actions).

% -1 -> No winner yet, 0 -> Tie, elsewhere player [1 | 2] wins
winner(Winner) :-
  get_piece_from_str("wq1", piece(_, _, _, Q1, R1, _)),
  get_piece_from_str("bq1", piece(_, _, _, Q2, R2, _)),
  (
    (
      is_surrounded(Q1, R1),
      is_surrounded(Q2, R2),
      Winner = 0, !
    );
    (
      is_surrounded(Q1, R1),
      \+ is_surrounded(Q2, R2),
      Winner = 2, !
    );
    (
      is_surrounded(Q2, R2),
      \+ is_surrounded(Q1, R1),
      Winner = 1, !
    );
    Winner = -1
  ).

step(
  action(
    piece(Class1, Color1, Id1, Q1, R1, S1),
    piece(Class2, Color2, Id2, _, _, _),
    _
  ),
  Turn
) :-
  Class1 = Class2, Color1 = Color2, Id1 = Id2,
  (Turn > 1 -> write("Invalid Action.\n"), false, ! ; true),
  can_add(piece(Class1, Color1, Id1, Q1, R1, S1), position(0, 0, 0), Turn),
  add_piece(piece(Class1, Color1, Id1, 0, 0, 0)), !.
step(
  action(
    piece(Class1, Color1, Id1, Q1, R1, S1),
    piece(_, _, _, Q2, R2, S2),
    Side
  ),
  Turn
) :-
  (S2 = -1 -> write("Destination move is not in board.\n"), !, false ; true),
  get_side_position(position(Q2, R2, S2), Side, position(NQ, NR, NS)),
  (S1 = -1 ->
    (
      can_add(piece(Class1, Color1, Id1, Q1, R1, S1), position(NQ, NR, NS), Turn),
      move_piece(piece(Class1, Color1, Id1, Q1, R1, S1), position(NQ, NR, NS))
    );
    (
      can_move(piece(Class1, Color1, Id1, Q1, R1, S1)),
      (
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

% Undo an action (only use with last action)
undo_action(
  action(
    piece(Class1, Color1, Id1, Q1, R1, S1),
    piece(Class2, Color2, Id2, _, _, _),
    _
  ),
  Turn
) :-
  Turn = 1,
  Class1 = Class2, Color1 = Color2, Id1 = Id2,
  remove_piece(piece(Class1, Color1, Id1, 0, 0, 0)).
undo_action(
  action(
    piece(Class1, Color1, Id1, Q1, R1, S1),
    piece(_, _, _, Q2, R2, S2),
    Side
  ),
  Turn
) :-
  get_side_position(position(Q2, R2, S2), Side, position(NQ, NR, NS)),
  (S1 = -1 ->
    (
      remove_piece(piece(Class1, Color1, Id1, NQ, NR, NS))
    );
    (
      move_piece(piece(Class1, Color1, Id1, NQ, NR, NS), position(Q1, R1, S1))
    )
  ).
