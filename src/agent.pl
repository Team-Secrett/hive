:- module(agent, [choose_action/3]).
:- use_module('./lib/string_methods').
:- use_module('./moves/index').
:- use_module('./moves/utils').
:- use_module('./environment').

pieces_around_queen(Color, Amount) :-
  Color = w,
  get_piece_from_str("wq1", piece(_, _, _, Q, R, S)),
  (
    (S = -1, Amount = 0, !);
    (
      piece_neighbours(Q, R, Neighbours),
      length(Neighbours, Length),
      Amount = Length
    )
  ).
pieces_around_queen(Color, Amount) :-
  Color = b,
  get_piece_from_str("bq1", piece(_, _, _, Q, R, S)),
  (
    (S = -1, Amount = 0, !);
    (
      piece_neighbours(Q, R, Neighbours),
      length(Neighbours, Length),
      Amount = Length
    )
  ).


% calculate the board value based on a heuristic function
% - More pieces is better
% - More free pieces is better
% - Number of pieces around the queen both offensive and defensive
board_value(Color, Value) :-
  winner(W),
  W = 1,
  (
    (Color = w, Value = 500),
    (Color = b, Value = -500)
  ), !.
board_value(Color, Value) :-
  winner(W),
  W = 2,
  (
    (Color = w, Value = -500),
    (Color = b, Value = 500)
  ), !.
board_value(Color, Value) :-
  winner(W),
  W = 0,
  Value = 250.
board_value(Color, Value) :-
  oposite_color(Color, Oposite),
  % free pieces
  free_pieces(Color, PlayerFreePieces),
  length(PlayerFreePieces, LengthPlayerFreePieces),
  free_pieces(Oposite, OponentFreePieces),
  length(OponentFreePieces, LengthOponentFreePieces),

  % pieces on board
  get_pieces(Color, PlayerPiecesOnBoard),
  length(PlayerPiecesOnBoard, LengthPlayerPiecesOnBoard),
  get_pieces(Oposite, OponentPiecesOnBoard),
  length(OponentPiecesOnBoard, LengthOponentPiecesOnBoard),

  % pieces surrounding oponent queen
  pieces_around_queen(Color, PlayerPiecesAroundQueen),
  pieces_around_queen(Oposite, OponentPiecesAroundQueen),


  Value is (
    10 * (OponentPiecesAroundQueen - PlayerPiecesAroundQueen)
    + 2 * (LengthPlayerFreePieces - LengthOponentFreePieces)
    + (LengthPlayerPiecesOnBoard - LengthOponentPiecesOnBoard)
  ).

% Get the board value after execute an action in the current state
board_value_after_action(Color, Turn, Action, Value) :-
  % write_lines([
  %   "Trying action",
  %   Action
  % ]),
  step(Action, Turn),
  board_value(Color, Value),
  undo_action(Action, Turn).

all_actions(Color, Actions) :-
  move_actions(Color, MoveActions),
  add_actions(Color, AddActions),
  append(MoveActions, AddActions, Actions).

best_action(_, _, [], BestValueSoFar, BestActionSoFar, BestValue, BestAction) :-
  BestValue = BestValueSoFar,
  BestAction = BestActionSoFar, !.

best_action(Color, Turn, [CurrentAction | Actions], BestValueSoFar, BestActionSoFar, BestValue, BestAction) :-
  board_value_after_action(Color, Turn, CurrentAction, Value),
  (
    (Value > BestValueSoFar, best_action(Color, Turn, Actions, Value, CurrentAction, BestValue, BestAction));
    (Value =< BestValueSoFar, best_action(Color, Turn, Actions, BestValueSoFar, BestActionSoFar, BestValue, BestAction))
  ).

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
  all_actions(Color, Actions),
  best_action(Color, Turn, Actions, -999999, 0, BestValue, BestAction),
  Action = BestAction.
