:- module(utils, [
  step/2,
  get_player_turn/2,
  cell_neighbours/4
]).


% Get the 6 neighbours of a cell

cell_neighbours(Col, Diag, NCols, NDiags) :-
  UP is Diag - 1,
  DOWN is Diag + 1,
  LEFT is Col - 1,
  RIGHT is Col + 1,
  NCols = [Col, RIGHT, RIGHT, Col, LEFT, LEFT],
  NDiags = [UP, UP, Diag, DOWN, DOWN, Diag].

% Check if 2 cells are adjacent
adjacent(C1, D1, C2, D2) :-
  write("C1:"), write(C1),
  write("C2:"), write(C2),
  write("C3:"), write(C3),
  write("C4:"), write(C4),
  C1 = C2, D1 is D2 - 1; % up
  C1 = C2, D1 is D2 + 1; % down
  C1 is C2 - 1, D1 is D2 - 1; % diag right up
  C1 is C2 - 1, D1 is D2 + 1; % diag right down
  C1 is C2 + 1, D1 is D2 - 1; % diag left up
  C1 is C2 + 1, D1 is D2 + 1. % diag left down

% Piece methods

% Add a piece to local DB
add_piece(Piece) :- assert(Piece).

% Remove a piece from local DB
remove_piece(Piece) :- retract(Piece).

% Get DB pieces
get_pieces(Pieces) :-
  findall(
    piece(Class, Color, C, D, Stacked),
    piece(Class, Color, C, D, Stacked),
    Pieces
  ).

% Get the piece neighbours of a position
piece_neighbours(C, D, Neighbours) :-
  findall(
    piece(Class, Color, NC, ND, Stacked),
    adjacent(NC, ND, C, D),
    Neighbours
  ).

% Get the player turn (1 or 2) based on current Turn number
get_player_turn(Turn, Ans) :- Ans is 2 - mod(Turn, 2).

% Make a move based on MoveStr code & updates env
step(MoveStr, Color) :-
  sub_string(MoveStr, 0, 1, _, Color).


% Test methods
test() :-
  add_piece(piece('M', 'w', 0, 0, 0)),
  add_piece(piece('M', 'w', 0, 1, 0)),
  add_piece(piece('M', 'w', 1, 1, 0)),
  piece_neighbours(0, 1, Neighbours),
  write(Neighbours).

