% :- module(utils, [
%   step/2,
%   get_player_turn/2,
%   cell_neighbours/4
% ]).


% Get the 6 neighbours of a cell
cell_neighbours(Col, Diag, NCols, NDiags) :-
  UP is Diag - 1,
  DOWN is Diag + 1,
  LEFT is Col - 1,
  RIGHT is Col + 1,
  NCols = [Col, RIGHT, RIGHT, Col, LEFT, LEFT],
  NDiags = [UP, UP, Diag, DOWN, DOWN, Diag].

% Check if 2 cells are adjacent
is_adjacent(C1, D1, C2, D2) :-
  C1 = C2, D1 is D2 - 1; % up
  C1 = C2, D1 is D2 + 1; % down
  C1 is C2 - 1, D1 = D2; % diag right up
  C1 is C2 - 1, D1 is D2 + 1; % diag right down
  C1 is C2 + 1, D1 is D2 - 1; % diag left up
  C1 is C2 + 1, D1 = D2. % diag left down

% Piece methods

% Add a piece to local DB
add_piece(Piece) :- assert(Piece).

% Remove a piece from local DB
remove_piece(Piece) :- retract(Piece).

% Get DB pieces
get_pieces(Pieces) :-
  findall(
    piece(Class, Color, Id, C, D, Stacked),
    piece(Class, Color, Id, C, D, Stacked),
    Pieces
  ).

% Get the piece neighbours of a position
piece_neighbours(C, D, Neighbours) :-
  findall(
    piece(Class, Color, Id, NC, ND, Stacked),
    (
      piece(Class, Color, Id, NC, ND, Stacked),
      is_adjacent(NC, ND, C, D)
    ),
    Neighbours
  ).

% Move a piece to a new position
move_piece(position(C, D, S), position(NC, ND, NS)) :-
  remove_piece(piece(Class, Color, Id, C, D, S)), !,
  add_piece(piece(Class, Color, Id, NC, ND, NS)).


% Get the player turn (1 or 2) based on current Turn number
get_player_turn(Turn, Ans) :- Ans is 2 - mod(Turn, 2).

get_piece_from_str(Str, Piece) :-
  string_chars(Str, [Color, Class, Id]),
  atom_number(Id, IId),
  piece(Class, Color, IId, C, D, S),
  Piece = piece(Class, Color, IId, C, D, S).

parse_action(ActionStr, Action) :-
  sub_string(ActionStr, 0, 3, _, Str1),
  sub_string(ActionStr, _, 3, 0, Str2),
  sub_string(ActionStr, 3, 2, _, Side),

  get_piece_from_str(Str1, Piece1),
  get_piece_from_str(Str2, Piece2),

  Action = action(Piece1, Piece2, Side).

% Test methods
test() :-
  add_piece(piece('b', 'w', 1, 0, 1, 0)),
  add_piece(piece('a', 'w', 1, 0, 0, 0)),
  add_piece(piece('m', 'b', 1, 1, 1, 0)),
  parse_action("wb1/*wa1", Action),
  write(Action).
  % get_piece_from_str("wb1", piece(Class, Color, Id, C, D, S)),
  % move_piece(position(C, D, S), position(9, 9, 0)),
  % get_pieces(N),
  % write(N),
  % write('\n'),
  % piece_neighbours(C, D, Ne),
  % write(Ne).

