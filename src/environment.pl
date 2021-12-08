% :- module(utils, [
%   step/2,
%   get_player_turn/2,
%   cell_neighbours/4
% ]).


% Get the 6 neighbours of a cell
cell_neighbours(R, D, NR, ND) :-
  UP is R - 1,
  DOWN is R + 1,
  LEFT is D - 1,
  RIGHT is D + 1,
  NR = [UP, UP, R, DOWN, DOWN, R],
  ND = [D, RIGHT, RIGHT, D, LEFT, LEFT].

% Check if 2 cells are adjacent
is_adjacent(R1, D1, R2, D2) :-
  R1 = R2, D1 is D2 - 1; % up
  R1 = R2, D1 is D2 + 1; % down
  R1 is R2 - 1, D1 = D2; % diag right up
  R1 is R2 - 1, D1 is D2 + 1; % diag right down
  R1 is R2 + 1, D1 is D2 - 1; % diag left up
  R1 is R2 + 1, D1 = D2. % diag left down

% Piece methods

% Add a piece to local DB
add_piece(Piece) :- assert(Piece).

% Remove a piece from local DB
remove_piece(Piece) :- retract(Piece).

% Get DB pieces
get_pieces(Pieces) :-
  findall(
    piece(Class, Color, Id, R, D, Stacked),
    piece(Class, Color, Id, R, D, Stacked),
    Pieces
  ).

% Get the piece neighbours of a position
piece_neighbours(R, D, Neighbours) :-
  findall(
    piece(Class, Color, Id, NR, ND, Stacked),
    (
      piece(Class, Color, Id, NR, ND, Stacked),
      is_adjacent(NR, ND, R, D)
    ),
    Neighbours
  ).

% Move a piece to a new position
move_piece(position(R, D, S), position(NR, ND, NS)) :-
  remove_piece(piece(Class, Color, Id, R, D, S)), !,
  add_piece(piece(Class, Color, Id, NR, ND, NS)).

% Get the player turn (1 or 2) based on current Turn number
get_player_turn(Turn, Ans) :- Ans is 2 - mod(Turn, 2).

get_piece_from_str(Str, Piece) :-
  string_chars(Str, [Color, Class, Id]),
  atom_number(Id, IId),
  piece(Class, Color, IId, R, D, S),
  Piece = piece(Class, Color, IId, R, D, S).

parse_action(ActionStr, Action) :-
  string_length(ActionStr, Length),
  Length = 3,
  get_piece_from_str(Str1, Piece1),
  add_piece(Piece1).
parse_action(ActionStr, Action) :-
  string_length(ActionStr, Length),
  Length = 8,
  sub_string(ActionStr, 0, 3, _, Str1),
  sub_string(ActionStr, _, 3, 0, Str2),
  sub_string(ActionStr, 3, 2, _, Side),

  get_piece_from_str(Str1, Piece1),
  get_piece_from_str(Str2, Piece2),

  Action = action(Piece1, Piece2, Side).

step(action(Piece1, Piece2, Side)) :-
  1 = 1.

% Test methods
test() :-
  add_piece(piece('b', 'w', 1, 0, 1, 0)),
  add_piece(piece('a', 'w', 1, 0, 0, 0)),
  add_piece(piece('m', 'b', 1, 1, 1, 0)),

  parse_action("wb1/*wa1", Action),
  % parse_action("wb1", Action),
  write(Action).
  % move_piece(position(0, 1, S), position(9, 9, 0)),
  % get_piece_from_str("wb1", piece(Class, Color, Id, C, D, S)),
  % get_pieces(N),
  % write(N),
  % write('\n'),
  % write(C),
  % write('\n'),
  % write(D),
  % write('\n'),
  % piece_neighbours(C, D, Ne),
  % write(Ne).
