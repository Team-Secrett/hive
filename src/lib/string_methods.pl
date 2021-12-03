:- module(string_methods, [
  write_list/1,
  write_line/1,
  write_lines/1
]).

% Print a list of strings
write_list([]).
write_list([Head | Tail]) :- write(Head), write_list(Tail).

% Print a line and a \n after it
write_line(Line) :- write(Line), write('\n').

% Print a list of string separated by \n
write_lines([]) :- write_line('').
write_lines([Head | Tail]) :- write_line(Head), write_lines(Tail).

