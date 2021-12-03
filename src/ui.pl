:- module(ui, [
  welcome_message/0
]).

:- use_module('./lib/string_methods.pl', [write_list/1]).


% Print welcome message
welcome_message() :-
  write_list(['Welcome to Hive Game', '\nUser']).

