#!/usr/bin/swipl -f -q

:- initialization(main, main).
:- set_prolog_flag(double_quotes, chars).
:- use_module(library(dcg/basics), [integer/3]).

main() :-
    part1("02-input.txt", Answer1),
    part2("02-input.txt", Answer2),
    write(Answer1),
    write('\n'),
    write(Answer2).

read_lines(Stream, []) :- at_end_of_stream(Stream).
read_lines(Stream, [X | L]) :-
    \+ at_end_of_stream(Stream),
    read_line_to_string(Stream, X),
    read_lines(Stream, L).

command(forward, Amount) -->
    "forward ",
    integer(Amount).

command(down, Amount) -->
    "down ",
    integer(Amount).

command(up, Amount) -->
    "up ",
    integer(Amount).

parse(String, Direction - Amount) :-
    string(String), !,
    string_chars(String, Chars),
    phrase(command(Direction, Amount), Chars).

parse_all(File, Commands) :-
    read_lines(File, Lines),
    maplist(parse, Lines, Commands).

navigate1([], 0, 0).

navigate1([forward - Amount|Commands], H, D) :-
    navigate1(Commands, H1, D),
    H is H1 + Amount.

navigate1([down - Amount|Commands], H, D) :-
    navigate1(Commands, H, D1),
    D is D1 + Amount.

navigate1([up - Amount | Commands], H, D) :-
    NegAmount is Amount * -1,
    navigate1([down-NegAmount | Commands], H, D).

part1(File, Answer) :-
    open(File, read, Stream),
    parse_all(Stream, Commands),
    reverse(C1, Commands),
    navigate1(C1, H, D),
    Answer is H * D.

navigate2([], 0, 0, _).

navigate2([down - Amount | Commands], H, D, Aim) :-
    Aim1 is Aim + Amount,
    navigate2(Commands, H, D, Aim1).

navigate2([up - Amount | Commands], H, D, Aim) :-
    Aim1 is Aim - Amount,
    navigate2(Commands, H, D, Aim1).

navigate2([forward - Amount | Commands], H, D, Aim) :-
    navigate2(Commands, H1, D1, Aim),
    H is H1 + Amount,
    D is D1 + (Aim * Amount).

part2(File, Answer) :-
    open(File, read, Stream),
    parse_all(Stream, Commands),
    navigate2(Commands, H, D, 0),
    Answer is H * D.