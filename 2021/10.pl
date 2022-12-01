#!/usr/bin/swipl -f -q

:- initialization(main, main).
:- use_module(library(clpfd)).
:- set_prolog_flag(double_quotes, chars).

main() :-
    part1("06-input.txt", Answer1),
    part2("06-input.txt", Answer2),
    format("~p~n", Answer1),
    format("~p~n", Answer2).

read_lines(Stream, []) :- at_end_of_stream(Stream).
read_lines(Stream, [X | L]) :-
    \+ at_end_of_stream(Stream),
    read_line_to_string(Stream, X),
    read_lines(Stream, L).

part1(File, Answer) :-
    open(File, read, Stream),
    read_lines(Stream, Lines).

part2(File, Answer) :-
    open(File, read, Stream),
    read_lines(Stream, Lines).

example(Answer) :-
    [Line] = ["((<>)"],
    group_fish(Line, Fishes),
    step(80, Fishes, NewFishes),
    sum(NewFishes, 0, Answer).

pair('(', ')').
pair('[', ']').
pair('{', '}').
pair('<', '>').

illegal([C|_], [C], [O|_]) :-
    pair(O1, C),
    O \== O1.

illegal([C|_], [C], []) :-
    pair(_, C).

illegal([C|Xs], Illegal, [O|Opens]) :-
    pair(O, C),
    illegal(Xs, Illegal, Opens).

illegal([O|Xs], Illegal, Opens) :-
    pair(O, _),
    illegal(Xs, Illegal, [O|Opens]).