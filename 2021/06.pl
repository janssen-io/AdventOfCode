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
    read_lines(Stream, Lines),
    [Line] = Lines,
    group_fish(Line, Fishes),
    step(80, Fishes, NewFishes),
    sum(NewFishes, 0, Answer).

part2(File, Answer) :-
    open(File, read, Stream),
    read_lines(Stream, Lines),
    [Line] = Lines,
    group_fish(Line, Fishes),
    step(256, Fishes, NewFishes),
    sum(NewFishes, 0, Answer).

example(Answer) :-
    [Line] = ["3,4,3,1,2"],
    group_fish(Line, Fishes),
    step(80, Fishes, NewFishes),
    sum(NewFishes, 0, Answer).

group_fish(Line, Fishes) :-
    split_string(Line, ",", "", FishChars),
    maplist(atom_number, FishChars, FishNums),
    group(FishNums, [0,0,0,0,0,0,0,0,0], Fishes).

count(Index, Counter, Result) :- count(Index, 1, Counter, Result).

count(0, N, [C|Counter], [C1|Counter]) :-
    C1 is C + N.

count(Index, N, [C|Counter], [C|Result]) :-
    Index > 0,
    I1 is Index - 1,
    count(I1, N, Counter, Result).

group([], Acc, Acc).

group([Fish|Fs], Acc, Fishes) :-
    count(Fish, Acc, Acc1),
    group(Fs, Acc1, Fishes).

step(0, Fishes, Fishes).
step(Times, [F|Fishes], NewFishes) :-
    % format("~p~n", Times),
    T1 is Times - 1,
    count(6, F, Fishes, Fs1),
    append(Fs1, [F], Fs2),
    step(T1, Fs2, NewFishes).

sum([], Acc, Acc).
sum([X|Xs], Acc, Result) :-
    Acc1 is Acc + X,
    sum(Xs, Acc1, Result).