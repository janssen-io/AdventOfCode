#!/usr/bin/swipl -f -q

:- initialization(main, main).
:- use_module(library(clpfd)).
:- set_prolog_flag(double_quotes, chars).

main() :-
    part1("03-input.txt", Answer1),
    part2("03-input.txt", Answer2, _, _),
    write(Answer1),
    write('\n'),
    write(Answer2).

read_lines(Stream, []) :- at_end_of_stream(Stream).
read_lines(Stream, [X | L]) :-
    \+ at_end_of_stream(Stream),
    read_line_to_string(Stream, X),
    read_lines(Stream, L).

parse_all(File, LinesChars) :-
    read_lines(File, Lines),
    maplist(string_chars, Lines, LinesChars).

part1(File, Answer, G, E) :-
    open(File, read, Stream),
    parse_all(Stream, Lines),
    transpose(Lines, Transposed),
    maplist(mostCommon, Transposed, Gamma),
    maplist(leastCommon, Transposed, Epsilon),
    binToDec(Gamma, G),
    binToDec(Epsilon, E),
    Answer is G * E.

part2(File, Answer, O2, CO2) :-
    open(File, read, Stream),
    parse_all(Stream, Lines),
    filter(gamma, Lines, Oxygen),
    filter(epsilon, Lines, Carbon),
    binToDec(Oxygen, O2),
    binToDec(Carbon, CO2),
    Answer is O2 * CO2.

binToDec(Chars, Dec) :-
    length(Chars, L0),
    binToDec(Chars, 0, L0, Dec).

binToDec([], Acc, 0, Acc).
binToDec(['0'|Xs], Acc, L, Dec) :-
    L1 is L - 1,
    binToDec(Xs, Acc, L1, Dec).

binToDec(['1'|Xs], Acc, L, Dec) :-
    L1 is L - 1,
    Acc1 is Acc + (1 << (L - 1)),
    binToDec(Xs, Acc1, L1, Dec).

gamma(Transposed, Gamma) :- maplist(mostCommon, Transposed, Gamma).
epsilon(Transposed, Epsilon) :- maplist(leastCommon, Transposed, Epsilon).

common(Cmp, [], O, Z, '1') :- call(Cmp, O, Z).
common(Cmp, [], O, Z, '0') :- \+ call(Cmp, O, Z).

common(Cmp, ['0'|Xs], O, Z, Result) :-
    Z1 is Z + 1,
    common(Cmp, Xs, O, Z1, Result).

common(Cmp, ['1'|Xs], O, Z, Result) :-
    O1 is O + 1,
    common(Cmp, Xs, O1, Z, Result).

gte(A, B) :- A >= B.
lt(A, B) :- A < B.

leastCommon(Chars, R) :- common(lt, Chars, 0, 0, R).
mostCommon(Chars, R) :- common(gte, Chars, 0, 0, R).

at([X|_], 0, X).
at([_|Xs], Index, Element) :-
    Index1 is Index - 1,
    at(Xs, Index1, Element).

filterPos(_, _, [], []).
filterPos(Position, Check, [Xs|Xss], [Xs|Result]) :-
    at(Check, Position, Bit),
    at(Xs, Position, Bit),
    filterPos(Position, Check, Xss, Result), !.

filterPos(Position, Check, [Xs|Xss], Result) :-
    at(Check, Position, Bit),
    \+ at(Xs, Position, Bit),
    filterPos(Position, Check, Xss, Result), !.

filter(Type, Xss, Result) :-
    transpose(Xss, Transposed),
    call(Type, Transposed, Rate),
    filter(Type, Rate, Xss, 0, Result), !.

filter(_, _, [Result], _, Result).
filter(Type, Check, Xss, Pos, Result) :-
    filterPos(Pos, Check, Xss, Filtered),
    transpose(Filtered, Transposed),
    call(Type, Transposed, Rate),
    Pos1 is Pos + 1,
    filter(Type, Rate, Filtered, Pos1, Result), !.
