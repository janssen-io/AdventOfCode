#!/usr/bin/swipl -f -q

:- initialization(main, main).
:- use_module(library(clpfd)).
:- set_prolog_flag(double_quotes, chars).

main() :-
    part1("04-input.txt", Answer1, _, _),
    part2("04-input.txt", Answer2, _, _),
    write(Answer1),
    write('\n'),
    write(Answer2).

read_lines(Stream, []) :- at_end_of_stream(Stream).
read_lines(Stream, [X | L]) :-
    \+ at_end_of_stream(Stream),
    read_line_to_string(Stream, X),
    read_lines(Stream, L).

part1(File, Answer, Sum, LastNumber) :-
    open(File, read, Stream),
    read_lines(Stream, Lines),
    read_chosen_numbers(Lines, Numbers, Rest),
    read_boards(Rest, [], Boards),
    length(Boards, L), print(L),
    % print(Boards),
    play_bingo(Boards, Numbers, Winner, LastNumber),
    sum_board(Winner, 0, Sum),
    Answer is Sum * LastNumber, !.

part2(File, Answer, Sum, LastNumber) :-
    open(File, read, Stream),
    read_lines(Stream, Lines),
    read_chosen_numbers(Lines, Numbers, Rest),
    read_boards(Rest, [], Boards),
    length(Boards, L), print(L),
    % print(Boards),
    play_bingo_all(Boards, Numbers, Winner, LastNumber),
    sum_board(Winner, 0, Sum),
    Answer is Sum * LastNumber, !.

read_chosen_numbers([Line,_|Rest], Numbers, Rest) :-
    split_string(Line, ",", "", NumChars),
    maplist(atom_number, NumChars, Numbers).

remove_empty([], []).
remove_empty([X|Xs], Acc) :-
    string_length(X, L), L = 0,
    remove_empty(Xs, Acc).

remove_empty([X|Xs], [X|Acc]) :-
    string_length(X, L), L > 0,
    remove_empty(Xs, Acc).

read_boards([], Board, [Board]).
read_boards([Line|Lines], Board, [Board|Boards]) :-
    string_length(Line, L), L = 0,
    read_boards(Lines, [], Boards), !.

read_boards([Line|Lines], Board, Boards) :-
    dif(Line, []),
    split_string(Line, " ", "", NC),
    remove_empty(NC, NumChars),
    maplist(atom_number, NumChars, Numbers),
    append(Board, Numbers, Board1),
    read_boards(Lines, Board1, Boards), !.

at([X|_], 0, X).
at([_|Xs], Index, Element) :-
    Index1 is Index - 1,
    at(Xs, Index1, Element).

cell(Board, Row, Column, Cell) :-
    Index is 5 * Row + Column,
    at(Board, Index, Cell), !.

cell_inv(Board, Column, Row, Cell) :-
    Index is 5 * Row + Column,
    at(Board, Index, Cell), !.

row(Board, Index, Row) :-
    Columns = [0,1,2,3,4],
    maplist(cell(Board, Index), Columns, Row).

column(Board, Index, Column) :-
    Rows = [0,1,2,3,4],
    maplist(cell_inv(Board, Index), Rows, Column).

replace_i(_, _, [], []).
replace_i(0, R, [_|Xs], [R|Xs]).
replace_i(Index, R, [X|Xs], [X|Ys]) :-
    Index > 0,
    Index1 is Index - 1,
    replace_i(Index1, R, Xs, Ys).

replace(_, _, [], []).
replace(O, R, [O|Xs], [R|Ys]) :- replace(O, R, Xs, Ys).
replace(O, R, [X|Xs], [X|Ys]) :-
    dif(O, X),
    replace(O, R, Xs, Ys).

bingo_single([]).
bingo_single([x|Xs]) :- bingo_single(Xs).

bingo_n(Board, N, _) :-    row(Board, N, R), bingo_single(R).
bingo_n(Board, N, _) :- column(Board, N, C), bingo_single(C).

bingo(Board, Board) :-
    convlist(bingo_n(Board), [0,1,2,3,4], Output),
    dif(Output, []).

play_bingo(Boards, [X|_], Winner, X) :-
    maplist(replace(X, x), Boards, NewBoards),
    convlist(bingo, NewBoards, BoardsWithBingo),
    dif(BoardsWithBingo, []),
    BoardsWithBingo = [Winner|_], !.
    % print_board(Winner, _)

play_bingo(Boards, [X|Numbers], Winner, LastNumber) :-
    maplist(replace(X, x), Boards, NewBoards),
    convlist(bingo, NewBoards, BoardsWithBingo),
    BoardsWithBingo = [],
    % maplist(print_board, NewBoards, _), write('\n'),
    play_bingo(NewBoards, Numbers, Winner, LastNumber), !.

play_bingo_all([Board], [X|_], Winner, X) :-
    replace(X, x, Board, NewBoard),
    bingo(NewBoard, _),
    NewBoard = Winner, !.

play_bingo_all(Boards, [X|Numbers], Winner, LastNumber) :-
    dif(Boards, []),
    maplist(replace(X, x), Boards, NewBoards),
    convlist(bingo, NewBoards, BoardsWithBingo),
    length(NewBoards, LB),
    length(BoardsWithBingo, LBB),
    subtract(NewBoards, BoardsWithBingo, Rest),
    length(Rest, LR),
    Before is LB - LBB,
    Before = LR,
    play_bingo_all(Rest, Numbers, Winner, LastNumber).

sum_board([], Acc, Acc).
sum_board([x|Board], Acc, Sum) :- sum_board(Board, Acc, Sum).
sum_board([Num|Board], Acc, Sum) :-
    Acc1 is Acc + Num,
    sum_board(Board, Acc1, Sum).

print_board(Board, _) :-
    row(Board, 0, R0), print(R0), write('\n'),
    row(Board, 1, R1), print(R1), write('\n'),
    row(Board, 2, R2), print(R2), write('\n'),
    row(Board, 3, R3), print(R3), write('\n'),
    row(Board, 4, R4), print(R4), write('\n').
