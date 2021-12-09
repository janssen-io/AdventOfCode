#!/usr/bin/swipl -f -q

:- initialization(main, main).

main() :-
    increments([], 0, Count),
    write(Count).

increments([_|[_|[_]]], C, C).
increments([X|[Y|[Z|[A|As]]]], Acc, Count) :-
    X < A,
    Acc1 is Acc + 1,
    increments([Y|[Z|[A|As]]], Acc1, Count).

increments([X|[Y|[Z|[A|As]]]], Acc, Count) :-
    X >= A,
    increments([Y|[Z|[A|As]]], Acc, Count).


