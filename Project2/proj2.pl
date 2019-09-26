:- ensure_loaded(library(clpfd)).

puzzle_solution([Header|Xs]) :-
    removeCorner(Header, Row),
    checkDiagonal(Xs),
    checkRow(Row),
    transpose(Row, Column),
    checkColumn(Column).
    


% remove the top left corner of the grid
removeCorner([_|Xs], Xs).


% checkDiagonal restrict the diagonal to have the same value
% checkDiagonal(+List)
% NOTE : TRO applied
checkDiagonal(List) :-
    length(List, 1).

checkDiagonal([X|Xs]) :-
    nth0(1, X, Num), % find the first diagonal number
    checkDiagonal(Xs, 2, Num, Num).

checkDiagonal([], _, Current, Dia) :-
    Current = Dia.
checkDiagonal([X|Xs], N, Current, Dia) :-
    Current = Dia,
    nth0(N, X, Num),
    N0 is N+1,
    checkDiagonal(Xs, N0, Num, Dia).


% bound X to either the sum or the product of the element in the list
% sun_or_product(+List, ?X)
sum_or_product(List, X) :-
    sum_list(List, X)
    ; product_list(List, X).


% find the product of the list of item, notice that the list could not be empty
% and must be provided
% list_product(+List, ?Total)
product_list([Item], Item).
product_list([A,B | Xs], Total) :-
    X is A*B,
    product_list([X | Xs], Total).


checkRow(_Row).

checkColumn(_Column).