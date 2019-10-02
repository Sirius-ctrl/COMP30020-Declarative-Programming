:- ensure_loaded(library(clpfd)).


puzzle_solution([[_|Y_axis]|Tail]) :-
    getX(Tail, X_axis, Grid),
    bound_diagonal(Grid),
    bound_range(Grid),
    bound_sum_or_product(Grid, X_axis),
    transpose(Grid, Grid_T),
    bound_sum_or_product(Grid_T, Y_axis),
    maplist(label, Grid).


% ---------------------------------------------------------------------
% split the header of each row away from the grid itself
% getX(+YandG, ?Y_axis, ?Grid)
getX(YnG, Y_axis, Grid) :-
    getX(YnG, [], Y, [], Grid),
    reverse(Y, Y_axis).
    
getX([], YA, YA, GA, GA).

getX([[Y|Ys]|Rest], YA, Y_axis, GA, Grid) :-
    append(GA, [Ys], GA0),
    getX(Rest, [Y|YA], Y_axis, GA0, Grid).


% ---------------------------------------------------------------------

% product(?List, +Operation, ?Target).
product([Result], Operation, Target) :-
    call(Operation, Result, Target).

product([A, B|Xs], Operation, Total) :-
    X #= A*B,
    product([X|Xs], Operation, Total).

bound_sum_or_product([], []).
bound_sum_or_product([List|Ls], [Target|Ts]) :-
    all_distinct(List),
    (   sum(List, #=, Target)
    ;   product(List, #=, Target)
    ),
    bound_sum_or_product(Ls, Ts).


% --------------------------------------------------------------------
bound_range([]).
bound_range([List|Ls]) :-
    List ins 1..9,
    bound_range(Ls).


% --------------------------------------------------------------------
% bound_diagonal restrict the diagonal to have the same value
% TRO applied
% bound_diagonal(+List)
bound_diagonal(List) :-
    length(List, 1).


bound_diagonal([X|Xs]) :-
    nth0(0, X, Num), % find the first diagonal number
    bound_diagonal(Xs, 1, Num, Num).


bound_diagonal([], _, Current, Dia) :-
    Current #= Dia.
bound_diagonal([X|Xs], N, Current, Dia) :-
    Current #= Dia,
    nth0(N, X, Num),
    N0 #= N+1,
    bound_diagonal(Xs, N0, Num, Dia).