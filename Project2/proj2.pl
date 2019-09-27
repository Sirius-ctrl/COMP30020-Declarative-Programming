:- ensure_loaded(library(clpfd)).

puzzle_solution([[_|X_aixs]|Tail]) :-
    getY(Tail, Y_axis, Grid),
    check_diagonal(Grid),
    check_condition(Grid, X_aixs),
    transpose(Grid, Grid_T),
    check_condition(Grid_T, Y_axis).


% split the header of each row away from the grid itself
% getY(+YandG, ?Y_axis, ?Grid)
getY(YnG, Y_axis, Grid) :-
    getY(YnG, [], Y_axis, [], Grid).

getY([], YA, YA, GA, GA).

getY([[Y|Ys]|Rest], YA, Y_axis, GA, Grid) :-
    append(GA, [Ys], GA0),
    getY(Rest, [Y|YA], Y_axis, GA0, Grid).


% restrict all number in the grid are between 0 and 9
check_range([]).
check_range([X|Xs]) :-
    check_range_list(X),
    check_range(Xs).


% restrict all number in a list are between 0 and 9
check_range_list([]).
check_range_list([X|Xs]) :-
    X>=1,
    X=<9,
    check_range_list(Xs).


check_condition([], []).
check_condition([X|Xs], [Y|Ys]) :-
    sum_or_product(X, Y),
    check_condition(Xs, Ys).


% check_diagonal restrict the diagonal to have the same value
% check_diagonal(+List)
check_diagonal(List) :-
    length(List, 1).


check_diagonal([X|Xs]) :-
    nth0(0, X, Num), % find the first diagonal number
    check_diagonal(Xs, 1, Num, Num).


check_diagonal([], _, Current, Dia) :-
    Current=Dia.
check_diagonal([X|Xs], N, Current, Dia) :-
    Current=Dia,
    nth0(N, X, Num),
    N0 is N+1,
    check_diagonal(Xs, N0, Num, Dia).


% bound X to either the sum or the product of the element in the list
% sun_or_product(+List, ?X)
sum_or_product(List, X) :-
    (   sum_list(List, X)
    ;   product_list(List, X)
    ).


% find the product of the list of item, notice that the list could not be empty
% and must be provided
% list_product(+List, ?Total)
product_list([Item], Item).
product_list([A, B|Xs], Total) :-
    X is A*B,
    product_list([X|Xs], Total).


% test number of ground variable in a list
% number_of_ground(+List, ?Res).
number_of_ground(List, Res) :-
    number_of_ground(List, 0, Res).

number_of_ground([], A, A).
number_of_ground([X|Xs], A, Res) :-
    (   ground(X)
    ->  A0 is A+1,
        number_of_ground(Xs, A0, Res)
    ;   number_of_ground(Xs, A, Res)
    ).

guess_numbers(List, Target) :-
    (   guess_product(List, Target)
%   ;   guess_sum(List, X)
    ).


% reference : https://stackoverflow.com/questions/6825218/how-to-create-a-list-of-numbers-that-add-up-to-a-specific-number
num_split(0, []).
num_split(N, [X|List]) :-
    between(1, 9, X),
    plus(X, Y, N),
    num_split(Y, List).

add_up_list(Target, Results, List) :-
    bagof(List, num_split(Target, List), Results).


% modified based on above
num_split_product(1, []).
num_split_product(N, [X|List]) :-
    between(1, 9, X),
    multiply_int(Y, X, N),
    num_split_product(Y, List).

multiply_int(N1,N2,Result) :-
    N1 is Result / N2,
    integer(N1),
    N1 < 9.
    

multi_up_list(Target, Results, List) :-
    bagof(List, num_split_product(Target, List), Results).