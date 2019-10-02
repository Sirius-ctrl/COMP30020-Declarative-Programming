:- ensure_loaded(library(clpfd)).

% pre-process the input list here
puzzle_solution([[_|X_axis]|Tail]) :-
    getY(Tail, Y_axis, Grid),
    grid_solution(Grid, Y_axis, X_axis),
    % FIXME : no false after a true solution
    !.  % stop after one solution has found.


% check whether the grid satisfy the condition
% grid_solution(+Grid, +X_axis, +Y_axis)
grid_solution(Grid, X_axis, Y_axis) :-
    ground(Grid),
    check_diagonal(Grid),
    check_condition(Grid, X_axis),
    transpose(Grid, Grid_T),
    check_condition(Grid_T, Y_axis).

% some of the part are still not grounded, so we keep going deeper.
grid_solution(Grid, X_axis, Y_axis) :-
    \+ ground(Grid),
    check_diagonal(Grid),
    % TODO :  for now, it can only guess row wise.
    guess(Grid, X_axis, Grid0),
    grid_solution(Grid0, X_axis, Y_axis).


% split the header of each row away from the grid itself
% getY(+YandG, ?Y_axis, ?Grid)
getY(YnG, Y_axis, Grid) :-
    getY(YnG, [], Y, [], Grid),
    reverse(Y, Y_axis).
    

getY([], YA, YA, GA, GA).

getY([[Y|Ys]|Rest], YA, Y_axis, GA, Grid) :-
    append(GA, [Ys], GA0),
    getY(Rest, [Y|YA], Y_axis, GA0, Grid).


% restrict all number in the grid are between 0 and 9
% check_ranage(+[List])
check_range([]).
check_range([X|Xs]) :-
    check_range_list(X),
    check_range(Xs).


% restrict all number in a list are between 0 and 9
% check_range_list(+List)
check_range_list([]).
check_range_list([X|Xs]) :-
    between(1, 9, X),
    check_range_list(Xs).


% check whether each row or column satisfy the two condition
% which specify by the question.
% the first lists are row/column, the second lists represent the target number.
% check_conditon(+[List], +List)
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


guess(Grid, Targets, Grid0) :-
    nth0(0, Grid, Current),
    guess(Grid, Targets, 0, Current, Grid0).
% FIXME : if the input is grounded, there will be two grounded solution, but I dont think the input will be grounded here.
guess(Grid, _Targets, _Loc, _Current, Result) :-
    ground(Grid),
    Result = Grid.


guess(Grid, Targets, Loc, Current, Result) :-
    (   ground(Current)
    % NOTE : we are not checking whether the bounded value are satisfy the condition or not.
    ->  Loc0 is Loc+1,
        nth0(Loc0, Grid, Current0),
        guess(Grid, Targets, Loc0, Current0, Result)
    ;   nth0(Loc, Targets, Tar),
        guess_numbers(Current, Tar, Solutions),
        apply_all(Grid, Targets, Loc, Current, Result, Solutions)
    ).


apply_all(Grid, Targets, Loc, Current, Result, [S1|SR]) :-
    nth0(Loc, Grid, S1),    % assign the unbounded row/coloum with our bounded guesses
    nth0(Loc, Grid, Current0),  % assign it with the new current
    guess(Grid, Targets, Loc, Current0, Result)
    ;
    apply_all(Grid, Targets, Loc, Current, Result, SR).


% enumerate all solutions which satisfy the sum and 
guess_numbers(List, Target, Res) :-
    multi_up_list(Target, X, List),
    \+ length(X, 0),
    Res = X.


guess_numbers(List, Target, Res) :-
    add_up_list(Target, X, List),
    \+ length(X, 0),
    Res = X.

% enumerate all solutions which has the format of List and add up to the target
% add_up_list(+Target, ?Results, ?List).
add_up_list(Target, Results, List) :-
    bagof(List, num_split(Target, List), Results).


% reference : https://stackoverflow.com/questions/6825218/how-to-create-a-list-of-numbers-that-add-up-to-a-specific-number
% this is the base function for add_up_list which will only be invoked by add_up_list to 
% enumerate all solutions.
num_split(0, []).
num_split(N, [X|List]) :-
    between(1, 9, X),
    plus(X, Y, N),
    num_split(Y, List).

% enumerate all solutions which ahs the format of List and the multiplication of
% those numbers are the target
% multi_up_list(+Target, ?Results, ?List)
multi_up_list(Target, Results, List) :-
    bagof(List, num_split_product(Target, List), Results).


% similar as num_split but now it are computing the multiplication
num_split_product(1, []).
num_split_product(N, [X|List]) :-
    between(1, 9, X),
    multiply_int(Y, X, N),
    num_split_product(Y, List).

multiply_int(N1, N2, Result) :-
    N1 is Result/N2,
    integer(N1),
    between(1,9,N1).
