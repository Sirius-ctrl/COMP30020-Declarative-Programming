%  Author   : Xinyao Niu 900721
%  Origin   : Wednesday, 2 October 2019
%  Purpose  : COMP30020 Project 2 - Math Puzzle Solver
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% This project mainly uses SWI Prolog’s Constraint Logic Programming 
%% facilities to solve the problem. By simply state all the constrains
%% and use label to ground the variables.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% libraries used in this project %%
:- ensure_loaded(library(clpfd)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% this is the entry point of this project solver which the input is the 
% puzzle itself. Notice that Puzzle should be provided with the form as 
% discribed on Grok, but the value can be either grounded or not grounded.
% Patter matching are used for spliting the puzzle into multiple part at 
% the first place.
% puzzle_solution(+Puzzle)
puzzle_solution([[_|Y_axis]|Tail]) :-
    getX(Tail, X_axis, Grid),   % split grid and X-label
    bound_diagonal(Grid),
    bound_range(Grid),
    bound_sum_or_product(Grid, X_axis),
    transpose(Grid, Grid_T),
    bound_sum_or_product(Grid_T, Y_axis),
    maplist(label, Grid).


% --------------------------------------------------------------------------------------
% split the header of each row away from the grid itself
% Tail Recursive applied
% getX(+XnG, ?X_axis, ?Grid)
% XnG:      A ListofList which has the sublist of each row with its target in the puzzle
% X_axis:   The 
% Grid  :   A listofList which contains only the slot that we need to fill in for the puzzle.
getX(XnG, X_axis, Grid) :-
    getX(XnG, [], X, [], Grid),
    reverse(X, X_axis).
    
getX([], XA, XA, GA, GA).
getX([[X|Xs]|Rest], XA, X_axis, GA, Grid) :-
    append(GA, [Xs], GA0),
    getX(Rest, [X|XA], X_axis, GA0, Grid).


% ---------------------------------------------------------------------
% Imitating the built-in function sum, but this time it trying to bound
% the product of a list. Tail Recursive applied.
% product(?List, +Operation, ?Target).
% List      : List that need to bound
% Operation : bouding operations. For this project, only #= will be used.
% Target    : the target that we want to bind the results for. For this
%             project, it will always be a grounded value.
product([Result], Operation, Target) :-
    call(Operation, Result, Target).

product([A, B|Xs], Operation, Total) :-
    X #= A*B,
    product([X|Xs], Operation, Total).


% ---------------------------------------------------------------------
% bound each sublist (row/column) of a list (Grid) to target values,
% tail recursive applied.
% bound_sum_or_product(+Grid, +Targets)
% Grid      : Same as above all Grid
% Targets   : ListofList which provide the value we want to bound to.
bound_sum_or_product([], []).
bound_sum_or_product([List|Ls], [Target|Ts]) :-
    all_distinct(List),
    (   sum(List, #=, Target)
    ;   product(List, #=, Target)
    ),
    bound_sum_or_product(Ls, Ts).


% --------------------------------------------------------------------
% restrict all number in the grid to 1..9 by frist flatten the grid.
% bound_range(+Grid)
% Grid: Same as all grid above.
bound_range(Grid) :-
    append(Grid, Flatten),
    Flatten ins 1..9.


% --------------------------------------------------------------------
% bound_diagonal restrict the diagonal to have the same value
% Tail Recursive applied
% bound_diagonal(+Grid)
% Grid: same as all grid above
bound_diagonal([X|Xs]) :-
    nth0(0, X, Num), % find the first diagonal number
    bound_diagonal(Xs, 1, Num, Num).    % restrict all diagonal value to the first one


bound_diagonal([], _, Current, Dia) :-
    Current #= Dia.
bound_diagonal([X|Xs], N, Current, Dia) :-
    Current #= Dia,
    nth0(N, X, Num),
    N0 #= N+1,
    bound_diagonal(Xs, N0, Num, Dia).

%%%%%%%%%%%%%%%%%%%%%%% END %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%