/* File     : Proj2.pl
 * Author   : Shuyang Fan
 * Purpose  : A math puzzle solver in Prolog

*/

/* In order to use the same predicate for both columns and
 * rows, clpfd module is imported to use transpose/2. 
 * The apply library provides predicates like Maplist/2 to 
 * apply predicateds on a list, which is heavily used to
 * solve the math puzzle.
*/
:- use_module(library(clpfd)).
:- use_module(library(apply)).


/* Puzzle_solution is the main entry point of the program.
 * It takes a puzzle
*/
puzzle_solution(Puzzle) :-
    maplist(same_length(Puzzle), Puzzle),
    transpose(Puzzle, Transposed),

    all_digits(Puzzle),

    no_duplicate(Puzzle),
    no_duplicate(Transposed),

    check_diagonal(Puzzle),

    validateHeadings(Puzzle),
    validateHeadings(Transposed),
    solve(Puzzle).


all_digits(Puzzle) :-
    Puzzle = [_|Rows],
    maplist(all_digits_row, Rows).

all_digits_row([_|Row]) :-
    Row ins 1..9.


validateHeadings(Puzzle) :-
    Puzzle = [_|Rows],
    maplist(isValidate, Rows).

isValidate([Heading|Row]) :-
    sum(Row, Heading).


isValidate([Heading|Row]) :-
    product(Row, Heading).

% sum(+List, -Sum)
sum(List, Sum) :- sumAcc(List, 0, Sum).

% sumAcc(+List, +A, -Sum)
sumAcc([], A, A).
sumAcc(List, A, Sum) :-
    List = [X|XS],
    A1 #= A + X,
    sumAcc(XS, A1, Sum).

% product(+List, -Product)
product(List, Product) :- productAcc(List, 1, Product).

productAcc([], A, A).

% productAcc(+List, +A, -P)
productAcc(List, A, P) :-
    List = [X|XS],
    A1 #= A * X,
    productAcc(XS, A1, P).


% no_duplicate(+Puzzle)
no_duplicate(Puzzle) :-
    Puzzle = [_|Rows],
    maplist(all_distinct_row, Rows).

% all_distinct_row(+RowWithHeading)
all_distinct_row(RowWithHeading) :- 
    RowWithHeading = [_|Row],
    all_distinct(Row).


% check_diagonal(+Puzzle)
check_diagonal(Puzzle) :- 
    Puzzle = [_|[FirstRow|Rows]],
    nth0(1, FirstRow, Elem),
    check_diagonalAcc(Rows, Elem, 2).

% check_diagonalAcc(+Rows, +Elem, +Index)
check_diagonalAcc([], _,_).
check_diagonalAcc(Rows, Elem, Index) :-
    Rows = [FirstRow|Rest],
    nth0(Index, FirstRow, Elem),
    Index1 #= Index + 1,
    check_diagonalAcc(Rest, Elem, Index1).
    

% solve(+Puzzle)
solve(Puzzle) :-
    Puzzle = [_| Rows],
    maplist(label, Rows).
