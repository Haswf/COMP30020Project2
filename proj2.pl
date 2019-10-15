/* File     : Proj2.pl
 * Author   : Shuyang Fan, shuyangf@student.unimelb.edu.au
 * Purpose  : A maths puzzle solver in Prolog. 
 *            Project 2 for Declarative Programming COMP30020
*/

/* ------------------------- The Maths Puzzle --------------------------*/
/* A maths puzzle is defined as a square grid of squares, each to be filled
with a single digit 1–9. A puzzle may or may not have a solution.

--------------------                        --------------------      
|   |  14|  10|  35|                        |   | 14 | 10 | 35 |
|14 |    |    |    |                        |14 |  7 |  2 |  1 | 
|15 |    |    |    |          -->           |15 |  3 |  7 |  5 |
|28 |    |   1|    |                        |28 |  4 |  1 |  7 |
--------------------                        --------------------
    Puzzle                                         Solution

To solve the puzzle, the following constraints must be satisfied:
- No repeat constraint: each row and each column contains no repeated digits.

- Diagonal constraint: all squares on the diagonal line from upper left to                             
                        lower right contain the same value.

- Heading constraint: the heading of reach row and column 
                        (leftmost square in a row and topmost square 
                        in a column) holds either the sum or the product 
                        of all the digits in that row or column.
*/

/* ------------------------- Library Usage -----------------------------*/
/* The program uses predicates provided by clpfd library to solve
    the puzzle, which includes transpose/2, all_distinct/1 and label/1.
*/
:- use_module(library(clpfd)).


/* ------------------------- Puzzle Solution ---------------------------*/
/* | puzzle_solution(?Puzzle)
    Puzzle_solution attempts to solve a puzzle. It should fail if the given
    puzzle is not solvable. It takes a list of lists as arguement, where each
    list represent a row in the puzzle.
*/
puzzle_solution(Puzzle) :-
    % Check the puzzle is square.
    maplist(same_length(Puzzle),  Puzzle),
    % Check the puzzle satisfies the diagonal constraint. 
    same_diagonal(Puzzle),
    % transpose the puzzle for further examing.
    transpose(Puzzle, Transposed),

    % Check all digits in the puzzle are valid.
    all_digits(Puzzle),

    % Check no repeat constraint for all rows and columns.
    no_duplicate(Puzzle),
    no_duplicate(Transposed),

    % Check the puzzle satisfies the heading constraint.
    validate_headings(Puzzle),
    validate_headings(Transposed),
    % Solve the puzzle by labeling.
    solve(Puzzle).

/* ---------------------- Checking Valid Digits ---------------------*/

/* | all_digits(?Puzzle)
    Checks the whole puzzle for digits constraint.
    Return true if all digits in the puzzle are in range 1 .. 9.
    The heading row is ignored for this check.
*/
all_digits(Puzzle) :-
    Puzzle = [_|Rows],
    maplist(all_digits_row, Rows).


/*  | all_digits_row(?RowWithHeading)
    Check if a row or a column satifised the digits constraint.
    Return true if all numbers in the row except the heading is 
    between 1 and 9. 
*/
all_digits_row(RowWithHeading) :-
    RowWithHeading = [_|Row],
    Row ins 1..9.


/* ------------------------- Checking Headings ------------------*/

/* | validate_headings(?Puzzle)
    Checks if the puzzle satisfis the heading constraint.
    Returns true if the heading of each row and column 
    (leftmost square in a row and topmost square in a column) 
    holds either the sum or the product of all the digits 
    in that row or column. 
*/
validate_headings(Puzzle) :-
    Puzzle = [_|Rows],
    maplist(is_validate, Rows).


/* | is_validate(?RowWithHeading)
    Check if a row or column satisfis the heading constraint.
    Returns true if the sum of all digits in that row or column 
    equals its heading.
*/ 
is_validate(RowWithHeading) :-
    RowWithHeading = [Heading|Row],
    sum(Row, Heading).


/* | is_validate(?RowWithHeading)
    Check if a row or column satisfis the heading constraint.
    Returns true if the product of all digits in that row or column 
    equals its heading.
*/ 
is_validate(RowWithHeading) :-
    RowWithHeading = [Heading|Row],
    product(Row, Heading).


/*  | sum(+List, ?Sum)
    Compute the sum of a list. 
    Returns true if the sum of all digits in List equals Sum. 
    It calls sum accumulator with an initial sum of 0 to compute 
    the sum of the list.
*/
sum(List, Sum) :- sum_acc(List, 0, Sum).


/*  | sum_acc(+List, +A, ?Sum)
    Helper predicate to compute the sum of a list using a accumulator. 
    Returns true if the accumulator value equals the supposed Sum 
    when the list is empty. Otherwise, recursively call itself with a 
    new accumulator until there is no element left in the List.
*/
sum_acc([], A, A).
sum_acc(List, A, Sum) :-
    List = [X|XS],
    A1 #= A + X,
    sum_acc(XS, A1, Sum).


/*  | product(+List, ?Product)
    Compute the product of a list.
    Returns true if the pruduct of all digits in List is equal to
    Product. It calls product_acc with an initial accumulator of 1
    to compute the product of the list.
*/
product(List, Product) :- product_acc(List, 1, Product).


/*  | product(+List, +A, ?Product)
    Helper predicate to compute the product of all numbers in a list
    using a accumulator. Returns true if the accumulator value equals 
    the supposed Product when the list is empty. 
    Otherwise, recursively call itself with a new 
    accumulator until there is no element left in the List.
*/
product_acc([], A, A).
product_acc(List, A, P) :-
    List = [X|XS],
    A1 #= A * X,
    product_acc(XS, A1, P).


/* ----------------------- Checking No Repeated Digits ----------------*/

/* | no_duplicate(?Puzzle)
   Returns true if there is no duplicate digit in each row or column.
   To validate that all rows satisfy this constraint, all rows except
   the heading one are examined by helper function all_distinct_row 
   using maplist/2.
*/
no_duplicate(Puzzle) :-
    Puzzle = [_|Rows],
    maplist(all_distinct_row, Rows).


/* | all_distinct_row(?RowWithHeading)
    Returns true if every single number in that row or column, except
    the leading one representing sum or product, is unique. all_distinct/1
    from clpfd is called to check pairwise uniqueness.
*/
all_distinct_row(RowWithHeading) :- 
    RowWithHeading = [_|Row],
    all_distinct(Row).


/* ---------------------- Checking Diagonal Constraint ----------- */

/* | same_diagonal(?Puzzle)
    Returns true if all squares on the diagonal line from upper left
    to lower right contain the same value. This is done by extracting 
    all diagonals and compare if they're the same.
*/
same_diagonal(Puzzle) :-
    Puzzle = [_|Rows],
    diagonal(Rows, Diagonal, 1),
    all_same(Diagonal).


/* | diagonal(+Rows, -Diagonal, +Index)
    Extract elements on the diagonal line from upper left to lower right.
    The index of the first element to be extracted is specified as Index.
*/
diagonal(Rows, Diagonal, Index) :- 
    diagonal_acc(Rows, [], Diagonal, Index).


/*  | diagonal_acc(+List, +A, -Diagonal, +Index)
    Helper predicate to extract all diagonal in a list.
*/
diagonal_acc([], Diagonal, Diagonal, _).
diagonal_acc([Row|Rest], A, Diagonal, Index) :-
    nth0(Index, Row, Elem),
    Index1 #= Index + 1,
    diagonal_acc(Rest, [Elem|A], Diagonal, Index1).

/* | all_same(+List)
    Check if all elements in a list are the same.
    Returns true if all elements in the list are the same.
    Empty list and singleton list have the same elements by definition.
    Otherwise, adjacent elements are checked recursively.
 */
all_same([]).
all_same([_]).
all_same([X, X|XS]) :-
    all_same([X|XS]).


/* ---------------------------- Puzzle Solver ---------------------------*/

/* | solve(?Puzzle)
   Call label/1 to assign a value to each variable in each row. By labeling, 
   Prolog stematically trying out values until all of missing square are 
   ground and all constraints are satisfied. 
*/
solve(Puzzle) :-
    Puzzle = [_| Rows],
    maplist(label, Rows).