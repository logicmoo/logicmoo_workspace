/*
  this is part of (H)MUARC  https://logicmoo.org/xwiki/bin/view/Main/ARC/

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)

The code provided is a Prolog program that represents a rule learning problem. The main goal of the problem is to 
 learn rules from example pairs of property groups, apply these rules to a given group of properties, and guess 
 the resulting group of properties.

To briefly explain the main parts of the code:

1. Constant values: The `constant_vals/17` predicate defines a set of constants that are used thDeleteout the program.

2. Example pairs: The `example_pair_of_property_groups/2` predicates define a series of example pairs of property groups. Each pair consists of two groups of properties (each group is represented as a list of properties), and the goal is to learn rules that can transform the first group into the second group.

3. Extracting properties: The `extract_properties/2` predicate is a helper predicate used to extract properties from example pairs.

4. Updating knowledge: The `update_knowledge/4` predicate is used to update the rule knowledge based on the given pair of property groups. The knowledge update process involves finding the least general generalization (LGG) of the two property groups and updating the knowledge with the LGG.

5. Learning rules: The `learn_rules/3` predicate is a recursive predicate that learns rules from example pairs. The predicate initializes the knowledge base with some basic rules and iteratively updates the knowledge based on the example pairs.

6. Initialization of knowledge: The `init_knowledge/1` predicate is used to initialize the knowledge base with some basic rules.

7. Learning rules from examples: The `learn_rules_from_examples/1` predicate is used to learn rules from example pairs.

8. Guessing rules: The `guess_rules_of_example_pairs/1` predicate is used to guess the rules of example pairs by learning rules from the examples.

9. Calculating offset: The `calculate_offset/2` predicate is used to calculate the offset from example pairs.

10. Applying rules: The `apply_rules_to_group/3` and related predicates are used to apply the learned rules to a given group of properties.

11. Guessing scene pairs: The `guess_scene_pair/2` predicate is the main predicate that takes a group of properties (Silvers) as input, applies the learned rules, and returns the resulting group of properties (Result).

You can run this Prolog program in a Prolog interpreter such as SWI-Prolog. 

The main predicate to execute is `guess_scene_pair/2`, providing a group of properties as input and receiving the resulting 
group of properties as output.

*/

% Helper predicates

% Cell properties
%cell(X, Y, Color).

% Get min and max X coordinates
min_max_x(Cells, MinX, MaxX) :-
    findall(X, (member(cell(X, _, _), Cells)), Xs),
    min_list(Xs, MinX),
    max_list(Xs, MaxX).

% Get min and max Y coordinates
min_max_y(Cells, MinY, MaxY) :-
    findall(Y, (member(cell(_, Y, _), Cells)), Ys),
    min_list(Ys, MinY),
    max_list(Ys, MaxY).


% Horizontal or Vertical Line
shape(Cells, Tolerance, obj([hv_line(Direction), color(C), len(Length), tol(Tolerance), cells(Cells)])) :- length(Cells,Length),Length>1,
    min_max_x(Cells, MinX, MaxX),
    min_max_y(Cells, MinY, MaxY),
    same_colors(C,Cells),
    (Length is MaxX - MinX + 1, Length > 1, Direction = horizontal, Tolerance is 0 ; 
     Length is MaxY - MinY + 1, Length > 1, Direction = vertical, Tolerance is 0).

% Diagonal Line
shape(Cells, Tolerance, obj([dg_line(Direction), color(C), len(Length), tol(Tolerance), cells(Cells)])) :- length(Cells,Length),Length>1,
    same_colors(C,Cells),
    sort_cells(Cells, SortedCells),
    (check_ascending(SortedCells), Direction = ascending, length(Cells, Length), Tolerance is 0 ;
     check_descending(SortedCells), Direction = descending, length(Cells, Length), Tolerance is 0).

% Rectangle
shape(Cells, Tolerance, 
  rectangle(Width, Height, Tolerance, InsideColor, OutsideColor, DeviantCells, Cells)) :-
    %between(0,10,Tolerance),
    min_max_x(Cells, MinX, MaxX),
    min_max_y(Cells, MinY, MaxY),
    Height is MaxY - MinY + 1, 
    Width is MaxX - MinX + 1,
    TotalCells is Width * Height,
    findall(cell(X, Y, Color), 
            (between(MinX, MaxX, X), between(MinY, MaxY, Y), 
             (member(cell(X, Y, Color), Cells) ; Color = empty)), 
            AllCells),
    exclude(=(cell(_, _, InsideColor)), AllCells, InsideDeviantCells),
    exclude(=(cell(_, _, OutsideColor)), AllCells, OutsideDeviantCells),
    append(InsideDeviantCells, OutsideDeviantCells, DeviantCells),
    length(DeviantCells, NumDeviantCells),
    NumDeviantCells / TotalCells =< Tolerance.

% Dot
shape([Cell], _Tolerance, Cell).

same_colors(_,[]).
same_colors(C,[cell(_,_,C)|Cells]):- same_colors(C,Cells).

% Check ascending diagonal
check_ascending([cell(X, Y, _) | Rest]) :-
    (Rest = [] ; Rest = [cell(X1, Y1, _) | _], X1 is X + 1, Y1 is Y + 1, check_ascending(Rest)).

% Check descending diagonal
check_descending([cell(X, Y, _) | Rest]) :-
    (Rest = [] ; Rest = [cell(X1, Y1, _) | _], X1 is X + 1, Y1 is Y - 1, check_descending(Rest)).

% Sort cells by X and Y
sort_cells(Cells, SortedCells) :-
    sort(2, @=<, Cells, SortedByY),
    sort(1, @=<, SortedByY, SortedCells).

% And the rest of your predicates...


/*
% Shape identification with tolerance
shape(Cells, dot, 0, []) :- dot(Cells), !.
shape(Cells, line, Tolerance, DeviantCells) :- 
    line(Cells, Tolerance, DeviantCells), !.
shape(Cells, square, Tolerance, InsideColor, Color, DeviantCells) :- 
    square(Cells, Tolerance, InsideColor, Color, DeviantCells), !.
shape(Cells, rectangle, Tolerance, InsideColor, Color, DeviantCells) :- 
    rectangle(Cells, Tolerance, InsideColor, Color, DeviantCells), !.
shape(_, unknown).
*/

% Detect the most tolerant shapes
detect_the_most_tolerant_shapes(Sells, Shapes) :-
  permutation(Sells,Cells),
  findall(Shape, shape_with_tolerance(Cells, Shape), Shapes).


shape_with_tolerance([],X):-!,X=[].
shape_with_tolerance(Cells, [Shape|ShapeL]):-
  between(0, 2, Tolerance), 
  append(LC, RC, Cells),
  shape(RC, Tolerance, Shape),
  shape_with_tolerance(LC, ShapeL).



% Cell facts
cell(1, 1, red).  % Square
cell(2, 1, red).
cell(3, 1, red).
cell(1, 2, red).
cell(2, 2, white).
cell(3, 2, red).
cell(1, 3, red).
cell(2, 3, red).
cell(3, 3, red).

cell(5, 1, blue). % Dot
cell(7, 1, blue). % Line
cell(7, 2, blue).
cell(7, 3, blue).

cell(9, 1, green).  % Rectangle
cell(10, 1, green).
cell(11, 1, green).
cell(9, 2, green).
cell(10, 2, white).
cell(11, 2, green).
cell(9, 3, green).
cell(10, 3, green).
cell(11, 3, green).


end_of_file.

% Construct and edit arbitrary number lists
add_number(N, List, [N|List]).

remove_number(_, [], []).
remove_number(N, [N|T], T) :- !.
remove_number(N, [H|T], [H|R]) :- remove_number(N, T, R).

% Storing pairs of number lists under a task name
:- dynamic(task/2).

store_task(TaskName, Pairs) :- assertz(task(TaskName, Pairs)).

print_task(TaskName) :- task(TaskName, Pairs), writeln(Pairs).

% Extract properties from left side of pairs
extract_property(_, _, []).
extract_property(Method, [H1|T1], [H2|T2]) :-
    property_extractor(Method, H1, H2), 
    extract_property(Method, T1, T2).

% Convert between number lists and properties
property_extractor(1, List, sum(S)) :- sum_list(List, S).
property_extractor(2, List, count(C)) :- length(List, C).
property_extractor(3, List, min(Min)) :- min_list(List, Min).
property_extractor(4, List, max(Max)) :- max_list(List, Max).
property_extractor(5, List, mean(Mean)) :-
    sum_list(List, S), 
    length(List, C), 
    Mean is S / C.

% Extract properties and store them
:- dynamic(properties/3).

store_properties(TaskName, Method) :-
    task(TaskName, Pairs), 
    findall(L, (member((L, _), Pairs)), Lefts), 
    findall(R, (member((_, R), Pairs)), Rights), 
    extract_property(Method, Lefts, LeftProps), 
    extract_property(Method, Rights, RightProps), 
    assertz(properties(TaskName, left, LeftProps)), 
    assertz(properties(TaskName, right, RightProps)).

% Confirm that rules would convert the left properties to the right properties
confirm_rules(TaskName, Rules) :-
    properties(TaskName, left, LeftProps), 
    properties(TaskName, right, RightProps), 
    apply_rules(Rules, LeftProps, RightProps).

apply_rules([], [], []).
apply_rules([Rule|Rules], [H1|T1], [H2|T2]) :-
    Rule =.. [_, H1, H2], 
    apply_rules(Rules, T1, T2).

% Deduce missing right side of a pair
deduce_right_side(TaskName, Left, Right) :-
    properties(TaskName, left, LeftProps), 
    properties(TaskName, right, RightProps), 
    task(TaskName, Pairs), 
    findall(Method, (member((Left, _), Pairs), property_extractor(Method, Left, _)), Methods), 
    member(Method, Methods), 
    property_extractor(Method, Left, LeftProp), 
    member(LeftProp, LeftProps), 
    nth0(Index, LeftProps, LeftProp), 
    nth0(Index, RightProps, RightProp), 
    property_extractor(Method, Right, RightProp).

