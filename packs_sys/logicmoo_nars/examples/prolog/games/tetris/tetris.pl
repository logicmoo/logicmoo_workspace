:- use_module(library(clpfd)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                      %
% Author: Fabian Faessler, Jonas Traub %
%                                      %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of all stones and their possible rotation                                                      %
% We use the rotation established by Nintendo (see: http://tetris.wikia.com/wiki/Category:Rotation_Systems) %
% The rotations in the game are NOT just rotated matrixes. They HAVE TO BE hard coded.                      %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%
% The I %
%%%%%%%%%
rotate([
            [0,0,0],
            [0,1,0],
            [0,1,0],
            [0,1,0],
            [0,1,0],
            [0,0,0]
        ],
        XPos, YPos,
        [
            [0,0,0,0,0,0],
            [0,1,1,1,1,0],
            [0,0,0,0,0,0]
        ],
        XNew,YNew):- XNew is XPos+2, YNew is YPos+2.

rotate([
            [0,0,0,0,0,0],
            [0,1,1,1,1,0],
            [0,0,0,0,0,0]
        ],
        XPos, YPos,
        [
            [0,0,0],
            [0,1,0],
            [0,1,0],
            [0,1,0],
            [0,1,0],
            [0,0,0]
        ],
        XNew,YNew):- XNew is XPos-2, YNew is YPos-2.

%%%%%%%%%
% The S %
%%%%%%%%%
rotate([
            [0,0,0,0,0],
            [0,0,1,1,0],
            [0,1,1,0,0],
            [0,0,0,0,0]
        ],
        XPos, YPos,
        [
            [0,0,0,0],
            [0,1,0,0],
            [0,1,1,0],
            [0,0,1,0],
            [0,0,0,0]
        ],
        XNew,YNew):- XNew is XPos+1, YNew is YPos-1.

rotate([
            [0,0,0,0],
            [0,1,0,0],
            [0,1,1,0],
            [0,0,1,0],
            [0,0,0,0]
        ],
        XPos, YPos,
        [
            [0,0,0,0,0],
            [0,0,1,1,0],
            [0,1,1,0,0],
            [0,0,0,0,0]
        ],
        XNew,YNew):- XNew is XPos-1, YNew is YPos+1.


%%%%%%%%%
% The Z %
%%%%%%%%%
rotate([
            [0,0,0,0,0],
            [0,1,1,0,0],
            [0,0,1,1,0],
            [0,0,0,0,0]
        ],
        XPos, YPos,
        [
            [0,0,0,0],
            [0,0,1,0],
            [0,1,1,0],
            [0,1,0,0],
            [0,0,0,0]
        ],
        XNew,YNew):- XNew is XPos+1, YNew is YPos.

rotate([
            [0,0,0,0],
            [0,0,1,0],
            [0,1,1,0],
            [0,1,0,0],
            [0,0,0,0]
        ],
        XPos, YPos,
        [
            [0,0,0,0,0],
            [0,1,1,0,0],
            [0,0,1,1,0],
            [0,0,0,0,0]
        ],
        XNew,YNew):- XNew is XPos-1, YNew is YPos.


%%%%%%%%%
% The T %
%%%%%%%%%
rotate([
            [0,0,0,0,0],
            [0,0,1,0,0],
            [0,1,1,1,0],
            [0,0,0,0,0]
        ],
        XPos, YPos,
        [
            [0,0,0,0],
            [0,1,0,0],
            [0,1,1,0],
            [0,1,0,0],
            [0,0,0,0]
        ],
        XNew,YNew):- XNew is XPos+1, YNew is YPos.

rotate([
            [0,0,0,0],
            [0,1,0,0],
            [0,1,1,0],
            [0,1,0,0],
            [0,0,0,0]
        ],
        XPos, YPos,
        [
            [0,0,0,0,0],
            [0,1,1,1,0],
            [0,0,1,0,0],
            [0,0,0,0,0]
        ],
        XNew,YNew):- XNew is XPos-1, YNew is YPos+1.

rotate([
            [0,0,0,0,0],
            [0,1,1,1,0],
            [0,0,1,0,0],
            [0,0,0,0,0]
        ],
        XPos, YPos,
        [
            [0,0,0,0],
            [0,0,1,0],
            [0,1,1,0],
            [0,0,1,0],
            [0,0,0,0]
        ],
        XNew,YNew):- XNew is XPos, YNew is YPos-1.

rotate([
            [0,0,0,0],
            [0,0,1,0],
            [0,1,1,0],
            [0,0,1,0],
            [0,0,0,0]
        ],
        XPos, YPos,
        [
            [0,0,0,0,0],
            [0,0,1,0,0],
            [0,1,1,1,0],
            [0,0,0,0,0]
        ],
        XNew,YNew):- XNew is XPos, YNew is YPos.


%%%%%%%%%
% The J %
%%%%%%%%%
rotate([
            [0,0,0,0,0],
            [0,1,0,0,0],
            [0,1,1,1,0],
            [0,0,0,0,0]
        ],
        XPos, YPos,
        [
            [0,0,0,0],
            [0,1,1,0],
            [0,1,0,0],
            [0,1,0,0],
            [0,0,0,0]
        ],
        XNew,YNew):- XNew is XPos+1, YNew is YPos.

rotate([
            [0,0,0,0],
            [0,1,1,0],
            [0,1,0,0],
            [0,1,0,0],
            [0,0,0,0]
        ],
        XPos, YPos,
        [
            [0,0,0,0,0],
            [0,1,1,1,0],
            [0,0,0,1,0],
            [0,0,0,0,0]
        ],
        XNew,YNew):- XNew is XPos-1, YNew is YPos+1.

rotate([
            [0,0,0,0,0],
            [0,1,1,1,0],
            [0,0,0,1,0],
            [0,0,0,0,0]
        ],
        XPos, YPos,
        [
            [0,0,0,0],
            [0,0,1,0],
            [0,0,1,0],
            [0,1,1,0],
            [0,0,0,0]
        ],
        XNew,YNew):- XNew is XPos, YNew is YPos-1.

rotate([
            [0,0,0,0],
            [0,0,1,0],
            [0,0,1,0],
            [0,1,1,0],
            [0,0,0,0]
        ],
        XPos, YPos,
        [
            [0,0,0,0,0],
            [0,1,0,0,0],
            [0,1,1,1,0],
            [0,0,0,0,0]
        ],
        XNew,YNew):- XNew is XPos, YNew is YPos.


%%%%%%%%%
% The L %
%%%%%%%%%
rotate([
            [0,0,0,0,0],
            [0,0,0,1,0],
            [0,1,1,1,0],
            [0,0,0,0,0]
        ],
        XPos, YPos,
        [
            [0,0,0,0],
            [0,1,0,0],
            [0,1,0,0],
            [0,1,1,0],
            [0,0,0,0]
        ],
        XNew,YNew):- XNew is XPos+1, YNew is YPos.

rotate([
            [0,0,0,0],
            [0,1,0,0],
            [0,1,0,0],
            [0,1,1,0],
            [0,0,0,0]
        ],
        XPos, YPos,
        [
            [0,0,0,0,0],
            [0,1,1,1,0],
            [0,1,0,0,0],
            [0,0,0,0,0]
        ],
        XNew,YNew):- XNew is XPos-1, YNew is YPos+1.

rotate([
            [0,0,0,0,0],
            [0,1,1,1,0],
            [0,1,0,0,0],
            [0,0,0,0,0]
        ],
        XPos, YPos,
        [
            [0,0,0,0],
            [0,1,1,0],
            [0,0,1,0],
            [0,0,1,0],
            [0,0,0,0]
        ],
        XNew,YNew):- XNew is XPos, YNew is YPos-1.

rotate([
            [0,0,0,0],
            [0,1,1,0],
            [0,0,1,0],
            [0,0,1,0],
            [0,0,0,0]
        ],
        XPos, YPos,
        [
            [0,0,0,0,0],
            [0,0,0,1,0],
            [0,1,1,1,0],
            [0,0,0,0,0]
        ],
        XNew,YNew):- XNew is XPos, YNew is YPos.

%%%%%%%%%
% The O %
%%%%%%%%%

rotate([
            [0,0,0,0],
            [0,1,1,0],
            [0,1,1,0],
            [0,0,0,0]
        ],
        XPos, YPos,
        [
            [0,0,0,0],
            [0,1,1,0],
            [0,1,1,0],
            [0,0,0,0]
        ],
        XNew,YNew):- XNew is XPos, YNew is YPos.







%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                       %
% The main function which is called by the python tetris game simulator %
%                                                                       %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% (+,+,-,-,-) Gives the best position and rotation variant for a stone

my_best_position(Field,Stone,XPos,YPos,SelectedVar):-
    all_variants(Stone,Variants),
    all_positions(Field,Variants,Z,0),
    highest(Z,[_|[XPos|[YPos|[VarId|_]]]],_),
    get_element(Variants,VarId,SelectedVar),
    check_path(XPos,YPos,Field,SelectedVar).
my_best_position(Field,Stone,XPos,YPos,OutVar):-
    all_variants(Stone,Variants),
    all_positions(Field,Variants,Z,0),
    highest(Z,[_|[Xtmp|[Ytmp|[VarId|_]]]],Rest),
    get_element(Variants,VarId,SelectedVar),
    \+ check_path(Xtmp,Ytmp,Field,SelectedVar),
    my_best_position(Field,Variants,XPos,YPos,Rest,OutVar).

my_best_position(_,[H|_],-1,-1,[],H).
my_best_position(Field,Variants,XPos,YPos,RestIn,SelectedVar):-
    highest(RestIn,[_|[XPos|[YPos|[VarId|_]]]],_),
    get_element(Variants,VarId,SelectedVar),
    check_path(XPos,YPos,Field,SelectedVar).
my_best_position(Field,Variants,XPos,YPos,RestIn,SelectedVar2):-
    highest(RestIn,[_|[Xtmp|[Ytmp|[VarId|_]]]],RestOut),
    get_element(Variants,VarId,SelectedVar),
    \+ check_path(Xtmp,Ytmp,Field,SelectedVar),
    my_best_position(Field,Variants,XPos,YPos,RestOut,SelectedVar2),!.







%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                      %
% Constants to influence the ranking of possible positions for a stone %
% ->Change this constants to test different player characteristics     %
%                                                                      %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

points_by_field(0,0).   % 0 = No stone in neighbourhood
points_by_field(1,15).  % 1 = stone in neihbourhood
points_by_field(2,12).  % 2 = field border in neighbourhood

get_constant(0,-50). % difinitive hole
get_constant(1,-50). % possible hole (00 pattern)
get_constant(2,-20). % possible hole on the left   ([[1,_],[0,1],[1,_]] pattern)
get_constant(3,-20). % possible hole on the right  ([[_,1],[1,0],[_,1]] pattern)
get_constant(4,-10). % possible hole on the bottom ([[_,1,_],[1,0,1]] pattern)
get_constant(5,-60). % 10 hole ([[1],[0]] pattern)
get_constant(6,-40). % 100 hole ([[1],[0],[0]] pattern)
points_full_row_stone(40).    % bonus points for a full row (stone based)
points_full_row_field(50).   % bonus points for a full row (field based)




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Functions to calculate the points for a stone at a position %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%(+,+,-) Calculates the Points for a Stone using the stone and the submatrix from the field at the position of the placement
%        punkte(Stein,Feldauschschnitt).
punkte([SteinH|[SteinTH|SteinTT]],[VglH|VglT],P):-
    toppunkte(SteinTH,VglH,0,P1),
    punkte([SteinTH|SteinTT],VglT,P1,P2),
    luecke([SteinH|[SteinTH|SteinTT]],[VglH|VglT],P2,P).

%(+,+,+,-) Recursive Steps for punkte/3 - Adds points to PIn for getting POut
punkte([SteinH|[_|[]]],[VglH|[VglTH|[]]],PIn,POut):-
    toppunkte(SteinH,VglTH,PIn,P1), 
    sidepunkte(SteinH,VglH,P1,POut).

punkte([SteinH|SteinT],[VglH|VglT],PIn,POut):-
    sidepunkte(SteinH,VglH,PIn,P1), 
    punkte(SteinT,VglT,P1,POut).

%(+,+,+,-) Calculate the points for neighbours at the top of a placed stone
%          toppunkte(rowOfStone,rowOfSubfield,PointsIn,PointsOut)
toppunkte([],[],N,N).
toppunkte([0|ST],[_|VT],PIn,POut):- 
    toppunkte(ST,VT,PIn,POut).
toppunkte([1|ST],[X|VT],PIn,POut):-
    points_by_field(X,P1),P2 is PIn+P1, 
    toppunkte(ST,VT,P2,POut).

%(+,+,+,-) Calculates the points for neighbours at the side of a placed stone
%          sidepunkte(rowOfStone,rowOfSubfield,PointsIn,PointsOut)
sidepunkte([_|[_|[]]],[_|[_|[]]],PIn,PIn).
sidepunkte([_|[0|TT]],[_|VT],PIn,POut):-
    sidepunkte([0|TT],VT,PIn,POut).
sidepunkte([_|[1|TT]],[VH|[VTH|[VTTH|VTTT]]],PIn,POut):-
    points_by_field(VH,P1), points_by_field(VTTH,P2), P3 is P1+P2+PIn,
    sidepunkte([1|TT],[VTH|[VTTH|VTTT]],P3,POut).

%(+,-) Calculate bonus points for complete rows.
%      zeile(row,points)
zeile([],0).
zeile([ComboH|ComboT],P):-
    ganze_zeile(ComboH,P1),
    zeile(ComboT,P2), P is P1+P2.
 
%(+-,+-) Detect if a row is a full row (detects only rows with possible stone dimension of the tetris game)
ganze_zeile(_,0).
ganze_zeile([1,1,1,1],P):-
    points_full_row_stone(P),!.
ganze_zeile([1,1,1,1,1],P):-
    points_full_row_stone(P),!.
ganze_zeile([1,1,1,1,1,1],P):-
    points_full_row_stone(P),!.

%(+,+,+,-) calculate the points for holes and bonus points for full rows
luecke(Stein,Feld,P,P4):-
    matrix_combine(Stein,Feld,Combo),
    luecke(Combo,P2),
    zeile(Combo,P3),
    P4 is P+P2+P3.

%(+,-) calculates the points for hole patterns
%      luecke(combinedMatrix,points)
luecke([ComboH|[ComboTH|ComboTT]],POINTS):-
    findall(X0,submatrix(X0,_,3,2,[ComboH|[ComboTH|ComboTT]],[[_,1,_],[_,0,_]]),List0),
    findall(X1,submatrix(X1,_,3,3,[ComboH|[ComboTH|ComboTT]],[[_,1,_],[1,0,1],[_,1,_]]),List1),
    findall(X2,submatrix(X2,_,2,2,[ComboH|[ComboTH]],[[0,1],[0,1]]),List2),
    length(List0,Count0),
    length(List1,Count1),
    length(List2,Count2),
    get_constant(0,X), 
    get_constant(1,Y),
    get_constant(5,C),
    transpose([ComboH|[ComboTH|ComboTT]],[Combo2H|[Combo2TH|Combo2TT]]),
    reverse([Combo2H|[Combo2TH|Combo2TT]],[Combo3H|[Combo3TH|_]]),
    reverse([ComboH|[ComboTH|ComboTT]],[Combo4H|[Combo4TH|_]]),
    findall(X3,submatrix(X3,_,3,2,[Combo2H|[Combo2TH]],[[1,0,1],[_,1,_]]),List3),
    findall(X4,submatrix(X4,_,3,2,[Combo3H|[Combo3TH]],[[1,0,1],[_,1,_]]),List4),
    findall(X5,submatrix(X5,_,3,2,[Combo4H|[Combo4TH]],[[1,0,1],[_,1,_]]),List5),
    findall(X6,submatrix(X6,_,3,3,[ComboH|[ComboTH|ComboTT]],[[_,1,_],[_,0,_],[_,0,_]]),List6),
    findall(X7,submatrix(X7,_,2,2,[ComboH|[ComboTH]],[[1,0],[1,0]]),List7),
    length(List3,Count3),
    length(List4,Count4),
    length(List5,Count5),
    length(List6,Count6),
    length(List7,Count7),
    get_constant(2,Z), 
    get_constant(3,A),
    get_constant(4,B),
    get_constant(6,D),
    POINTS is Count1*X + Count2*Y + Count3*Z + Count4*A + Count5*B + Count0*C + Count6*D + Count7*Y.

% (+,+,+,+,-); Calculates bonus points for rows which will be deleted
full_rows(XPos,YPos,[StoneH|StoneT],[FieldH|FieldT],P):-
    length(FieldH,FW),
    length(StoneH,SW),
    length([StoneH|StoneT],H),
    submatrix(0,YPos,FW,H,[FieldH|FieldT],Submatrix),
    full_row(Submatrix,[StoneH|StoneT],SW,XPos,0,P).
    
full_row([],[],_,_,P,P).
full_row([SubmatrixH|SubmatrixT],[StoneH|StoneT],SW,XPos,PIn,POut):-
    full_row(SubmatrixH,StoneH,SW,XPos,0),
    points_full_row_field(F), 
    Tmp is PIn+F,
    full_row(SubmatrixT,StoneT,SW,XPos,Tmp,POut),!.
full_row([_|SubmatrixT],[_|StoneT],SW,XPos,PIn,POut):-
    full_row(SubmatrixT,StoneT,SW,XPos,PIn,POut).

full_row([],[],_,_,_).
full_row([FieldrowH|FieldrowT],StoneRow,StoneWidth,XPos,Curr):-
    Tmp is XPos+StoneWidth,
    (
        XPos>Curr
    ;
        Tmp=<Curr
    ),
    (
        FieldrowH=1
    ;
        FieldrowH=2
    ),
    Curr2 is Curr+1,
    full_row(FieldrowT,StoneRow,StoneWidth,XPos,Curr2).
full_row([FieldrowH|FieldrowT],[StoneRowH|StoneRowT],StoneWidth,XPos,Curr):-
    XPos=<Curr,
    (
        FieldrowH=1
    ;
        FieldrowH=2
    ;
        StoneRowH=1
    ),
    Curr2 is Curr+1,
    full_row(FieldrowT,StoneRowT,StoneWidth,XPos,Curr2).







%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Functions for validating that there is a possible path to a postion for a stone %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% (+,+,+,+) Gives true if there is a legal path to place the stone
check_path(_,0,_,_).

check_path(XPos,YPos,Field,Stone):-
    YPos>0,
    step_top(XPos,YPos,Field,Stone,YNeu),
    check_path(XPos,YNeu,Field,Stone),!.

check_path(XPos,YPos,Field,Stone):-
    YPos>0,
    move_side(XPos,YPos,Field,Stone,NewStone,XNeu,YNeu,2,_),
    step_top(XNeu,YNeu,Field,NewStone,YNeu2),
    check_path(XNeu,YNeu2,Field,NewStone),!.

% (+,+,+,-,-,-,+,+) simulates a sidwarts move of a stone or a rotation
move_side(XPos,YPos,Field,Stone,Stone,XNeu,YPos,Max,Next):-
    move_right(XPos,YPos,Field,Stone,XNeu,Max,Next).

move_side(XPos,YPos,Field,Stone,Stone,XNeu,YPos,Max,Next):-
    move_left(XPos,YPos,Field,Stone,XNeu,Max,Next).

move_side(XPos,YPos,Field,Stone,NeuStein,XNeu,YNeu,Max,Next):-
    Max>1,
    Next is Max-1,
    rotate_options(XPos,YPos,Field,Stone,NeuStein,XNeu,YNeu).

move_side(XPos,YPos,Field,Stone,NeuNeuNeuStein,XNeu,YNeu,Max,Next):-
    Max>1,
    NextTmp3 is Max-1,
    move_side(XPos,YPos,Field,Stone,NeuStein,XNeu,YNeu,NextTmp3,NextTmp1),
    rotate_options(XPos,YPos,Field,NeuStein,NeuNeuStein,XNeu,YNeu),
    NextTmp2 is NextTmp1-1,
    move_side(XPos,YPos,Field,NeuNeuStein,NeuNeuNeuStein,XNeu,YNeu,NextTmp2,Next),
    Next>=1.

% (+,+,+,-,-,-,+,+) simulates a rotation and gives the rotated stone an its postion
rotate_options(XPos,YPos,Field,Stone,[SH|ST],XNeu,YNeu):-
    rotate(Stone,XPos,YPos,[SH|ST],XNeu,YNeu),
    XNeu>=0,
    YNeu>=0,
    length([SH|ST],Height), 
    length(SH,Width),
    submatrix(XNeu,YNeu,Width,Height,Field,Subfield),
    check_matrix([SH|ST],Subfield,_).

rotate_options(XPos,YPos,Field,Stone,[SH|ST],XNeu,YNeu):-
    rotate(Stone,XPos,YPos,SteinTmp,XTmp,YTmp),
    rotate(SteinTmp,XTmp,YTmp,SteinTmp2,XTmp2,YTmp2),
    rotate(SteinTmp2,XTmp2,YTmp2,[SH|ST],XNeu,YNeu),
    XNeu>=0,
    YNeu>=0,
    length([SH|ST],Height), 
    length(SH,Width),
    submatrix(XNeu,YNeu,Width,Height,Field,Subfield),
    check_matrix([SH|ST],Subfield,_).

% (+,+,+,-,-,-,+,+) simulates a move to the right and gives the new postion
move_right(XPos,YPos,Field,Stone,XNeu,Max,0):-
    1=<Max,
    step_right(XPos,YPos,Field,Stone,XNeu).

move_right(XPos,YPos,Field,Stone,XNeu,Max,Next2):-
    1<Max,
    Next is Max-1,
    step_right(XPos,YPos,Field,Stone,Xtmp),
    move_right(Xtmp,YPos,Field,Stone,XNeu,Next,Next2).

% (+,+,+,-,-,-,+,+) simulates a move to the left and gives the new postion
move_left(XPos,YPos,Field,Stone,XNeu,Max,0):-
    1=<Max,
    XPos>0,
    step_left(XPos,YPos,Field,Stone,XNeu).

move_left(XPos,YPos,Field,Stone,XNeu,Max,Next2):-
    1<Max,
    XPos>1,
    Next is Max-1,
    step_left(XPos,YPos,Field,Stone,Xtmp),
    move_left(Xtmp,YPos,Field,Stone,XNeu,Next,Next2).

% (+,+,+,-,-,-,+,+) simulates a move to the top and gives the new postion (This tool uses reverse check of paths from bottom to top)
step_top(XPos,YPos,Field,[SH|ST],YNeu):-
    YNeu is YPos-1,
    length([SH|ST],Height), 
    length(SH,Width),
    submatrix(XPos,YNeu,Width,Height,Field,Subfield),
    check_matrix([SH|ST],Subfield,_).

% (+,+,+,+,-) simulates a single step to the right
step_right(XPos,YPos,Field,[SH|ST],XNeu):-
    XNeu is XPos+1,
    length([SH|ST],Height), 
    length(SH,Width),
    submatrix(XNeu,YPos,Width,Height,Field,Subfield),
    check_matrix([SH|ST],Subfield,_).

% (+,+,+,+,-) simulates a single step to the left
step_left(XPos,YPos,Field,[SH|ST],XNeu):-
    XNeu is XPos-1,
    length([SH|ST],Height), 
    length(SH,Width),
    submatrix(XNeu,YPos,Width,Height,Field,Subfield),
    check_matrix([SH|ST],Subfield,_).







%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Functions to compare and Combine Lists %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%(+,+); checks if a stone matrix fits onto a submatrix of the field... aka. is stone placable there
check_matrix([],[]).
check_matrix([XH|XT],[YH|YT]):- 
    check_row(XH,YH),
    check_matrix(XT,YT).

%(+,+,-); checks if a stone matrix fits onto a submatrix of the field... aka. is stone placable there
%         And calculates the points for the position
check_matrix([XH|XT],[YH|YT],P):- 
    check_row(XH,YH),
    check_matrix(XT,YT),
    punkte([XH|XT],[YH|YT],P).

% (+,+); ([1,1,0],[0,0,0]):-true. ; ([1,1,0],[0,1,1]):-false. - checkt if a stone fits onto a field row
check_row([],[]).
check_row([_|XT],[YH|YT]):- 
    YH=0,
    check_row(XT,YT).
check_row([XH|XT],[YH|YT]):- 
    YH=2,
    XH=0,
    check_row(XT,YT).
check_row([XH|XT],[YH|YT]):- 
    YH=1,
    XH=0,
    check_row(XT,YT).

% (+,+,-); checks if a list is a sublist of another list. C is the position/index where it occurs
sub(S,[H|T],0):- 
    prefix(S,[H|T]).
sub(S,[_|T],Index):- 
    sub(S,T,D),
    succ(D,Index).

% Combines two Lists [0,1,1]+[0,0,1] becomes [0,1,1]
list_combine([],[],[]).
list_combine([L1H|L1T],[L2H|L2T],[1|Rest]):-
    Sum is L1H+L2H, 
    Sum>0,
    list_combine(L1T,L2T,Rest).
list_combine([L1H|L1T],[L2H|L2T],[0|Rest]):-
    Sum is L1H+L2H, 
    Sum=<0,
    list_combine(L1T,L2T,Rest).

% (-,-,+,+); gives the position and length of a given sublist in a list
cut(Index,Length,List,Sublist):- 
    length(Sublist,Length),
    sub(Sublist,List,Index).

% (+,+,-) Gets element with id N from a list
get_element([H|_],0,H).
get_element([_|T],N,Out):-
    N>0, 
    M is N-1, 
    get_element(T,M,Out).







%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Basic Matrix/List helper functions %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Combines two Matrixes [[0,1,1],[0,1,0]]+[[1,0,1],[0,1,0]] becomes [[1,1,1],[0,1,0]]
matrix_combine([],[],[]).
matrix_combine([M1H|M1T],[M2H|M2T],[Row|Rest]):-
    list_combine(M1H,M2H,Row),
    matrix_combine(M1T,M2T,Rest).

% (-,-,+,+,+,-); gives all submatrixes with the position in a matrix
submatrix(XPos,YPos,Width,Height,Matrix,Submatrix):- 
    cut(YPos,Height,Matrix,T1), 
    transpose(T1,T2),
    cut(XPos,Width,T2,T3), 
    transpose(T3,Submatrix).

% (+,-,-) Gives the Position with the highest score (including rotation variant) and the rest list 
highest([],[-1,-1,-1,0],[]):- !.
highest([H|[]],H,[]):- 
    H\=[].
highest([H|T],Big,[Small|RTmp]):-
    highest(T,Tmp,RTmp),
    vglpos(H,Tmp,Big,Small).

%(+,+,-,-) brings two positions in order of their score 
%      Pos1      Pos2      Groesser  Kleiner
vglpos([P1H|P1T],[P2H|P2T],[P1H|P1T],[P2H|P2T]):- 
    P1H>P2H.
vglpos([P1H|P1T],[P2H|P2T],[P2H|P2T],[P1H|P1T]):- 
    P1H=<P2H.







%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Tetris specific functions %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% (-,-,+,+,-); gives all possible positions where a stone could be placed and the points for the placement
positions(XPos,YPos,Field,[SH|ST],Q):-
    length([SH|ST],Height), 
    length(SH,Width),
    submatrix(XPos,YPos,Width,Height,Field,Subfield),
    check_matrix([SH|ST],Subfield),
    NewY is YPos+1,
    (
        (
            submatrix(XPos,NewY,Width,Height,Field,G),
            \+ check_matrix([SH|ST],G)
        )
    ;
        (\+ submatrix(XPos,NewY,Width,Height,Field,G))
    ),
    punkte([SH|ST],Subfield,P),
    full_rows(XPos,YPos,[SH|ST],Field,P1),
    Q is P+P1+YPos*5.

% (+,+,-,-); gives a list of all positions + Points
all_positions(_,[],[],_).
all_positions(Field,[StoneH|StoneT],Z,N):-
    findall([P,XPos,YPos,N],positions(XPos,YPos,Field,StoneH,P),Z1),
    M is N+1,
    all_positions(Field,StoneT,Z2,M),
    append(Z1,Z2,Z).

% (+,-) gives an list with all rotation variants of a stone
all_variants(Stone,Variants):-
    rotate(Stone,5,5,NewStone,_,_),
    rotate(NewStone,5,5,NewStone2,_,_),
    rotate(NewStone2,5,5,NewStone3,_,_),
    clear_variants([Stone,NewStone,NewStone2,NewStone3],Variants).

% (+,-) removes duplicated rotations
clear_variants([Stone,Stone,Stone,Stone],[Stone]).
clear_variants([Stone,Stone2,Stone,Stone2],[Stone,Stone2]):- 
    Stone\=Stone2.
clear_variants([Stone,Stone2,Stone3,Stone4],[Stone,Stone2,Stone3,Stone4]):- 
    Stone\=Stone2, 
    Stone2\=Stone4.








%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Alternative old funktions - not longer in use %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Not Longer in use because of exponential growing complexity
%highest([],[-1,-1,-1,0],[]):- print('h1\n').
%highest([[LH|LHT]|LT],[LH|LHT],LT):-print('h2\n'), highest(LT,[TH|_],_),LH>TH.
%highest([[LH|LHT]|LT],[TH|TL],[[LH|LHT]|Rest]):-print('h3\n'), highest(LT,[TH|TL],Rest),LH=<TH.

% Not Longer in use because of exponential growing complexity
%highest([],[-1,-1,-1,0],[]).
%highest([[Kopf|KT]|T],[Kopf|KT],T):- highest(T,[Rest|_],_), Kopf>Rest.
%highest([[Kopf|KT]|T],[Rest|RestT],[[Kopf|KT]|Rueck]):- highest(T,[Rest|RestT],Rueck), Kopf=<Rest.

% Nostalgic variant of check path - not longer in use.
%check_path(XPos,YPos,Field,Stone):-
%    step_left(XPos,YPos,Field,Stone,XNeu),
%    step_top(XNeu,YPos,Field,Stone,YNeu),
%    check_path(XNeu,YNeu,Field,Stone).

% (+,-,-); input is a list of 3 element lists [[1,2,3],[2,2,2],[3,4,5]] and gives back the list with the highest index -> [3,4,5]
% not longer used (use highest instead)
%highest([],[-1,-1,-1]).
%highest([[LH|LHT]|LT],[LH|LHT]):- highest(LT,[TH|_]),LH>TH.
%highest([[LH|_]|LT],[TH|TL]):- highest(LT,[TH|TL]),LH=<TH.

% (+,+,-,-) gives the best position for a stone
% not longer used (use my_best_position instead)
%bestposition(Field,Stone,XPos,YPos):- all_positions(Field,Stone,Z),highest(Z,[_|[XPos|[YPos|_]]]).

