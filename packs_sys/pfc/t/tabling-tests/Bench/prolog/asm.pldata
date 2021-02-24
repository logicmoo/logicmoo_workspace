%   File   : asm.pl
%   Author : Neng-Fa ZHOU
%   Completed October 1993
%   Updated: Febuary 1994
%   Purpose: Assembler of NTOAM

/* asm.pl
   this file translates instructions and symbols into byte code */


:-determinate([cmp_error/1,
	       name/2,
	       length/2,
	       asm_hash_value/2]).

asm_bp(Infile,Outfile) :- 
    true :
    global_set('$asm_bp',0,1),
    see(Infile),
    asm_getaslist(Insts),
    seen,
    tell(Outfile),
    asm0(Insts,Prog,Index,0,NIndex),
    functor(Psctable,csym,255),   
    functor(Labeltable,lsym,255),
    global_create('$sym_no',0,0),
%    output_mess('==>asm_pass1'),
    asm_pass1_bp(Prog,Index,Psctable,Labeltable,Ntext,Npsc),
    asm_magic(3),
    asm_putnum(Npsc, 4), 
    asm_putnum(Ntext, 4), 
    asm_putnum(NIndex, 4),
%    output_mess('==>asm_pass2'),
    asm_pass2(Prog, Index,Psctable, Labeltable),
    asm_mark_eot,
    global_del('$asm_bp',0),
    told.

asm(Infile,Outfile) :- 
    true :
    see(Infile),
    asm_getaslist(Insts),
    seen,
    tell(Outfile),
    asm0(Insts,Prog,Index,0,NIndex),
    functor(Psctable,csym,255),   
    functor(Labeltable,lsym,255),
    global_create('$sym_no',0,0),
%    output_mess('==>asm_pass1'),
    asm_pass1(Prog,Index,Psctable,Labeltable,Ntext,Npsc),
    asm_magic(3),
    asm_putnum(Npsc, 4), 
    asm_putnum(Ntext, 4), 
    asm_putnum(NIndex, 4),
%    output_mess('==>asm_pass2'),
    asm_pass2(Prog, Index,Psctable, Labeltable),
    asm_mark_eot,
    told.

asm_getaslist(Insts):-
    true :
    read(Inst),
    asm_getaslist1(Inst,Insts).

asm_getaslist1(end_of_file,Insts):-
    true :
    Insts:=[].
asm_getaslist1(Inst,Insts):-
    true :
    Insts:=[Inst|Insts1],
    read(Inst1),
    asm_getaslist1(Inst1,Insts1).

asm(Insts):-
    true :
    asm0(Insts,Prog,Index,0,NIndex),
    functor(Psctable,csym,255),   
    functor(Labeltable,lsym,255),
    global_create('$sym_no',0,0),
%    output_mess('==>asm_pass1'),
    asm_pass1(Prog,Index,Psctable,Labeltable,Ntext,Npsc),
    asm_magic(3),
    asm_putnum(Npsc, 4), 
    asm_putnum(Ntext, 4), 
    asm_putnum(NIndex, 4),
%    output_mess('==>asm_pass2'),
    asm_pass2(Prog, Index,Psctable, Labeltable),
    asm_mark_eot.

asm0([],Prog,Index,NIndex0,NIndex):-
    true :
    Prog:=[],
    Index:=[],
    NIndex:=NIndex0.
asm0([Inst|Insts],Prog,Index,NIndex0,NIndex):-
    asm_index_inst(Inst,Size) :
    NIndex1 is NIndex0+Size,
    Index = [Inst|Index1],
    asm0(Insts,Prog,Index1,NIndex1,NIndex).
asm0([Inst|Insts],Prog,Index,NIndex0,NIndex):-
    true :
    Prog:=[Inst|Prog1],
    asm0(Insts,Prog1,Index,NIndex0,NIndex).

asm_pass1(AsmInsts, Index,Csym, Lsym, Ntext, Npsc) :-
    true :
    asm_pass11(AsmInsts,Lsym,Csym,0,Ntext),
    asm_index_pass1(Index,Csym),
    asmpass1_fillin(Lsym,255,Csym ),
    asmpass1_setundef( Csym,255,0,Npsc).

asm_pass1_bp(AsmInsts, Index,Csym, Lsym, Ntext, Npsc) :-
    true :
    asm_pass11(AsmInsts,Lsym,Csym,0,Ntext),
    asm_index_pass1(Index,Csym),
    asmpass1_fillin_bp(Lsym,255,Csym ),
    asmpass1_setundef( Csym,255,0,Npsc).

asm_pass11([],_,_,Lc,NLc):-
    true : NLc:=Lc.
asm_pass11([Inst| Rest], Lsym, Csym, Lc, NLc):-
    label(X)<=Inst,
    lab_member1(lab(X, Lc), Lsym) :
    asm_pass11(Rest, Lsym, Csym, Lc, NLc).
asm_pass11([Inst| Rest], Lsym, Csym, Lc, NLc):-
    label(X)<=Inst :
    error_double_define(X),
    asm_pass11(Rest, Lsym, Csym, Lc, NLc).
asm_pass11([Inst| Rest], Lsym, Csym, Lc, NLc):-
     true :
     asm_pass12(Inst,Csym,N),
     Lc0 is Lc + N,
     asm_pass11(Rest, Lsym, Csym, Lc0, NLc).

error_double_define((Pred,Arity,_)):-
    true : true.
error_double_define((Pred,Arity)):-
    true :
    cmp_error(['The predicate ', Pred/Arity, ' is doubly defined']).

asm_index_pass1([],_):-true : true.
asm_index_pass1([pred(_,_,_,_)|Rest], Csym) :- 
    true : asm_index_pass1(Rest, Csym).
asm_index_pass1([arglabel(T,Val,Label)|Rest],Csym) :-
    T == c :
    sym_member1(sym(Val, 0, _,_), Csym),
    asm_index_pass1(Rest, Csym).
asm_index_pass1([arglabel(T,(Str,Ar),Label)|Rest],Csym) :-
    T == s :
    sym_member1(sym(Str, Ar, _,_), Csym),
    asm_index_pass1(Rest, Csym).
asm_index_pass1([arglabel(T,Val,Label)|Rest],Csym) :-
    true :
    asm_index_pass1(Rest,Csym).

/* Fill in the values of any predicates which are defined within this module.*/
asmpass1_fillin(Lsym,N,_):-
    N<1 :
    true.
asmpass1_fillin(Lsym,N,Csym):-
    true :
    arg(N,Lsym,L),
    asmpass1_fillin(L,Csym),
    N1 is N-1,
    asmpass1_fillin(Lsym,N1,Csym).
    
asmpass1_fillin(L, _):-var(L) : true.
asmpass1_fillin([lab((_,_,_),LcValue)|Rest], Table) :-
    true :
    asmpass1_fillin(Rest, Table).
asmpass1_fillin([lab((Name, Arity),LcValue)|Rest],Table) :-
    true :
    sym_member1(sym(Name, Arity, LcValue,_), Table),
    asmpass1_fillin(Rest, Table).


asmpass1_fillin_bp(Lsym,N,_):-
    N<1 :
    true.
asmpass1_fillin_bp(Lsym,N,Csym):-
    true :
    arg(N,Lsym,L),
    asmpass1_fillin_bp(L,Csym),
    N1 is N-1,
    asmpass1_fillin_bp(Lsym,N1,Csym).
    
asmpass1_fillin_bp(L, _):-var(L) : true.
asmpass1_fillin_bp([lab((_,_,_),LcValue)|Rest], Table) :-
    true :
    asmpass1_fillin_bp(Rest, Table).
asmpass1_fillin_bp([lab((Name, Arity),LcValue)|Rest],Table) :-
    predefined(Name,Arity) :
    sym_member1(sym(Name, Arity, LcValue,_), Table),
    asmpass1_fillin_bp(Rest, Table).
asmpass1_fillin_bp([lab((Name, Arity),LcValue)|Rest],Table) :-
    true :
    asmpass1_fillin_bp(Rest, Table).

/*
asmpass1_fillin([lab((Name, Arity),LcValue)|Rest],Table) :-
    true :
    cmp_error(['the predicate ', Name/Arity, ' is doubly defined']),
    asmpass1_fillin(Rest, Table).
*/
/* Fill in the values of any symbols which have not been defined
   with the value -2. */
asmpass1_setundef(Csym,N,S0,S):-
    N<1 :
    S=S0.
asmpass1_setundef(Csym,N,S0,S):-
    true :
    arg(N,Csym,L),
    asmpass1_setundef(L,S0,S1),
    N1 is N-1,
    asmpass1_setundef(Csym,N1,S1,S).

asmpass1_setundef(Tab, S0,S):-
    var(Tab) :
    S=S0.
asmpass1_setundef([sym(Pred, Arity,Val,_)|Rest], S0,S):-
    var(Val) :
    Val := -2,
    b_GET_LENGTH_cf(Pred,L),
    S1 is S0+L+6,
    asmpass1_setundef(Rest, S1,S).
asmpass1_setundef([sym(Pred, Arity, Val,_)|Rest],S0,S) :-
    true :
    b_GET_LENGTH_cf(Pred,L),
    S1 is S0+L+6,
    asmpass1_setundef(Rest, S1,S).

:-mode asm_pass12(c,d,f).
asm_pass12( label(_),                        _,  0).

/* Conditional Jump */
asm_pass12( jmp(_),                          _,  2).
asm_pass12( jmpn_eq_struct_x(_,(S,N),_,_),Csym,  5):-sym_member1(sym(S,N,_,_),Csym).
asm_pass12( jmpn_eq_struct_y(_,(S,N),_,_),Csym,  5):-sym_member1(sym(S,N,_,_),Csym).
asm_pass12( jmpn_eq_atom_x(_,C,_,_),      Csym,  5):-sym_member1(sym(C,0,_,_),Csym).
asm_pass12( jmpn_eq_atom_y(_,C,_,_),      Csym,  5):-sym_member1(sym(C,0,_,_),Csym).
asm_pass12( jmpn_nil_x(_,_,_),               _,  4).
asm_pass12( jmpn_nil_y(_,_,_),               _,  4).
asm_pass12( switch_list_x(_,_,_,_),          _,  5).
asm_pass12( switch_list_y(_,_,_,_),          _,  5).
asm_pass12( switch_list_yxx(_,_,_,_),        _,  5).
asm_pass12( switch_list_yxy(_,_,_,_),        _,  5).
asm_pass12( switch_list_yyx(_,_,_,_),        _,  5).
asm_pass12( switch_list_yyy(_,_,_,_),        _,  5).
asm_pass12( jmpn_eq_int_x(_,_,_,_),         _,   5).
asm_pass12( jmpn_eq_int_y(_,_,_,_),         _,   5).
asm_pass12( jmpn_eql(_,_,_),                 _,  4).
asm_pass12( jmp_eql(_,_,_),                _,    4).
asm_pass12( jmp_eql_yy(_,_,_),             _,    4).
asm_pass12( jmpn_gt(_,_,_),                _,    4).
asm_pass12( jmpn_gt_yy(_,_,_),             _,    4).
asm_pass12( jmpn_ge(_,_,_),               _,     4).
asm_pass12( jmpn_ge_yy(_,_,_),            _,     4).
asm_pass12( jmpn_id(_,_,_),               _,     4).
asm_pass12( jmp_id(_,_,_),                _,     4).
asm_pass12( jmpn_var_x(_,_),               _,    3).
asm_pass12( jmpn_var_y(_,_),               _,    3).
asm_pass12( jmp_var_x(_,_),                _,    3).
asm_pass12( jmp_var_y(_,_),                _,    3).
asm_pass12( jmpn_atom_x(_,_),              _,    3).
asm_pass12( jmpn_atom_y(_,_),              _,    3).
asm_pass12( jmpn_atomic_x(_,_),            _,    3).
asm_pass12( jmpn_atomic_y(_,_),            _,    3).
asm_pass12( jmpn_num_x(_,_),               _,    3).
asm_pass12( jmpn_num_y(_,_),               _,    3).
asm_pass12( jmpn_int_x(_,_),               _,    3).
asm_pass12( jmpn_int_y(_,_),               _,    3).
asm_pass12( jmpn_float_x(_,_),             _,    3).
asm_pass12( jmpn_float_y(_,_),             _,    3).
asm_pass12( hash_jmpn_nil(_),              _,    2).
asm_pass12( hash_jmpn_list(_),             _,    2).
asm_pass12( hash_jmpn_int(_,_),            _,    3).
asm_pass12( hash_jmpn_atom(C,_),        Csym,    3):-sym_member1(sym(C,0,_,_),Csym).
asm_pass12( hash_jmpn_struct((S,N),_),  Csym,    3):-sym_member1(sym(S,N,_,_),Csym).
asm_pass12( hash_jmpn_struct_x((S,N),_),Csym,    3):-sym_member1(sym(S,N,_,_),Csym).
asm_pass12( hash_jmpn_struct_y((S,N),_),Csym,    3):-sym_member1(sym(S,N,_,_),Csym).
asm_pass12( hash_jmpn_struct_xx((S,N),_),Csym,   3):-sym_member1(sym(S,N,_,_),Csym).
asm_pass12( hash_jmpn_struct_xy((S,N),_),Csym,   3):-sym_member1(sym(S,N,_,_),Csym).
asm_pass12( hash_jmpn_struct_yx((S,N),_),Csym,   3):-sym_member1(sym(S,N,_,_),Csym).
asm_pass12( hash_jmpn_struct_yy((S,N),_),Csym,   3):-sym_member1(sym(S,N,_,_),Csym).

/* Unify */
asm_pass12( unify_struct_x(_,(S,N)),     Csym,   3):-sym_member1(sym(S,N,_,_),Csym).
asm_pass12( unify_struct_y(_,(S,N)),     Csym,   3):-sym_member1(sym(S,N,_,_),Csym).
asm_pass12( unify_list_x(_),               _,    2).
asm_pass12( unify_list_y(_),               _,    2).
asm_pass12( unify_nil_x(_),                _,    2).
asm_pass12( unify_nil_y(_),                _,    2).
asm_pass12( unify_atom_x(_,C),          Csym,    3):-sym_member1(sym(C,0,_,_),Csym).
asm_pass12( unify_atom_y(_,C),          Csym,    3):-sym_member1(sym(C,0,_,_),Csym).
asm_pass12( unify_int_x(_,_),              _,    3).
asm_pass12( unify_int_y(_,_),              _,    3).
asm_pass12( unify_ux_ux(_,_),              _,    3).
asm_pass12( unify_ux_uy(_,_),              _,    3).
asm_pass12( unify_uy_uy(_,_),              _,    3).
asm_pass12( unify_cons_x(_,_),             _,    3).
asm_pass12( unify_cons_y(_,_),             _,    3).

asm_pass12( fork_unify_struct_y(_,(S,N),_), Csym,  4):-sym_member1(sym(S,N,_,_),Csym).
asm_pass12( fork_unify_list_y(_,_),         _,    3).
asm_pass12( fork_unify_nil_y(_,_),          _,    3).
asm_pass12( fork_unify_atom_y(_,C,_),    Csym,    4):-sym_member1(sym(C,0,_,_),Csym).
asm_pass12( fork_unify_int_y(_,_,_),        _,    4).
asm_pass12( fork_unify_ux_uy(_,_,_),        _,    4).
asm_pass12( fork_unify_uy_uy(_,_,_),        _,    4).

asm_pass12( fork_unicut_struct_y(_,(S,N),_), Csym,  4):-sym_member1(sym(S,N,_,_),Csym).
asm_pass12( fork_unicut_list_y(_,_),         _,    3).
asm_pass12( fork_unicut_nil_y(_,_),          _,    3).
asm_pass12( fork_unicut_atom_y(_,C,_),    Csym,    4):-sym_member1(sym(C,0,_,_),Csym).
asm_pass12( fork_unicut_int_y(_,_,_),        _,    4).
asm_pass12( fork_unicut_ux_uy(_,_,_),        _,    4).
asm_pass12( fork_unicut_uy_uy(_,_,_),        _,    4).


asm_pass12( unify0_struct_y(_,(S,N)),     Csym,   3):-sym_member1(sym(S,N,_,_),Csym).
asm_pass12( unify0_list_y(_),               _,    2).
asm_pass12( unify0_nil_y(_),                _,    2).
asm_pass12( unify0_atom_y(_,C),          Csym,    3):-sym_member1(sym(C,0,_,_),Csym).
asm_pass12( unify0_int_y(_,_),              _,    3).
asm_pass12( unify0_ux_uy(_,_),              _,    3).
asm_pass12( unify0_uy_uy(_,_),              _,    3).
asm_pass12( unicut_struct_y(_,(S,N)),     Csym,   3):-sym_member1(sym(S,N,_,_),Csym).
asm_pass12( unicut_list_y(_),               _,    2).
asm_pass12( unicut_nil_y(_),                _,    2).
asm_pass12( unicut_atom_y(_,C),          Csym,    3):-sym_member1(sym(C,0,_,_),Csym).
asm_pass12( unicut_int_y(_,_),              _,    3).
asm_pass12( unicut_uy_uy(_,_),              _,    3).
asm_pass12( unicut,                          _,    1).

/* Unify argument */
asm_pass12( unify_arg_nil,                 _,    1).
asm_pass12( unify_arg_atom(C),          Csym,    2):-sym_member1(sym(C,0,_,_),Csym).
asm_pass12( unify_arg_int(_),              _,    2).
asm_pass12( unify_arg_ux_ux(_,_),          _,    3).
asm_pass12( unify_arg_ux(_),               _,    2).
asm_pass12( unify_arg_ux_vy(_,_),          _,    3).
asm_pass12( unify_arg_ux_vx(_,_),          _,    3).
asm_pass12( unify_arg_uy_uy(_,_),          _,    3).
asm_pass12( unify_arg_uy(_),               _,    2).
asm_pass12( unify_arg_vx(_),               _,    2).
asm_pass12( unify_arg_vy(_),               _,    2).
asm_pass12( unify_arg_list,                _,    1).
asm_pass12( unify_arg_struct((S,N)),    Csym,    2):-sym_member1(sym(S,N,_,_),Csym).
asm_pass12( unify_arg_void_one,            _,    1).
asm_pass12( unify_arg_void(_),             _,    2).
asm_pass12( unify_arg_wy(_),               _,    2).
asm_pass12( unify_arg_vx_vx(_,_),          _,    3).
asm_pass12( unify_arg_vx_vy(_,_),          _,    3).
asm_pass12( unify_arg_vx_ux(_,_),          _,    3).
asm_pass12( unify_arg_vx_uy(_,_),          _,    3).
asm_pass12( unify_arg_vy_vx(_,_),          _,    3).
asm_pass12( unify_arg_vy_vy(_,_),          _,    3).
asm_pass12( unify_arg_vy_ux(_,_),          _,    3).
asm_pass12( unify_arg_vy_uy(_,_),          _,    3).
asm_pass12( unify_arg_iii(_,_,_),          _,    4).

/* Move */
asm_pass12( move_struct_x(_,(S,N)),      Csym,   3):-sym_member1(sym(S,N,_,_),Csym).
asm_pass12( move_struct_y(_,(S,N)),      Csym,   3):-sym_member1(sym(S,N,_,_),Csym).
asm_pass12( move_list_x(_),                 _,   2).
asm_pass12( move_list_y(_),                 _,   2).
asm_pass12( move_nil_x(_),                  _,   2).
asm_pass12( move_nil_y(_),                  _,   2).
asm_pass12( move_atom_x(_,C),            Csym,   3):-sym_member1(sym(C,0,_,_),Csym).
asm_pass12( move_atom_y(_,C),            Csym,   3):-sym_member1(sym(C,0,_,_),Csym).
asm_pass12( move_int_x(_,_),                _,   3).
asm_pass12( move_int_y(_,_),                _,   3).
asm_pass12( move_x_ux(_,_),                 _,   3).
asm_pass12( move_x_uy(_,_),                 _,   3).
asm_pass12( move_y_ux(_,_),                 _,   3).
asm_pass12( move_y_uy(_,_),                 _,   3).
asm_pass12( move_vx(_),                     _,   2).
asm_pass12( move_vy(_),                     _,   2).
asm_pass12( move_x_wy(_,_),                 _,   3).
asm_pass12( move_y_wy(_,_),                 _,   3).
asm_pass12( move_yy_yw(_,_,_,_),            _,   5).
asm_pass12( move_yw_yy(_,_,_,_),            _,   5).
asm_pass12( move_yy_yy(_,_,_,_),            _,   5).
asm_pass12( move_yy_yy_yy(_,_,_,_,_,_),     _,   7).

/* Numeric */
asm_pass12( and(_,_,_),                     _,    4).
asm_pass12( or(_,_,_),                      _,    4).
asm_pass12( lshiftl(_,_,_),                 _,    4).
asm_pass12( lshiftr(_,_,_),                 _,    4).
asm_pass12( complement(_,_),                _,    3).
asm_pass12( add(_,_,_),                     _,    4).
asm_pass12( add1_y(_),                      _,    2).
asm_pass12( sub(_,_,_),                     _,    4).
asm_pass12( sub1_y(_),                      _,    2).
asm_pass12( mul(_,_,_),                     _,    4).
asm_pass12( div(_,_,_),                     _,    4).
asm_pass12( idiv(_,_,_),                    _,    4).
asm_pass12( mod(_,_,_),                     _,    4).

/*Parameter passing */
asm_pass12( para_struct((S,N)),          Csym,    2):-sym_member1(sym(S,N,_,_),Csym).
asm_pass12( para_list,                      _,    1).
asm_pass12( para_nil,                       _,    1).
asm_pass12( para_atom(C),                Csym,    2):-sym_member1(sym(C,0,_,_),Csym).
asm_pass12( para_int(_),                    _,    2).
asm_pass12( para_ux(_),                     _,    2).
asm_pass12( para_uy(_),                     _,    2).
asm_pass12( para_vx(_),                     _,    2).
asm_pass12( para_vy(_),                     _,    2).
asm_pass12( para_void_one,                  _,    1).
asm_pass12( para_void(_),                   _,    2).
asm_pass12( para_vy_vy(_,_),                _,    3).
asm_pass12( para_vy_ux(_,_),                _,    3).
asm_pass12( para_vy_uy(_,_),                _,    3).
asm_pass12( para_ux_vy(_,_),                _,    3).
asm_pass12( para_ux_ux(_,_),                _,    3).
asm_pass12( para_ux_uy(_,_),                _,    3).
asm_pass12( para_uy_vy(_,_),                _,    3).
asm_pass12( para_uy_ux(_,_),                _,    3).
asm_pass12( para_uy_uy(_,_),                _,    3).
asm_pass12( para_ux_ux_ux(_,_,_),           _,    4).
asm_pass12( para_ux_ux_uy(_,_,_),           _,    4).
asm_pass12( para_ux_uy_ux(_,_,_),           _,    4).
asm_pass12( para_ux_uy_uy(_,_,_),           _,    4).
asm_pass12( para_uy_ux_ux(_,_,_),           _,    4).
asm_pass12( para_uy_ux_uy(_,_,_),           _,    4).
asm_pass12( para_uy_uy_ux(_,_,_),           _,    4).
asm_pass12( para_uy_uy_uy(_,_,_),           _,    4).
asm_pass12( para_uy_uy_uy_uy(_,_,_,_),      _,    5).

/* Procedural */
asm_pass12( call((P,N)),               Csym,    2):-asm_pass12_call(P,N,Csym).
asm_pass12(callv(_),                        _,    2).
asm_pass12( execute((P,N)),              Csym,  2):-asm_pass12_call(P,N,Csym).
asm_pass12(executev(_),                     _,    2).
asm_pass12( return_a,                       _,    1).
asm_pass12( return_b,                       _,    1).
asm_pass12( jmpn_det(L),                    _,    2).
asm_pass12( save_ht_jmp(_,_),               _,    3).

/* Allocate */
asm_pass12( allocate_flat(N),               _,    2).
asm_pass12( allocate_nonflat(N),            _,    2).
asm_pass12( allocate_nondet(N),             _,    2).
asm_pass12( flat_to_nondet(N),              _,    2).

/* Backtracking */
asm_pass12( fail,                           _,    1).
asm_pass12( fail0,                          _,    1).

asm_pass12( fork(_),                        _,    2).

asm_pass12( commit,                         _,    1).
asm_pass12( cut,                            _,    1).
asm_pass12( cut_fail,                       _,    1).
asm_pass12( cut_return,                     _,    1).
asm_pass12( save_b,                         _,    1).
asm_pass12(getbreg(_),                      _,    2).
asm_pass12(getpbreg(_),                     _,    2).
asm_pass12(putbreg(_),                      _,    2).

/* Hashing */
asm_pass12( hash_x(_,_),                    _,    5).
asm_pass12( hash_y(_,_),                    _,    5).
asm_pass12(tabsize(_),                      _,    2).

/* builtin */
asm_pass12( arg(_,_,_),                     _,    4).
asm_pass12( arg0(_,_,_),                    _,    4).
asm_pass12( setarg(_,_,_),                  _,    4).
asm_pass12( setarg0(_,_,_),                 _,    4).
asm_pass12(functor(_,_,_),                  _,    4).
asm_pass12(builtin0(_,_),                   _,    3).
asm_pass12(builtin1(_,_,_),                 _,    4).
asm_pass12(builtin2(_,_,_,_),               _,    5).
asm_pass12(builtin3(_,_,_,_,_),             _,    6).
asm_pass12(builtin4(_,_,_,_,_,_),           _,    7).
asm_pass12(func_arity(_,_),                  _,   3).

/* Miscellaneous */
asm_pass12( halt,                           _,    1).
asm_pass12( endfile,                        _,    1).
asm_pass12( gethtreg(_,_),                  _,    3).
asm_pass12( puthtreg(_,_),                  _,    3).
asm_pass12( endfile,                        _,    1).
asm_pass12( get_ar_cps,                 _,    1).
asm_pass12( put_ar_cps(_),              _,    2).
asm_pass12( move_ar_cps(_),             _,    2).
asm_pass12( jmpn_det_get_ar_cps(L),     _,    2).
asm_pass12( allocate_susp(N),           _,    2).
asm_pass12( susp_var_x(_),              _,    2).
asm_pass12( susp_var_y(_),              _,    2).
asm_pass12( delay((S,N),_),          Csym,    3):-sym_member1(sym(S,N,_,_),Csym).	
asm_pass12( susp_var_delay(_,(S,N),_,_),Csym, 5):-sym_member1(sym(S,N,_,_),Csym).
asm_pass12( end_delay,                  _,    1).

asm_pass12( nondet(_),                  _,    2).
asm_pass12( jmp_susp(_),                _,    2).
asm_pass12( jmpn_dvar_y(_,_),           _,    3).
asm_pass12( susp_var2_delay(_,(S,N),_,_),Csym,5):-sym_member1(sym(S,N,_,_),Csym).
asm_pass12( domain_set_false_yy(_,_),      _,    3).
asm_pass12( domain_set_false_yx(_,_),      _,    3).
asm_pass12_call(P,N,Csym):-
    isglobal('$asm_bp',0),
    b_GET_SYM_TYPE_ccf(P,N,Type),
    Type =\= 3 :     /* no a call to a c function */
    true.
asm_pass12_call(P,N,Csym):-
    true :
    sym_member1(sym(P,N,_,_),Csym).

/**/
/*
asm_pass12( Junk,_,N) :- 
    true :
    N=0,
    cmp_error(['*** Error in assembly: unknown opcode: ',Junk]).
*/
/******************* Pass 2 **************************************/
asm_pass2(Prog,Index,Csym,Lsym) :-
    true :
    asm_rearange_csym(Csym,255,_,Csym1),
    asm_symbol(Csym1),
    asm_pass2a(Prog,Csym,Lsym),
    asm_index(Index,Csym,Lsym).

asm_index([],_,_):-true : true.
asm_index([Inst|Index],Csym,Lsym) :-
    true :
    asm_proc_index(Inst, Csym, Lsym),
    asm_index(Index,Csym,Lsym).

asm_proc_index(pred(HashLab,Op,Num,Alt), Csym, Lsym) :-
    true :
    x_or_y(Op,XY),
    asm_lookup_ltab(HashLab,Lsym,Val1),
    asm_lookup_ltab(Alt,Lsym,Val2),
    b_ASPN4_cccc(Val1,XY,Num,Val2).
asm_proc_index(arglabel(T,Val,Label), Csym, Lsym) :-
    T == c :
    asm_lookup_ctab(Val,0,Csym,Nval),
    asm_lookup_ltab(Label, Lsym,L),
    writename(T),b_ASPN2_cc(Nval,L).
asm_proc_index(arglabel(T,Val,Label), Csym, Lsym) :-
    T == s :
    Val = (Str, Arity),
    asm_lookup_ctab(Str, Arity,Csym, Nval),
    asm_lookup_ltab(Label,Lsym, L),
    writename(T),b_ASPN2_cc(Nval,L).
asm_proc_index(arglabel(T,Val,Label), Csym, Lsym) :-
    true :
    asm_lookup_ltab(Label, Lsym, L),
    writename(T),b_ASPN2_cc(Val,L).

asm_pass2a([],_,_):-true : true.
asm_pass2a([Inst|Insts],Csym,Lsym) :-
    true :
    asm_pass2_inst(Inst,Csym,Lsym),
    asm_pass2a(Insts,Csym,Lsym).


/*
asm_pass2_inst(X,_,_) :-
    true ?
	output_mess(X),
    fail.
*/
asm_pass2_inst(label(_),_,_) :-
    true : true.
asm_pass2_inst(call(Lab),Csym,Lsym) :-
    asm_lookup_ltab(Lab,Lsym,EPaddr) :
    opcode(call_d,X),
    b_ASPN2_cc(X,EPaddr).
asm_pass2_inst(call((P,N)),Csym,Lsym) :-
    true :
	(asm_lookup_ctab(P,N,Csym,Index) -> 
	 opcode(call,X),
	 b_ASPN2_cc(X,Index);
	 warning([P/N,' is called but not defined'])).
asm_pass2_inst(execute(Lab),Csym,Lsym) :-
    asm_lookup_ltab(Lab,Lsym,EPaddr) :
    opcode(djmp,X),
    b_ASPN2_cc(X,EPaddr).
asm_pass2_inst(execute((P,N)),Csym,Lsym) :-
    true :
	(asm_lookup_ctab(P,N,Csym,Index) ->
	 opcode(execute,X),
	 b_ASPN2_cc(X,Index);
	 warning([P/N,' is called but not defined'])).
asm_pass2_inst(jmp(L),Csym,Lsym) :-
    asm_lookup_ltab(L,Lsym,Val) :
    opcode(jmp,X),
    b_ASPN2_cc(X,Val).
asm_pass2_inst(jmp_susp(L),Csym,Lsym) :-
    asm_lookup_ltab(L,Lsym,Val) :
    opcode(jmp_susp,X),
    b_ASPN2_cc(X,Val).
asm_pass2_inst(jmpn_eq_struct_x(Op,(S,N),L1,L2),Csym,Lsym) :-
    true :
    asm_lookup_ctab(S,N,Csym,I),
    asm_lookup_ltab(L1,Lsym,Val1),
    asm_lookup_ltab(L2,Lsym,Val2),
    opcode(jmpn_eq_struct_x,X),
    b_ASPN4_cccc(X,Op,I,Val1),b_ASPN_c(Val2).
asm_pass2_inst(jmpn_eq_struct_y(Op,(S,N),L1,L2),Csym,Lsym) :-
    true :
    asm_lookup_ctab(S,N,Csym,I),
    asm_lookup_ltab(L1,Lsym,Val1),
    asm_lookup_ltab(L2,Lsym,Val2),
    opcode(jmpn_eq_struct_y,X),
    b_ASPN4_cccc(X,Op,I,Val1),b_ASPN_c(Val2).
asm_pass2_inst(jmpn_eq_atom_x(Op,C,L1,L2),Csym,Lsym) :-
    true :
    asm_lookup_ctab(C,0,Csym,I),
    asm_lookup_ltab(L1,Lsym,Val1),
    asm_lookup_ltab(L2,Lsym,Val2),
    opcode(jmpn_eq_atom_x,X),
    b_ASPN4_cccc(X,Op,I,Val1),b_ASPN_c(Val2).
asm_pass2_inst(jmpn_eq_atom_y(Op,C,L1,L2),Csym,Lsym) :-
    true :
    asm_lookup_ctab(C,0,Csym,I),
    asm_lookup_ltab(L1,Lsym,Val1),
    asm_lookup_ltab(L2,Lsym,Val2),
    opcode(jmpn_eq_atom_y,X),
    b_ASPN4_cccc(X,Op,I,Val1),b_ASPN_c(Val2).
asm_pass2_inst(jmpn_nil_x(Op,L1,L2),Csym,Lsym) :-
    true :
    asm_lookup_ltab(L1,Lsym,Val1),
    asm_lookup_ltab(L2,Lsym,Val2),
    opcode(jmpn_nil_x,X),
    b_ASPN4_cccc(X,Op,Val1,Val2).
asm_pass2_inst(jmpn_nil_y(Op,L1,L2),Csym,Lsym) :-
    true :
    asm_lookup_ltab(L1,Lsym,Val1),
    asm_lookup_ltab(L2,Lsym,Val2),
    opcode(jmpn_nil_y,X),
    b_ASPN4_cccc(X,Op,Val1,Val2).
asm_pass2_inst(switch_list_x(Op,L1,L2,L3),Csym,Lsym) :-
    true :
    asm_lookup_ltab(L1,Lsym,Val1),
    asm_lookup_ltab(L2,Lsym,Val2),
    asm_lookup_ltab(L3,Lsym,Val3),
    opcode(switch_list_x,X),
    b_ASPN4_cccc(X,Op,Val1,Val2),b_ASPN_c(Val3).
asm_pass2_inst(switch_list_y(Op,L1,L2,L3),Csym,Lsym) :-
    true :
    asm_lookup_ltab(L1,Lsym,Val1),
    asm_lookup_ltab(L2,Lsym,Val2),
    asm_lookup_ltab(L3,Lsym,Val3),
    opcode(switch_list_y,X),
    b_ASPN4_cccc(X,Op,Val1,Val2),b_ASPN_c(Val3).
asm_pass2_inst(switch_list_yxx(Op,L1,L2,L3),Csym,Lsym) :-
    true :
    asm_lookup_ltab(L1,Lsym,Val1),
    asm_lookup_ltab(L2,Lsym,Val2),
    asm_lookup_ltab(L3,Lsym,Val3),
    opcode(switch_list_yxx,X),
    b_ASPN4_cccc(X,Op,Val1,Val2),b_ASPN_c(Val3).
asm_pass2_inst(switch_list_yxy(Op,L1,L2,L3),Csym,Lsym) :-
    true :
    asm_lookup_ltab(L1,Lsym,Val1),
    asm_lookup_ltab(L2,Lsym,Val2),
    asm_lookup_ltab(L3,Lsym,Val3),
    opcode(switch_list_yxy,X),
    b_ASPN4_cccc(X,Op,Val1,Val2),b_ASPN_c(Val3).
asm_pass2_inst(switch_list_yyx(Op,L1,L2,L3),Csym,Lsym) :-
    true :
    asm_lookup_ltab(L1,Lsym,Val1),
    asm_lookup_ltab(L2,Lsym,Val2),
    asm_lookup_ltab(L3,Lsym,Val3),
    opcode(switch_list_yyx,X),
    b_ASPN4_cccc(X,Op,Val1,Val2),b_ASPN_c(Val3).
asm_pass2_inst(switch_list_yyy(Op,L1,L2,L3),Csym,Lsym) :-
    true :
    asm_lookup_ltab(L1,Lsym,Val1),
    asm_lookup_ltab(L2,Lsym,Val2),
    asm_lookup_ltab(L3,Lsym,Val3),
    opcode(switch_list_yyy,X),
    b_ASPN4_cccc(X,Op,Val1,Val2),b_ASPN_c(Val3).
asm_pass2_inst(jmpn_eq_int_x(Op,I,L1,L2),Csym,Lsym) :-
    true :
    asm_lookup_ltab(L1,Lsym,Val1),
    asm_lookup_ltab(L2,Lsym,Val2),
    opcode(jmpn_eq_int_x,X),
    b_ASPN4_cccc(X,Op,I,Val1),b_ASPN_c(Val2).
asm_pass2_inst(jmpn_eq_int_y(Op,I,L1,L2),Csym,Lsym) :-
    true :
    asm_lookup_ltab(L1,Lsym,Val1),
    asm_lookup_ltab(L2,Lsym,Val2),
    opcode(jmpn_eq_int_y,X),
    b_ASPN4_cccc(X,Op,I,Val1),b_ASPN_c(Val2).
asm_pass2_inst(jmpn_eql(Op1,Op2,L),Csym,Lsym) :-
    true :
    asm_lookup_ltab(L,Lsym,Val),
    x_or_y(Op1,XY1),
    x_or_y(Op2,XY2),
    opcode(jmpn_eql,X),
    b_ASPN4_cccc(X,XY1,XY2,Val).
asm_pass2_inst(jmp_eql(Op1,Op2,L),Csym,Lsym) :-
    true :
    asm_lookup_ltab(L,Lsym,Val),
    x_or_y(Op1,XY1),
    x_or_y(Op2,XY2),
    opcode(jmp_eql,X),
    b_ASPN4_cccc(X,XY1,XY2,Val).
asm_pass2_inst(jmp_eql_yy(Op1,Op2,L),Csym,Lsym) :-
    true :
    asm_lookup_ltab(L,Lsym,Val),
    opcode(jmp_eql_yy,X),
    b_ASPN4_cccc(X,Op1,Op2,Val).
asm_pass2_inst(jmpn_gt(Op1,Op2,L),Csym,Lsym) :-
    true :
    asm_lookup_ltab(L,Lsym,Val),
    x_or_y(Op1,XY1),
    x_or_y(Op2,XY2),
    opcode(jmpn_gt,X),
    b_ASPN4_cccc(X,XY1,XY2,Val).
asm_pass2_inst(jmpn_gt_yy(Op1,Op2,L),Csym,Lsym) :-
    true :
    asm_lookup_ltab(L,Lsym,Val),
    opcode(jmpn_gt_yy,X),
    b_ASPN4_cccc(X,Op1,Op2,Val).
asm_pass2_inst(jmpn_ge(Op1,Op2,L),Csym,Lsym) :-
    true :
    asm_lookup_ltab(L,Lsym,Val),
    x_or_y(Op1,XY1),
    x_or_y(Op2,XY2),
    opcode(jmpn_ge,X),
    b_ASPN4_cccc(X,XY1,XY2,Val).
asm_pass2_inst(jmpn_ge_yy(Op1,Op2,L),Csym,Lsym) :-
    true :
    asm_lookup_ltab(L,Lsym,Val),
    opcode(jmpn_ge_yy,X),
    b_ASPN4_cccc(X,Op1,Op2,Val).
asm_pass2_inst(jmpn_id(Op1,Op2,L),Csym,Lsym) :-
    true :
    asm_lookup_ltab(L,Lsym,Val),
    x_or_y(Op1,XY1),
    x_or_y(Op2,XY2),
    opcode(jmpn_id,X),
    b_ASPN4_cccc(X,XY1,XY2,Val).
asm_pass2_inst(jmp_id(Op1,Op2,L),Csym,Lsym) :-
    true :
    asm_lookup_ltab(L,Lsym,Val),
    x_or_y(Op1,XY1),
    x_or_y(Op2,XY2),
    opcode(jmp_id,X),
    b_ASPN4_cccc(X,XY1,XY2,Val).
asm_pass2_inst(jmpn_var_x(Op,L),Csym,Lsym) :-
    true :
    asm_lookup_ltab(L,Lsym,Val),
    opcode(jmpn_var_x,X),
    b_ASPN3_ccc(X,Op,Val).
asm_pass2_inst(jmpn_var_y(Op,L),Csym,Lsym) :-
    true :
    asm_lookup_ltab(L,Lsym,Val),
    opcode(jmpn_var_y,X),
    b_ASPN3_ccc(X,Op,Val).
asm_pass2_inst(jmp_var_x(Op,L),Csym,Lsym) :-
    true :
    asm_lookup_ltab(L,Lsym,Val),
    opcode(jmp_var_x,X),
    b_ASPN3_ccc(X,Op,Val).
asm_pass2_inst(jmp_var_y(Op,L),Csym,Lsym) :-
    true :
    asm_lookup_ltab(L,Lsym,Val),
    opcode(jmp_var_y,X),
    b_ASPN3_ccc(X,Op,Val).
asm_pass2_inst(jmpn_atom_x(Op,L),Csym,Lsym) :-
    true :
    asm_lookup_ltab(L,Lsym,Val),
    opcode(jmpn_atom_x,X),
    b_ASPN3_ccc(X,Op,Val).
asm_pass2_inst(jmpn_atom_y(Op,L),Csym,Lsym) :-
    true :
    asm_lookup_ltab(L,Lsym,Val),
    opcode(jmpn_atom_y,X),
    b_ASPN3_ccc(X,Op,Val).
asm_pass2_inst(jmpn_atomic_x(Op,L),Csym,Lsym) :-
    true :
    asm_lookup_ltab(L,Lsym,Val),
    opcode(jmpn_atomic_x,X),
    b_ASPN3_ccc(X,Op,Val).
asm_pass2_inst(jmpn_atomic_y(Op,L),Csym,Lsym) :-
    true :
    asm_lookup_ltab(L,Lsym,Val),
    opcode(jmpn_atomic_y,X),
    b_ASPN3_ccc(X,Op,Val).
asm_pass2_inst(jmpn_num_x(Op,L),Csym,Lsym) :-
    true :
    asm_lookup_ltab(L,Lsym,Val),
    opcode(jmpn_num_x,X),
    b_ASPN3_ccc(X,Op,Val).
asm_pass2_inst(jmpn_num_y(Op,L),Csym,Lsym) :-
    true :
    asm_lookup_ltab(L,Lsym,Val),
    opcode(jmpn_num_y,X),
    b_ASPN3_ccc(X,Op,Val).
asm_pass2_inst(jmpn_float_x(Op,L),Csym,Lsym) :-
    true :
    asm_lookup_ltab(L,Lsym,Val),
    opcode(jmpn_float_x,X),
    b_ASPN3_ccc(X,Op,Val).
asm_pass2_inst(jmpn_float_y(Op,L),Csym,Lsym) :-
    true :
    asm_lookup_ltab(L,Lsym,Val),
    opcode(jmpn_float_y,X),
    b_ASPN3_ccc(X,Op,Val).
asm_pass2_inst(jmpn_int_x(Op,L),Csym,Lsym) :-
    true :
    asm_lookup_ltab(L,Lsym,Val),
    opcode(jmpn_int_x,X),
    b_ASPN3_ccc(X,Op,Val).
asm_pass2_inst(jmpn_int_y(Op,L),Csym,Lsym) :-
    true :
    asm_lookup_ltab(L,Lsym,Val),
    opcode(jmpn_int_y,X),
    b_ASPN3_ccc(X,Op,Val).
asm_pass2_inst(hash_jmpn_nil(L),Csym,Lsym) :-
    true :
    asm_lookup_ltab(L,Lsym,Val),
    opcode(hash_jmpn_nil,X),
    b_ASPN2_cc(X,Val).
asm_pass2_inst(hash_jmpn_list(L),Csym,Lsym) :-
    true :
    asm_lookup_ltab(L,Lsym,Val),
    opcode(hash_jmpn_list,X),
    b_ASPN2_cc(X,Val).
asm_pass2_inst(hash_jmpn_int(I,L),Csym,Lsym) :-
    true :
    asm_lookup_ltab(L,Lsym,Val),
    opcode(hash_jmpn_int,X),
    b_ASPN3_ccc(X,I,Val).
asm_pass2_inst(hash_jmpn_atom(C,L),Csym,Lsym) :-
    true :
    asm_lookup_ctab(C,0,Csym,I),
    asm_lookup_ltab(L,Lsym,Val),
    opcode(hash_jmpn_atom,X),
    b_ASPN3_ccc(X,I,Val).
asm_pass2_inst(hash_jmpn_struct((S,N),L),Csym,Lsym) :-
    true :
    asm_lookup_ctab(S,N,Csym,I),
    asm_lookup_ltab(L,Lsym,Val),
    opcode(hash_jmpn_struct,X),
    b_ASPN3_ccc(X,I,Val).
asm_pass2_inst(hash_jmpn_struct_x((S,N),L),Csym,Lsym) :-
    true :
    asm_lookup_ctab(S,N,Csym,I),
    asm_lookup_ltab(L,Lsym,Val),
    opcode(hash_jmpn_struct_x,X),
    b_ASPN3_ccc(X,I,Val).
asm_pass2_inst(hash_jmpn_struct_y((S,N),L),Csym,Lsym) :-
    true :
    asm_lookup_ctab(S,N,Csym,I),
    asm_lookup_ltab(L,Lsym,Val),
    opcode(hash_jmpn_struct_y,X),
    b_ASPN3_ccc(X,I,Val).
asm_pass2_inst(hash_jmpn_struct_xx((S,N),L),Csym,Lsym) :-
    true :
    asm_lookup_ctab(S,N,Csym,I),
    asm_lookup_ltab(L,Lsym,Val),
    opcode(hash_jmpn_struct_xx,X),
    b_ASPN3_ccc(X,I,Val).
asm_pass2_inst(hash_jmpn_struct_xy((S,N),L),Csym,Lsym) :-
    true :
    asm_lookup_ctab(S,N,Csym,I),
    asm_lookup_ltab(L,Lsym,Val),
    opcode(hash_jmpn_struct_xy,X),
    b_ASPN3_ccc(X,I,Val).
asm_pass2_inst(hash_jmpn_struct_yx((S,N),L),Csym,Lsym) :-
    true :
    asm_lookup_ctab(S,N,Csym,I),
    asm_lookup_ltab(L,Lsym,Val),
    opcode(hash_jmpn_struct_yx,X),
    b_ASPN3_ccc(X,I,Val).
asm_pass2_inst(hash_jmpn_struct_yy((S,N),L),Csym,Lsym) :-
    true :
    asm_lookup_ctab(S,N,Csym,I),
    asm_lookup_ltab(L,Lsym,Val),
    opcode(hash_jmpn_struct_yy,X),
    b_ASPN3_ccc(X,I,Val).
asm_pass2_inst(jmpn_det(L),Csym,Lsym) :-
    true :
    asm_lookup_ltab(L,Lsym,Val),
    opcode(jmpn_det,X),
    b_ASPN2_cc(X,Val).
asm_pass2_inst(jmpn_det_get_ar_cps(L),Csym,Lsym) :-
    true :
    asm_lookup_ltab(L,Lsym,Val),
    opcode(jmpn_det_get_ar_cps,X),
    b_ASPN2_cc(X,Val).
asm_pass2_inst(save_ht_jmp(L1,L2),Csym,Lsym) :-
    true :
    asm_lookup_ltab(L1,Lsym,Val1),
    asm_lookup_ltab(L2,Lsym,Val2),
    opcode(save_ht_jmp,X),
    b_ASPN3_ccc(X,Val1,Val2).
asm_pass2_inst(unify_struct_x(Op,(S,N)),Csym,Lsym):-
    true :
    asm_lookup_ctab(S,N,Csym,I),
    opcode(unify_struct_x,X),
    b_ASPN3_ccc(X,Op,I).
asm_pass2_inst(unify_struct_y(Op,(S,N)),Csym,Lsym):-
    true :
    asm_lookup_ctab(S,N,Csym,I),
    opcode(unify_struct_y,X),
    b_ASPN3_ccc(X,Op,I).
asm_pass2_inst(unify0_struct_y(Op,(S,N)),Csym,Lsym):-
    true :
    asm_lookup_ctab(S,N,Csym,I),
    opcode(unify0_struct_y,X),
    b_ASPN3_ccc(X,Op,I).
asm_pass2_inst(unicut_struct_y(Op,(S,N)),Csym,Lsym):-
    true :
    asm_lookup_ctab(S,N,Csym,I),
    opcode(unicut_struct_y,X),
    b_ASPN3_ccc(X,Op,I).
asm_pass2_inst(unify_atom_x(Op,C),Csym,Lsym):-
    true :
    asm_lookup_ctab(C,0,Csym,I),
    opcode(unify_atom_x,X),
    b_ASPN3_ccc(X,Op,I).
asm_pass2_inst(unify_atom_y(Op,C),Csym,Lsym):-
    true :
    asm_lookup_ctab(C,0,Csym,I),
    opcode(unify_atom_y,X),
    b_ASPN3_ccc(X,Op,I).
asm_pass2_inst(unify0_atom_y(Op,C),Csym,Lsym):-
    true :
    asm_lookup_ctab(C,0,Csym,I),
    opcode(unify0_atom_y,X),
    b_ASPN3_ccc(X,Op,I).
asm_pass2_inst(unicut_atom_y(Op,C),Csym,Lsym):-
    true :
    asm_lookup_ctab(C,0,Csym,I),
    opcode(unicut_atom_y,X),
    b_ASPN3_ccc(X,Op,I).
asm_pass2_inst(unify_arg_atom(C),Csym,Lsym):-
    true :
    asm_lookup_ctab(C,0,Csym,I),
    opcode(unify_arg_atom,X),
    b_ASPN2_cc(X,I).
asm_pass2_inst(unify_arg_struct((S,N)),Csym,Lsym):-
    true :
    asm_lookup_ctab(S,N,Csym,I),
    opcode(unify_arg_struct,X),
    b_ASPN2_cc(X,I).

asm_pass2_inst(move_struct_x(Op,(S,N)),Csym,Lsym):-
    true :
    asm_lookup_ctab(S,N,Csym,I),
    opcode(move_struct_x,X),
    b_ASPN3_ccc(X,Op,I).
asm_pass2_inst(move_struct_y(Op,(S,N)),Csym,Lsym):-
    true :
    asm_lookup_ctab(S,N,Csym,I),
    opcode(move_struct_y,X),
    b_ASPN3_ccc(X,Op,I).
asm_pass2_inst(move_atom_x(Op,C),Csym,Lsym):-
    true :
    asm_lookup_ctab(C,0,Csym,I),
    opcode(move_atom_x,X),
    b_ASPN3_ccc(X,Op,I).
asm_pass2_inst(move_atom_y(Op,C),Csym,Lsym):-
    true :
    asm_lookup_ctab(C,0,Csym,I),
    opcode(move_atom_y,X),
    b_ASPN3_ccc(X,Op,I).
asm_pass2_inst(and(Op1,Op2,Op3),Csym,Lsym):-
    true :
    x_or_y(Op1,XY1),
    x_or_y(Op2,XY2),
    x_or_y(Op3,XY3),
    opcode(and,X),
    b_ASPN4_cccc(X,XY1,XY2,XY3).
asm_pass2_inst(or(Op1,Op2,Op3),Csym,Lsym):-
    true :
    x_or_y(Op1,XY1),
    x_or_y(Op2,XY2),
    x_or_y(Op3,XY3),
    opcode(or,X),
    b_ASPN4_cccc(X,XY1,XY2,XY3).
asm_pass2_inst(lshiftl(Op1,Op2,Op3),Csym,Lsym):-
    true :
    x_or_y(Op1,XY1),
    x_or_y(Op2,XY2),
    x_or_y(Op3,XY3),
    opcode(lshiftl,X),
    b_ASPN4_cccc(X,XY1,XY2,XY3).
asm_pass2_inst(lshiftr(Op1,Op2,Op3),Csym,Lsym):-
    true :
    x_or_y(Op1,XY1),
    x_or_y(Op2,XY2),
    x_or_y(Op3,XY3),
    opcode(lshiftr,X),
    b_ASPN4_cccc(X,XY1,XY2,XY3).
asm_pass2_inst(complement(Op1,Op2),Csym,Lsym):-
    true :
    x_or_y(Op1,XY1),
    x_or_y(Op2,XY2),
    opcode(complement,X),
    b_ASPN3_ccc(X,XY1,XY2).
asm_pass2_inst(add(Op1,Op2,Op3),Csym,Lsym):-
    true :
    x_or_y(Op1,XY1),
    x_or_y(Op2,XY2),
    x_or_y(Op3,XY3),
    opcode(add,X),
    b_ASPN4_cccc(X,XY1,XY2,XY3).
asm_pass2_inst(sub(Op1,Op2,Op3),Csym,Lsym):-
    true :
    x_or_y(Op1,XY1),
    x_or_y(Op2,XY2),
    x_or_y(Op3,XY3),
    opcode(sub,X),
    b_ASPN4_cccc(X,XY1,XY2,XY3).
asm_pass2_inst(mul(Op1,Op2,Op3),Csym,Lsym):-
    true :
    x_or_y(Op1,XY1),
    x_or_y(Op2,XY2),
    x_or_y(Op3,XY3),
    opcode(mul,X),
    b_ASPN4_cccc(X,XY1,XY2,XY3).
asm_pass2_inst(div(Op1,Op2,Op3),Csym,Lsym):-
    true :
    x_or_y(Op1,XY1),
    x_or_y(Op2,XY2),
    x_or_y(Op3,XY3),
    opcode(div,X),
    b_ASPN4_cccc(X,XY1,XY2,XY3).
asm_pass2_inst(idiv(Op1,Op2,Op3),Csym,Lsym):-
    true :
    x_or_y(Op1,XY1),
    x_or_y(Op2,XY2),
    x_or_y(Op3,XY3),
    opcode(idiv,X),
    b_ASPN4_cccc(X,XY1,XY2,XY3).
asm_pass2_inst(mod(Op1,Op2,Op3),Csym,Lsym):-
    true :
    x_or_y(Op1,XY1),
    x_or_y(Op2,XY2),
    x_or_y(Op3,XY3),
    opcode(mod,X),
    b_ASPN4_cccc(X,XY1,XY2,XY3).
asm_pass2_inst(para_struct((S,N)),Csym,Lsym):-
    true :
    asm_lookup_ctab(S,N,Csym,I),
    opcode(para_struct,X),
    b_ASPN2_cc(X,I).
asm_pass2_inst(para_atom(C),Csym,Lsym):-
    true :
    asm_lookup_ctab(C,0,Csym,I),
    opcode(para_atom,X),
    b_ASPN2_cc(X,I).
asm_pass2_inst(fork(L),Csym,Lsym) :-
    true :
    asm_lookup_ltab(L,Lsym,Val),
    opcode(fork,X),
    b_ASPN2_cc(X,Val).
asm_pass2_inst(fork_unify_struct_y(Op,(S,N),L),Csym,Lsym):-
    true :
    asm_lookup_ltab(L,Lsym,Val),
    asm_lookup_ctab(S,N,Csym,I),
    opcode(fork_unify_struct_y,X),
    b_ASPN4_cccc(X,Op,I,Val).
asm_pass2_inst(fork_unify_list_y(Op,L),Csym,Lsym) :-
    true :
    asm_lookup_ltab(L,Lsym,Val),
    opcode(fork_unify_list_y,X),
    b_ASPN3_ccc(X,Op,Val).
asm_pass2_inst(fork_unify_nil_y(Op,L),Csym,Lsym) :-
    true :
    asm_lookup_ltab(L,Lsym,Val),
    opcode(fork_unify_nil_y,X),
    b_ASPN3_ccc(X,Op,Val).
asm_pass2_inst(fork_unify_atom_y(Op,C,L),Csym,Lsym):-
    true :
    asm_lookup_ctab(C,0,Csym,I),
    asm_lookup_ltab(L,Lsym,Val),
    opcode(fork_unify_atom_y,X),
    b_ASPN4_cccc(X,Op,I,Val).
asm_pass2_inst(fork_unify_int_y(Op1,Op2,L),Csym,Lsym) :-
    true :
    asm_lookup_ltab(L,Lsym,Val),
    opcode(fork_unify_int_y,X),
    b_ASPN4_cccc(X,Op1,Op2,Val).
asm_pass2_inst(fork_unify_uy_uy(Op1,Op2,L),Csym,Lsym) :-
    true :
    asm_lookup_ltab(L,Lsym,Val),
    opcode(fork_unify_uy_uy,X),
    b_ASPN4_cccc(X,Op1,Op2,Val).

asm_pass2_inst(fork_unicut_struct_y(Op,(S,N),L),Csym,Lsym):-
    true :
    asm_lookup_ltab(L,Lsym,Val),
    asm_lookup_ctab(S,N,Csym,I),
    opcode(fork_unicut_struct_y,X),
    b_ASPN4_cccc(X,Op,I,Val).
asm_pass2_inst(fork_unicut_list_y(Op,L),Csym,Lsym) :-
    true :
    asm_lookup_ltab(L,Lsym,Val),
    opcode(fork_unicut_list_y,X),
    b_ASPN3_ccc(X,Op,Val).
asm_pass2_inst(fork_unicut_nil_y(Op,L),Csym,Lsym) :-
    true :
    asm_lookup_ltab(L,Lsym,Val),
    opcode(fork_unicut_nil_y,X),
    b_ASPN3_ccc(X,Op,Val).
asm_pass2_inst(fork_unicut_atom_y(Op,C,L),Csym,Lsym):-
    true :
    asm_lookup_ctab(C,0,Csym,I),
    asm_lookup_ltab(L,Lsym,Val),
    opcode(fork_unicut_atom_y,X),
    b_ASPN4_cccc(X,Op,I,Val).
asm_pass2_inst(fork_unicut_int_y(Op1,Op2,L),Csym,Lsym) :-
    true :
    asm_lookup_ltab(L,Lsym,Val),
    opcode(fork_unicut_int_y,X),
    b_ASPN4_cccc(X,Op1,Op2,Val).
asm_pass2_inst(fork_unicut_ux_uy(Op1,Op2,L),Csym,Lsym) :-
    true :
    asm_lookup_ltab(L,Lsym,Val),
    opcode(fork_unicut_ux_uy,X),
    b_ASPN4_cccc(X,Op1,Op2,Val).
asm_pass2_inst(fork_unicut_uy_uy(Op1,Op2,L),Csym,Lsym) :-
    true :
    asm_lookup_ltab(L,Lsym,Val),
    opcode(fork_unicut_uy_uy,X),
    b_ASPN4_cccc(X,Op1,Op2,Val).

asm_pass2_inst(hash_x(Op,L),Csym,Lsym):-
    true :
    opcode(hash_x,X),
    asm_lookup_ltab(L,Lsym,Val),
    b_ASPN4_cccc(X,Op,0,0),b_ASPN_c(Val).
asm_pass2_inst(hash_y(Op,L),Csym,Lsym):-
    true :
    opcode(hash_y,X),
    asm_lookup_ltab(L,Lsym,Val),
    b_ASPN4_cccc(X,Op,0,0),b_ASPN_c(Val).
asm_pass2_inst(callv(Op1),Csym,Lsym):-
    true :
    opcode(callv,X),
    x_or_y(Op1,XY1),
    b_ASPN2_cc(X,XY1).
asm_pass2_inst(executev(Op1),Csym,Lsym):-
    true :
    opcode(executev,X),
    x_or_y(Op1,XY1),
    b_ASPN2_cc(X,XY1).
asm_pass2_inst(functor(Op1,Op2,Op3),Csym,Lsym):-
    true :
    opcode(functor,X),
    x_or_y(Op1,XY1),
    x_or_y(Op2,XY2),
    x_or_y(Op3,XY3),
    b_ASPN4_cccc(X,XY1,XY2,XY3).
asm_pass2_inst(func_arity(Op1,Op2),Csym,Lsym):-
    true :
    opcode(func_arity,X),
    x_or_y(Op1,XY1),
    x_or_y(Op2,XY2),
    b_ASPN3_ccc(X,XY1,XY2).
asm_pass2_inst(arg0(Op1,Op2,Op3),Csym,Lsym):-
    integer(Op1) :
    opcode(arg0,X),
    x_or_y(Op2,XY2),
    x_or_y(Op3,XY3),
    b_ASPN4_cccc(X,Op1,XY2,XY3).
asm_pass2_inst(arg(Op1,Op2,Op3),Csym,Lsym):-
    true :
    opcode(arg,X),
    x_or_y(Op1,XY1),
    x_or_y(Op2,XY2),
    x_or_y(Op3,XY3),
    b_ASPN4_cccc(X,XY1,XY2,XY3).
asm_pass2_inst(setarg0(Op1,Op2,Op3),Csym,Lsym):-
    integer(Op1) :
    opcode(setarg0,X),
    x_or_y(Op2,XY2),
    x_or_y(Op3,XY3),
    b_ASPN4_cccc(X,Op1,XY2,XY3).
asm_pass2_inst(setarg(Op1,Op2,Op3),Csym,Lsym):-
    true :
    opcode(setarg,X),
    x_or_y(Op1,XY1),
    x_or_y(Op2,XY2),
    x_or_y(Op3,XY3),
    b_ASPN4_cccc(X,XY1,XY2,XY3).
asm_pass2_inst(builtin0(N,L),Csym,Lsym):-
    true :
    asm_lookup_ltab(L,Lsym,Val),
    opcode(builtin0,X),
    b_ASPN3_ccc(X,N,Val).
asm_pass2_inst(builtin1(N,L,Op1),Csym,Lsym):-
    true :
    asm_lookup_ltab(L,Lsym,Val),
    opcode(builtin1,X),
    x_or_y(Op1,XY1),
    b_ASPN4_cccc(X,N,Val,XY1).
asm_pass2_inst(builtin2(N,L,Op1,Op2),Csym,Lsym):-
    true :
    asm_lookup_ltab(L,Lsym,Val),
    opcode(builtin2,X),
    x_or_y(Op1,XY1),
    x_or_y(Op2,XY2),
    b_ASPN4_cccc(X,N,Val,XY1),b_ASPN_c(XY2).
asm_pass2_inst(builtin3(N,L,Op1,Op2,Op3),Csym,Lsym):-
    true :
    asm_lookup_ltab(L,Lsym,Val),
    opcode(builtin3,X),
    x_or_y(Op1,XY1),
    x_or_y(Op2,XY2),
    x_or_y(Op3,XY3),
    b_ASPN4_cccc(X,N,Val,XY1),b_ASPN2_cc(XY2,XY3).
asm_pass2_inst(builtin4(N,L,Op1,Op2,Op3,Op4),Csym,Lsym):-
    true :
    asm_lookup_ltab(L,Lsym,Val),
    opcode(builtin4,X),
    x_or_y(Op1,XY1),
    x_or_y(Op2,XY2),
    x_or_y(Op3,XY3),
    x_or_y(Op4,XY4),
    b_ASPN4_cccc(X,N,Val,XY1),b_ASPN3_ccc(XY2,XY3,XY4).
asm_pass2_inst(getbreg(Op1),Csym,Lsym):-
    true :
    opcode(getbreg,X),
    x_or_y(Op1,XY1),
    b_ASPN2_cc(X,XY1).
asm_pass2_inst(getpbreg(Op1),Csym,Lsym):-
    true :
    opcode(getpbreg,X),
    x_or_y(Op1,XY1),
    b_ASPN2_cc(X,XY1).
asm_pass2_inst(putbreg(Op1),Csym,Lsym):-
    true :
    opcode(putbreg,X),
    x_or_y(Op1,XY1),
    b_ASPN2_cc(X,XY1).
asm_pass2_inst(gethtreg(y(X1),y(X2)),Csym,Lsym):-
    true :
    opcode(gethtreg,X),
    b_ASPN3_ccc(X,X1,X2).
asm_pass2_inst(puthtreg(y(X1),y(X2)),Csym,Lsym):-
    true :
    opcode(puthtreg,X),
    b_ASPN3_ccc(X,X1,X2).
asm_pass2_inst(delay((S,N),L),Csym,Lsym) :-
    true :
    asm_lookup_ctab(S,N,Csym,I),
    asm_lookup_ltab(L,Lsym,Val),
    opcode(delay,X),
    b_ASPN3_ccc(X,I,Val).
asm_pass2_inst(susp_var_delay(Op,(S,N),L1,L2),Csym,Lsym) :-
    true :
    asm_lookup_ctab(S,N,Csym,I),
    asm_lookup_ltab(L1,Lsym,Val1),
    asm_lookup_ltab(L2,Lsym,Val2),
    opcode(susp_var_delay,X),
    b_ASPN4_cccc(X,Op,I,Val1),
    b_ASPN_c(Val2).
asm_pass2_inst(jmpn_dvar_y(Op,L),Csym,Lsym) :-
    true :
    asm_lookup_ltab(L,Lsym,Val),
    opcode(jmpn_dvar_y,X),
    b_ASPN3_ccc(X,Op,Val).
asm_pass2_inst(susp_var2_delay(Op,(S,N),L1,L2),Csym,Lsym) :-
    true :
    asm_lookup_ctab(S,N,Csym,I),
    asm_lookup_ltab(L1,Lsym,Val1),
    asm_lookup_ltab(L2,Lsym,Val2),
    opcode(susp_var2_delay,X),
    b_ASPN4_cccc(X,Op,I,Val1),
    b_ASPN_c(Val2).
asm_pass2_inst(Inst,Csym,Lsym):-
    functor(Inst,F,N),
    opcode(F,X) : b_ASPN_c(X), asm_pass2_inst_op(Inst,0,N).
asm_pass2_inst(Inst,Csym,Lsym):-
    true :
     cmp_error(['error in asm pass2 :',Inst,'is not defined']).

asm_pass2_inst_op(Inst,N0,N1):-
    N0=:=N1 : true.
asm_pass2_inst_op(Inst,N0,Nn):-
    true :
    N1 is N0+1,
    arg(N1,Inst,Op),
    b_ASPN_c(Op),
    asm_pass2_inst_op(Inst,N1,Nn).

asm_magic(N) :-
    true :
    asm_putnum(17,1),
    asm_putnum(18,1),
    asm_putnum(19,1),
    asm_putnum(N,1).

asm_index_inst(pred(_,_,_,_),Size):-
    true : Size=16.
asm_index_inst(arglabel(i,_,_),N) :-
    true :
    N:=9.
asm_index_inst(arglabel(c,_,_),N) :-
    true :
    N:=9.
asm_index_inst(arglabel(s,_,_),N) :-
    true :
    N:=9.

asm_symbol(Tab):-var(Tab) : true.
asm_symbol([sym(Pred,Arity,Val,_)|Symtab]) :-
    true :
    b_ASPN_c(Val),
    asm_putnum(Arity,1),
    b_GET_LENGTH_cf(Pred,L),
    asm_putnum(L,1),
    writename(Pred),
    asm_symbol(Symtab).

/* putnum(Number, Length) will write Number as a binary number
which will be Length bytes long */
/*
b_ASPN2_cc(X,Y):-b_ASPN_c(X),b_ASPN_c(Y).
b_ASPN3_ccc(X,Y,U):-b_ASPN_c(X),b_ASPN_c(Y),b_ASPN_c(U).
b_ASPN4_cccc(X,Y,U,V):-b_ASPN_c(X),b_ASPN_c(Y),b_ASPN_c(U),b_ASPN_c(V).
*/
/*
aspn(X):-
    true :
    write((X,4)),nl.

asm_putnum(X,Bytes):-
    true :
    write((X,Bytes)),nl.
*/

/*
aspn(Num):-
    true :
    asm_putnum(Num,4).
*/

asm_putnum(Num,NBytes) :-
    NBytes > 1 :
    Byte is Num /\ 255,
    Rest is Num >> 8,
    N is NBytes - 1,
    asm_putnum(Rest,N),
    put(Byte).
asm_putnum(Num,NBytes):-
%    Num < 256 :
    true :
    put(Num).

asm_mark_eot:- 
    true : 
    opcode(endfile,X), 
    b_ASPN2_cc(X,0).

/* local utilities */

sym_member1(Sym,Csym):-
    sym(F,N,Val,I)<=Sym :
    asm_hash_value(F,HashVal),
    Index is (HashVal+N) mod 255 + 1,
    arg(Index,Csym,L),
    sym_member1(F,N,Sym,L).

sym_member1(F,N,Sym,List):-
    var(List) :
    next_sym_no(I),
    arg(4,Sym,I),
    List:=[Sym|_].
sym_member1(F,N,Sym,[sym(F,N,Val2,I)|List]):-
    true :
    Sym=sym(F,N,Val2,I).
sym_member1(F,N,Sym,[X1|List]):-
    true :
    sym_member1(F,N,Sym,List).

    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lab_member1(Lab,Lsym):-
    lab(X,Val)<=Lab :
    asm_hash_value(X,HashVal),
    Index is HashVal mod 255 + 1,
%    write(user,Index),nl(user),
    arg(Index,Lsym,L),
    lab_member1(X,Lab,L).

lab_member1(X,Lab,List):-
    var(List) :
    List:=[Lab|_].
lab_member1(X,Lab,[lab(X,Val2)|List]):-
    true :
    Lab=lab(X,Val2).
lab_member1(X,Lab,[_|List]):-
    true :
    lab_member1(X,Lab,List).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

asm_lookup_ltab(Lab,Lsym,Val1):-
    true :
    asm_hash_value(Lab,HashVal),
    Index is HashVal mod 255 + 1,
    arg(Index,Lsym,L),
    asm_lookup_ltab1(Lab,L,Val1).
    
asm_lookup_ltab1(Lab,Var,Val1):-
    var(Var) : fail.
asm_lookup_ltab1(Lab,[lab(Lab,Val)|_],Val1):-
    true :
    Val1=Val.
asm_lookup_ltab1(Lab,[_|Tab],Val):-
    true :
    asm_lookup_ltab1(Lab,Tab,Val).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

asm_lookup_ctab(F,N,Csym,I):-
    true :
    asm_hash_value(F,HashVal),
    Index is (HashVal+N) mod 255 + 1,
    arg(Index,Csym,L),
    asm_lookup_ctab1(F,N,L,I).

asm_lookup_ctab1(F,N,[sym(F,N,Val,I)|L],I1):-
    true :
    I1=I.
asm_lookup_ctab1(F,N,[_|L],I):-
    true :
    asm_lookup_ctab1(F,N,L,I).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
asm_rearange_csym(Csym,N,LCsym0,LCsym):-
    N<1 :
    LCsym=LCsym0.
asm_rearange_csym(Csym,N,LCsym0,LCsym):-
    true :
    arg(N,Csym,L),
    asm_merge(L,LCsym0,LCsym1),
    N1 is N-1,
    asm_rearange_csym(Csym,N1,LCsym1,LCsym).

asm_merge(L1,L2,L3):-
    var(L1) :
    L3=L2.
asm_merge(L1,L2,L3):-
    var(L2) :
    L3=L1.
asm_merge(L1,L2,L3):-
    [Sym1|T1]<=L1,
    [Sym2|T2]<=L2,
    sym(F1,N1,Val1,I1)<=Sym1,
    sym(F2,N2,Val2,I2)<=Sym2,
    I1 < I2 :
    L3:=[Sym1|L4],
    asm_merge(T1,L2,L4).
asm_merge(L1,L2,L3):-
    [Sym1|T1]<=L1,
    [Sym2|T2]<=L2 :
    L3:=[Sym2|L4],
    asm_merge(L1,T2,L4).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/*
asm_hash_value(F,N,HashVal):-
    true :
    name(F,L),
    asm_sum_list(L,0,Sum),
    HashVal is Sum+N.

asm_hash_value((F,N,No),HashVal):-
    true :
    name(F,L),
    asm_sum_list(L,0,Sum),
    HashVal is Sum+N+No.
asm_hash_value((F,N),HashVal):-
    true :
    name(F,L),
    asm_sum_list(L,0,Sum),
    HashVal is Sum+N.

asm_sum_list([],Sum0,Sum):-
    true :
    Sum=Sum0.
asm_sum_list([X|Xs],Sum0,Sum):-
    true :
    Sum1 is Sum0+X,
    asm_sum_list(Xs,Sum1,Sum).
*/    
next_sym_no(I):-
    true :
    global_get('$sym_no',0,I),
    I1 is I+1,
    global_set('$sym_no',0,I1).

x_or_y(vx(X),Code):-  % xv
    true :
    Code is X<<3.
x_or_y(x(X),Code):-  % ux
    true :
    Code is X<<3+1.
x_or_y(vy(Y),Code):- %vy
    true :
    Code is (Y<<3) \/ 2'11.
x_or_y(y(Y),Code):-
    true :
    Code is (Y<<3) \/ 2'111.
x_or_y(Op,Code):-
    true :
    Code is (Op<<2) \/ 2'10.

:-mode opcode(c,f).
opcode(noop,0).
opcode(jmp,1).
opcode(djmp,2).
opcode(jmpn_eq_struct_x,3).
opcode(jmpn_eq_struct_y,4).
opcode(jmpn_eq_atom_x,5).
opcode(jmpn_eq_atom_y,6).
opcode(jmpn_nil_x,7).
opcode(jmpn_nil_y,8).
opcode(switch_list_x,9).
opcode(switch_list_y,10).
opcode(switch_list_yxx,11).
opcode(switch_list_yxy,12).
opcode(switch_list_yyx,13).
opcode(switch_list_yyy,14).
opcode(jmpn_eq_int_x,15).
opcode(jmpn_eq_int_y,16).
opcode(jmpn_eql,17).
opcode(jmp_eql,18).
opcode(jmp_eql_yy,19).
opcode(jmpn_gt,20).
opcode(jmpn_gt_yy,21).
opcode(jmpn_ge,22).
opcode(jmpn_ge_yy,23).
opcode(jmpn_id,24).
opcode(jmp_id,25).
opcode(jmpn_var_x,26).
opcode(jmpn_var_y,27).
opcode(jmp_var_x,28).
opcode(jmp_var_y,29).
opcode(jmpn_atom_x,30).
opcode(jmpn_atom_y,31).
opcode(jmpn_atomic_x,32).
opcode(jmpn_atomic_y,33).
opcode(jmpn_num_x,34).
opcode(jmpn_num_y,35).
opcode(jmpn_float_x,36).
opcode(jmpn_float_y,37).
opcode(jmpn_int_x,38).
opcode(jmpn_int_y,39).
opcode(hash_jmpn_nil,40).
opcode(hash_jmpn_list,41).
opcode(hash_jmpn_int,42).
opcode(hash_jmpn_atom,43).
opcode(hash_jmpn_struct,44).
opcode(hash_jmpn_struct_x,45).
opcode(hash_jmpn_struct_y,46).
opcode(hash_jmpn_struct_xx,47).
opcode(hash_jmpn_struct_xy,48).
opcode(hash_jmpn_struct_yx,49).
opcode(hash_jmpn_struct_yy,50).
opcode(unify_struct_x,51).
opcode(unify_struct_y,52).
opcode(unify_list_x,53).
opcode(unify_list_y,54).
opcode(unify_nil_x,55).
opcode(unify_nil_y,56).
opcode(unify_atom_x,57).
opcode(unify_int_x,58).
opcode(unify_atom_y,59).
opcode(unify_int_y,60).
opcode(unify_ux_ux,61).
opcode(unify_ux_uy,62).
opcode(unify_uy_uy,63).
opcode(unify_cons_x,64).
opcode(unify_cons_y,65).
opcode(fork_unify_struct_y,66).
opcode(fork_unify_list_y,67).
opcode(fork_unify_nil_y,68).
opcode(fork_unify_atom_y,69).
opcode(fork_unify_int_y,70).
opcode(fork_unify_uy_uy,71).
opcode(fork_unicut_struct_y,72).
opcode(fork_unicut_list_y,73).
opcode(fork_unicut_nil_y,74).
opcode(fork_unicut_atom_y,75).
opcode(fork_unicut_int_y,76).
opcode(fork_unicut_uy_uy,77).
opcode(unify0_struct_y,78).
opcode(unify0_list_y,79).
opcode(unify0_nil_y,80).
opcode(unify0_atom_y,81).
opcode(unify0_int_y,82).
opcode(unify0_uy_uy,83).
opcode(unicut_struct_y,84).
opcode(unicut_list_y,85).
opcode(unicut_nil_y,86).
opcode(unicut_atom_y,87).
opcode(unicut_int_y,88).
opcode(unicut_uy_uy,89).
opcode(unicut,90).
opcode(unify_arg_nil,91).
opcode(unify_arg_atom,92).
opcode(unify_arg_int,93).
opcode(unify_arg_ux_ux,94).
opcode(unify_arg_ux,95).
opcode(unify_arg_ux_vx,96).
opcode(unify_arg_ux_vy,97).
opcode(unify_arg_uy_uy,98).
opcode(unify_arg_uy,99).
opcode(unify_arg_wy,100).
opcode(unify_arg_vx_vx,101).
opcode(unify_arg_vx,102).
opcode(unify_arg_vy_vy,103).
opcode(unify_arg_vy,104).
opcode(unify_arg_list,105).
opcode(unify_arg_struct,106).
opcode(unify_arg_void_one,107).
opcode(unify_arg_void,108).
opcode(unify_arg_vx_vy,109).
opcode(unify_arg_vx_ux,110).
opcode(unify_arg_vx_uy,111).
opcode(unify_arg_vy_vx,112).
opcode(unify_arg_vy_ux,113).
opcode(unify_arg_vy_uy,114).
opcode(unify_arg_iii,115).
opcode(move_struct_x,116).
opcode(move_struct_y,117).
opcode(move_list_x,118).
opcode(move_list_y,119).
opcode(move_nil_x,120).
opcode(move_nil_y,121).
opcode(move_atom_x,122).
opcode(move_int_x,123).
opcode(move_atom_y,124).
opcode(move_int_y,125).
opcode(move_x_ux,126).
opcode(move_x_uy,127).
opcode(move_y_ux,128).
opcode(move_yy_yy_yy,129).
opcode(move_yy_yy,130).
opcode(move_y_uy,131).
opcode(move_vx,132).
opcode(move_vy,133).
opcode(move_x_wy,134).
opcode(move_yy_yw,135).
opcode(move_y_wy,136).
opcode(move_yw_yy,137).
opcode(and,138).
opcode(or,139).
opcode(lshiftl,140).
opcode(lshiftr,141).
opcode(complement,142).
opcode(add,143).
opcode(add1_y,144).
opcode(sub,145).
opcode(sub1_y,146).
opcode(mul,147).
opcode(div,148).
opcode(idiv,149).
opcode(mod,150).
opcode(para_struct,151).
opcode(para_list,152).
opcode(para_nil,153).
opcode(para_atom,154).
opcode(para_int,155).
opcode(para_ux_ux_ux,156).
opcode(para_ux_ux,157).
opcode(para_ux,158).
opcode(para_uy_uy_uy_uy,159).
opcode(para_uy_uy_uy,160).
opcode(para_uy_uy,161).
opcode(para_uy,162).
opcode(para_vx,163).
opcode(para_vy_vy,164).
opcode(para_vy,165).
opcode(para_void_one,166).
opcode(para_void,167).
opcode(para_vy_ux,168).
opcode(para_vy_uy,169).
opcode(para_ux_vy,170).
opcode(para_ux_ux_uy,171).
opcode(para_ux_uy,172).
opcode(para_ux_uy_uy,173).
opcode(para_uy_vy,174).
opcode(para_ux_uy_ux,175).
opcode(para_uy_ux,176).
opcode(para_uy_ux_ux,177).
opcode(para_uy_ux_uy,178).
opcode(para_uy_uy_ux,179).
opcode(call,180).
opcode(call_d,181).
opcode(callv,182).
opcode(execute,183).
opcode(executev,184).
opcode(return_a,185).
opcode(return_b,186).
opcode(jmpn_det,187).
opcode(save_ht_jmp,188).
opcode(allocate_flat,189).
opcode(allocate_nonflat,190).
opcode(allocate_nondet,191).
opcode(fail,192).
opcode(fail0,193).
opcode(fork,194).
opcode(commit,195).
opcode(cut,196).
opcode(cut_fail,197).
opcode(cut_return,198).
opcode(save_b,199).
opcode(getbreg,200).
opcode(putbreg,201).
opcode(getpbreg,202).
opcode(hash_x,203).
opcode(hash_y,204).
opcode(arg,205).
opcode(arg0,206).
opcode(setarg,207).
opcode(setarg0,208).
opcode(functor,209).
opcode(func_arity,210).
opcode(get_ar_cps,211).
opcode(put_ar_cps,212).
opcode(move_ar_cps,213).
opcode(jmpn_det_get_ar_cps,214).
opcode(builtin0,215).
opcode(builtin1,216).
opcode(builtin2,217).
opcode(builtin3,218).
opcode(builtin4,219).
opcode(allocate_susp,220).
opcode(susp_var_x,221).
opcode(susp_var_y,222).
opcode(delay,223).
opcode(susp_var_delay,224).
opcode(end_delay,225).
opcode(nondet,226).
opcode(jmp_susp,227).
opcode(jmpn_dvar_y,228).
opcode(susp_var2_delay,229).
opcode(domain_set_false_yy,230).
opcode(domain_set_false_yx,231).
opcode(halt,232).
opcode(endfile,233).
opcode(tabsize,234).

/*
opcode(X,N):-
    true :
    cmp_error(['no this instruction : ',X]).
*/
output_mess(Mess):-
    true :
    telling(X),
    tell(user),
    write(Mess),
    nl,
    tell(X).

is_unify_arg_inst(Inst):-
    true :
    functor(Inst,F,N),
    opcode(F,Code),
    Code>=91,Code=<115.

