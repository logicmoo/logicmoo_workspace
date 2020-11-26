package shef.nlp.supple.prolog.cafe;
import jp.ac.kobe_u.cs.prolog.lang.*;
import jp.ac.kobe_u.cs.prolog.builtin.*;

/*
 * *** Please do not edit ! ***
 * @(#) PRED_assert_edge_10.java
 * @procedure assert_edge/10 in supple.pl
 */

/*
 * @version Prolog Cafe 0.8 November 2003
 * @author Mutsunori Banbara (banbara@kobe-u.ac.jp)
 * @author Naoyuki Tamura    (tamura@kobe-u.ac.jp)
 */

public class PRED_assert_edge_10 extends Predicate {
    static Predicate assert_edge_10_1 = new PRED_assert_edge_10_1();
    static Predicate assert_edge_10_2 = new PRED_assert_edge_10_2();
    static Predicate assert_edge_10_sub_1 = new PRED_assert_edge_10_sub_1();

    public Term arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10;

    public PRED_assert_edge_10(Term a1, Term a2, Term a3, Term a4, Term a5, Term a6, Term a7, Term a8, Term a9, Term a10, Predicate cont) {
        arg1 = a1; 
        arg2 = a2; 
        arg3 = a3; 
        arg4 = a4; 
        arg5 = a5; 
        arg6 = a6; 
        arg7 = a7; 
        arg8 = a8; 
        arg9 = a9; 
        arg10 = a10; 
        this.cont = cont;
    }

    public PRED_assert_edge_10(){}
    public void setArgument(Term[] args, Predicate cont) {
        arg1 = args[0]; 
        arg2 = args[1]; 
        arg3 = args[2]; 
        arg4 = args[3]; 
        arg5 = args[4]; 
        arg6 = args[5]; 
        arg7 = args[6]; 
        arg8 = args[7]; 
        arg9 = args[8]; 
        arg10 = args[9]; 
        this.cont = cont;
    }

    public Predicate exec(Prolog engine) {
        engine.aregs[1] = arg1;
        engine.aregs[2] = arg2;
        engine.aregs[3] = arg3;
        engine.aregs[4] = arg4;
        engine.aregs[5] = arg5;
        engine.aregs[6] = arg6;
        engine.aregs[7] = arg7;
        engine.aregs[8] = arg8;
        engine.aregs[9] = arg9;
        engine.aregs[10] = arg10;
        engine.cont = cont;
        return call(engine);
    }

    public Predicate call(Prolog engine) {
        engine.setB0();
        return engine.jtry(assert_edge_10_1, assert_edge_10_sub_1);
    }

    public int arity() { return 10; }

    public String toString() {
        return "assert_edge(" + arg1 + ", " + arg2 + ", " + arg3 + ", " + arg4 + ", " + arg5 + ", " + arg6 + ", " + arg7 + ", " + arg8 + ", " + arg9 + ", " + arg10 + ")";
    }
}

class PRED_assert_edge_10_sub_1 extends PRED_assert_edge_10 {

    public Predicate exec(Prolog engine) {
        return engine.trust(assert_edge_10_2);
    }
}

class PRED_assert_edge_10_1 extends PRED_assert_edge_10 {
    static SymbolTerm s1 = SymbolTerm.makeSymbol("[]");
    static SymbolTerm f2 = SymbolTerm.makeSymbol("edge", 10);

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14;
        Predicate p1, p2, p3, p4, p5;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        a3 = engine.aregs[3].dereference();
        a4 = engine.aregs[4].dereference();
        a5 = engine.aregs[5].dereference();
        a6 = engine.aregs[6].dereference();
        a7 = engine.aregs[7].dereference();
        a8 = engine.aregs[8].dereference();
        a9 = engine.aregs[9].dereference();
        a10 = engine.aregs[10].dereference();
        Predicate cont = engine.cont;

        if ( !s1.unify(a4, engine.trail) ) return engine.fail();
        a11 = new VariableTerm(engine);
        a13 = new VariableTerm(engine);
        a12 = new ListTerm(new VariableTerm(engine), a13);
        Term[] h3 = {a1, a2, a3, s1, a5, a6, a7, a8, a9, a10};
        a14 = new StructureTerm(f2, h3);
        p1 = new PRED_$cut_1(a11, cont);
        p2 = new PRED_asserta_1(a14, p1);
        p3 = new PRED_$dummy_supple46pl_5_1(a10, p2);
        p4 = new PRED_$dummy_supple46pl_4_3(a8, a9, a13, p3);
        p5 = new PRED_$614646_2(a3, a12, p4);
        return new PRED_$get_level_1(a11, p5);
    }
}

class PRED_assert_edge_10_2 extends PRED_assert_edge_10 {
    static SymbolTerm f1 = SymbolTerm.makeSymbol("edge", 10);

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15;
        Predicate p1, p2;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        a3 = engine.aregs[3].dereference();
        a4 = engine.aregs[4].dereference();
        a5 = engine.aregs[5].dereference();
        a6 = engine.aregs[6].dereference();
        a7 = engine.aregs[7].dereference();
        a8 = engine.aregs[8].dereference();
        a9 = engine.aregs[9].dereference();
        a10 = engine.aregs[10].dereference();
        Predicate cont = engine.cont;

        if ( a4.isList() ){
            a11 = ((ListTerm)a4).car();
            a12 = ((ListTerm)a4).cdr();
        } else if ( a4.isVariable() ){
            a11 = new VariableTerm(engine);
            a12 = new VariableTerm(engine);
            if ( !a4.unify(new ListTerm(a11, a12), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        a13 = new VariableTerm(engine);
        a15 = new ListTerm(a11, a12);
        Term[] h2 = {a1, a2, a3, a15, a5, a6, a7, a8, a9, a10};
        a14 = new StructureTerm(f1, h2);
        p1 = new PRED_$cut_1(a13, cont);
        p2 = new PRED_asserta_1(a14, p1);
        return new PRED_$get_level_1(a13, p2);
    }
}

