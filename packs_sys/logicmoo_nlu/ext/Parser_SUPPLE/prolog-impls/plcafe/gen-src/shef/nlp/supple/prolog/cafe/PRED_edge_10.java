package shef.nlp.supple.prolog.cafe;
import jp.ac.kobe_u.cs.prolog.lang.*;
import jp.ac.kobe_u.cs.prolog.builtin.*;

/*
 * *** Please do not edit ! ***
 * @(#) PRED_edge_10.java
 * @procedure edge/10 in supple.pl
 */

/*
 * @version Prolog Cafe 0.8 November 2003
 * @author Mutsunori Banbara (banbara@kobe-u.ac.jp)
 * @author Naoyuki Tamura    (tamura@kobe-u.ac.jp)
 */

public class PRED_edge_10 extends Predicate {
    static SymbolTerm f1 = SymbolTerm.makeSymbol("edge", 10);
    static SymbolTerm f3 = SymbolTerm.makeSymbol(":", 2);
    static SymbolTerm s4 = SymbolTerm.makeSymbol("shef.nlp.supple.prolog.cafe");

    public Term arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10;

    public PRED_edge_10(Term a1, Term a2, Term a3, Term a4, Term a5, Term a6, Term a7, Term a8, Term a9, Term a10, Predicate cont) {
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

    public PRED_edge_10(){}
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
        engine.setB0();
        Term a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13;
        Predicate p1;
        a1 = arg1.dereference();
        a2 = arg2.dereference();
        a3 = arg3.dereference();
        a4 = arg4.dereference();
        a5 = arg5.dereference();
        a6 = arg6.dereference();
        a7 = arg7.dereference();
        a8 = arg8.dereference();
        a9 = arg9.dereference();
        a10 = arg10.dereference();

        Term[] h2 = {a1, a2, a3, a4, a5, a6, a7, a8, a9, a10};
        a11 = new StructureTerm(f1, h2);
        a12 = new VariableTerm(engine);
        Term[] h5 = {s4, a12};
        a13 = new StructureTerm(f3, h5);
        p1 = new PRED_translated_goal_1(a13, cont);
        return new PRED_clause_2(a11, a12, p1);
    }

    public int arity() { return 10; }

    public String toString() {
        return "edge(" + arg1 + ", " + arg2 + ", " + arg3 + ", " + arg4 + ", " + arg5 + ", " + arg6 + ", " + arg7 + ", " + arg8 + ", " + arg9 + ", " + arg10 + ")";
    }
}

