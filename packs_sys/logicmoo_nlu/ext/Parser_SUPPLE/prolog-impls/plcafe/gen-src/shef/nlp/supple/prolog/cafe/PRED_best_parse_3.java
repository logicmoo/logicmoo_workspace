package shef.nlp.supple.prolog.cafe;
import jp.ac.kobe_u.cs.prolog.lang.*;
import jp.ac.kobe_u.cs.prolog.builtin.*;

/*
 * *** Please do not edit ! ***
 * @(#) PRED_best_parse_3.java
 * @procedure best_parse/3 in best_parse.pl
 */

/*
 * @version Prolog Cafe 0.8 November 2003
 * @author Mutsunori Banbara (banbara@kobe-u.ac.jp)
 * @author Naoyuki Tamura    (tamura@kobe-u.ac.jp)
 */

public class PRED_best_parse_3 extends Predicate {
    static SymbolTerm f1 = SymbolTerm.makeSymbol("vertex_best_parse", 3);
    static IntegerTerm s3 = new IntegerTerm(0);
    static SymbolTerm f4 = SymbolTerm.makeSymbol("-", 2);
    static Term[] h9 = {s3, s3};
    static StructureTerm s5 = new StructureTerm(f4, h9);
    static SymbolTerm s6 = SymbolTerm.makeSymbol("nil");
    static Term[] h10 = {s3, s5, s6};
    static StructureTerm s7 = new StructureTerm(f1, h10);
    static IntegerTerm s8 = new IntegerTerm(1);

    public Term arg1, arg2, arg3;

    public PRED_best_parse_3(Term a1, Term a2, Term a3, Predicate cont) {
        arg1 = a1; 
        arg2 = a2; 
        arg3 = a3; 
        this.cont = cont;
    }

    public PRED_best_parse_3(){}
    public void setArgument(Term[] args, Predicate cont) {
        arg1 = args[0]; 
        arg2 = args[1]; 
        arg3 = args[2]; 
        this.cont = cont;
    }

    public Predicate exec(Prolog engine) {
        engine.setB0();
        Term a1, a2, a3, a4, a5;
        Predicate p1, p2, p3, p4;
        a1 = arg1.dereference();
        a2 = arg2.dereference();
        a3 = arg3.dereference();

        a4 = new VariableTerm(engine);
        Term[] h2 = {new VariableTerm(engine), new VariableTerm(engine), new VariableTerm(engine)};
        a5 = new StructureTerm(f1, h2);
        p1 = new PRED_get_best_parse_edges_2(a3, a2, cont);
        p2 = new PRED_best_parse_loop_4(s8, a3, a4, a1, p1);
        p3 = new PRED_assert_1(s7, p2);
        p4 = new PRED_retractall_1(a5, p3);
        return new PRED_best_parse_cats_2(a1, a4, p4);
    }

    public int arity() { return 3; }

    public String toString() {
        return "best_parse(" + arg1 + ", " + arg2 + ", " + arg3 + ")";
    }
}

