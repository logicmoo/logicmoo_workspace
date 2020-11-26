package shef.nlp.supple.prolog.cafe;
import jp.ac.kobe_u.cs.prolog.lang.*;
import jp.ac.kobe_u.cs.prolog.builtin.*;

/*
 * *** Please do not edit ! ***
 * @(#) PRED_parse_2.java
 * @procedure parse/2 in parse_file.pl
 */

/*
 * @version Prolog Cafe 0.8 November 2003
 * @author Mutsunori Banbara (banbara@kobe-u.ac.jp)
 * @author Naoyuki Tamura    (tamura@kobe-u.ac.jp)
 */

public class PRED_parse_2 extends Predicate {
    static SymbolTerm f1 = SymbolTerm.makeSymbol("external_sem_file", 1);
    static SymbolTerm f3 = SymbolTerm.makeSymbol("semantic_output", 1);
    static SymbolTerm s5 = SymbolTerm.makeSymbol("[]");
    static Term[] h8 = {s5};
    static StructureTerm s6 = new StructureTerm(f3, h8);
    static SymbolTerm s7 = SymbolTerm.makeSymbol("append");

    public Term arg1, arg2;

    public PRED_parse_2(Term a1, Term a2, Predicate cont) {
        arg1 = a1; 
        arg2 = a2; 
        this.cont = cont;
    }

    public PRED_parse_2(){}
    public void setArgument(Term[] args, Predicate cont) {
        arg1 = args[0]; 
        arg2 = args[1]; 
        this.cont = cont;
    }

    public Predicate exec(Prolog engine) {
        engine.setB0();
        Term a1, a2, a3, a4, a5, a6, a7;
        Predicate p1, p2, p3, p4, p5, p6, p7, p8, p9;
        a1 = arg1.dereference();
        a2 = arg2.dereference();

        Term[] h2 = {new VariableTerm(engine)};
        a3 = new StructureTerm(f1, h2);
        Term[] h4 = {new VariableTerm(engine)};
        a4 = new StructureTerm(f3, h4);
        a5 = new VariableTerm(engine);
        a6 = new VariableTerm(engine);
        a7 = new VariableTerm(engine);
        p1 = new PRED_close_1(a7, cont);
        p2 = new PRED_write_semantics_2(a7, a2, p1);
        p3 = new PRED_open_3(a6, s7, a7, p2);
        p4 = new PRED_output_file_1(a6, p3);
        p5 = new PRED_update_match_2(a5, a2, p4);
        p6 = new PRED_semantic_output_1(a5, p5);
        p7 = new PRED_parse_1(a1, p6);
        p8 = new PRED_assert_1(s6, p7);
        p9 = new PRED_retractall_1(a4, p8);
        return new PRED_retractall_1(a3, p9);
    }

    public int arity() { return 2; }

    public String toString() {
        return "parse(" + arg1 + ", " + arg2 + ")";
    }
}

