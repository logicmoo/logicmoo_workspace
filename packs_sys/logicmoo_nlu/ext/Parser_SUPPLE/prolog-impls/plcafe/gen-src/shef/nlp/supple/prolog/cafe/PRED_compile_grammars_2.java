package shef.nlp.supple.prolog.cafe;
import jp.ac.kobe_u.cs.prolog.lang.*;
import jp.ac.kobe_u.cs.prolog.builtin.*;

/*
 * *** Please do not edit ! ***
 * @(#) PRED_compile_grammars_2.java
 * @procedure compile_grammars/2 in compile_grammar.pl
 */

/*
 * @version Prolog Cafe 0.8 November 2003
 * @author Mutsunori Banbara (banbara@kobe-u.ac.jp)
 * @author Naoyuki Tamura    (tamura@kobe-u.ac.jp)
 */

public class PRED_compile_grammars_2 extends Predicate {
    static SymbolTerm f1 = SymbolTerm.makeSymbol("compiled_rule", 5);
    static SymbolTerm f3 = SymbolTerm.makeSymbol("compiled_word", 2);
    static SymbolTerm f5 = SymbolTerm.makeSymbol("gensymmark", 2);
    static SymbolTerm s6 = SymbolTerm.makeSymbol("grammar");
    static IntegerTerm s8 = new IntegerTerm(0);
    static Term[] h13 = {s6, s8};
    static StructureTerm s9 = new StructureTerm(f5, h13);
    static SymbolTerm s10 = SymbolTerm.makeSymbol("rule");
    static Term[] h14 = {s10, s8};
    static StructureTerm s12 = new StructureTerm(f5, h14);

    public Term arg1, arg2;

    public PRED_compile_grammars_2(Term a1, Term a2, Predicate cont) {
        arg1 = a1; 
        arg2 = a2; 
        this.cont = cont;
    }

    public PRED_compile_grammars_2(){}
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

        a3 = new VariableTerm(engine);
        Term[] h2 = {new VariableTerm(engine), new VariableTerm(engine), new VariableTerm(engine), new VariableTerm(engine), new VariableTerm(engine)};
        a4 = new StructureTerm(f1, h2);
        Term[] h4 = {new VariableTerm(engine), new VariableTerm(engine)};
        a5 = new StructureTerm(f3, h4);
        Term[] h7 = {s6, new VariableTerm(engine)};
        a6 = new StructureTerm(f5, h7);
        Term[] h11 = {s10, new VariableTerm(engine)};
        a7 = new StructureTerm(f5, h11);
        p1 = new PRED_$cut_1(a3, cont);
        p2 = new PRED_compile_grammars2_2(a1, a2, p1);
        p3 = new PRED_assert_1(s12, p2);
        p4 = new PRED_retractall_1(a7, p3);
        p5 = new PRED_buchart_gensym_2(s6, a2, p4);
        p6 = new PRED_assert_1(s9, p5);
        p7 = new PRED_retractall_1(a6, p6);
        p8 = new PRED_retractall_1(a5, p7);
        p9 = new PRED_retractall_1(a4, p8);
        return new PRED_$get_level_1(a3, p9);
    }

    public int arity() { return 2; }

    public String toString() {
        return "compile_grammars(" + arg1 + ", " + arg2 + ")";
    }
}

