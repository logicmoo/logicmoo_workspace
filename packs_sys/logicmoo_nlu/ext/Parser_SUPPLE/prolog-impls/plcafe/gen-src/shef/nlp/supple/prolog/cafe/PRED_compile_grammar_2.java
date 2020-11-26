package shef.nlp.supple.prolog.cafe;
import jp.ac.kobe_u.cs.prolog.lang.*;
import jp.ac.kobe_u.cs.prolog.builtin.*;

/*
 * *** Please do not edit ! ***
 * @(#) PRED_compile_grammar_2.java
 * @procedure compile_grammar/2 in compile_grammar.pl
 */

/*
 * @version Prolog Cafe 0.8 November 2003
 * @author Mutsunori Banbara (banbara@kobe-u.ac.jp)
 * @author Naoyuki Tamura    (tamura@kobe-u.ac.jp)
 */

public class PRED_compile_grammar_2 extends Predicate {
    static Predicate compile_grammar_2_1 = new PRED_compile_grammar_2_1();
    static Predicate compile_grammar_2_2 = new PRED_compile_grammar_2_2();
    static Predicate compile_grammar_2_sub_1 = new PRED_compile_grammar_2_sub_1();

    public Term arg1, arg2;

    public PRED_compile_grammar_2(Term a1, Term a2, Predicate cont) {
        arg1 = a1; 
        arg2 = a2; 
        this.cont = cont;
    }

    public PRED_compile_grammar_2(){}
    public void setArgument(Term[] args, Predicate cont) {
        arg1 = args[0]; 
        arg2 = args[1]; 
        this.cont = cont;
    }

    public Predicate exec(Prolog engine) {
        engine.aregs[1] = arg1;
        engine.aregs[2] = arg2;
        engine.cont = cont;
        return call(engine);
    }

    public Predicate call(Prolog engine) {
        engine.setB0();
        return engine.jtry(compile_grammar_2_1, compile_grammar_2_sub_1);
    }

    public int arity() { return 2; }

    public String toString() {
        return "compile_grammar(" + arg1 + ", " + arg2 + ")";
    }
}

class PRED_compile_grammar_2_sub_1 extends PRED_compile_grammar_2 {

    public Predicate exec(Prolog engine) {
        return engine.trust(compile_grammar_2_2);
    }
}

class PRED_compile_grammar_2_1 extends PRED_compile_grammar_2 {
    static SymbolTerm f1 = SymbolTerm.makeSymbol("filter_grammar", 1);
    static SymbolTerm f3 = SymbolTerm.makeSymbol("best_parse_cats", 2);
    static SymbolTerm s5 = SymbolTerm.makeSymbol("grammar");
    static SymbolTerm f6 = SymbolTerm.makeSymbol("gensymmark", 2);
    static SymbolTerm s7 = SymbolTerm.makeSymbol("rule");
    static IntegerTerm s9 = new IntegerTerm(0);
    static Term[] h14 = {s7, s9};
    static StructureTerm s10 = new StructureTerm(f6, h14);
    static SymbolTerm f11 = SymbolTerm.makeSymbol("best_parse_cats", 1);
    static SymbolTerm s13 = SymbolTerm.makeSymbol("filter_chart");

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4, a5, a6, a7;
        Predicate p1, p2, p3, p4, p5, p6, p7, p8, p9;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        Predicate cont = engine.cont;

        a3 = new VariableTerm(engine);
        Term[] h2 = {a1};
        a4 = new StructureTerm(f1, h2);
        Term[] h4 = {a1, a3};
        a5 = new StructureTerm(f3, h4);
        Term[] h8 = {s7, new VariableTerm(engine)};
        a6 = new StructureTerm(f6, h8);
        Term[] h12 = {new VariableTerm(engine)};
        a7 = new StructureTerm(f11, h12);
        p1 = new PRED_retractall_1(s13, cont);
        p2 = new PRED_retractall_1(a7, p1);
        p3 = new PRED_assert_1(s10, p2);
        p4 = new PRED_retractall_1(a6, p3);
        p5 = new PRED_buchart_gensym_2(s5, a2, p4);
        p6 = new PRED_compile_grammar3_1(a1, p5);
        p7 = new PRED_assert_1(a5, p6);
        p8 = new PRED_assert_1(a4, p7);
        p9 = new PRED_best_parse_cats_1(a3, p8);
        return new PRED_filter_chart_0(p9);
    }
}

class PRED_compile_grammar_2_2 extends PRED_compile_grammar_2 {
    static SymbolTerm f1 = SymbolTerm.makeSymbol("best_parse_cats", 1);

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3;
        Predicate p1;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        Predicate cont = engine.cont;

        if ( !a1.unify(a2, engine.trail) ) return engine.fail();
        Term[] h2 = {new VariableTerm(engine)};
        a3 = new StructureTerm(f1, h2);
        p1 = new PRED_retractall_1(a3, cont);
        return new PRED_compile_grammar3_1(a1, p1);
    }
}

