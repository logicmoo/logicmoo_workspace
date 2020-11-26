package shef.nlp.supple.prolog.cafe;
import jp.ac.kobe_u.cs.prolog.lang.*;
import jp.ac.kobe_u.cs.prolog.builtin.*;

/*
 * *** Please do not edit ! ***
 * @(#) PRED_write_compiled_grammar_0.java
 * @procedure write_compiled_grammar/0 in compile_grammar.pl
 */

/*
 * @version Prolog Cafe 0.8 November 2003
 * @author Mutsunori Banbara (banbara@kobe-u.ac.jp)
 * @author Naoyuki Tamura    (tamura@kobe-u.ac.jp)
 */

public class PRED_write_compiled_grammar_0 extends Predicate {
    static Predicate write_compiled_grammar_0_1 = new PRED_write_compiled_grammar_0_1();
    static Predicate write_compiled_grammar_0_2 = new PRED_write_compiled_grammar_0_2();
    static Predicate write_compiled_grammar_0_sub_1 = new PRED_write_compiled_grammar_0_sub_1();

    public PRED_write_compiled_grammar_0(Predicate cont) {
        this.cont = cont;
    }

    public PRED_write_compiled_grammar_0(){}
    public void setArgument(Term[] args, Predicate cont) {
        this.cont = cont;
    }

    public Predicate exec(Prolog engine) {
        engine.cont = cont;
        return call(engine);
    }

    public Predicate call(Prolog engine) {
        engine.setB0();
        return engine.jtry(write_compiled_grammar_0_1, write_compiled_grammar_0_sub_1);
    }

    public int arity() { return 0; }

    public String toString() {
        return "write_compiled_grammar";
    }
}

class PRED_write_compiled_grammar_0_sub_1 extends PRED_write_compiled_grammar_0 {

    public Predicate exec(Prolog engine) {
        return engine.trust(write_compiled_grammar_0_2);
    }
}

class PRED_write_compiled_grammar_0_1 extends PRED_write_compiled_grammar_0 {
    static SymbolTerm s1 = SymbolTerm.makeSymbol("compiled_rule");
    static SymbolTerm s2 = SymbolTerm.makeSymbol("[]");
    static SymbolTerm s3 = SymbolTerm.makeSymbol(".");

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12;
        Predicate p1, p2, p3, p4, p5;
        Predicate cont = engine.cont;

        a1 = new VariableTerm(engine);
        a2 = new VariableTerm(engine);
        a3 = new VariableTerm(engine);
        a4 = new VariableTerm(engine);
        a5 = new VariableTerm(engine);
        a6 = new VariableTerm(engine);
        a12 = new ListTerm(a5, s2);
        a11 = new ListTerm(a4, a12);
        a10 = new ListTerm(a3, a11);
        a9 = new ListTerm(a2, a10);
        a8 = new ListTerm(a1, a9);
        a7 = new ListTerm(s1, a8);
        p1 = new PRED_fail_0(cont);
        p2 = new PRED_nl_0(p1);
        p3 = new PRED_write_1(s3, p2);
        p4 = new PRED_writeq_1(a6, p3);
        p5 = new PRED_$614646_2(a6, a7, p4);
        return new PRED_compiled_rule_5(a1, a2, a3, a4, a5, p5);
    }
}

class PRED_write_compiled_grammar_0_2 extends PRED_write_compiled_grammar_0 {

    public Predicate exec(Prolog engine) {
        return engine.cont;
    }
}

