package shef.nlp.supple.prolog.cafe;
import jp.ac.kobe_u.cs.prolog.lang.*;
import jp.ac.kobe_u.cs.prolog.builtin.*;

/*
 * *** Please do not edit ! ***
 * @(#) PRED_compile_lexicon_1.java
 * @procedure compile_lexicon/1 in compile_grammar.pl
 */

/*
 * @version Prolog Cafe 0.8 November 2003
 * @author Mutsunori Banbara (banbara@kobe-u.ac.jp)
 * @author Naoyuki Tamura    (tamura@kobe-u.ac.jp)
 */

public class PRED_compile_lexicon_1 extends Predicate {
    static Predicate compile_lexicon_1_1 = new PRED_compile_lexicon_1_1();
    static Predicate compile_lexicon_1_2 = new PRED_compile_lexicon_1_2();
    static Predicate compile_lexicon_1_sub_1 = new PRED_compile_lexicon_1_sub_1();

    public Term arg1;

    public PRED_compile_lexicon_1(Term a1, Predicate cont) {
        arg1 = a1; 
        this.cont = cont;
    }

    public PRED_compile_lexicon_1(){}
    public void setArgument(Term[] args, Predicate cont) {
        arg1 = args[0]; 
        this.cont = cont;
    }

    public Predicate exec(Prolog engine) {
        engine.aregs[1] = arg1;
        engine.cont = cont;
        return call(engine);
    }

    public Predicate call(Prolog engine) {
        engine.setB0();
        return engine.jtry(compile_lexicon_1_1, compile_lexicon_1_sub_1);
    }

    public int arity() { return 1; }

    public String toString() {
        return "compile_lexicon(" + arg1 + ")";
    }
}

class PRED_compile_lexicon_1_sub_1 extends PRED_compile_lexicon_1 {

    public Predicate exec(Prolog engine) {
        return engine.trust(compile_lexicon_1_2);
    }
}

class PRED_compile_lexicon_1_1 extends PRED_compile_lexicon_1 {
    static SymbolTerm s1 = SymbolTerm.makeSymbol("[]");
    static SymbolTerm f2 = SymbolTerm.makeSymbol("compiled_word", 2);

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4, a5, a6, a7, a8, a9, a10;
        Predicate p1, p2, p3, p4, p5, p6;
        a1 = engine.aregs[1].dereference();
        Predicate cont = engine.cont;

        a2 = new VariableTerm(engine);
        a3 = new VariableTerm(engine);
        a4 = new ListTerm(a3, s1);
        a6 = new VariableTerm(engine);
        a5 = new ListTerm(a6, s1);
        a8 = new VariableTerm(engine);
        a7 = new ListTerm(new VariableTerm(engine), a8);
        a9 = new VariableTerm(engine);
        Term[] h3 = {a2, a6};
        a10 = new StructureTerm(f2, h3);
        p1 = new PRED_fail_0(cont);
        p2 = new PRED_assert_1(a10, p1);
        p3 = new PRED_add_default_values_2(a9, a8, p2);
        p4 = new PRED_default_table_2(a2, a9, p3);
        p5 = new PRED_$614646_2(a6, a7, p4);
        p6 = new PRED_compile_features_2(a4, a5, p5);
        return new PRED_word_2(a2, a3, p6);
    }
}

class PRED_compile_lexicon_1_2 extends PRED_compile_lexicon_1 {

    public Predicate exec(Prolog engine) {
        Term a1;
        a1 = engine.aregs[1].dereference();
        Predicate cont = engine.cont;

        return new PRED_$neck_cut_0(cont);
    }
}

