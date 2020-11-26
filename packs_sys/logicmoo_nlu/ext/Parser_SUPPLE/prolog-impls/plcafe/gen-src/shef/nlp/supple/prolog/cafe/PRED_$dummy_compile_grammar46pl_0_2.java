package shef.nlp.supple.prolog.cafe;
import jp.ac.kobe_u.cs.prolog.lang.*;
import jp.ac.kobe_u.cs.prolog.builtin.*;

/*
 * *** Please do not edit ! ***
 * @(#) PRED_$dummy_compile_grammar46pl_0_2.java
 * @procedure $dummy_compile_grammar.pl_0/2 in compile_grammar.pl
 */

/*
 * @version Prolog Cafe 0.8 November 2003
 * @author Mutsunori Banbara (banbara@kobe-u.ac.jp)
 * @author Naoyuki Tamura    (tamura@kobe-u.ac.jp)
 */

public class PRED_$dummy_compile_grammar46pl_0_2 extends Predicate {
    static Predicate $dummy_compile_grammar46pl_0_2_1 = new PRED_$dummy_compile_grammar46pl_0_2_1();
    static Predicate $dummy_compile_grammar46pl_0_2_2 = new PRED_$dummy_compile_grammar46pl_0_2_2();
    static Predicate $dummy_compile_grammar46pl_0_2_sub_1 = new PRED_$dummy_compile_grammar46pl_0_2_sub_1();

    public Term arg1, arg2;

    public PRED_$dummy_compile_grammar46pl_0_2(Term a1, Term a2, Predicate cont) {
        arg1 = a1; 
        arg2 = a2; 
        this.cont = cont;
    }

    public PRED_$dummy_compile_grammar46pl_0_2(){}
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
        return engine.jtry($dummy_compile_grammar46pl_0_2_1, $dummy_compile_grammar46pl_0_2_sub_1);
    }

    public int arity() { return 2; }

    public String toString() {
        return "$dummy_compile_grammar.pl_0(" + arg1 + ", " + arg2 + ")";
    }
}

class PRED_$dummy_compile_grammar46pl_0_2_sub_1 extends PRED_$dummy_compile_grammar46pl_0_2 {

    public Predicate exec(Prolog engine) {
        return engine.trust($dummy_compile_grammar46pl_0_2_2);
    }
}

class PRED_$dummy_compile_grammar46pl_0_2_1 extends PRED_$dummy_compile_grammar46pl_0_2 {

    public Predicate exec(Prolog engine) {
        Term a1, a2;
        Predicate p1;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        Predicate cont = engine.cont;

        p1 = new PRED_compile_grammar_2(a1, a2, cont);
        return new PRED_filter_chart_0(p1);
    }
}

class PRED_$dummy_compile_grammar46pl_0_2_2 extends PRED_$dummy_compile_grammar46pl_0_2 {

    public Predicate exec(Prolog engine) {
        Term a1, a2;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        Predicate cont = engine.cont;

        return new PRED_compile_last_grammar_1(a1, cont);
    }
}

