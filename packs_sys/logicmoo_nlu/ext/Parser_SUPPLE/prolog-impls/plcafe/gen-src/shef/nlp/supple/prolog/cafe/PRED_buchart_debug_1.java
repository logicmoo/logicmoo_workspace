package shef.nlp.supple.prolog.cafe;
import jp.ac.kobe_u.cs.prolog.lang.*;
import jp.ac.kobe_u.cs.prolog.builtin.*;

/*
 * *** Please do not edit ! ***
 * @(#) PRED_buchart_debug_1.java
 * @procedure buchart_debug/1 in plcafe_supple_io.pl
 */

/*
 * @version Prolog Cafe 0.8 November 2003
 * @author Mutsunori Banbara (banbara@kobe-u.ac.jp)
 * @author Naoyuki Tamura    (tamura@kobe-u.ac.jp)
 */

public class PRED_buchart_debug_1 extends Predicate {
    static Predicate fail_0 = new PRED_fail_0();
    static Predicate buchart_debug_1_1 = new PRED_buchart_debug_1_1();
    static Predicate buchart_debug_1_2 = new PRED_buchart_debug_1_2();
    static Predicate buchart_debug_1_var = new PRED_buchart_debug_1_var();

    public Term arg1;

    public PRED_buchart_debug_1(Term a1, Predicate cont) {
        arg1 = a1; 
        this.cont = cont;
    }

    public PRED_buchart_debug_1(){}
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
        return engine.switch_on_term(
                                   buchart_debug_1_var,
                                   buchart_debug_1_var,
                                   fail_0,
                                   fail_0,
                                   fail_0
                                   );
    }

    public int arity() { return 1; }

    public String toString() {
        return "buchart_debug(" + arg1 + ")";
    }
}

class PRED_buchart_debug_1_var extends PRED_buchart_debug_1 {
    static Predicate buchart_debug_1_var_1 = new PRED_buchart_debug_1_var_1();

    public Predicate exec(Prolog engine) {
        return engine.jtry(buchart_debug_1_1, buchart_debug_1_var_1);
    }
}

class PRED_buchart_debug_1_var_1 extends PRED_buchart_debug_1 {

    public Predicate exec(Prolog engine) {
        return engine.trust(buchart_debug_1_2);
    }
}

class PRED_buchart_debug_1_1 extends PRED_buchart_debug_1 {
    static IntegerTerm s1 = new IntegerTerm(1);
    static SymbolTerm s2 = SymbolTerm.makeSymbol("debug_on");
    static SymbolTerm s3 = SymbolTerm.makeSymbol("verbose");

    public Predicate exec(Prolog engine) {
        Term a1;
        Predicate p1, p2, p3;
        a1 = engine.aregs[1].dereference();
        Predicate cont = engine.cont;

        if ( !s1.unify(a1, engine.trail) ) return engine.fail();
        p1 = new PRED_assert_1(s2, cont);
        p2 = new PRED_assert_1(s3, p1);
        p3 = new PRED_retractall_1(s3, p2);
        return new PRED_retractall_1(s2, p3);
    }
}

class PRED_buchart_debug_1_2 extends PRED_buchart_debug_1 {
    static IntegerTerm s1 = new IntegerTerm(0);
    static SymbolTerm s2 = SymbolTerm.makeSymbol("debug_on");

    public Predicate exec(Prolog engine) {
        Term a1;
        a1 = engine.aregs[1].dereference();
        Predicate cont = engine.cont;

        if ( !s1.unify(a1, engine.trail) ) return engine.fail();
        return new PRED_retractall_1(s2, cont);
    }
}

