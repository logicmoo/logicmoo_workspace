package shef.nlp.supple.prolog.cafe;
import jp.ac.kobe_u.cs.prolog.lang.*;
import jp.ac.kobe_u.cs.prolog.builtin.*;

/*
 * *** Please do not edit ! ***
 * @(#) PRED_foreach_2.java
 * @procedure foreach/2 in supple_utils.pl
 */

/*
 * @version Prolog Cafe 0.8 November 2003
 * @author Mutsunori Banbara (banbara@kobe-u.ac.jp)
 * @author Naoyuki Tamura    (tamura@kobe-u.ac.jp)
 */

public class PRED_foreach_2 extends Predicate {
    static Predicate foreach_2_1 = new PRED_foreach_2_1();
    static Predicate foreach_2_2 = new PRED_foreach_2_2();
    static Predicate foreach_2_sub_1 = new PRED_foreach_2_sub_1();

    public Term arg1, arg2;

    public PRED_foreach_2(Term a1, Term a2, Predicate cont) {
        arg1 = a1; 
        arg2 = a2; 
        this.cont = cont;
    }

    public PRED_foreach_2(){}
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
        return engine.jtry(foreach_2_1, foreach_2_sub_1);
    }

    public int arity() { return 2; }

    public String toString() {
        return "foreach(" + arg1 + ", " + arg2 + ")";
    }
}

class PRED_foreach_2_sub_1 extends PRED_foreach_2 {

    public Predicate exec(Prolog engine) {
        return engine.trust(foreach_2_2);
    }
}

class PRED_foreach_2_1 extends PRED_foreach_2 {
    static SymbolTerm f1 = SymbolTerm.makeSymbol(":", 2);
    static SymbolTerm s2 = SymbolTerm.makeSymbol("shef.nlp.supple.prolog.cafe");

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3;
        Predicate p1, p2;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        Predicate cont = engine.cont;

        Term[] h3 = {s2, a1};
        a3 = new StructureTerm(f1, h3);
        p1 = new PRED_fail_0(cont);
        p2 = new PRED_do_1(a2, p1);
        return new PRED_translated_goal_1(a3, p2);
    }
}

class PRED_foreach_2_2 extends PRED_foreach_2 {

    public Predicate exec(Prolog engine) {
        return engine.cont;
    }
}

