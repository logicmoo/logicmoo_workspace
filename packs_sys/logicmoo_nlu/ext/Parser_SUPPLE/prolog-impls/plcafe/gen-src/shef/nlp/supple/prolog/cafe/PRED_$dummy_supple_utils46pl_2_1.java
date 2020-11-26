package shef.nlp.supple.prolog.cafe;
import jp.ac.kobe_u.cs.prolog.lang.*;
import jp.ac.kobe_u.cs.prolog.builtin.*;

/*
 * *** Please do not edit ! ***
 * @(#) PRED_$dummy_supple_utils46pl_2_1.java
 * @procedure $dummy_supple_utils.pl_2/1 in supple_utils.pl
 */

/*
 * @version Prolog Cafe 0.8 November 2003
 * @author Mutsunori Banbara (banbara@kobe-u.ac.jp)
 * @author Naoyuki Tamura    (tamura@kobe-u.ac.jp)
 */

public class PRED_$dummy_supple_utils46pl_2_1 extends Predicate {
    static Predicate $dummy_supple_utils46pl_2_1_1 = new PRED_$dummy_supple_utils46pl_2_1_1();
    static Predicate $dummy_supple_utils46pl_2_1_2 = new PRED_$dummy_supple_utils46pl_2_1_2();
    static Predicate $dummy_supple_utils46pl_2_1_sub_1 = new PRED_$dummy_supple_utils46pl_2_1_sub_1();

    public Term arg1;

    public PRED_$dummy_supple_utils46pl_2_1(Term a1, Predicate cont) {
        arg1 = a1; 
        this.cont = cont;
    }

    public PRED_$dummy_supple_utils46pl_2_1(){}
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
        return engine.jtry($dummy_supple_utils46pl_2_1_1, $dummy_supple_utils46pl_2_1_sub_1);
    }

    public int arity() { return 1; }

    public String toString() {
        return "$dummy_supple_utils.pl_2(" + arg1 + ")";
    }
}

class PRED_$dummy_supple_utils46pl_2_1_sub_1 extends PRED_$dummy_supple_utils46pl_2_1 {

    public Predicate exec(Prolog engine) {
        return engine.trust($dummy_supple_utils46pl_2_1_2);
    }
}

class PRED_$dummy_supple_utils46pl_2_1_1 extends PRED_$dummy_supple_utils46pl_2_1 {
    static SymbolTerm f1 = SymbolTerm.makeSymbol(":", 2);
    static SymbolTerm s2 = SymbolTerm.makeSymbol("shef.nlp.supple.prolog.cafe");

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3;
        Predicate p1, p2, p3;
        a1 = engine.aregs[1].dereference();
        Predicate cont = engine.cont;

        a2 = new VariableTerm(engine);
        Term[] h3 = {s2, a1};
        a3 = new StructureTerm(f1, h3);
        p1 = new PRED_fail_0(cont);
        p2 = new PRED_$cut_1(a2, p1);
        p3 = new PRED_translated_goal_1(a3, p2);
        return new PRED_$get_level_1(a2, p3);
    }
}

class PRED_$dummy_supple_utils46pl_2_1_2 extends PRED_$dummy_supple_utils46pl_2_1 {

    public Predicate exec(Prolog engine) {
        return engine.cont;
    }
}

