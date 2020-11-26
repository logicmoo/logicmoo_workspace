package shef.nlp.supple.prolog.cafe;
import jp.ac.kobe_u.cs.prolog.lang.*;
import jp.ac.kobe_u.cs.prolog.builtin.*;

/*
 * *** Please do not edit ! ***
 * @(#) PRED_display_tree_2.java
 * @procedure display_tree/2 in plcafe_supple_io.pl
 */

/*
 * @version Prolog Cafe 0.8 November 2003
 * @author Mutsunori Banbara (banbara@kobe-u.ac.jp)
 * @author Naoyuki Tamura    (tamura@kobe-u.ac.jp)
 */

public class PRED_display_tree_2 extends Predicate {
    static Predicate display_tree_2_1 = new PRED_display_tree_2_1();
    static Predicate display_tree_2_2 = new PRED_display_tree_2_2();
    static Predicate display_tree_2_sub_1 = new PRED_display_tree_2_sub_1();

    public Term arg1, arg2;

    public PRED_display_tree_2(Term a1, Term a2, Predicate cont) {
        arg1 = a1; 
        arg2 = a2; 
        this.cont = cont;
    }

    public PRED_display_tree_2(){}
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
        return engine.jtry(display_tree_2_1, display_tree_2_sub_1);
    }

    public int arity() { return 2; }

    public String toString() {
        return "display_tree(" + arg1 + ", " + arg2 + ")";
    }
}

class PRED_display_tree_2_sub_1 extends PRED_display_tree_2 {

    public Predicate exec(Prolog engine) {
        return engine.trust(display_tree_2_2);
    }
}

class PRED_display_tree_2_1 extends PRED_display_tree_2 {
    static SymbolTerm s1 = SymbolTerm.makeSymbol("[]");
    static SymbolTerm s2 = SymbolTerm.makeSymbol("inactive");

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4, a5, a6, a7, a8, a9;
        Predicate p1, p2, p3, p4, p5, p6, p7, p8, p9;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        Predicate cont = engine.cont;

        a3 = new VariableTerm(engine);
        a4 = new VariableTerm(engine);
        a5 = new VariableTerm(engine);
        a7 = new VariableTerm(engine);
        a6 = new ListTerm(a1, a7);
        a8 = new VariableTerm(engine);
        a9 = new VariableTerm(engine);
        p1 = new PRED_fail_0(cont);
        p2 = new PRED_nl_0(p1);
        p3 = new PRED_nl_0(p2);
        p4 = new PRED_write_1(a9, p3);
        p5 = new PRED_semantics_4(a1, s2, a5, a9, p4);
        p6 = new PRED_nl_0(p5);
        p7 = new PRED_display_tree_3(a1, a7, a8, p6);
        p8 = new PRED_reverse_2(a4, a8, p7);
        p9 = new PRED_$614646_2(a3, a6, p8);
        return new PRED_edge_10(new VariableTerm(engine), new VariableTerm(engine), a3, s1, new VariableTerm(engine), a4, a2, new VariableTerm(engine), new VariableTerm(engine), a5, p9);
    }
}

class PRED_display_tree_2_2 extends PRED_display_tree_2 {

    public Predicate exec(Prolog engine) {
        Term a1, a2;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        Predicate cont = engine.cont;

        return new PRED_$neck_cut_0(cont);
    }
}

