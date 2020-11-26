package shef.nlp.supple.prolog.cafe;
import jp.ac.kobe_u.cs.prolog.lang.*;
import jp.ac.kobe_u.cs.prolog.builtin.*;

/*
 * *** Please do not edit ! ***
 * @(#) PRED_display_tree_3.java
 * @procedure display_tree/3 in plcafe_supple_io.pl
 */

/*
 * @version Prolog Cafe 0.8 November 2003
 * @author Mutsunori Banbara (banbara@kobe-u.ac.jp)
 * @author Naoyuki Tamura    (tamura@kobe-u.ac.jp)
 */

public class PRED_display_tree_3 extends Predicate {
    static Predicate display_tree_3_1 = new PRED_display_tree_3_1();
    static Predicate display_tree_3_2 = new PRED_display_tree_3_2();
    static Predicate display_tree_3_sub_1 = new PRED_display_tree_3_sub_1();

    public Term arg1, arg2, arg3;

    public PRED_display_tree_3(Term a1, Term a2, Term a3, Predicate cont) {
        arg1 = a1; 
        arg2 = a2; 
        arg3 = a3; 
        this.cont = cont;
    }

    public PRED_display_tree_3(){}
    public void setArgument(Term[] args, Predicate cont) {
        arg1 = args[0]; 
        arg2 = args[1]; 
        arg3 = args[2]; 
        this.cont = cont;
    }

    public Predicate exec(Prolog engine) {
        engine.aregs[1] = arg1;
        engine.aregs[2] = arg2;
        engine.aregs[3] = arg3;
        engine.cont = cont;
        return call(engine);
    }

    public Predicate call(Prolog engine) {
        engine.setB0();
        return engine.jtry(display_tree_3_1, display_tree_3_sub_1);
    }

    public int arity() { return 3; }

    public String toString() {
        return "display_tree(" + arg1 + ", " + arg2 + ", " + arg3 + ")";
    }
}

class PRED_display_tree_3_sub_1 extends PRED_display_tree_3 {

    public Predicate exec(Prolog engine) {
        return engine.trust(display_tree_3_2);
    }
}

class PRED_display_tree_3_1 extends PRED_display_tree_3 {
    static SymbolTerm s1 = SymbolTerm.makeSymbol("[]");
    static SymbolTerm f2 = SymbolTerm.makeSymbol(":", 2);
    static SymbolTerm s3 = SymbolTerm.makeSymbol("s_form");
    static IntegerTerm s5 = new IntegerTerm(2);
    static SymbolTerm s6 = SymbolTerm.makeSymbol("\"");

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4, a5, a6;
        Predicate p1, p2, p3, p4, p5, p6, p7, p8, p9, p10;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        a3 = engine.aregs[3].dereference();
        Predicate cont = engine.cont;

        if ( !s1.unify(a3, engine.trail) ) return engine.fail();
        a4 = new VariableTerm(engine);
        a6 = new VariableTerm(engine);
        Term[] h4 = {s3, a6};
        a5 = new StructureTerm(f2, h4);
        p1 = new PRED_$cut_1(a4, cont);
        p2 = new PRED_nl_0(p1);
        p3 = new PRED_write_1(s6, p2);
        p4 = new PRED_write_1(a6, p3);
        p5 = new PRED_write_1(s6, p4);
        p6 = new PRED_tab_1(s5, p5);
        p7 = new PRED_write_1(a1, p6);
        p8 = new PRED_member_2(a5, a2, p7);
        p9 = new PRED_nl_0(p8);
        p10 = new PRED_write_1(a1, p9);
        return new PRED_$get_level_1(a4, p10);
    }
}

class PRED_display_tree_3_2 extends PRED_display_tree_3 {
    static IntegerTerm s1 = new IntegerTerm(1);

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4;
        Predicate p1, p2, p3, p4;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        a3 = engine.aregs[3].dereference();
        Predicate cont = engine.cont;

        a4 = new VariableTerm(engine);
        p1 = new PRED_$cut_1(a4, cont);
        p2 = new PRED_display_tree2_2(a3, s1, p1);
        p3 = new PRED_nl_0(p2);
        p4 = new PRED_write_1(a1, p3);
        return new PRED_$get_level_1(a4, p4);
    }
}

