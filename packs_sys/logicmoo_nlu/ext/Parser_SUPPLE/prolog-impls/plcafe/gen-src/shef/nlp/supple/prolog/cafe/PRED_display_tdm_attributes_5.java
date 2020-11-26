package shef.nlp.supple.prolog.cafe;
import jp.ac.kobe_u.cs.prolog.lang.*;
import jp.ac.kobe_u.cs.prolog.builtin.*;

/*
 * *** Please do not edit ! ***
 * @(#) PRED_display_tdm_attributes_5.java
 * @procedure display_tdm_attributes/5 in plcafe_supple_io.pl
 */

/*
 * @version Prolog Cafe 0.8 November 2003
 * @author Mutsunori Banbara (banbara@kobe-u.ac.jp)
 * @author Naoyuki Tamura    (tamura@kobe-u.ac.jp)
 */

public class PRED_display_tdm_attributes_5 extends Predicate {
    static Predicate display_tdm_attributes_5_1 = new PRED_display_tdm_attributes_5_1();
    static Predicate display_tdm_attributes_5_2 = new PRED_display_tdm_attributes_5_2();
    static Predicate display_tdm_attributes_5_sub_1 = new PRED_display_tdm_attributes_5_sub_1();

    public Term arg1, arg2, arg3, arg4, arg5;

    public PRED_display_tdm_attributes_5(Term a1, Term a2, Term a3, Term a4, Term a5, Predicate cont) {
        arg1 = a1; 
        arg2 = a2; 
        arg3 = a3; 
        arg4 = a4; 
        arg5 = a5; 
        this.cont = cont;
    }

    public PRED_display_tdm_attributes_5(){}
    public void setArgument(Term[] args, Predicate cont) {
        arg1 = args[0]; 
        arg2 = args[1]; 
        arg3 = args[2]; 
        arg4 = args[3]; 
        arg5 = args[4]; 
        this.cont = cont;
    }

    public Predicate exec(Prolog engine) {
        engine.aregs[1] = arg1;
        engine.aregs[2] = arg2;
        engine.aregs[3] = arg3;
        engine.aregs[4] = arg4;
        engine.aregs[5] = arg5;
        engine.cont = cont;
        return call(engine);
    }

    public Predicate call(Prolog engine) {
        engine.setB0();
        return engine.jtry(display_tdm_attributes_5_1, display_tdm_attributes_5_sub_1);
    }

    public int arity() { return 5; }

    public String toString() {
        return "display_tdm_attributes(" + arg1 + ", " + arg2 + ", " + arg3 + ", " + arg4 + ", " + arg5 + ")";
    }
}

class PRED_display_tdm_attributes_5_sub_1 extends PRED_display_tdm_attributes_5 {

    public Predicate exec(Prolog engine) {
        return engine.trust(display_tdm_attributes_5_2);
    }
}

class PRED_display_tdm_attributes_5_1 extends PRED_display_tdm_attributes_5 {
    static SymbolTerm s1 = SymbolTerm.makeSymbol("[]");

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4, a5;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        a3 = engine.aregs[3].dereference();
        a4 = engine.aregs[4].dereference();
        a5 = engine.aregs[5].dereference();
        Predicate cont = engine.cont;

        if ( !s1.unify(a2, engine.trail) ) return engine.fail();
        return new PRED_$neck_cut_0(cont);
    }
}

class PRED_display_tdm_attributes_5_2 extends PRED_display_tdm_attributes_5 {
    static SymbolTerm s1 = SymbolTerm.makeSymbol("syntax");
    static SymbolTerm s2 = SymbolTerm.makeSymbol("constituents");
    static SymbolTerm s3 = SymbolTerm.makeSymbol("[]");

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13;
        Predicate p1, p2, p3, p4, p5;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        a3 = engine.aregs[3].dereference();
        a4 = engine.aregs[4].dereference();
        a5 = engine.aregs[5].dereference();
        Predicate cont = engine.cont;

        a6 = new VariableTerm(engine);
        a7 = new VariableTerm(engine);
        a13 = new ListTerm(a7, s3);
        a12 = new ListTerm(s2, a13);
        a11 = new ListTerm(a1, a12);
        a10 = new ListTerm(a4, a11);
        a9 = new ListTerm(a3, a10);
        a8 = new ListTerm(s1, a9);
        p1 = new PRED_$cut_1(a6, cont);
        p2 = new PRED_display_tdm_child_attributes_2(a2, a5, p1);
        p3 = new PRED_nl_0(p2);
        p4 = new PRED_write_list3_1(a8, p3);
        p5 = new PRED_length_2(a2, a7, p4);
        return new PRED_$get_level_1(a6, p5);
    }
}

