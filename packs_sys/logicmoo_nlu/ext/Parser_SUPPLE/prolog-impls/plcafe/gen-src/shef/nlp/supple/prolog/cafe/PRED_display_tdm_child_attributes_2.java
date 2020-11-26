package shef.nlp.supple.prolog.cafe;
import jp.ac.kobe_u.cs.prolog.lang.*;
import jp.ac.kobe_u.cs.prolog.builtin.*;

/*
 * *** Please do not edit ! ***
 * @(#) PRED_display_tdm_child_attributes_2.java
 * @procedure display_tdm_child_attributes/2 in plcafe_supple_io.pl
 */

/*
 * @version Prolog Cafe 0.8 November 2003
 * @author Mutsunori Banbara (banbara@kobe-u.ac.jp)
 * @author Naoyuki Tamura    (tamura@kobe-u.ac.jp)
 */

public class PRED_display_tdm_child_attributes_2 extends Predicate {
    static PRED_display_tdm_child_attributes_2 entry_code;
    static Predicate fail_0 = new PRED_fail_0();
    static Predicate display_tdm_child_attributes_2_1 = new PRED_display_tdm_child_attributes_2_1();
    static Predicate display_tdm_child_attributes_2_2 = new PRED_display_tdm_child_attributes_2_2();
    static Predicate display_tdm_child_attributes_2_3 = new PRED_display_tdm_child_attributes_2_3();
    static Predicate display_tdm_child_attributes_2_4 = new PRED_display_tdm_child_attributes_2_4();
    static Predicate display_tdm_child_attributes_2_lis = new PRED_display_tdm_child_attributes_2_lis();
    static Predicate display_tdm_child_attributes_2_var = new PRED_display_tdm_child_attributes_2_var();

    public Term arg1, arg2;

    public PRED_display_tdm_child_attributes_2(Term a1, Term a2, Predicate cont) {
        arg1 = a1; 
        arg2 = a2; 
        this.cont = cont;
    }

    public PRED_display_tdm_child_attributes_2(){}
    public void setArgument(Term[] args, Predicate cont) {
        arg1 = args[0]; 
        arg2 = args[1]; 
        this.cont = cont;
    }

    public Predicate exec(Prolog engine) {
        entry_code = this;
        engine.aregs[1] = arg1;
        engine.aregs[2] = arg2;
        engine.cont = cont;
        return call(engine);
    }

    public Predicate call(Prolog engine) {
        engine.setB0();
        return engine.switch_on_term(
                                   display_tdm_child_attributes_2_var,
                                   fail_0,
                                   display_tdm_child_attributes_2_1,
                                   fail_0,
                                   display_tdm_child_attributes_2_lis
                                   );
    }

    public int arity() { return 2; }

    public String toString() {
        return "display_tdm_child_attributes(" + arg1 + ", " + arg2 + ")";
    }
}

class PRED_display_tdm_child_attributes_2_var extends PRED_display_tdm_child_attributes_2 {
    static Predicate display_tdm_child_attributes_2_var_1 = new PRED_display_tdm_child_attributes_2_var_1();

    public Predicate exec(Prolog engine) {
        return engine.jtry(display_tdm_child_attributes_2_1, display_tdm_child_attributes_2_var_1);
    }
}

class PRED_display_tdm_child_attributes_2_var_1 extends PRED_display_tdm_child_attributes_2 {
    static Predicate display_tdm_child_attributes_2_var_2 = new PRED_display_tdm_child_attributes_2_var_2();

    public Predicate exec(Prolog engine) {
        return engine.retry(display_tdm_child_attributes_2_2, display_tdm_child_attributes_2_var_2);
    }
}

class PRED_display_tdm_child_attributes_2_var_2 extends PRED_display_tdm_child_attributes_2 {
    static Predicate display_tdm_child_attributes_2_var_3 = new PRED_display_tdm_child_attributes_2_var_3();

    public Predicate exec(Prolog engine) {
        return engine.retry(display_tdm_child_attributes_2_3, display_tdm_child_attributes_2_var_3);
    }
}

class PRED_display_tdm_child_attributes_2_var_3 extends PRED_display_tdm_child_attributes_2 {

    public Predicate exec(Prolog engine) {
        return engine.trust(display_tdm_child_attributes_2_4);
    }
}

class PRED_display_tdm_child_attributes_2_lis extends PRED_display_tdm_child_attributes_2 {
    static Predicate display_tdm_child_attributes_2_lis_1 = new PRED_display_tdm_child_attributes_2_lis_1();

    public Predicate exec(Prolog engine) {
        return engine.jtry(display_tdm_child_attributes_2_2, display_tdm_child_attributes_2_lis_1);
    }
}

class PRED_display_tdm_child_attributes_2_lis_1 extends PRED_display_tdm_child_attributes_2 {
    static Predicate display_tdm_child_attributes_2_lis_2 = new PRED_display_tdm_child_attributes_2_lis_2();

    public Predicate exec(Prolog engine) {
        return engine.retry(display_tdm_child_attributes_2_3, display_tdm_child_attributes_2_lis_2);
    }
}

class PRED_display_tdm_child_attributes_2_lis_2 extends PRED_display_tdm_child_attributes_2 {

    public Predicate exec(Prolog engine) {
        return engine.trust(display_tdm_child_attributes_2_4);
    }
}

class PRED_display_tdm_child_attributes_2_1 extends PRED_display_tdm_child_attributes_2 {
    static SymbolTerm s1 = SymbolTerm.makeSymbol("[]");

    public Predicate exec(Prolog engine) {
        Term a1, a2;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        Predicate cont = engine.cont;

        if ( !s1.unify(a1, engine.trail) ) return engine.fail();
        return new PRED_$neck_cut_0(cont);
    }
}

class PRED_display_tdm_child_attributes_2_2 extends PRED_display_tdm_child_attributes_2 {
    static SymbolTerm s1 = SymbolTerm.makeSymbol("syntax");
    static SymbolTerm s2 = SymbolTerm.makeSymbol("constituents");
    static IntegerTerm s3 = new IntegerTerm(0);
    static SymbolTerm s4 = SymbolTerm.makeSymbol("[]");
    static ListTerm s5 = new ListTerm(s3, s4);
    static ListTerm s6 = new ListTerm(s2, s5);

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14;
        Predicate p1, p2, p3, p4, p5, p6;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        Predicate cont = engine.cont;

        if ( a1.isList() ){
            a3 = ((ListTerm)a1).car();
            a4 = ((ListTerm)a1).cdr();
        } else if ( a1.isVariable() ){
            a3 = new VariableTerm(engine);
            a4 = new VariableTerm(engine);
            if ( !a1.unify(new ListTerm(a3, a4), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        a5 = new VariableTerm(engine);
        a6 = new VariableTerm(engine);
        a7 = new VariableTerm(engine);
        a8 = new VariableTerm(engine);
        a10 = new VariableTerm(engine);
        a9 = new ListTerm(a10, new VariableTerm(engine));
        a14 = new ListTerm(a10, s6);
        a13 = new ListTerm(a8, a14);
        a12 = new ListTerm(a7, a13);
        a11 = new ListTerm(s1, a12);
        p1 = new PRED_$cut_1(a5, cont);
        p2 = new PRED_display_tdm_child_attributes_2(a4, a2, p1);
        p3 = new PRED_nl_0(p2);
        p4 = new PRED_write_list3_1(a11, p3);
        p5 = new PRED_$614646_2(a6, a9, p4);
        p6 = new PRED_$dummy_plcafe_supple_io46pl_2_13(a3, a2, new VariableTerm(engine), new VariableTerm(engine), a6, new VariableTerm(engine), new VariableTerm(engine), a7, a8, new VariableTerm(engine), new VariableTerm(engine), new VariableTerm(engine), new VariableTerm(engine), p5);
        return new PRED_$get_level_1(a5, p6);
    }
}

class PRED_display_tdm_child_attributes_2_3 extends PRED_display_tdm_child_attributes_2 {
    static SymbolTerm s1 = SymbolTerm.makeSymbol("syntax");
    static SymbolTerm s2 = SymbolTerm.makeSymbol("constituents");
    static SymbolTerm s3 = SymbolTerm.makeSymbol("[]");

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19;
        Predicate p1, p2, p3, p4, p5, p6, p7, p8, p9, p10;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        Predicate cont = engine.cont;

        if ( a1.isList() ){
            a3 = ((ListTerm)a1).car();
            a4 = ((ListTerm)a1).cdr();
        } else if ( a1.isVariable() ){
            a3 = new VariableTerm(engine);
            a4 = new VariableTerm(engine);
            if ( !a1.unify(new ListTerm(a3, a4), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        a5 = new VariableTerm(engine);
        a6 = new VariableTerm(engine);
        a7 = new VariableTerm(engine);
        a8 = new VariableTerm(engine);
        a9 = new VariableTerm(engine);
        a11 = new VariableTerm(engine);
        a10 = new ListTerm(a11, new VariableTerm(engine));
        a12 = new VariableTerm(engine);
        a18 = new ListTerm(a12, s3);
        a17 = new ListTerm(s2, a18);
        a16 = new ListTerm(a11, a17);
        a15 = new ListTerm(a9, a16);
        a14 = new ListTerm(a8, a15);
        a13 = new ListTerm(s1, a14);
        a19 = new VariableTerm(engine);
        p1 = new PRED_$cut_1(a5, cont);
        p2 = new PRED_display_tdm_child_attributes_2(a4, a2, p1);
        p3 = new PRED_display_tdm_child_attributes_2(a19, a2, p2);
        p4 = new PRED_reverse_2(a7, a19, p3);
        p5 = new PRED_nl_0(p4);
        p6 = new PRED_write_list3_1(a13, p5);
        p7 = new PRED_length_2(a7, a12, p6);
        p8 = new PRED_$614646_2(a6, a10, p7);
        p9 = new PRED_$cut_1(a5, p8);
        p10 = new PRED_$dummy_plcafe_supple_io46pl_3_14(a3, a2, new VariableTerm(engine), new VariableTerm(engine), a6, new VariableTerm(engine), a7, new VariableTerm(engine), a8, a9, new VariableTerm(engine), new VariableTerm(engine), new VariableTerm(engine), new VariableTerm(engine), p9);
        return new PRED_$get_level_1(a5, p10);
    }
}

class PRED_display_tdm_child_attributes_2_4 extends PRED_display_tdm_child_attributes_2 {

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        Predicate cont = engine.cont;

        if ( a1.isList() ){
            a3 = ((ListTerm)a1).cdr();
        } else if ( a1.isVariable() ){
            a3 = new VariableTerm(engine);
            if ( !a1.unify(new ListTerm(new VariableTerm(engine), a3), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        engine.aregs[1] = a3;
        engine.aregs[2] = a2;
        engine.cont = cont;
        return entry_code.call(engine);
    }
}

