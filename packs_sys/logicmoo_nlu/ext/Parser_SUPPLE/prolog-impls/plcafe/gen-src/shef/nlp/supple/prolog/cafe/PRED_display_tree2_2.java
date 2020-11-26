package shef.nlp.supple.prolog.cafe;
import jp.ac.kobe_u.cs.prolog.lang.*;
import jp.ac.kobe_u.cs.prolog.builtin.*;

/*
 * *** Please do not edit ! ***
 * @(#) PRED_display_tree2_2.java
 * @procedure display_tree2/2 in plcafe_supple_io.pl
 */

/*
 * @version Prolog Cafe 0.8 November 2003
 * @author Mutsunori Banbara (banbara@kobe-u.ac.jp)
 * @author Naoyuki Tamura    (tamura@kobe-u.ac.jp)
 */

public class PRED_display_tree2_2 extends Predicate {
    static Predicate fail_0 = new PRED_fail_0();
    static Predicate display_tree2_2_1 = new PRED_display_tree2_2_1();
    static Predicate display_tree2_2_2 = new PRED_display_tree2_2_2();
    static Predicate display_tree2_2_3 = new PRED_display_tree2_2_3();
    static Predicate display_tree2_2_lis = new PRED_display_tree2_2_lis();
    static Predicate display_tree2_2_var = new PRED_display_tree2_2_var();

    public Term arg1, arg2;

    public PRED_display_tree2_2(Term a1, Term a2, Predicate cont) {
        arg1 = a1; 
        arg2 = a2; 
        this.cont = cont;
    }

    public PRED_display_tree2_2(){}
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
        return engine.switch_on_term(
                                   display_tree2_2_var,
                                   fail_0,
                                   display_tree2_2_3,
                                   fail_0,
                                   display_tree2_2_lis
                                   );
    }

    public int arity() { return 2; }

    public String toString() {
        return "display_tree2(" + arg1 + ", " + arg2 + ")";
    }
}

class PRED_display_tree2_2_var extends PRED_display_tree2_2 {
    static Predicate display_tree2_2_var_1 = new PRED_display_tree2_2_var_1();

    public Predicate exec(Prolog engine) {
        return engine.jtry(display_tree2_2_1, display_tree2_2_var_1);
    }
}

class PRED_display_tree2_2_var_1 extends PRED_display_tree2_2 {
    static Predicate display_tree2_2_var_2 = new PRED_display_tree2_2_var_2();

    public Predicate exec(Prolog engine) {
        return engine.retry(display_tree2_2_2, display_tree2_2_var_2);
    }
}

class PRED_display_tree2_2_var_2 extends PRED_display_tree2_2 {

    public Predicate exec(Prolog engine) {
        return engine.trust(display_tree2_2_3);
    }
}

class PRED_display_tree2_2_lis extends PRED_display_tree2_2 {
    static Predicate display_tree2_2_lis_1 = new PRED_display_tree2_2_lis_1();

    public Predicate exec(Prolog engine) {
        return engine.jtry(display_tree2_2_1, display_tree2_2_lis_1);
    }
}

class PRED_display_tree2_2_lis_1 extends PRED_display_tree2_2 {

    public Predicate exec(Prolog engine) {
        return engine.trust(display_tree2_2_2);
    }
}

class PRED_display_tree2_2_1 extends PRED_display_tree2_2 {
    static SymbolTerm s1 = SymbolTerm.makeSymbol("[]");
    static SymbolTerm f2 = SymbolTerm.makeSymbol(":", 2);
    static SymbolTerm s3 = SymbolTerm.makeSymbol("s_form");
    static IntegerTerm s5 = new IntegerTerm(2);
    static SymbolTerm s6 = SymbolTerm.makeSymbol("\"");

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4, a5, a6, a7, a8, a9, a10;
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
        a7 = new VariableTerm(engine);
        a8 = new VariableTerm(engine);
        a6 = new ListTerm(a7, a8);
        a10 = new VariableTerm(engine);
        Term[] h4 = {s3, a10};
        a9 = new StructureTerm(f2, h4);
        p1 = new PRED_display_tree2_2(a4, a2, cont);
        p2 = new PRED_nl_0(p1);
        p3 = new PRED_write_1(s6, p2);
        p4 = new PRED_write_1(a10, p3);
        p5 = new PRED_write_1(s6, p4);
        p6 = new PRED_tab_1(s5, p5);
        p7 = new PRED_write_1(a7, p6);
        p8 = new PRED_tab_1(a2, p7);
        p9 = new PRED_member_2(a9, a8, p8);
        p10 = new PRED_$614646_2(a5, a6, p9);
        return new PRED_edge_10(new VariableTerm(engine), new VariableTerm(engine), a5, s1, new VariableTerm(engine), s1, new VariableTerm(engine), new VariableTerm(engine), new VariableTerm(engine), a3, p10);
    }
}

class PRED_display_tree2_2_2 extends PRED_display_tree2_2 {
    static SymbolTerm s1 = SymbolTerm.makeSymbol("[]");
    static IntegerTerm s2 = new IntegerTerm(2);

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4, a5, a6, a7, a8, a9, a10;
        Predicate p1, p2, p3, p4, p5, p6, p7, p8;
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
        a8 = new VariableTerm(engine);
        a7 = new ListTerm(a8, new VariableTerm(engine));
        a9 = new VariableTerm(engine);
        a10 = new VariableTerm(engine);
        p1 = new PRED_display_tree2_2(a4, a2, cont);
        p2 = new PRED_display_tree2_2(a9, a10, p1);
        p3 = new PRED_$plus_3(a2, s2, a10, p2);
        p4 = new PRED_nl_0(p3);
        p5 = new PRED_write_1(a8, p4);
        p6 = new PRED_tab_1(a2, p5);
        p7 = new PRED_reverse_2(a6, a9, p6);
        p8 = new PRED_$614646_2(a5, a7, p7);
        return new PRED_edge_10(new VariableTerm(engine), new VariableTerm(engine), a5, s1, new VariableTerm(engine), a6, new VariableTerm(engine), new VariableTerm(engine), new VariableTerm(engine), a3, p8);
    }
}

class PRED_display_tree2_2_3 extends PRED_display_tree2_2 {
    static SymbolTerm s1 = SymbolTerm.makeSymbol("[]");

    public Predicate exec(Prolog engine) {
        Term a1, a2;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        Predicate cont = engine.cont;

        if ( !s1.unify(a1, engine.trail) ) return engine.fail();
        return cont;
    }
}

