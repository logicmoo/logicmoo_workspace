package shef.nlp.supple.prolog.cafe;
import jp.ac.kobe_u.cs.prolog.lang.*;
import jp.ac.kobe_u.cs.prolog.builtin.*;

/*
 * *** Please do not edit ! ***
 * @(#) PRED_write_edges_1.java
 * @procedure write_edges/1 in plcafe_supple_io.pl
 */

/*
 * @version Prolog Cafe 0.8 November 2003
 * @author Mutsunori Banbara (banbara@kobe-u.ac.jp)
 * @author Naoyuki Tamura    (tamura@kobe-u.ac.jp)
 */

public class PRED_write_edges_1 extends Predicate {
    static Predicate fail_0 = new PRED_fail_0();
    static Predicate write_edges_1_1 = new PRED_write_edges_1_1();
    static Predicate write_edges_1_2 = new PRED_write_edges_1_2();
    static Predicate write_edges_1_3 = new PRED_write_edges_1_3();
    static Predicate write_edges_1_lis = new PRED_write_edges_1_lis();
    static Predicate write_edges_1_var = new PRED_write_edges_1_var();

    public Term arg1;

    public PRED_write_edges_1(Term a1, Predicate cont) {
        arg1 = a1; 
        this.cont = cont;
    }

    public PRED_write_edges_1(){}
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
                                   write_edges_1_var,
                                   fail_0,
                                   write_edges_1_1,
                                   fail_0,
                                   write_edges_1_lis
                                   );
    }

    public int arity() { return 1; }

    public String toString() {
        return "write_edges(" + arg1 + ")";
    }
}

class PRED_write_edges_1_var extends PRED_write_edges_1 {
    static Predicate write_edges_1_var_1 = new PRED_write_edges_1_var_1();

    public Predicate exec(Prolog engine) {
        return engine.jtry(write_edges_1_1, write_edges_1_var_1);
    }
}

class PRED_write_edges_1_var_1 extends PRED_write_edges_1 {
    static Predicate write_edges_1_var_2 = new PRED_write_edges_1_var_2();

    public Predicate exec(Prolog engine) {
        return engine.retry(write_edges_1_2, write_edges_1_var_2);
    }
}

class PRED_write_edges_1_var_2 extends PRED_write_edges_1 {

    public Predicate exec(Prolog engine) {
        return engine.trust(write_edges_1_3);
    }
}

class PRED_write_edges_1_lis extends PRED_write_edges_1 {
    static Predicate write_edges_1_lis_1 = new PRED_write_edges_1_lis_1();

    public Predicate exec(Prolog engine) {
        return engine.jtry(write_edges_1_2, write_edges_1_lis_1);
    }
}

class PRED_write_edges_1_lis_1 extends PRED_write_edges_1 {

    public Predicate exec(Prolog engine) {
        return engine.trust(write_edges_1_3);
    }
}

class PRED_write_edges_1_1 extends PRED_write_edges_1 {
    static SymbolTerm s1 = SymbolTerm.makeSymbol("[]");

    public Predicate exec(Prolog engine) {
        Term a1, a2;
        Predicate p1, p2;
        a1 = engine.aregs[1].dereference();
        Predicate cont = engine.cont;

        if ( !s1.unify(a1, engine.trail) ) return engine.fail();
        a2 = new VariableTerm(engine);
        p1 = new PRED_$cut_1(a2, cont);
        p2 = new PRED_nl_0(p1);
        return new PRED_$get_level_1(a2, p2);
    }
}

class PRED_write_edges_1_2 extends PRED_write_edges_1 {
    static SymbolTerm s1 = SymbolTerm.makeSymbol("[]");
    static SymbolTerm s2 = SymbolTerm.makeSymbol("  ");

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3;
        Predicate p1, p2, p3, p4;
        a1 = engine.aregs[1].dereference();
        Predicate cont = engine.cont;

        if ( a1.isList() ){
            a2 = ((ListTerm)a1).car();
            if ( !s1.unify(((ListTerm)a1).cdr(), engine.trail) )
                return engine.fail();
        } else if ( a1.isVariable() ){
            a2 = new VariableTerm(engine);
            if ( !a1.unify(new ListTerm(a2, s1), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        a3 = new VariableTerm(engine);
        p1 = new PRED_$cut_1(a3, cont);
        p2 = new PRED_nl_0(p1);
        p3 = new PRED_writeq_1(a2, p2);
        p4 = new PRED_write_1(s2, p3);
        return new PRED_$get_level_1(a3, p4);
    }
}

class PRED_write_edges_1_3 extends PRED_write_edges_1 {
    static SymbolTerm s1 = SymbolTerm.makeSymbol("  ");
    static SymbolTerm s2 = SymbolTerm.makeSymbol(",");

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4;
        Predicate p1, p2, p3, p4, p5, p6;
        a1 = engine.aregs[1].dereference();
        Predicate cont = engine.cont;

        if ( a1.isList() ){
            a2 = ((ListTerm)a1).car();
            a3 = ((ListTerm)a1).cdr();
        } else if ( a1.isVariable() ){
            a2 = new VariableTerm(engine);
            a3 = new VariableTerm(engine);
            if ( !a1.unify(new ListTerm(a2, a3), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        a4 = new VariableTerm(engine);
        p1 = new PRED_$cut_1(a4, cont);
        p2 = new PRED_write_edges_1(a3, p1);
        p3 = new PRED_nl_0(p2);
        p4 = new PRED_write_1(s2, p3);
        p5 = new PRED_writeq_1(a2, p4);
        p6 = new PRED_write_1(s1, p5);
        return new PRED_$get_level_1(a4, p6);
    }
}

