package shef.nlp.supple.prolog.cafe;
import jp.ac.kobe_u.cs.prolog.lang.*;
import jp.ac.kobe_u.cs.prolog.builtin.*;

/*
 * *** Please do not edit ! ***
 * @(#) PRED_write_semantics_1.java
 * @procedure write_semantics/1 in plcafe_supple_io.pl
 */

/*
 * @version Prolog Cafe 0.8 November 2003
 * @author Mutsunori Banbara (banbara@kobe-u.ac.jp)
 * @author Naoyuki Tamura    (tamura@kobe-u.ac.jp)
 */

public class PRED_write_semantics_1 extends Predicate {
    static SymbolTerm s1 = SymbolTerm.makeSymbol("[]");
    static SymbolTerm s2 = SymbolTerm.makeSymbol("semantics");
    static SymbolTerm s3 = SymbolTerm.makeSymbol(" ");

    public Term arg1;

    public PRED_write_semantics_1(Term a1, Predicate cont) {
        arg1 = a1; 
        this.cont = cont;
    }

    public PRED_write_semantics_1(){}
    public void setArgument(Term[] args, Predicate cont) {
        arg1 = args[0]; 
        this.cont = cont;
    }

    public Predicate exec(Prolog engine) {
        engine.setB0();
        Term a1, a2, a3, a4, a5, a6;
        Predicate p1, p2, p3, p4, p5, p6;
        a1 = arg1.dereference();

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
        if ( a3.isList() ){
            a4 = ((ListTerm)a3).car();
            a5 = ((ListTerm)a3).cdr();
        } else if ( a3.isVariable() ){
            a4 = new VariableTerm(engine);
            a5 = new VariableTerm(engine);
            if ( !a3.unify(new ListTerm(a4, a5), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a5.isList() ){
            a6 = ((ListTerm)a5).car();
            if ( !s1.unify(((ListTerm)a5).cdr(), engine.trail) )
                return engine.fail();
        } else if ( a5.isVariable() ){
            a6 = new VariableTerm(engine);
            if ( !a5.unify(new ListTerm(a6, s1), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        p1 = new PRED_write_qlf_1(a6, cont);
        p2 = new PRED_nl_0(p1);
        p3 = new PRED_write_1(a4, p2);
        p4 = new PRED_write_1(s3, p3);
        p5 = new PRED_write_1(a2, p4);
        p6 = new PRED_write_1(s3, p5);
        return new PRED_write_1(s2, p6);
    }

    public int arity() { return 1; }

    public String toString() {
        return "write_semantics(" + arg1 + ")";
    }
}

