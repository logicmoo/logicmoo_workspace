package shef.nlp.supple.prolog.cafe;
import jp.ac.kobe_u.cs.prolog.lang.*;
import jp.ac.kobe_u.cs.prolog.builtin.*;

/*
 * *** Please do not edit ! ***
 * @(#) PRED_replace_whitespace_2.java
 * @procedure replace_whitespace/2 in semantics.pl
 */

/*
 * @version Prolog Cafe 0.8 November 2003
 * @author Mutsunori Banbara (banbara@kobe-u.ac.jp)
 * @author Naoyuki Tamura    (tamura@kobe-u.ac.jp)
 */

public class PRED_replace_whitespace_2 extends Predicate {
    static PRED_replace_whitespace_2 entry_code;
    static Predicate fail_0 = new PRED_fail_0();
    static Predicate replace_whitespace_2_1 = new PRED_replace_whitespace_2_1();
    static Predicate replace_whitespace_2_2 = new PRED_replace_whitespace_2_2();
    static Predicate replace_whitespace_2_3 = new PRED_replace_whitespace_2_3();
    static Predicate replace_whitespace_2_lis = new PRED_replace_whitespace_2_lis();
    static Predicate replace_whitespace_2_var = new PRED_replace_whitespace_2_var();

    public Term arg1, arg2;

    public PRED_replace_whitespace_2(Term a1, Term a2, Predicate cont) {
        arg1 = a1; 
        arg2 = a2; 
        this.cont = cont;
    }

    public PRED_replace_whitespace_2(){}
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
                                   replace_whitespace_2_var,
                                   fail_0,
                                   replace_whitespace_2_1,
                                   fail_0,
                                   replace_whitespace_2_lis
                                   );
    }

    public int arity() { return 2; }

    public String toString() {
        return "replace_whitespace(" + arg1 + ", " + arg2 + ")";
    }
}

class PRED_replace_whitespace_2_var extends PRED_replace_whitespace_2 {
    static Predicate replace_whitespace_2_var_1 = new PRED_replace_whitespace_2_var_1();

    public Predicate exec(Prolog engine) {
        return engine.jtry(replace_whitespace_2_1, replace_whitespace_2_var_1);
    }
}

class PRED_replace_whitespace_2_var_1 extends PRED_replace_whitespace_2 {
    static Predicate replace_whitespace_2_var_2 = new PRED_replace_whitespace_2_var_2();

    public Predicate exec(Prolog engine) {
        return engine.retry(replace_whitespace_2_2, replace_whitespace_2_var_2);
    }
}

class PRED_replace_whitespace_2_var_2 extends PRED_replace_whitespace_2 {

    public Predicate exec(Prolog engine) {
        return engine.trust(replace_whitespace_2_3);
    }
}

class PRED_replace_whitespace_2_lis extends PRED_replace_whitespace_2 {
    static Predicate replace_whitespace_2_lis_1 = new PRED_replace_whitespace_2_lis_1();

    public Predicate exec(Prolog engine) {
        return engine.jtry(replace_whitespace_2_2, replace_whitespace_2_lis_1);
    }
}

class PRED_replace_whitespace_2_lis_1 extends PRED_replace_whitespace_2 {

    public Predicate exec(Prolog engine) {
        return engine.trust(replace_whitespace_2_3);
    }
}

class PRED_replace_whitespace_2_1 extends PRED_replace_whitespace_2 {
    static SymbolTerm s1 = SymbolTerm.makeSymbol("[]");

    public Predicate exec(Prolog engine) {
        Term a1, a2;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        Predicate cont = engine.cont;

        if ( !s1.unify(a1, engine.trail) ) return engine.fail();
        if ( !s1.unify(a2, engine.trail) ) return engine.fail();
        return new PRED_$neck_cut_0(cont);
    }
}

class PRED_replace_whitespace_2_2 extends PRED_replace_whitespace_2 {
    static IntegerTerm s1 = new IntegerTerm(32);
    static IntegerTerm s2 = new IntegerTerm(95);

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4;
        Predicate p1;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        Predicate cont = engine.cont;

        if ( a1.isList() ){
            if ( !s1.unify(((ListTerm)a1).car(), engine.trail) )
                return engine.fail();
            a3 = ((ListTerm)a1).cdr();
        } else if ( a1.isVariable() ){
            a3 = new VariableTerm(engine);
            if ( !a1.unify(new ListTerm(s1, a3), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a2.isList() ){
            if ( !s2.unify(((ListTerm)a2).car(), engine.trail) )
                return engine.fail();
            a4 = ((ListTerm)a2).cdr();
        } else if ( a2.isVariable() ){
            a4 = new VariableTerm(engine);
            if ( !a2.unify(new ListTerm(s2, a4), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        p1 = new PRED_replace_whitespace_2(a3, a4, cont);
        return new PRED_$neck_cut_0(p1);
    }
}

class PRED_replace_whitespace_2_3 extends PRED_replace_whitespace_2 {

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4, a5;
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
        if ( a2.isList() ){
            if ( !a3.unify(((ListTerm)a2).car(), engine.trail) )
                return engine.fail();
            a5 = ((ListTerm)a2).cdr();
        } else if ( a2.isVariable() ){
            a5 = new VariableTerm(engine);
            if ( !a2.unify(new ListTerm(a3, a5), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        engine.aregs[1] = a4;
        engine.aregs[2] = a5;
        engine.cont = cont;
        return entry_code.call(engine);
    }
}

