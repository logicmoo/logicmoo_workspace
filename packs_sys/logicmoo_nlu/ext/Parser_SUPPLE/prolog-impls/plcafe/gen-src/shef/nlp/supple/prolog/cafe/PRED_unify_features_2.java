package shef.nlp.supple.prolog.cafe;
import jp.ac.kobe_u.cs.prolog.lang.*;
import jp.ac.kobe_u.cs.prolog.builtin.*;

/*
 * *** Please do not edit ! ***
 * @(#) PRED_unify_features_2.java
 * @procedure unify_features/2 in compile_grammar.pl
 */

/*
 * @version Prolog Cafe 0.8 November 2003
 * @author Mutsunori Banbara (banbara@kobe-u.ac.jp)
 * @author Naoyuki Tamura    (tamura@kobe-u.ac.jp)
 */

public class PRED_unify_features_2 extends Predicate {
    static Predicate fail_0 = new PRED_fail_0();
    static Predicate unify_features_2_1 = new PRED_unify_features_2_1();
    static Predicate unify_features_2_2 = new PRED_unify_features_2_2();
    static Predicate unify_features_2_3 = new PRED_unify_features_2_3();
    static Predicate unify_features_2_4 = new PRED_unify_features_2_4();
    static Predicate unify_features_2_lis = new PRED_unify_features_2_lis();
    static Predicate unify_features_2_var = new PRED_unify_features_2_var();

    public Term arg1, arg2;

    public PRED_unify_features_2(Term a1, Term a2, Predicate cont) {
        arg1 = a1; 
        arg2 = a2; 
        this.cont = cont;
    }

    public PRED_unify_features_2(){}
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
                                   unify_features_2_var,
                                   fail_0,
                                   unify_features_2_1,
                                   fail_0,
                                   unify_features_2_lis
                                   );
    }

    public int arity() { return 2; }

    public String toString() {
        return "unify_features(" + arg1 + ", " + arg2 + ")";
    }
}

class PRED_unify_features_2_var extends PRED_unify_features_2 {
    static Predicate unify_features_2_var_1 = new PRED_unify_features_2_var_1();

    public Predicate exec(Prolog engine) {
        return engine.jtry(unify_features_2_1, unify_features_2_var_1);
    }
}

class PRED_unify_features_2_var_1 extends PRED_unify_features_2 {
    static Predicate unify_features_2_var_2 = new PRED_unify_features_2_var_2();

    public Predicate exec(Prolog engine) {
        return engine.retry(unify_features_2_2, unify_features_2_var_2);
    }
}

class PRED_unify_features_2_var_2 extends PRED_unify_features_2 {
    static Predicate unify_features_2_var_3 = new PRED_unify_features_2_var_3();

    public Predicate exec(Prolog engine) {
        return engine.retry(unify_features_2_3, unify_features_2_var_3);
    }
}

class PRED_unify_features_2_var_3 extends PRED_unify_features_2 {

    public Predicate exec(Prolog engine) {
        return engine.trust(unify_features_2_4);
    }
}

class PRED_unify_features_2_lis extends PRED_unify_features_2 {
    static Predicate unify_features_2_lis_1 = new PRED_unify_features_2_lis_1();

    public Predicate exec(Prolog engine) {
        return engine.jtry(unify_features_2_2, unify_features_2_lis_1);
    }
}

class PRED_unify_features_2_lis_1 extends PRED_unify_features_2 {
    static Predicate unify_features_2_lis_2 = new PRED_unify_features_2_lis_2();

    public Predicate exec(Prolog engine) {
        return engine.retry(unify_features_2_3, unify_features_2_lis_2);
    }
}

class PRED_unify_features_2_lis_2 extends PRED_unify_features_2 {

    public Predicate exec(Prolog engine) {
        return engine.trust(unify_features_2_4);
    }
}

class PRED_unify_features_2_1 extends PRED_unify_features_2 {
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

class PRED_unify_features_2_2 extends PRED_unify_features_2 {
    static SymbolTerm s1 = SymbolTerm.makeSymbol("Duplicated feature in rule");
    static SymbolTerm s2 = SymbolTerm.makeSymbol("[]");

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4, a5;
        Predicate p1;
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
        a5 = new ListTerm(a3, s2);
        p1 = new PRED_error_2(s1, a5, cont);
        return new PRED_member_2(a3, a4, p1);
    }
}

class PRED_unify_features_2_3 extends PRED_unify_features_2 {

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4, a5;
        Predicate p1, p2, p3;
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
        p1 = new PRED_$cut_1(a5, cont);
        p2 = new PRED_unify_features_2(a4, a2, p1);
        p3 = new PRED_member_2(a3, a2, p2);
        return new PRED_$get_level_1(a5, p3);
    }
}

class PRED_unify_features_2_4 extends PRED_unify_features_2 {
    static SymbolTerm s1 = SymbolTerm.makeSymbol("Feature in rule not defined in feature table");
    static SymbolTerm s2 = SymbolTerm.makeSymbol("[]");

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        Predicate cont = engine.cont;

        if ( a1.isList() ){
            a3 = ((ListTerm)a1).car();
        } else if ( a1.isVariable() ){
            a3 = new VariableTerm(engine);
            if ( !a1.unify(new ListTerm(a3, new VariableTerm(engine)), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        a4 = new ListTerm(a3, s2);
        return new PRED_error_2(s1, a4, cont);
    }
}

