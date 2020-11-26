package shef.nlp.supple.prolog.cafe;
import jp.ac.kobe_u.cs.prolog.lang.*;
import jp.ac.kobe_u.cs.prolog.builtin.*;

/*
 * *** Please do not edit ! ***
 * @(#) PRED_compile_features_2.java
 * @procedure compile_features/2 in compile_grammar.pl
 */

/*
 * @version Prolog Cafe 0.8 November 2003
 * @author Mutsunori Banbara (banbara@kobe-u.ac.jp)
 * @author Naoyuki Tamura    (tamura@kobe-u.ac.jp)
 */

public class PRED_compile_features_2 extends Predicate {
    static PRED_compile_features_2 entry_code;
    static Predicate fail_0 = new PRED_fail_0();
    static Predicate compile_features_2_1 = new PRED_compile_features_2_1();
    static Predicate compile_features_2_2 = new PRED_compile_features_2_2();
    static Predicate compile_features_2_3 = new PRED_compile_features_2_3();
    static Predicate compile_features_2_4 = new PRED_compile_features_2_4();
    static Predicate compile_features_2_5 = new PRED_compile_features_2_5();
    static Predicate compile_features_2_lis = new PRED_compile_features_2_lis();
    static Predicate compile_features_2_var = new PRED_compile_features_2_var();

    public Term arg1, arg2;

    public PRED_compile_features_2(Term a1, Term a2, Predicate cont) {
        arg1 = a1; 
        arg2 = a2; 
        this.cont = cont;
    }

    public PRED_compile_features_2(){}
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
                                   compile_features_2_var,
                                   fail_0,
                                   compile_features_2_1,
                                   fail_0,
                                   compile_features_2_lis
                                   );
    }

    public int arity() { return 2; }

    public String toString() {
        return "compile_features(" + arg1 + ", " + arg2 + ")";
    }
}

class PRED_compile_features_2_var extends PRED_compile_features_2 {
    static Predicate compile_features_2_var_1 = new PRED_compile_features_2_var_1();

    public Predicate exec(Prolog engine) {
        return engine.jtry(compile_features_2_1, compile_features_2_var_1);
    }
}

class PRED_compile_features_2_var_1 extends PRED_compile_features_2 {
    static Predicate compile_features_2_var_2 = new PRED_compile_features_2_var_2();

    public Predicate exec(Prolog engine) {
        return engine.retry(compile_features_2_2, compile_features_2_var_2);
    }
}

class PRED_compile_features_2_var_2 extends PRED_compile_features_2 {
    static Predicate compile_features_2_var_3 = new PRED_compile_features_2_var_3();

    public Predicate exec(Prolog engine) {
        return engine.retry(compile_features_2_3, compile_features_2_var_3);
    }
}

class PRED_compile_features_2_var_3 extends PRED_compile_features_2 {
    static Predicate compile_features_2_var_4 = new PRED_compile_features_2_var_4();

    public Predicate exec(Prolog engine) {
        return engine.retry(compile_features_2_4, compile_features_2_var_4);
    }
}

class PRED_compile_features_2_var_4 extends PRED_compile_features_2 {

    public Predicate exec(Prolog engine) {
        return engine.trust(compile_features_2_5);
    }
}

class PRED_compile_features_2_lis extends PRED_compile_features_2 {
    static Predicate compile_features_2_lis_1 = new PRED_compile_features_2_lis_1();

    public Predicate exec(Prolog engine) {
        return engine.jtry(compile_features_2_2, compile_features_2_lis_1);
    }
}

class PRED_compile_features_2_lis_1 extends PRED_compile_features_2 {
    static Predicate compile_features_2_lis_2 = new PRED_compile_features_2_lis_2();

    public Predicate exec(Prolog engine) {
        return engine.retry(compile_features_2_3, compile_features_2_lis_2);
    }
}

class PRED_compile_features_2_lis_2 extends PRED_compile_features_2 {
    static Predicate compile_features_2_lis_3 = new PRED_compile_features_2_lis_3();

    public Predicate exec(Prolog engine) {
        return engine.retry(compile_features_2_4, compile_features_2_lis_3);
    }
}

class PRED_compile_features_2_lis_3 extends PRED_compile_features_2 {

    public Predicate exec(Prolog engine) {
        return engine.trust(compile_features_2_5);
    }
}

class PRED_compile_features_2_1 extends PRED_compile_features_2 {
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

class PRED_compile_features_2_2 extends PRED_compile_features_2 {
    static SymbolTerm s1 = SymbolTerm.makeSymbol("[]");

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4, a5, a6, a7, a8, a9;
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
        a7 = new ListTerm(a6, s1);
        a8 = new VariableTerm(engine);
        a9 = new VariableTerm(engine);
        p1 = new PRED_$dummy_compile_grammar46pl_2_4(a2, a8, a9, new VariableTerm(engine), cont);
        p2 = new PRED_compile_features_2(a4, a9, p1);
        p3 = new PRED_compile_features_2(a7, a8, p2);
        p4 = new PRED_member_2(a6, a3, p3);
        p5 = new PRED_$cut_1(a5, p4);
        p6 = new PRED_is_list_1(a3, p5);
        return new PRED_$get_level_1(a5, p6);
    }
}

class PRED_compile_features_2_3 extends PRED_compile_features_2 {
    static SymbolTerm f1 = SymbolTerm.makeSymbol("{}", 1);

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4;
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
        if ( a3.isStructure() ){
            if (! f1.strictEqual(((StructureTerm)a3).functor()) )
                return engine.fail();
            Term[] args = ((StructureTerm)a3).args();
        } else if (a3.isVariable() ){
            Term[] args = {new VariableTerm(engine)};
            if ( !a3.unify(new StructureTerm(f1, args), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        engine.aregs[1] = a4;
        engine.aregs[2] = a2;
        engine.cont = cont;
        return entry_code.call(engine);
    }
}

class PRED_compile_features_2_4 extends PRED_compile_features_2 {
    static SymbolTerm f1 = SymbolTerm.makeSymbol("{}", 1);
    static SymbolTerm s2 = SymbolTerm.makeSymbol("[]");

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4, a5, a6, a7, a8;
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
        if ( a3.isStructure() ){
            if (! f1.strictEqual(((StructureTerm)a3).functor()) )
                return engine.fail();
            Term[] args = ((StructureTerm)a3).args();
            a5 = args[0];
        } else if (a3.isVariable() ){
            a5 = new VariableTerm(engine);
            Term[] args = {a5};
            if ( !a3.unify(new StructureTerm(f1, args), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        a6 = new ListTerm(a5, s2);
        a7 = new VariableTerm(engine);
        a8 = new VariableTerm(engine);
        p1 = new PRED_$dummy_compile_grammar46pl_3_4(a2, a7, a8, new VariableTerm(engine), cont);
        p2 = new PRED_compile_features_2(a4, a8, p1);
        p3 = new PRED_compile_features_2(a6, a7, p2);
        return new PRED_$neck_cut_0(p3);
    }
}

class PRED_compile_features_2_5 extends PRED_compile_features_2 {

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12;
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
        if ( a2.isList() ){
            a5 = ((ListTerm)a2).car();
            a6 = ((ListTerm)a2).cdr();
        } else if ( a2.isVariable() ){
            a5 = new VariableTerm(engine);
            a6 = new VariableTerm(engine);
            if ( !a2.unify(new ListTerm(a5, a6), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        a7 = new VariableTerm(engine);
        a9 = new VariableTerm(engine);
        a10 = new VariableTerm(engine);
        a8 = new ListTerm(a9, a10);
        a11 = new VariableTerm(engine);
        a12 = new ListTerm(a9, a11);
        p1 = new PRED_compile_features_2(a4, a6, cont);
        p2 = new PRED_$cut_1(a7, p1);
        p3 = new PRED_$614646_2(a5, a12, p2);
        p4 = new PRED_unify_features_2(a10, a11, p3);
        p5 = new PRED_feature_table_2(a9, a11, p4);
        p6 = new PRED_$614646_2(a3, a8, p5);
        return new PRED_$get_level_1(a7, p6);
    }
}

