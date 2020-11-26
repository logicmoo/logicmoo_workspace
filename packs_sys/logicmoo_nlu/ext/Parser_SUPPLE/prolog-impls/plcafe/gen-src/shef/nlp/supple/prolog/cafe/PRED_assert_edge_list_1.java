package shef.nlp.supple.prolog.cafe;
import jp.ac.kobe_u.cs.prolog.lang.*;
import jp.ac.kobe_u.cs.prolog.builtin.*;

/*
 * *** Please do not edit ! ***
 * @(#) PRED_assert_edge_list_1.java
 * @procedure assert_edge_list/1 in parse_file.pl
 */

/*
 * @version Prolog Cafe 0.8 November 2003
 * @author Mutsunori Banbara (banbara@kobe-u.ac.jp)
 * @author Naoyuki Tamura    (tamura@kobe-u.ac.jp)
 */

public class PRED_assert_edge_list_1 extends Predicate {
    static Predicate fail_0 = new PRED_fail_0();
    static Predicate assert_edge_list_1_1 = new PRED_assert_edge_list_1_1();
    static Predicate assert_edge_list_1_2 = new PRED_assert_edge_list_1_2();
    static Predicate assert_edge_list_1_3 = new PRED_assert_edge_list_1_3();
    static Predicate assert_edge_list_1_lis = new PRED_assert_edge_list_1_lis();
    static Predicate assert_edge_list_1_var = new PRED_assert_edge_list_1_var();

    public Term arg1;

    public PRED_assert_edge_list_1(Term a1, Predicate cont) {
        arg1 = a1; 
        this.cont = cont;
    }

    public PRED_assert_edge_list_1(){}
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
                                   assert_edge_list_1_var,
                                   fail_0,
                                   assert_edge_list_1_1,
                                   fail_0,
                                   assert_edge_list_1_lis
                                   );
    }

    public int arity() { return 1; }

    public String toString() {
        return "assert_edge_list(" + arg1 + ")";
    }
}

class PRED_assert_edge_list_1_var extends PRED_assert_edge_list_1 {
    static Predicate assert_edge_list_1_var_1 = new PRED_assert_edge_list_1_var_1();

    public Predicate exec(Prolog engine) {
        return engine.jtry(assert_edge_list_1_1, assert_edge_list_1_var_1);
    }
}

class PRED_assert_edge_list_1_var_1 extends PRED_assert_edge_list_1 {
    static Predicate assert_edge_list_1_var_2 = new PRED_assert_edge_list_1_var_2();

    public Predicate exec(Prolog engine) {
        return engine.retry(assert_edge_list_1_2, assert_edge_list_1_var_2);
    }
}

class PRED_assert_edge_list_1_var_2 extends PRED_assert_edge_list_1 {

    public Predicate exec(Prolog engine) {
        return engine.trust(assert_edge_list_1_3);
    }
}

class PRED_assert_edge_list_1_lis extends PRED_assert_edge_list_1 {
    static Predicate assert_edge_list_1_lis_1 = new PRED_assert_edge_list_1_lis_1();

    public Predicate exec(Prolog engine) {
        return engine.jtry(assert_edge_list_1_2, assert_edge_list_1_lis_1);
    }
}

class PRED_assert_edge_list_1_lis_1 extends PRED_assert_edge_list_1 {

    public Predicate exec(Prolog engine) {
        return engine.trust(assert_edge_list_1_3);
    }
}

class PRED_assert_edge_list_1_1 extends PRED_assert_edge_list_1 {
    static SymbolTerm s1 = SymbolTerm.makeSymbol("[]");

    public Predicate exec(Prolog engine) {
        Term a1;
        a1 = engine.aregs[1].dereference();
        Predicate cont = engine.cont;

        if ( !s1.unify(a1, engine.trail) ) return engine.fail();
        return cont;
    }
}

class PRED_assert_edge_list_1_2 extends PRED_assert_edge_list_1 {
    static SymbolTerm f1 = SymbolTerm.makeSymbol("edge", 10);

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13;
        Predicate p1;
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
        if ( a2.isStructure() ){
            if (! f1.strictEqual(((StructureTerm)a2).functor()) )
                return engine.fail();
            Term[] args = ((StructureTerm)a2).args();
            a4 = args[0];
            a5 = args[1];
            a6 = args[2];
            a7 = args[3];
            a8 = args[4];
            a9 = args[5];
            a10 = args[6];
            a11 = args[7];
            a12 = args[8];
            a13 = args[9];
        } else if (a2.isVariable() ){
            a4 = new VariableTerm(engine);
            a5 = new VariableTerm(engine);
            a6 = new VariableTerm(engine);
            a7 = new VariableTerm(engine);
            a8 = new VariableTerm(engine);
            a9 = new VariableTerm(engine);
            a10 = new VariableTerm(engine);
            a11 = new VariableTerm(engine);
            a12 = new VariableTerm(engine);
            a13 = new VariableTerm(engine);
            Term[] args = {a4, a5, a6, a7, a8, a9, a10, a11, a12, a13};
            if ( !a2.unify(new StructureTerm(f1, args), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        p1 = new PRED_assert_edge_list_1(a3, cont);
        return new PRED_edge_10(a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, p1);
    }
}

class PRED_assert_edge_list_1_3 extends PRED_assert_edge_list_1 {
    static SymbolTerm f1 = SymbolTerm.makeSymbol("edge", 10);

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14;
        Predicate p1;
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
        if ( a2.isStructure() ){
            if (! f1.strictEqual(((StructureTerm)a2).functor()) )
                return engine.fail();
            Term[] args = ((StructureTerm)a2).args();
            a4 = args[0];
            a5 = args[1];
            a6 = args[2];
            a7 = args[3];
            a8 = args[4];
            a9 = args[5];
            a10 = args[6];
            a11 = args[7];
            a12 = args[8];
            a13 = args[9];
        } else if (a2.isVariable() ){
            a4 = new VariableTerm(engine);
            a5 = new VariableTerm(engine);
            a6 = new VariableTerm(engine);
            a7 = new VariableTerm(engine);
            a8 = new VariableTerm(engine);
            a9 = new VariableTerm(engine);
            a10 = new VariableTerm(engine);
            a11 = new VariableTerm(engine);
            a12 = new VariableTerm(engine);
            a13 = new VariableTerm(engine);
            Term[] args = {a4, a5, a6, a7, a8, a9, a10, a11, a12, a13};
            if ( !a2.unify(new StructureTerm(f1, args), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        Term[] h2 = {a4, a5, a6, a7, a8, a9, a10, a11, a12, a13};
        a14 = new StructureTerm(f1, h2);
        p1 = new PRED_assert_edge_list_1(a3, cont);
        return new PRED_assert_1(a14, p1);
    }
}

