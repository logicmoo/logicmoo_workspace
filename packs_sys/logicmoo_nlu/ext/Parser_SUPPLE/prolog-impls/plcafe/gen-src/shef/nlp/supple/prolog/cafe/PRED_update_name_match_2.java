package shef.nlp.supple.prolog.cafe;
import jp.ac.kobe_u.cs.prolog.lang.*;
import jp.ac.kobe_u.cs.prolog.builtin.*;

/*
 * *** Please do not edit ! ***
 * @(#) PRED_update_name_match_2.java
 * @procedure update_name_match/2 in update_name_match.pl
 */

/*
 * @version Prolog Cafe 0.8 November 2003
 * @author Mutsunori Banbara (banbara@kobe-u.ac.jp)
 * @author Naoyuki Tamura    (tamura@kobe-u.ac.jp)
 */

public class PRED_update_name_match_2 extends Predicate {
    static Predicate fail_0 = new PRED_fail_0();
    static Predicate update_name_match_2_1 = new PRED_update_name_match_2_1();
    static Predicate update_name_match_2_2 = new PRED_update_name_match_2_2();
    static Predicate update_name_match_2_var = new PRED_update_name_match_2_var();

    public Term arg1, arg2;

    public PRED_update_name_match_2(Term a1, Term a2, Predicate cont) {
        arg1 = a1; 
        arg2 = a2; 
        this.cont = cont;
    }

    public PRED_update_name_match_2(){}
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
                                   update_name_match_2_var,
                                   fail_0,
                                   update_name_match_2_1,
                                   fail_0,
                                   update_name_match_2_2
                                   );
    }

    public int arity() { return 2; }

    public String toString() {
        return "update_name_match(" + arg1 + ", " + arg2 + ")";
    }
}

class PRED_update_name_match_2_var extends PRED_update_name_match_2 {
    static Predicate update_name_match_2_var_1 = new PRED_update_name_match_2_var_1();

    public Predicate exec(Prolog engine) {
        return engine.jtry(update_name_match_2_1, update_name_match_2_var_1);
    }
}

class PRED_update_name_match_2_var_1 extends PRED_update_name_match_2 {

    public Predicate exec(Prolog engine) {
        return engine.trust(update_name_match_2_2);
    }
}

class PRED_update_name_match_2_1 extends PRED_update_name_match_2 {
    static SymbolTerm s1 = SymbolTerm.makeSymbol("[]");

    public Predicate exec(Prolog engine) {
        Term a1, a2;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        Predicate cont = engine.cont;

        if ( !s1.unify(a1, engine.trail) ) return engine.fail();
        if ( !s1.unify(a2, engine.trail) ) return engine.fail();
        return cont;
    }
}

class PRED_update_name_match_2_2 extends PRED_update_name_match_2 {
    static SymbolTerm s1 = SymbolTerm.makeSymbol("[]");
    static SymbolTerm f2 = SymbolTerm.makeSymbol("match", 2);
    static SymbolTerm f3 = SymbolTerm.makeSymbol("list", 1);
    static SymbolTerm f6 = SymbolTerm.makeSymbol(":", 2);
    static SymbolTerm s7 = SymbolTerm.makeSymbol("shef.nlp.supple.prolog.cafe");
    static SymbolTerm f8 = SymbolTerm.makeSymbol("member", 2);

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30, a31, a32, a33, a34, a35, a36, a37;
        Predicate p1, p2, p3, p4;
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
        if ( a3.isList() ){
            a5 = ((ListTerm)a3).car();
            a6 = ((ListTerm)a3).cdr();
        } else if ( a3.isVariable() ){
            a5 = new VariableTerm(engine);
            a6 = new VariableTerm(engine);
            if ( !a3.unify(new ListTerm(a5, a6), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a6.isList() ){
            a7 = ((ListTerm)a6).car();
            a8 = ((ListTerm)a6).cdr();
        } else if ( a6.isVariable() ){
            a7 = new VariableTerm(engine);
            a8 = new VariableTerm(engine);
            if ( !a6.unify(new ListTerm(a7, a8), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a8.isList() ){
            a9 = ((ListTerm)a8).car();
            if ( !s1.unify(((ListTerm)a8).cdr(), engine.trail) )
                return engine.fail();
        } else if ( a8.isVariable() ){
            a9 = new VariableTerm(engine);
            if ( !a8.unify(new ListTerm(a9, s1), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a9.isStructure() ){
            if (! f2.strictEqual(((StructureTerm)a9).functor()) )
                return engine.fail();
            Term[] args = ((StructureTerm)a9).args();
            a10 = args[0];
            a11 = args[1];
        } else if (a9.isVariable() ){
            a10 = new VariableTerm(engine);
            a11 = new VariableTerm(engine);
            Term[] args = {a10, a11};
            if ( !a9.unify(new StructureTerm(f2, args), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a11.isStructure() ){
            if (! f3.strictEqual(((StructureTerm)a11).functor()) )
                return engine.fail();
            Term[] args = ((StructureTerm)a11).args();
            a12 = args[0];
        } else if (a11.isVariable() ){
            a12 = new VariableTerm(engine);
            Term[] args = {a12};
            if ( !a11.unify(new StructureTerm(f3, args), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        a14 = new VariableTerm(engine);
        a16 = new VariableTerm(engine);
        a19 = new VariableTerm(engine);
        Term[] h4 = {a12};
        a20 = new StructureTerm(f3, h4);
        Term[] h5 = {a19, a20};
        a18 = new StructureTerm(f2, h5);
        a17 = new ListTerm(a18, s1);
        a15 = new ListTerm(a16, a17);
        a13 = new ListTerm(a14, a15);
        Term[] h9 = {a12};
        a27 = new StructureTerm(f3, h9);
        Term[] h10 = {a19, a27};
        a26 = new StructureTerm(f2, h10);
        a25 = new ListTerm(a26, s1);
        a24 = new ListTerm(a16, a25);
        a23 = new ListTerm(a14, a24);
        Term[] h11 = {a23, a4};
        a22 = new StructureTerm(f8, h11);
        Term[] h12 = {s7, a22};
        a21 = new StructureTerm(f6, h12);
        a28 = new VariableTerm(engine);
        a29 = new VariableTerm(engine);
        Term[] h13 = {a12};
        a35 = new StructureTerm(f3, h13);
        Term[] h14 = {a10, a35};
        a34 = new StructureTerm(f2, h14);
        a33 = new ListTerm(a34, s1);
        a32 = new ListTerm(a7, a33);
        a31 = new ListTerm(a5, a32);
        a30 = new ListTerm(a31, a28);
        a36 = new VariableTerm(engine);
        a37 = new VariableTerm(engine);
        p1 = new PRED_append_3(a36, a37, a2, cont);
        p2 = new PRED_update_name_match_2(a29, a37, p1);
        p3 = new PRED_generate_name_match_2(a30, a36, p2);
        p4 = new PRED_delete_3(a28, a4, a29, p3);
        return new PRED_findall_3(a13, a21, a28, p4);
    }
}

