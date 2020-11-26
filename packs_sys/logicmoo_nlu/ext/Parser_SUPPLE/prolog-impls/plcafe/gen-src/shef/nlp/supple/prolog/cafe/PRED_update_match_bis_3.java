package shef.nlp.supple.prolog.cafe;
import jp.ac.kobe_u.cs.prolog.lang.*;
import jp.ac.kobe_u.cs.prolog.builtin.*;

/*
 * *** Please do not edit ! ***
 * @(#) PRED_update_match_bis_3.java
 * @procedure update_match_bis/3 in update_name_match.pl
 */

/*
 * @version Prolog Cafe 0.8 November 2003
 * @author Mutsunori Banbara (banbara@kobe-u.ac.jp)
 * @author Naoyuki Tamura    (tamura@kobe-u.ac.jp)
 */

public class PRED_update_match_bis_3 extends Predicate {
    static Predicate update_match_bis_3_1 = new PRED_update_match_bis_3_1();
    static Predicate update_match_bis_3_2 = new PRED_update_match_bis_3_2();
    static Predicate update_match_bis_3_sub_1 = new PRED_update_match_bis_3_sub_1();

    public Term arg1, arg2, arg3;

    public PRED_update_match_bis_3(Term a1, Term a2, Term a3, Predicate cont) {
        arg1 = a1; 
        arg2 = a2; 
        arg3 = a3; 
        this.cont = cont;
    }

    public PRED_update_match_bis_3(){}
    public void setArgument(Term[] args, Predicate cont) {
        arg1 = args[0]; 
        arg2 = args[1]; 
        arg3 = args[2]; 
        this.cont = cont;
    }

    public Predicate exec(Prolog engine) {
        engine.aregs[1] = arg1;
        engine.aregs[2] = arg2;
        engine.aregs[3] = arg3;
        engine.cont = cont;
        return call(engine);
    }

    public Predicate call(Prolog engine) {
        engine.setB0();
        return engine.jtry(update_match_bis_3_1, update_match_bis_3_sub_1);
    }

    public int arity() { return 3; }

    public String toString() {
        return "update_match_bis(" + arg1 + ", " + arg2 + ", " + arg3 + ")";
    }
}

class PRED_update_match_bis_3_sub_1 extends PRED_update_match_bis_3 {

    public Predicate exec(Prolog engine) {
        return engine.trust(update_match_bis_3_2);
    }
}

class PRED_update_match_bis_3_1 extends PRED_update_match_bis_3 {
    static SymbolTerm s1 = SymbolTerm.makeSymbol("[]");

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        a3 = engine.aregs[3].dereference();
        Predicate cont = engine.cont;

        if ( !s1.unify(a2, engine.trail) ) return engine.fail();
        if ( !a1.unify(a3, engine.trail) ) return engine.fail();
        return cont;
    }
}

class PRED_update_match_bis_3_2 extends PRED_update_match_bis_3 {
    static SymbolTerm s1 = SymbolTerm.makeSymbol("[]");
    static SymbolTerm f2 = SymbolTerm.makeSymbol("name_match", 2);

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17;
        Predicate p1;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        a3 = engine.aregs[3].dereference();
        Predicate cont = engine.cont;

        if ( a2.isList() ){
            a4 = ((ListTerm)a2).car();
            a5 = ((ListTerm)a2).cdr();
        } else if ( a2.isVariable() ){
            a4 = new VariableTerm(engine);
            a5 = new VariableTerm(engine);
            if ( !a2.unify(new ListTerm(a4, a5), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a4.isList() ){
            a6 = ((ListTerm)a4).car();
            a7 = ((ListTerm)a4).cdr();
        } else if ( a4.isVariable() ){
            a6 = new VariableTerm(engine);
            a7 = new VariableTerm(engine);
            if ( !a4.unify(new ListTerm(a6, a7), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a7.isList() ){
            a8 = ((ListTerm)a7).car();
            a9 = ((ListTerm)a7).cdr();
        } else if ( a7.isVariable() ){
            a8 = new VariableTerm(engine);
            a9 = new VariableTerm(engine);
            if ( !a7.unify(new ListTerm(a8, a9), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a9.isList() ){
            a10 = ((ListTerm)a9).car();
            if ( !s1.unify(((ListTerm)a9).cdr(), engine.trail) )
                return engine.fail();
        } else if ( a9.isVariable() ){
            a10 = new VariableTerm(engine);
            if ( !a9.unify(new ListTerm(a10, s1), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a10.isStructure() ){
            if (! f2.strictEqual(((StructureTerm)a10).functor()) )
                return engine.fail();
            Term[] args = ((StructureTerm)a10).args();
            a11 = args[0];
            a12 = args[1];
        } else if (a10.isVariable() ){
            a11 = new VariableTerm(engine);
            a12 = new VariableTerm(engine);
            Term[] args = {a11, a12};
            if ( !a10.unify(new StructureTerm(f2, args), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        Term[] h3 = {a11, a12};
        a16 = new StructureTerm(f2, h3);
        a15 = new ListTerm(a16, s1);
        a14 = new ListTerm(a8, a15);
        a13 = new ListTerm(a6, a14);
        a17 = new VariableTerm(engine);
        p1 = new PRED_update_match_bis_3(a17, a5, a3, cont);
        return new PRED_update_match_element_3(a13, a1, a17, p1);
    }
}

