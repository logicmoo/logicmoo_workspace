package shef.nlp.supple.prolog.cafe;
import jp.ac.kobe_u.cs.prolog.lang.*;
import jp.ac.kobe_u.cs.prolog.builtin.*;

/*
 * *** Please do not edit ! ***
 * @(#) PRED_generate_uninstantiated_2.java
 * @procedure generate_uninstantiated/2 in update_name_match.pl
 */

/*
 * @version Prolog Cafe 0.8 November 2003
 * @author Mutsunori Banbara (banbara@kobe-u.ac.jp)
 * @author Naoyuki Tamura    (tamura@kobe-u.ac.jp)
 */

public class PRED_generate_uninstantiated_2 extends Predicate {
    static PRED_generate_uninstantiated_2 entry_code;
    static Predicate fail_0 = new PRED_fail_0();
    static Predicate generate_uninstantiated_2_1 = new PRED_generate_uninstantiated_2_1();
    static Predicate generate_uninstantiated_2_2 = new PRED_generate_uninstantiated_2_2();
    static Predicate generate_uninstantiated_2_var = new PRED_generate_uninstantiated_2_var();

    public Term arg1, arg2;

    public PRED_generate_uninstantiated_2(Term a1, Term a2, Predicate cont) {
        arg1 = a1; 
        arg2 = a2; 
        this.cont = cont;
    }

    public PRED_generate_uninstantiated_2(){}
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
                                   generate_uninstantiated_2_var,
                                   fail_0,
                                   generate_uninstantiated_2_1,
                                   fail_0,
                                   generate_uninstantiated_2_2
                                   );
    }

    public int arity() { return 2; }

    public String toString() {
        return "generate_uninstantiated(" + arg1 + ", " + arg2 + ")";
    }
}

class PRED_generate_uninstantiated_2_var extends PRED_generate_uninstantiated_2 {
    static Predicate generate_uninstantiated_2_var_1 = new PRED_generate_uninstantiated_2_var_1();

    public Predicate exec(Prolog engine) {
        return engine.jtry(generate_uninstantiated_2_1, generate_uninstantiated_2_var_1);
    }
}

class PRED_generate_uninstantiated_2_var_1 extends PRED_generate_uninstantiated_2 {

    public Predicate exec(Prolog engine) {
        return engine.trust(generate_uninstantiated_2_2);
    }
}

class PRED_generate_uninstantiated_2_1 extends PRED_generate_uninstantiated_2 {
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

class PRED_generate_uninstantiated_2_2 extends PRED_generate_uninstantiated_2 {
    static SymbolTerm s1 = SymbolTerm.makeSymbol("[]");
    static SymbolTerm f2 = SymbolTerm.makeSymbol("match", 2);
    static SymbolTerm f3 = SymbolTerm.makeSymbol("name_match", 2);

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15;
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
        } else if (a9.isVariable() ){
            a10 = new VariableTerm(engine);
            Term[] args = {a10, new VariableTerm(engine)};
            if ( !a9.unify(new StructureTerm(f2, args), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a2.isList() ){
            a11 = ((ListTerm)a2).car();
            a12 = ((ListTerm)a2).cdr();
        } else if ( a2.isVariable() ){
            a11 = new VariableTerm(engine);
            a12 = new VariableTerm(engine);
            if ( !a2.unify(new ListTerm(a11, a12), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a11.isList() ){
            if ( !a5.unify(((ListTerm)a11).car(), engine.trail) )
                return engine.fail();
            a13 = ((ListTerm)a11).cdr();
        } else if ( a11.isVariable() ){
            a13 = new VariableTerm(engine);
            if ( !a11.unify(new ListTerm(a5, a13), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a13.isList() ){
            if ( !a7.unify(((ListTerm)a13).car(), engine.trail) )
                return engine.fail();
            a14 = ((ListTerm)a13).cdr();
        } else if ( a13.isVariable() ){
            a14 = new VariableTerm(engine);
            if ( !a13.unify(new ListTerm(a7, a14), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a14.isList() ){
            a15 = ((ListTerm)a14).car();
            if ( !s1.unify(((ListTerm)a14).cdr(), engine.trail) )
                return engine.fail();
        } else if ( a14.isVariable() ){
            a15 = new VariableTerm(engine);
            if ( !a14.unify(new ListTerm(a15, s1), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a15.isStructure() ){
            if (! f3.strictEqual(((StructureTerm)a15).functor()) )
                return engine.fail();
            Term[] args = ((StructureTerm)a15).args();
            if ( !a10.unify(args[0],  engine.trail) )
                return engine.fail();
        } else if (a15.isVariable() ){
            Term[] args = {a10, new VariableTerm(engine)};
            if ( !a15.unify(new StructureTerm(f3, args), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        engine.aregs[1] = a4;
        engine.aregs[2] = a12;
        engine.cont = cont;
        return entry_code.call(engine);
    }
}

