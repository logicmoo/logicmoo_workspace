package shef.nlp.supple.prolog.cafe;
import jp.ac.kobe_u.cs.prolog.lang.*;
import jp.ac.kobe_u.cs.prolog.builtin.*;

/*
 * *** Please do not edit ! ***
 * @(#) PRED_update_match_element_3.java
 * @procedure update_match_element/3 in update_name_match.pl
 */

/*
 * @version Prolog Cafe 0.8 November 2003
 * @author Mutsunori Banbara (banbara@kobe-u.ac.jp)
 * @author Naoyuki Tamura    (tamura@kobe-u.ac.jp)
 */

public class PRED_update_match_element_3 extends Predicate {
    static PRED_update_match_element_3 entry_code;
    static Predicate update_match_element_3_1 = new PRED_update_match_element_3_1();
    static Predicate update_match_element_3_2 = new PRED_update_match_element_3_2();
    static Predicate update_match_element_3_3 = new PRED_update_match_element_3_3();
    static Predicate update_match_element_3_var = new PRED_update_match_element_3_var();

    public Term arg1, arg2, arg3;

    public PRED_update_match_element_3(Term a1, Term a2, Term a3, Predicate cont) {
        arg1 = a1; 
        arg2 = a2; 
        arg3 = a3; 
        this.cont = cont;
    }

    public PRED_update_match_element_3(){}
    public void setArgument(Term[] args, Predicate cont) {
        arg1 = args[0]; 
        arg2 = args[1]; 
        arg3 = args[2]; 
        this.cont = cont;
    }

    public Predicate exec(Prolog engine) {
        entry_code = this;
        engine.aregs[1] = arg1;
        engine.aregs[2] = arg2;
        engine.aregs[3] = arg3;
        engine.cont = cont;
        return call(engine);
    }

    public Predicate call(Prolog engine) {
        engine.setB0();
        return engine.switch_on_term(
                                   update_match_element_3_var,
                                   update_match_element_3_3,
                                   update_match_element_3_3,
                                   update_match_element_3_3,
                                   update_match_element_3_var
                                   );
    }

    public int arity() { return 3; }

    public String toString() {
        return "update_match_element(" + arg1 + ", " + arg2 + ", " + arg3 + ")";
    }
}

class PRED_update_match_element_3_var extends PRED_update_match_element_3 {
    static Predicate update_match_element_3_var_1 = new PRED_update_match_element_3_var_1();

    public Predicate exec(Prolog engine) {
        return engine.jtry(update_match_element_3_1, update_match_element_3_var_1);
    }
}

class PRED_update_match_element_3_var_1 extends PRED_update_match_element_3 {
    static Predicate update_match_element_3_var_2 = new PRED_update_match_element_3_var_2();

    public Predicate exec(Prolog engine) {
        return engine.retry(update_match_element_3_2, update_match_element_3_var_2);
    }
}

class PRED_update_match_element_3_var_2 extends PRED_update_match_element_3 {

    public Predicate exec(Prolog engine) {
        return engine.trust(update_match_element_3_3);
    }
}

class PRED_update_match_element_3_1 extends PRED_update_match_element_3 {
    static SymbolTerm s1 = SymbolTerm.makeSymbol("[]");
    static SymbolTerm f2 = SymbolTerm.makeSymbol("name_match", 2);

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        a3 = engine.aregs[3].dereference();
        Predicate cont = engine.cont;

        if ( a1.isList() ){
            a4 = ((ListTerm)a1).car();
            a5 = ((ListTerm)a1).cdr();
        } else if ( a1.isVariable() ){
            a4 = new VariableTerm(engine);
            a5 = new VariableTerm(engine);
            if ( !a1.unify(new ListTerm(a4, a5), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a5.isList() ){
            a6 = ((ListTerm)a5).car();
            a7 = ((ListTerm)a5).cdr();
        } else if ( a5.isVariable() ){
            a6 = new VariableTerm(engine);
            a7 = new VariableTerm(engine);
            if ( !a5.unify(new ListTerm(a6, a7), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a7.isList() ){
            a8 = ((ListTerm)a7).car();
            if ( !s1.unify(((ListTerm)a7).cdr(), engine.trail) )
                return engine.fail();
        } else if ( a7.isVariable() ){
            a8 = new VariableTerm(engine);
            if ( !a7.unify(new ListTerm(a8, s1), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a8.isStructure() ){
            if (! f2.strictEqual(((StructureTerm)a8).functor()) )
                return engine.fail();
            Term[] args = ((StructureTerm)a8).args();
            a9 = args[0];
            a10 = args[1];
        } else if (a8.isVariable() ){
            a9 = new VariableTerm(engine);
            a10 = new VariableTerm(engine);
            Term[] args = {a9, a10};
            if ( !a8.unify(new StructureTerm(f2, args), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a2.isList() ){
            if ( !a4.unify(((ListTerm)a2).car(), engine.trail) )
                return engine.fail();
            a11 = ((ListTerm)a2).cdr();
        } else if ( a2.isVariable() ){
            a11 = new VariableTerm(engine);
            if ( !a2.unify(new ListTerm(a4, a11), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a11.isList() ){
            if ( !a6.unify(((ListTerm)a11).car(), engine.trail) )
                return engine.fail();
            a12 = ((ListTerm)a11).cdr();
        } else if ( a11.isVariable() ){
            a12 = new VariableTerm(engine);
            if ( !a11.unify(new ListTerm(a6, a12), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a12.isList() ){
            a13 = ((ListTerm)a12).car();
            a14 = ((ListTerm)a12).cdr();
        } else if ( a12.isVariable() ){
            a13 = new VariableTerm(engine);
            a14 = new VariableTerm(engine);
            if ( !a12.unify(new ListTerm(a13, a14), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a3.isList() ){
            if ( !a4.unify(((ListTerm)a3).car(), engine.trail) )
                return engine.fail();
            a15 = ((ListTerm)a3).cdr();
        } else if ( a3.isVariable() ){
            a15 = new VariableTerm(engine);
            if ( !a3.unify(new ListTerm(a4, a15), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a15.isList() ){
            if ( !a6.unify(((ListTerm)a15).car(), engine.trail) )
                return engine.fail();
            a16 = ((ListTerm)a15).cdr();
        } else if ( a15.isVariable() ){
            a16 = new VariableTerm(engine);
            if ( !a15.unify(new ListTerm(a6, a16), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a16.isList() ){
            a17 = ((ListTerm)a16).car();
            if ( !a14.unify(((ListTerm)a16).cdr(), engine.trail) )
                return engine.fail();
        } else if ( a16.isVariable() ){
            a17 = new VariableTerm(engine);
            if ( !a16.unify(new ListTerm(a17, a14), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a17.isList() ){
            if ( !a8.unify(((ListTerm)a17).car(), engine.trail) )
                return engine.fail();
            if ( !a13.unify(((ListTerm)a17).cdr(), engine.trail) )
                return engine.fail();
        } else if ( a17.isVariable() ){
            if ( !a17.unify(new ListTerm(a8, a13), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a8.isStructure() ){
            if (! f2.strictEqual(((StructureTerm)a8).functor()) )
                return engine.fail();
            Term[] args = ((StructureTerm)a8).args();
            if ( !a9.unify(args[0],  engine.trail) )
                return engine.fail();
            if ( !a10.unify(args[1],  engine.trail) )
                return engine.fail();
        } else if (a8.isVariable() ){
            Term[] args = {a9, a10};
            if ( !a8.unify(new StructureTerm(f2, args), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        return new PRED_$neck_cut_0(cont);
    }
}

class PRED_update_match_element_3_2 extends PRED_update_match_element_3 {
    static SymbolTerm s1 = SymbolTerm.makeSymbol("[]");
    static SymbolTerm f2 = SymbolTerm.makeSymbol("name_match", 2);

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        a3 = engine.aregs[3].dereference();
        Predicate cont = engine.cont;

        if ( a1.isList() ){
            a4 = ((ListTerm)a1).car();
            a5 = ((ListTerm)a1).cdr();
        } else if ( a1.isVariable() ){
            a4 = new VariableTerm(engine);
            a5 = new VariableTerm(engine);
            if ( !a1.unify(new ListTerm(a4, a5), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a5.isList() ){
            a6 = ((ListTerm)a5).car();
            a7 = ((ListTerm)a5).cdr();
        } else if ( a5.isVariable() ){
            a6 = new VariableTerm(engine);
            a7 = new VariableTerm(engine);
            if ( !a5.unify(new ListTerm(a6, a7), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a7.isList() ){
            a8 = ((ListTerm)a7).car();
            if ( !s1.unify(((ListTerm)a7).cdr(), engine.trail) )
                return engine.fail();
        } else if ( a7.isVariable() ){
            a8 = new VariableTerm(engine);
            if ( !a7.unify(new ListTerm(a8, s1), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a8.isStructure() ){
            if (! f2.strictEqual(((StructureTerm)a8).functor()) )
                return engine.fail();
            Term[] args = ((StructureTerm)a8).args();
            a9 = args[0];
            a10 = args[1];
        } else if (a8.isVariable() ){
            a9 = new VariableTerm(engine);
            a10 = new VariableTerm(engine);
            Term[] args = {a9, a10};
            if ( !a8.unify(new StructureTerm(f2, args), engine.trail) )
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
        if ( a12.isList() ){
            a13 = ((ListTerm)a12).car();
            a14 = ((ListTerm)a12).cdr();
        } else if ( a12.isVariable() ){
            a13 = new VariableTerm(engine);
            a14 = new VariableTerm(engine);
            if ( !a12.unify(new ListTerm(a13, a14), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a14.isList() ){
            a15 = ((ListTerm)a14).car();
            a16 = ((ListTerm)a14).cdr();
        } else if ( a14.isVariable() ){
            a15 = new VariableTerm(engine);
            a16 = new VariableTerm(engine);
            if ( !a14.unify(new ListTerm(a15, a16), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a3.isList() ){
            if ( !a11.unify(((ListTerm)a3).car(), engine.trail) )
                return engine.fail();
            a17 = ((ListTerm)a3).cdr();
        } else if ( a3.isVariable() ){
            a17 = new VariableTerm(engine);
            if ( !a3.unify(new ListTerm(a11, a17), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a17.isList() ){
            if ( !a13.unify(((ListTerm)a17).car(), engine.trail) )
                return engine.fail();
            a18 = ((ListTerm)a17).cdr();
        } else if ( a17.isVariable() ){
            a18 = new VariableTerm(engine);
            if ( !a17.unify(new ListTerm(a13, a18), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        if ( a18.isList() ){
            if ( !a15.unify(((ListTerm)a18).car(), engine.trail) )
                return engine.fail();
            a19 = ((ListTerm)a18).cdr();
        } else if ( a18.isVariable() ){
            a19 = new VariableTerm(engine);
            if ( !a18.unify(new ListTerm(a15, a19), engine.trail) )
                return engine.fail();
        } else {
            return engine.fail();
        }
        Term[] h3 = {a9, a10};
        a23 = new StructureTerm(f2, h3);
        a22 = new ListTerm(a23, s1);
        a21 = new ListTerm(a6, a22);
        a20 = new ListTerm(a4, a21);
        engine.aregs[1] = a20;
        engine.aregs[2] = a16;
        engine.aregs[3] = a19;
        engine.cont = cont;
        return entry_code.call(engine);
    }
}

class PRED_update_match_element_3_3 extends PRED_update_match_element_3 {
    static SymbolTerm s1 = SymbolTerm.makeSymbol("[]");

    public Predicate exec(Prolog engine) {
        Term a1, a2, a3;
        a1 = engine.aregs[1].dereference();
        a2 = engine.aregs[2].dereference();
        a3 = engine.aregs[3].dereference();
        Predicate cont = engine.cont;

        if ( !s1.unify(a2, engine.trail) ) return engine.fail();
        if ( !s1.unify(a3, engine.trail) ) return engine.fail();
        return cont;
    }
}

